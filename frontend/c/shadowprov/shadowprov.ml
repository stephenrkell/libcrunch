(* Copyright (c) 2014--18,
 *  Stephen Kell        <stephen.kell@cl.cam.ac.uk>
 *)

open Cil
open Pretty
open Map
open Str
open Cilallocs
open Shadow
open Unix

(* We define a simple shadow value tool that does the following:
 * every integer and pointer value is shadowed.
 * Pointers are shadowed at creation with the one-byte value (char) 42.
 * Integers are shadowed at creation with the one-byte value (char) 0.
 * Casts, arithmetic and all other expressions simply propagate shadows,
 * choosing the leftmost in the case of binary operations.
 *)

(* Will fill these in during initializer *) 
class helperFunctionsFull = object(self)
    inherit helperFunctionsRecord as super
    val mutable makeShadow = emptyFunction "__make_shadow"
    val mutable fetchShadowInl = emptyFunction "__fetch_shadow_inl"
    val mutable storeShadowNonLocal = emptyFunction "__store_shadow_nonlocal"
    val mutable checkDeref = emptyFunction "__check_Deref"
    
    method getMakeShadow (_:unit) = makeShadow
    method getFetchShadowInl (_:unit) = fetchShadowInl
    method getStoreShadowNonLocal (_:unit) = storeShadowNonLocal
    method getCheckDeref (_:unit) = checkDeref
    method initializeFromFile f =
        super#initializeFromFile f;
        makeShadow <- findFunOrFail "__make_shadow" f.globals;
        fetchShadowInl <- findFunOrFail "__fetch_shadow_inl" f.globals;
        storeShadowNonLocal <- findFunOrFail "__store_shadow_nonlocal" f.globals;
        checkDeref <- findFunOrFail "__check_deref" f.globals
end

let helperFunctions = new helperFunctionsFull

(* FIXME: drop pure and const attrs for any shadow-passing/returning function. *)


(* This class does (unfortunately) two things:
 * (1) fills in the virtual methods of the base class,
 * such that the base-class visitor logic will instrument
 * the code to propagate shadow values -- but not to use them.
 * (2) actually use the shadows *)

class shadowProvVisitor
 = fun enclosingFile ->
       object(self)
   inherit shadowBasicVisitor enclosingFile
    (* primitiveTypeIsShadowed *)
                     (fun t -> match unrollType t with TInt(_) | TPtr(_) -> true | _ -> false)
    (* shadowCompName *) "shadow_byte"
    (* opaqueShadowableT *) ulongType
    (* helperHeaderNames *) ["liballocs_cil_inlines.h"; "shadowprov_helpers.h"]
    (* makeCallToMakeShadow *) (fun maybeLv -> fun exprs -> fun valE -> fun l -> Call(maybeLv,
            (Lval(Var((helperFunctions#getMakeShadow ()).svar),NoOffset)),
            exprs,
            l
        ))
    (* makeCallToFetchShadow *) (fun lv -> fun descr -> fun e -> fun l -> [
        Call( Some(lv),
           (Lval(Var((helperFunctions#getFetchShadowInl ()).svar),NoOffset)),
           [
               CastE(ulongType, e);
               (* where did we load it from? *)
               (match descr with
                    LoadedFrom(lh,lo) -> mkAddrOf (lh,lo)
                  | UnknownOrigin -> failwith "unknown origin expr"
                  | ExternObject(v) -> failwith "don't use extern object"
               );
               (* what's the pointee type? *)
               (CastE(voidPtrType, zero))
           ],
           l
        )
    ])
    (* helperFunctions *) (helperFunctions :> helperFunctionsRecord)
    (* descriptorExprForShadowableExpr *) (fun e -> (CastE(voidPtrType, zero)))
    (* defaultShadowValueForType *) (fun t -> (CastE(charType, zero)))
    (* shadowString *) "shadow"
   as super
    
   method makeStoreHelperCall e lv descr l = 
        let slv = self#ensureShadowLocalLval e descr
        in
        let (sExpr, preInstrs) = (Lval(slv), self#localShadowUpdateInstrs slv e descr l)
        in
        preInstrs @ 
        [Call( None, 
              Lval(Var((helperFunctions#getStoreShadowNonLocal ()).svar), NoOffset),
              [ 
                CastE(voidPtrType, mkAddrOf lv) ;
                CastE(ulongType, e);
                sExpr;
                CastE(voidPtrType, zero)
              ],
              l
        )]

    val currentFrameAddressVar = ref None
    method vfunc (f: fundec) : fundec visitAction =
        let tmpVar = Cil.makeTempVar f ~name:"__frame_address" ulongType
        in
        let (retT, argTs, isVa) = Hashtbl.find Cil.builtinFunctions "__builtin_frame_address"
        in
        let helperFunc = emptyFunction "__builtin_frame_address"
        in
        helperFunc.svar.vtype <- (TFun(retT, 
            Some(List.mapi (fun i -> fun t -> ("arg" ^ (string_of_int i), t, [])) argTs),
            isVa, 
            []));
        currentFrameAddressVar := Some(tmpVar);
        self#queueInstr [Call(Some(Var(tmpVar), NoOffset),
            Lval(Var(helperFunc.svar), NoOffset),
            [ Const(CInt64(Int64.zero, IInt, None)) ],
            f.svar.vdecl
        )];
        let res = super#vfunc f in
        currentFrameAddressVar := None; res

    method shadowDescrForExpr outerE =
        let castIsRelevant targetT sourceT =
            (* do we shadow them both, the same way? then it's not relevant.
             * (do we shadow them both, differently? doesn't arise for us)
             * do we not shadow either of them? then it's not relevant.
             * are we throwing away a shadow? then it's relevant, and we will fail.
             * are we creating a fresh/different shadow? then it's relevant.
             *)
             match (self#typeNeedsShadow targetT, self#typeNeedsShadow sourceT) with
                 (true, true) -> false
               | (true, false) -> true
               | (false, true) -> true
               | (false, false) -> false
        in
        let origT = Cil.typeOf outerE
        in
        let rec stripIrrelevantCasts someE = match someE with
                CastE(t, innerE) ->
                    if not (castIsRelevant t (Cil.typeOf innerE))
                    then stripIrrelevantCasts innerE
                    else someE
              | _ -> someE
        in
        let e = stripIrrelevantCasts (simplifyPtrExprs outerE) in
        let t = Cil.typeOf e in
        if not (self#typeNeedsShadow t) then failwith "shadow for non-shadowable"
        else
        (* stripping irrelevant casts should never turn
         * a shadow-needing expression into a non-shadow-needing one. *)
        if self#typeNeedsShadow t <> self#typeNeedsShadow origT then failwith "cast-stripping affected shadowing"
        else
        let plainIntegerShadow = ShadowMakeFromRvals(let x = Const(CInt64(Int64.zero, IInt, None)) in [x; x]) in
        let makePointerShadow = fun e -> fun offs -> ShadowMakeFromRvals([CastE(ulongType, e); Const(CInt64(Int64.of_int offs, IInt, None))]) in
        let isUnboundedArrayType t = (match Cil.unrollType t with TArray(_, None, _) -> true | _ -> false) in
        let rec offsetContainsField offs = match offs with
                NoOffset -> false
              | Field(fi, _) -> true
              | Index(intExp, rest) -> offsetContainsField rest
        in
        let rec stripLeadingIndexesInList ol = match ol with
            [] -> []
            | Index(_, _) :: rest -> stripLeadingIndexesInList rest
            | Field(_, _) :: rest -> ol
            | NoOffset :: _ -> failwith "bad offset list"
        in
        let stripTrailingIndexes offs =
        offsetFromList (List.rev (stripLeadingIndexesInList (List.rev (offsetToList offs))))
        in
        let offsetUpToField offs = stripTrailingIndexes offs
        in
        match (e, t) with
            (Const _, TInt(_)) -> plainIntegerShadow
          | (Const(CStr(s)), _) ->  makePointerShadow e 0 (* FIXME: doesn't account for string merge/dedup in linker *)
          | (Const(CWStr(s)), _) -> makePointerShadow e 0
          | (Const _, TPtr(_,_)) -> makePointerShadow e 0
          | (Const _, _) -> failwith "shadow for literal non-shadowable"
          | (Lval(lh, lo), _) when self#hostIsLocal lh ->
                ShadowLocalLval(self#shadowLvalForLocalLval (lh,lo))
          | (Lval(lh, lo), _) ->
                (* "Doing a load" means "reading a non-local value". It's that simple.
                 * So this is the "doing a load" case.
                 * Note that doing AddrOf is not a load, except AddrOf(Mem(... ) -- handled below).
                 * In some cases, StartOf is doing a load and a cast.
                 * We rewrote those by calling simplifyPtrExprs. *)
                ShadowFetch(LoadedFrom((lh, lo)))
          | (AddrOf(Var(someVi), someOffset), _) when not someVi.vglob ->
                let currentFrameAddressExpr = match !currentFrameAddressVar with
                    None -> failwith "no current frame address var"
                    | Some(cfa) -> (Lval(Var(cfa), NoOffset))        
                in
                makePointerShadow currentFrameAddressExpr someVi.vid
          | (AddrOf(Var(someVi), someOffset), _) (* vglob *) -> makePointerShadow e 0
          | (StartOf(Var(someVi), someOffset), _) when (* by implication, not local *)
                (* is it an extern array of unknown dimensions? *)
                someVi.vstorage = Extern && isUnboundedArrayType someVi.vtype
                (* OVERRIDEME: the crunchbound code tries handleAddrOfVarOrField here,
                 * then does MustFetch. We can just create a fresh shadow, because
                 * we don't need to know the bounds. *)
                -> makePointerShadow e 0
          | (StartOf(Var(someVi), someOffset), _) when self#hostIsLocal (Var(someVi)) ->
                (* array-to-pointer decay on a local var's array. *)
                let currentFrameAddressExpr = match !currentFrameAddressVar with
                    None -> failwith "no current frame address var"
                    | Some(cfa) -> (Lval(Var(cfa), NoOffset))        
                in
                makePointerShadow currentFrameAddressExpr someVi.vid
          | (StartOf(Var(someVi), someOffset), _) -> (* the "normal" StartOf case *)
                (* Even when the offset does not contain a field,
                 * here StartOf is always making a new pointer,
                 * i.e. taking address of an array.
                 * We rewrote the cases that aren't, e.g. *p_arr,
                 * into CastE of an original pointer. *)
                makePointerShadow e 0
          | (AddrOf(Mem(memExp), someOffset), _) when offsetContainsField someOffset ->
                (* taking a non-local subobject's address, + possibly array-indexing into it *)
                makePointerShadow (AddrOf(Mem(memExp), offsetUpToField someOffset)) 0
          | (AddrOf(Mem(memExp), someOffset), _) (* someOffset does *not* contain field *) ->
                (* indexing into some array offset within *memExp
                 * ... shadow is whatever the shadow for memExp was,
                 *     so make a recursive call *)
                self#shadowDescrForExpr memExp
          | (StartOf(Mem(memExp), someOffset), _) when offsetContainsField someOffset ->
                makePointerShadow (AddrOf(Mem(memExp), offsetUpToField someOffset)) 0 (* see above *)
          | (StartOf(Mem(memExp), someOffset), _) (* no field in offset; offset yields an array *) ->
                makePointerShadow (StartOf(Mem(memExp), stripTrailingIndexes someOffset)) 0
          | (BinOp(PlusPI, Lval(Var(someVi), someOffset), someIntExp, somePtrT), _) when self#hostIsLocal (Var(someVi)) ->
                (* This is an adjustment of a locally shadowed ptr.
                 * Does it have the same shadow as the original one?
                 * In the crunchbound case, there is a complex argument ending in "yes".
                 * In our case, we just say yes.
                 * Provenance gets interesting here if the integer also has a provenance. *)
                ShadowLocalLval(self#shadowLvalForLocalLval (Var(someVi), someOffset))
          | (BinOp(op, e1, _, _), _) -> self#shadowDescrForExpr e1
          | (UnOp(op, someE, _), _) -> self#shadowDescrForExpr someE
          | (CastE(targetT, subE), _) ->
                (* We stripped the irrelevant casts, so this must be a relevant one,
                 * i.e. from non-shadowed to shadowed. *)
                if isPointerType targetT
                then failwith ("can't materialise pointer from type " ^ (typToString t))
                else plainIntegerShadow
          | (Question(e1, e2, e3, finalT), _) -> (* hmm *) failwith "please turn off Cil.useLogicalOperators"
          | (SizeOf _, _) | (SizeOfE _, _) | (AlignOf _, _) | (AlignOfE _, _) | (SizeOfStr _, _) -> plainIntegerShadow
          | (AddrOfLabel _, _) -> failwith "no support for address-of-label yet; try excluding this func from instrumentation?"

    val underAddrOf = ref false
    method latchAddrOf (outerE: exp) : unit =
        let initialSimplifiedE = simplifyPtrExprs outerE in
        let simplifiedE = match initialSimplifiedE with
            (* HACK around CIL's bonkers encoding of __builtin_va* primitives. *)
          SizeOfE(Lval(Var(x), NoOffset)) when stringStartsWith x.vname "__builtin_"
              -> initialSimplifiedE
        | SizeOfE(subE) -> SizeOf(Cil.typeOf subE)
        | _ -> initialSimplifiedE
        in
        match simplifiedE with
        AddrOf(lv) -> underAddrOf := true
          | StartOf(lv) -> underAddrOf := true (* necessary? *)
          | _ -> underAddrOf := false

    method vexpr (outerE: exp) : exp visitAction =
        self#latchAddrOf outerE; super#vexpr outerE
    
    method vlval outerLv = 
        currentLval := Some(outerLv);
        let theFunc = match !currentFunc with
            None -> failwith "lvalue outside function"
          | Some(f) -> f
        in
        let currentLoc = match !currentInst with
            Some(i) -> get_instrLoc i
          | None -> locUnknown
        in
        (* latch underAddrOf on the way down, so that we remember 
         * on the way back up (ChangeDoChildrenPost) *)
        let weAreUnderAddrOf = !underAddrOf
        in
        ChangeDoChildrenPost(outerLv, fun (lhost,loff) ->
            (* We only care about derefs here. Indexing is okay
             * because Index(_) offsets are always acting within
             * a subobject; it is the Mem(_) host that embodies
             * a pointer deref. *)
             let hoistDeref memExpr offsetList =
                let simplifiedMemExpr = foldConstants memExpr
                in
                (* If we have an expr using PlusPI, like "ptr + 1", our BoundsDescrForExpr
                 * should just use the bounds of the input pointer. *)
                let shadowDescr = self#shadowDescrForExpr simplifiedMemExpr
                in
                let slv = self#ensureShadowLocalLval simplifiedMemExpr shadowDescr
                in
                let (shadowLoadInstrs : Cil.instr list)
                = self#localShadowUpdateInstrs slv simplifiedMemExpr shadowDescr currentLoc
                in
                let checkResultVar = Cil.makeTempVar ~name:"__cil_derefcheck_" theFunc boolType in
                (checkResultVar.vattr <- [Attr("unused", [])];
                self#queueInstr (shadowLoadInstrs @ [Call(Some(Var(checkResultVar), NoOffset),
                    (Lval(Var((helperFunctions#getCheckDeref ()).svar),NoOffset)),
                    [ simplifiedMemExpr
                    ; (* what are the bounds of memExpr? *)
                      Lval(slv)
                    ], currentLoc
                )]));
                (* the same lval *)
                (Mem(memExpr), offsetFromList offsetList)
             in
             match lhost with
                 Mem(memExpr) when not weAreUnderAddrOf ->
                     hoistDeref memExpr (offsetToList loff)
                   | _ -> (lhost,loff)
        )
    

end

let feature : Feature.t = 
  { fd_name = "shadowprov";
    fd_enabled = false;
    fd_description = "pointer provenance shadow value tool";
    fd_extraopt = [];
    fd_doit = 
    (function (fl: file) -> (* Unix.sleep 10; *) (* debugging *)
      visitCilFileSameGlobals (new shadowProvVisitor fl :> cilVisitor) fl
    );
    fd_post_check = true;
  } 

let () = Feature.register feature
