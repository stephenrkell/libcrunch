(* Copyright (c) 2014--18,
 *  Stephen Kell        <stephen.kell@cl.cam.ac.uk>
 *)

open Cil
open Pretty
open Map
open Str
open Cilallocs
open Shadow

(* We define a simple shadow value tool that does the following:
 * every integer and pointer value is shadowed.
 * Pointers are shadowed at creation with the one-byte value (char) 42.
 * Integers are shadowed at creation with the one-byte value (char) 0.
 * Casts, arithmetic and all other expressions simply propagate shadows,
 * choosing the leftmost in the case of binary operations.
 *)

(* Will fill these in during initializer *) 
class helperFunctionsFull = object(self) inherit helperFunctionsRecord
    val mutable makeShadow = emptyFunction "__make_shadow"
    val mutable fetchShadowInl = emptyFunction "__fetch_shadow_inl"
    val mutable storeShadowNonLocal = emptyFunction "__store_shadow_non_local"
    
    method getMakeShadow (_:unit) = makeShadow
    method getFetchShadowInl (_:unit) = fetchShadowInl
    method getStoreShadowNonLocal (_:unit) = storeShadowNonLocal
end

let helperFunctions = new helperFunctionsFull

class shadowTestVisitor
 = fun enclosingFile ->
       object(self)
   inherit shadowBasicVisitor enclosingFile
    (* primitiveTypeIsShadowed *)
                     (fun t -> match unrollType t with TInt(_) | TPtr(_) -> true | _ -> false)
    (* shadowCompName *) "shadow_byte"
    (* helperHeaderNames *) ["liballocs_cil_inlines.h"; "shadowtest_helpers.h"]
    (* makeCallToMakeShadow *) (fun maybeLv -> fun exprs -> fun valE -> fun l -> Call(maybeLv,
            (Lval(Var((helperFunctions#getMakeShadow ()).svar),NoOffset)),
            exprs,
            l
        ))
    (* makeCallToFetchShadow *) (fun lv -> fun descr -> fun e -> fun l -> [
        Call( Some(lv),
           (Lval(Var((helperFunctions#getFetchShadowInl ()).svar),NoOffset)),
           [
               e; 
               (* where did we load it from? *)
               CastE(voidPtrPtrType, 
                match descr with
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
    
    method makeStoreHelperCall e lv descr l = 
        let slv = self#ensureShadowLocalLval e descr
        in
        let (sExpr, preInstrs) = (Lval(slv), self#localShadowUpdateInstrs slv e descr l)
        in
        preInstrs @ 
        [Call( None, 
              Lval(Var((helperFunctions#getStoreShadowNonLocal ()).svar), NoOffset),
              [ 
                CastE(voidConstPtrPtrType, mkAddrOf lv) ;
                e;
                sExpr;
                CastE(voidPtrType, zero)
              ],
              l
        )]



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
        (* stripping irrelevant casts should never turn
         * a shadow-needing expression into a non-shadow-needing one. *)
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
        let freshIntegerShadow = ShadowFreshFromRvals([Const(CInt64(Int64.zero, IInt, None))]) in
        let freshPointerShadow = ShadowFreshFromRvals([Const(CInt64(Int64.of_int 42, IInt, None))]) in
        let isUnboundedArrayType t = (match Cil.unrollType t with TArray(_, None, _) -> true | _ -> false) in
        let rec offsetContainsField offs = match offs with
                NoOffset -> false
              | Field(fi, _) -> true
              | Index(intExp, rest) -> offsetContainsField rest
        in
        match (e, t) with
            (Const _, TInt(_)) -> freshPointerShadow
          | (Const _, _) -> failwith "shadow for literal non-shadowable"
          | (Lval(lh, lo), _) when self#hostIsLocal lh ->
                ShadowLocalLval(self#shadowLvalForLocalLval (lh,lo))
          | (Lval(lh, lo), _) ->
                (* "Doing a load" means "reading a non-local value". It's that simple.
                 * So this is the "doing a load" case.
                 * Note that doing AddrOf is not a load, except AddrOf(Mem(... )).
                 * In some cases, StartOf is doing a load and a cast.
                 * We rewrote those by calling simplifyPtrExprs. *)
                ShadowFetch(LoadedFrom((lh, lo)))
          | (AddrOf(Var(someVi), someOffset), _) -> freshPointerShadow
          | (StartOf(Var(someVi), someOffset), _) when (* by implication, not local *)
                (* is it an extern array of unknown dimensions? *)
                someVi.vstorage = Extern && isUnboundedArrayType someVi.vtype
                (* OVERRIDEME: the crunchbound code tries handleAddrOfVarOrField here,
                 * then does MustFetch. We can just create a fresh shadow, because
                 * we don't need to know the bounds. *)
                -> freshPointerShadow
          | (StartOf(Var(someVi), someOffset), _) when self#hostIsLocal (Var(someVi)) ->
                (* array-to-pointer decay on a local var's array. *)
                freshPointerShadow
          | (StartOf(Var(someVi), someOffset), _) -> (* the "normal" StartOf case *)
                (* Even when the offset does not contain a field,
                 * here StartOf is always making a new pointer,
                 * i.e. taking address of an array.
                 * We rewrote the cases that aren't, e.g. *p_arr,
                 * into CastE of an original pointer. *)
                freshPointerShadow
          | (AddrOf(Mem(memExp), someOffset), _) when offsetContainsField someOffset ->
                (* taking a non-local subobject's address, + possibly array-indexing into it *)
                freshPointerShadow
          | (AddrOf(Mem(memExp), someOffset), _) (* someOffset does *not* contain field *) ->
                (* indexing into some array offset within *memExp
                 * ... shadow is whatever the shadow for memExp was *)
                self#shadowDescrForExpr memExp
          | (StartOf(Mem(memExp), someOffset), _) when offsetContainsField someOffset ->
                freshPointerShadow (* see above *)
          | (StartOf(Mem(memExp), someOffset), _) (* no field in offset *) ->
                freshPointerShadow (* see above *)
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
                if isPointerType targetT then freshPointerShadow else freshIntegerShadow
          | (Const(CStr(s)), _) ->  freshPointerShadow
          | (Const(CWStr(s)), _) -> freshPointerShadow
          | (Question(e1, e2, e3, finalT), _) -> (* hmm *) failwith "please turn off Cil.useLogicalOperators"
          | (SizeOf _, _) | (SizeOfE _, _) | (AlignOf _, _) | (AlignOfE _, _) -> freshIntegerShadow
          | (AddrOfLabel _, _) -> freshPointerShadow

end

let feature : Feature.t = 
  { fd_name = "shadowtest";
    fd_enabled = false;
    fd_description = "test shadow value tool";
    fd_extraopt = [];
    fd_doit = 
    (function (fl: file) -> 
      visitCilFileSameGlobals (new shadowTestVisitor fl :> cilVisitor) fl
    );
    fd_post_check = true;
  } 

let () = Feature.register feature
