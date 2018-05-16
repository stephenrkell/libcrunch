(* Copyright (c) 2014--18,
 *  Stephen Kell        <stephen.kell@cl.cam.ac.uk>
 *)

open Cil
open Pretty
open Map
open Str
open Cilallocs

let stringStartsWith s pref = 
  if (String.length s) >= (String.length pref) 
  then (String.sub s 0 (String.length pref)) = pref 
  else false


let instrLoc (maybeInst : Cil.instr option) =
   match maybeInst with 
   Some(i) -> Cil.get_instrLoc i
 | None -> locUnknown

let rec uncastExpr e = 
    match e with
        CastE(castT, subE) -> uncastExpr subE
        | _ -> e
 
let rec findFiNamed name (cfs: fieldinfo list) =
    match cfs with
        [] -> raise Not_found
      | f :: fs -> if f.fname = name then f else findFiNamed name fs

let isCompOrArray t = match Cil.typeSig t with
    TSArray(_, _, _) -> true
  | TSComp(_, _, _) -> true
  | _ -> false

let instrIsCallTo f instr = match instr with
    Call(_, Lval(Var(fvar), NoOffset), _, _) when fvar == f -> true
  | _ -> false

let makeSizeOfPointeeType ptrExp = 
    let pointeeT = Cil.typeOf (Lval(Mem(ptrExp), NoOffset))
    in
    let rec decayPointeeArrayT pt = match pt with
      | TArray(at, _, _) -> decayPointeeArrayT at
      | TNamed(ti, attrs) -> decayPointeeArrayT ti.ttype
      | _ -> pt
    in
    SizeOf(decayPointeeArrayT pointeeT)

let rec simplifyPtrExprs someE =
  (* Try to see through AddrOf and Mems to get down to a local lval if possible. 
   * Here we are applying the equivalences from the CIL documentation.
   * We first simplify any nested pointer expressions
   * (which may nest inside *non*-pointer expressions!)
   * then simplify the whole. This is necessary to, say, turn
   * "*&*&*&*&p" into "p". *)
  let rec simplifyOffset offs = match offs with
      NoOffset -> NoOffset
    | Field(fi, rest) -> Field(fi, simplifyOffset rest)
    | Index(intExp, rest) -> Index(simplifyPtrExprs intExp, simplifyOffset rest)
  in
  let withSubExprsSimplified = match someE with
    |Lval(Var(vi), offs) -> Lval(Var(vi), simplifyOffset offs)
    |Lval(Mem(subE), offs) -> Lval(Mem(simplifyPtrExprs subE), simplifyOffset offs)
    |UnOp(op, subE, subT) -> UnOp(op, simplifyPtrExprs subE, subT)
    |BinOp(op, subE1, subE2, subT) -> BinOp(op, simplifyPtrExprs subE1, simplifyPtrExprs subE2, subT)
    |CastE(subT, subE) -> 
        let simplifiedSubE = simplifyPtrExprs subE
        in
        if getConcreteType (Cil.typeSig subT) = getConcreteType (Cil.typeSig (Cil.typeOf simplifiedSubE))
        then simplifiedSubE else CastE(subT, simplifiedSubE)
    |AddrOf(Var(vi), offs) -> AddrOf(Var(vi), simplifyOffset offs)
    |AddrOf(Mem(subE), offs) -> AddrOf(Mem(simplifyPtrExprs subE), simplifyOffset offs)
    |StartOf(Var(vi), offs) -> StartOf(Var(vi), simplifyOffset offs)
    |StartOf(Mem(subE), offs) -> StartOf(Mem(simplifyPtrExprs subE), offs)
    |_ -> someE
  in
  match withSubExprsSimplified with
     (* e.g.  (&p->foo)->bar           is   p->foo.bar      *)
     Lval(Mem(AddrOf(Mem a, aoff)), off) -> 
        Lval(Mem a, offsetFromList((offsetToList aoff) @ (offsetToList off)))
     (* e.g.  (&v.baz)->bum            is   v.baz.bum       *)
   | Lval(Mem(AddrOf(Var v, aoff)), off) -> 
        Lval(Var v, offsetFromList((offsetToList aoff) @ (offsetToList off)))
     (* e.g.  &*p                      is   p               *)
   | AddrOf (Mem a, NoOffset)            -> a
     (* note that unlike &( *p ),
              &p->f   (i.e. with some offset)  cannot be simplified          *)
   | StartOf (Mem s, NoOffset) -> s     (* likewise *)
     (* note that unlike *p    (where p is of pointer-to-array type),
              p->arr  (i.e. some offset) cannot be simplified      *)
   | _ -> withSubExprsSimplified

class addressTakenVisitor = fun seenAddressTakenLocalNames -> object(self) inherit nopCilVisitor
    method vexpr (e: exp) : exp visitAction = begin
        match e with
             AddrOf(Var(vi), _) when not vi.vglob ->
                seenAddressTakenLocalNames :=
                    if List.mem vi.vname !seenAddressTakenLocalNames
                    then !seenAddressTakenLocalNames
                    else vi.vname :: !seenAddressTakenLocalNames
                ; DoChildren
           | StartOf(Var(vi), _) when not vi.vglob -> 
                seenAddressTakenLocalNames :=
                    if List.mem vi.vname !seenAddressTakenLocalNames 
                    then !seenAddressTakenLocalNames
                    else vi.vname :: !seenAddressTakenLocalNames
                ; DoChildren
           | _ -> DoChildren
    end
end

type shadow_load_store_origin =
    UnknownOrigin
  | LoadedFrom of Cil.lval (* the loaded-from address of the value whose shadow
                                 * we're getting, *or* of another shadow which necessarily
                                 * has the same shadow. *)
  | ExternObject of Cil.varinfo (* the varinfo for an extern object *)

type shadow_for_expr = 
    ShadowLocalLval of lval
  | ShadowFreshFromRvals of Cil.exp list
  | ShadowFetch of shadow_load_store_origin

let rec reverseMapOverInitializedLvalues revAcc f lhost lofflist (someInit : Cil.init option) someLocation = 
match someInit with
    None -> revAcc
    (* In both the other cases, we need to
     * - make the lvalue that is being assigned to
     * - if it is of pointer type, we should have a single init
     * - if it is of non-pointer type, we may have a compound init
            that we need to recurse down
            by enumerating all the lvalues 
     *)
    | Some(SingleInit(anInitExpr)) -> 
        (* For each initialized thing, *)
        (debug_print 1 ("saw a statically initialized lvalue " ^ 
                (lvalToString (lhost, offsetFromList lofflist)) ^ 
                ", initialized to " ^ (expToString anInitExpr) ^ "\n"));
        ((f (lhost, offsetFromList lofflist) anInitExpr) :: revAcc)
    | Some(CompoundInit(t, offsetInitPairs)) ->
        (* We want to cons the result items on in reverse order. *)
        List.fold_left (fun newRevAcc -> fun (anOffset, anInit) ->
            reverseMapOverInitializedLvalues newRevAcc f lhost (lofflist @ [anOffset]) (Some(anInit)) someLocation
        ) revAcc offsetInitPairs

let mapOverInitializedLvalues f lv someInit someLocation = 
    let (lhost, loff) = lv in
    List.rev (reverseMapOverInitializedLvalues [] f lhost (offsetToList loff) someInit someLocation)

(* type helperFunctionsRecord = <
    pushLocalArgumentShadow : Cil.fundec;
    pushArgumentShadowManifest : Cil.fundec;
    fetchAndPushArgumentShadow : Cil.fundec;
    pushArgumentShadowCookie : Cil.fundec;
    tweakArgumentShadowCookie : Cil.fundec;
    peekArgumentShadow : Cil.fundec;
    peekAndShadowStoreArgumentShadow : Cil.fundec;
    pushLocalResultShadow : Cil.fundec;
    pushResultShadowManifest : Cil.fundec;
    fetchAndPushResultShadow : Cil.fundec;
    peekResultShadow : Cil.fundec;
    cleanupShadowStack : Cil.fundec
>
*)
(*    pushLocalArgumentShadow = emptyFunction "__push_local_argument_shadow"
    pushArgumentShadowManifest = emptyFunction "__push_argument_shadow_manifest"
    fetchAndPushArgumentShadow = emptyFunction "__fetch_and_push_argument_shadow"
    pushArgumentShadowCookie = emptyFunction "__push_argument_shadow_cookie"
    tweakArgumentShadowCookie = emptyFunction "__tweak_argument_shadow_cookie"
    peekArgumentShadow = emptyFunction "__peek_argument_shadow"
    peekAndShadowStoreArgumentShadow = emptyFunction "__peek_and_shadow_store_argument_shadow"
    pushLocalResultShadow = emptyFunction "__push_local_result_shadow"
    pushResultShadowManifest = emptyFunction "__push_result_shadow_manifest"
    fetchAndPushResultShadow = emptyFunction "__fetch_and_push_result_shadow"
    peekResultShadow = emptyFunction "__peek_result_shadow"
    cleanupShadowStack = emptyFunction "__cleanup_shadow_stack"
}
*)

(* We want to define a suite of helper methods, in a way that clients can
 * extend with new helpers. We also don't want to fix the details of the
 * helpers, which subclasses might want to vary (the helpers' C-language
 * names, signatures etc.)
 *
 * I don't know a way in OCaml to get the row polymorphism that'd be required,
 * except by defining a class. And that doesn't let you expose fields publicly,
 * so we have to define an accessor for each helper. That's ugly, but I can't
 * find a better way. *)
class helperFunctionsRecord = object(self)
    val mutable pushLocalArgumentShadow = emptyFunction "__push_local_argument_shadow"
    val mutable pushArgumentShadowManifest = emptyFunction "__push_argument_shadow_manifest"
    val mutable fetchAndPushArgumentShadow = emptyFunction "__fetch_and_push_argument_shadow"
    val mutable pushArgumentShadowCookie = emptyFunction "__push_argument_shadow_cookie"
    val mutable tweakArgumentShadowCookie = emptyFunction "__tweak_argument_shadow_cookie"
    val mutable peekArgumentShadow = emptyFunction "__peek_argument_shadow"
    val mutable peekAndShadowStoreArgumentShadow = emptyFunction "__peek_and_shadow_store_argument_shadow"
    val mutable pushLocalResultShadow = emptyFunction "__push_local_result_shadow"
    val mutable pushResultShadowManifest = emptyFunction "__push_result_shadow_manifest"
    val mutable fetchAndPushResultShadow = emptyFunction "__fetch_and_push_result_shadow"
    val mutable peekResultShadow = emptyFunction "__peek_result_shadow"
    val mutable cleanupShadowStack = emptyFunction "__cleanup_shadow_stack"
    
    method getPushLocalArgumentShadow (_:unit) = pushLocalArgumentShadow
    method getPushArgumentShadowManifest (_:unit) = pushArgumentShadowManifest
    method getFetchAndPushArgumentShadow (_:unit) = fetchAndPushArgumentShadow
    method getPushArgumentShadowCookie (_:unit) = pushArgumentShadowCookie
    method getTweakArgumentShadowCookie (_:unit) = tweakArgumentShadowCookie
    method getPeekArgumentShadow (_:unit) = peekArgumentShadow
    method getPeekAndShadowStoreArgumentShadow (_:unit) = peekAndShadowStoreArgumentShadow
    method getPushLocalResultShadow (_:unit) = pushLocalResultShadow
    method getPushResultShadowManifest (_:unit) = pushResultShadowManifest
    method getFetchAndPushResultShadow (_:unit) = fetchAndPushResultShadow
    method getPeekResultShadow (_:unit) = peekResultShadow
    method getCleanupShadowStack (_:unit) = cleanupShadowStack
end

class virtual shadowBasicVisitor = fun (enclosingFile : Cil.file) ->
                            fun (primitiveTypeIsShadowed : Cil.typ -> bool)(* function *) ->
                            fun shadowCompName ->
                            fun helperHeaderNames ->
                            fun makeCallToMakeShadow ->
                            fun (makeCallToFetchShadow : lval -> shadow_load_store_origin -> Cil.exp -> Cil.location -> Cil.instr list) ->
                            fun (helperFunctions : helperFunctionsRecord) ->
                            fun descriptorExprForShadowableExpr (* this is the uniqtype ptr, in crunchbound *) ->
                            fun defaultShadowValueForType ->
                            fun shadowString (* name in identifiers *) ->
                               object(self)
    inherit nopCilVisitor

    val shadowType = findStructTypeByName enclosingFile.globals shadowCompName

    method virtual shadowDescrForExpr :
        Cil.exp (* e *) ->
        shadow_for_expr

    method varIsOurs vi = stringStartsWith vi.vname "__liballocs_" 
                 || stringStartsWith vi.vname "__libcrunch_" 
                  || (List.fold_left (fun acc -> fun x -> acc || stringEndsWith vi.vdecl.file x) false helperHeaderNames)

    val currentInst : instr option ref = ref None
    val currentFunc : fundec option ref = ref None
    val currentLval : lval option ref = ref None
    val currentBlock : block option ref = ref None
    val currentFuncAddressTakenLocalNames : string list ref = ref []
    (* Remember the mapping from locals to their shadows. We zap this on each
    * vfunc. *)
    val shadowLocals : (Cil.varinfo VarinfoMap.t) ref = ref VarinfoMap.empty
    (* Remember the mapping from expressions to the location
    * where the original expression was loaded from, if any. We zap this on each
    * vfunc. Each loaded-from addr is latched into its own temporary var, because
    * the lvalue's meaning might change from where we do the load to where
    * we use the shadow (consider "e = e->next" -- we want to fetch the shadow
    * of e, loaded from "e->next"). FIXME: I'm not convinced this is necessary,
    * so long as we fetch shadows (of e->next) before we do the actual program
    * statement (modifying e). *)
    val tempLoadExprs : (Cil.lval option) VarinfoMap.t ref = ref VarinfoMap.empty

    (* In each function we *may* create a flag to remember whether we detected, at 
    * entry, that the caller was instrumented. We use this to decide whether to
    * pass shadow back or not. *)
    val currentFuncCallerIsInstFlag : Cil.varinfo option ref = ref None

    val initFunc : fundec = emptyFunction "__libcrunch_crunchbound_init" 

    method varinfoIsLocal vi = not vi.vglob && 
    ( let isAT = (List.mem vi.vname !currentFuncAddressTakenLocalNames)
      in
        (if isAT then () (* debug_print 1 ("Local var " ^ vi.vname ^ " would count as local " ^ 
        "but is address-taken\n")*) else ())
        ;
        not isAT
    )

    method hostIsLocal lhost = 
        match lhost with
            Var(vi) -> self#varinfoIsLocal vi
              | Mem(_) -> false

    method shadowTAsNumber maybeShadowT = 
        match maybeShadowT with
            None -> 
                (* Zero shadow objects. *)
                Int64.of_int 0
          | Some(TComp(ci, attrs)) when ci.cname = shadowCompName ->
                (* A singleton shadow struct. *)
                Int64.of_int 1
          | Some(TArray(TComp(ci, attrs), Some(boundExpr), [])) when ci.cname = shadowCompName ->
                (* An array of the shadow struct. *)
                begin match constInt64ValueOfExpr boundExpr with
                    Some(x) -> x
                  | None -> failwith (shadowString ^ " array type does not have constant bound")
                end
          | _ -> failwith "not a shadow type (asNumber)"

    method shadowTTimesN maybeShadowT (n : int64) =
        (* debug_print 1 ((Int64.to_string n) ^ " times shadow type " ^ 
            (match maybeShadowT with Some(shadowT) -> typToString shadowT | _ -> "(none)") ^ 
            " is: "); flush stderr; *)
        let prod = match maybeShadowT with
            None -> None
          | Some(TComp(ci, attrs)) when ci.cname = shadowCompName ->
                if n = Int64.of_int 1 then Some(shadowType)
                else Some(TArray(shadowType, Some(Const(CInt64(n, IInt, None))), []))
          | Some(TArray(TComp(ci, attrs), Some(boundExpr), [])) when ci.cname = shadowCompName ->
                (* Okay, now we have n times as many. *)
                Some(
                    TArray(shadowType, 
                            Some(Const(CInt64(Int64.mul n (match constInt64ValueOfExpr boundExpr with
                                Some(m) -> m
                              | None -> failwith "array bound is not constant (*)"), IInt, None))),
                          []
                    )
                )
          | _ -> failwith "not a shadow type (*)"
        in
        (* debug_print 1 ((match prod with Some(shadowT) -> typToString shadowT | _ -> "(none)") ^ 
            "\n"); flush stderr; *)
        prod

    method shadowTPlusN maybeShadowT (n : int64) : Cil.typ option =
        (* debug_print 1 ((Int64.to_string n) ^ " plus shadow type " ^ 
            (match maybeShadowT with Some(shadowT) -> typToString shadowT | _ -> "(none)") ^ 
            " is: "); flush stderr; *)
        let sum = match maybeShadowT with
            None -> if n = Int64.of_int 0 then None
                    else if n = Int64.of_int 1 then Some(shadowType)
                    else Some(TArray(shadowType, Some(makeIntegerConstant n), []))
          | Some(TComp(ci, attrs)) when ci.cname = "__libcrunch_shadow_s" -> 
                if n = Int64.of_int 0 then Some(shadowType)
                else Some(TArray(shadowType, Some(makeIntegerConstant ((Int64.add n (Int64.of_int 1)))), []))
          | Some(TArray(TComp(ci, attrs), Some(boundExpr), [])) when ci.cname = shadowCompName ->
                (* Okay, now we have n more. *)
                Some(TArray(TComp(ci, attrs), 
                    Some(makeIntegerConstant(Int64.add n (match constInt64ValueOfExpr boundExpr with
                            Some(m) -> m
                          | None -> failwith "array bound is not constant (+)"
                        ))),
                    []
                ))
          | _ -> failwith "not a shadow type (+)"
        in
        (* debug_print 1 ((match sum with Some(shadowT) -> typToString shadowT | _ -> "(none)") ^ 
            "\n"); *)
        sum

    method shadowTPlusShadowT maybeShadowT1 maybeShadowT2 : Cil.typ option =
        let sum = match maybeShadowT2 with
            None -> self#shadowTPlusN maybeShadowT1 (Int64.of_int 0)
          | Some(TComp(ci, attrs)) when ci.cname = shadowCompName -> 
                self#shadowTPlusN maybeShadowT1 (Int64.of_int 1)
          | Some(TArray(TComp(ci, attrs), Some(boundExpr), [])) when ci.cname = shadowCompName ->
                let m = (match constInt64ValueOfExpr boundExpr with
                    Some n -> n
                  | None -> failwith "internal error: shadow type has unbounded array type"
                )
                in
                self#shadowTPlusN maybeShadowT1 m
          | _ -> failwith "not a shadow type (T plus T)"
        in
        (* debug_print 1 ("Sum of shadow types " ^ 
            (match maybeShadowT1 with Some(shadowT) -> typToString shadowT | _ -> "(none)") ^ 
            " and " ^ 
            (match maybeShadowT2 with Some(shadowT) -> typToString shadowT | _ -> "(none)") ^ 
            " is " ^
            (match sum with Some(shadowT) -> typToString shadowT | _ -> "(none)") ^ 
            "\n"); *)
        sum

    method shadowTForT (t : Cil.typ) : Cil.typ option = 
        (* We want to produce either nothing, or a single shadow_t, or 
         * a one-dimensional array of shadow_ts. 
         * This is basically because structs are heterogeneous -- they might contain
         * an array of m pointers and then an array of n pointers. 
         * There's no way to preserve this structure while still keeping,
         * a single type for the whole structure's shadow (i.e. without
         * creating a new struct type mirroring the input struct type;
         * we could also have done this). *)
        let maybeShadowT = match t with
            TVoid(attrs) ->  failwith "asked shadow type for void"
          | TInt(_, _)
          | TFloat(_, _)
          | TPtr(_, _) -> if primitiveTypeIsShadowed t then Some(shadowType) else None
          | TArray(at, None, attrs) -> failwith "asked shadow type for unbounded array type"
          | TArray(at, Some(boundExpr), attrs) ->
                self#shadowTTimesN (self#shadowTForT at)
                    (match constInt64ValueOfExpr boundExpr with
                        Some n -> n
                      | None -> failwith "getting shadow type for non-constant array bound"
                    )
          | TFun(_, _, _, _) -> failwith "asked shadow type for incomplete (function) type"
          | TNamed(ti, attrs) -> self#shadowTForT ti.ttype
          | TComp(ci, attrs) ->
                List.fold_left (fun (x : Cil.typ option) -> fun (y : Cil.typ option) -> self#shadowTPlusShadowT x y) None (
                    ((List.map (fun fi -> self#shadowTForT fi.ftype) ci.cfields) : Cil.typ option list)
                )
          | TEnum(ei, attrs) -> self#shadowTForT intType
          | TBuiltin_va_list(attrs) -> None
        in
        debug_print 1 ("Shadow type for " ^ 
            (typToString t) ^ 
            " is " ^ (match maybeShadowT with Some(shadowT) -> typToString shadowT | _ -> "none") ^ 
            "\n");
        maybeShadowT

    method typeNeedsShadow t = 
        match self#shadowTForT t with
            Some(_) -> true
          | None -> false

    method exprNeedsShadow expr =
        self#typeNeedsShadow (Cil.typeOf expr)

    method shadowIndexExprForOffset (startIndexExpr: Cil.exp) (offs : Cil.offset) (host : Cil.lhost) (prevOffsets : Cil.offset) = 
        debug_print 1 ("Hello from shadowIndexExprForOffset\n");
        let indexExpr = match offs with
            Field(fi, nextOffset) -> 
                debug_print 1 ("Hit Field case\n");
                let rec addEarlierFields = fun acc -> fun someFis -> (
                    debug_print 1 ("In addEarlierFields\n");
                    match someFis with
                        [] -> acc
                      | someFi :: more -> 
                        (* HACK: can't use deep equality on fieldinfos, and I'm uneasy 
                         * about the shallow one. So test name and location. *)
                        if (someFi.floc = fi.floc && someFi.fname = fi.fname) then acc (* terminate early *)
                        else addEarlierFields (acc @ [someFi]) more
                )
                in
                let earlierFields = addEarlierFields [] fi.fcomp.cfields
                in
                debug_print 1 ("Found " ^ (string_of_int (List.length earlierFields)) ^ "\n");
                (* Add up all the shadowTs for *earlier* fields in the struct. *)
                let fieldOffsetAsShadowT = List.fold_left 
                    (fun x -> fun y -> self#shadowTPlusShadowT x y) 
                    None 
                    (List.map (fun fi -> self#shadowTForT fi.ftype) earlierFields)
                in
                let fieldOffset = self#shadowTAsNumber fieldOffsetAsShadowT
                in
                debug_print 1 ("Recursing on nextOffset in (ignore host): " ^ (lvalToString (host, nextOffset)) ^ "\n");
                self#shadowIndexExprForOffset
                    (BinOp(PlusA, startIndexExpr, makeIntegerConstant fieldOffset, intType))
                    nextOffset host (offsetAppend prevOffsets (Field(fi, NoOffset)))
          | Index(intExp, nextOffset) -> 
                debug_print 1 ("Hit Index case\n");
                (* We're indexing into an array at intExp (which we assume is in
                 * bounds -- if we're crunchbound, we've already checked that).
                 *
                 * Example:
                 * 
                 *    x[i][j].p
                 * 
                 * We want the expression for i.
                 * First we recursively compute the offset that we'd want if i were zero.
                 * Then we add i times the array size for Cil.typeOf (x[i]), 
                 * i.e. the number of shadow objects that the whole [j].p tail accounts for.
                 *)
                let offsetUpToHere = offsetAppend prevOffsets (Index(intExp, NoOffset))
                in
                debug_print 1 ("Recursing on nextOffset in (ignore host): " ^ (lvalToString (host, nextOffset)) ^ "\n");
                let zeroIndexExpr = self#shadowIndexExprForOffset startIndexExpr nextOffset
                    host offsetUpToHere
                in
                (BinOp(PlusA, zeroIndexExpr, ( 
                    (BinOp(Mult, intExp, (
                        let elemT = Cil.typeOf (Lval(host, offsetUpToHere))
                        in
                        let maybeElemTShadowT = self#shadowTForT elemT
                        in
                        makeIntegerConstant (self#shadowTAsNumber maybeElemTShadowT)
                    ), intType))
                ), intType))
          | NoOffset -> startIndexExpr
        in
        debug_print 1 ("Shadow index expression for indexing " ^ 
            (lvalToString (host, prevOffsets)) ^ " giving " ^ 
            (lvalToString (host, offsetFromList ((offsetToList prevOffsets) @ (offsetToList offs)))) ^ 
            " is " ^ (expToString indexExpr) ^ "\n");
        indexExpr

    method getOrCreateShadowLocal vi =
        debug_print 1 ("Ensuring we have shadow local for " ^ vi.vname ^ "\n");
        try  
            let found = VarinfoMap.find vi !shadowLocals
            in 
            debug_print 1 ("Already exists\n");
            found
        with Not_found -> 
         debug_print 1 ("Creating new shadow local for local " ^ vi.vname ^ "\n");
         let maybe_bt = self#shadowTForT vi.vtype 
         in
         let bt = match maybe_bt with
                Some(bt) -> bt
              | None -> failwith ("creating shadow local for a type that doesn't need it: " ^ 
                    typToString vi.vtype ^ "\n")
         in
         let newLocalVi = match !currentFunc with
            Some(f) -> Cil.makeTempVar f ~name:(("__cil_local" ^ shadowString ^ "_") ^ vi.vname ^ "_") bt
          | None -> failwith "making shadow local when not inside a function"
         in
         let newShadowLocalsMap = (VarinfoMap.add vi newLocalVi !shadowLocals)
         in
         shadowLocals := newShadowLocalsMap;
         newLocalVi

    method shadowLvalForLocalLval ((lh, loff) : lval) : lval =
        debug_print 1 ("Hello from shadowLvalForLocalLval, lval " ^ (lvalToString (lh, loff)) ^ "\n");
        match lh with
            Var(local_vi) -> 
                debug_print 1 ("Hello from Var case, name " ^ local_vi.vname ^ "\n");
                let shadowVi = self#getOrCreateShadowLocal local_vi
                in 
                let indexExpr = self#shadowIndexExprForOffset zero loff (Var(local_vi)) NoOffset
                in
                debug_print 1 ("Shadow index expr is `" ^ (expToString indexExpr) ^ "'\n");
                let offs = (
                    match Cil.typeSig (Cil.typeOf (Lval(Var(shadowVi), NoOffset))) with
                        TSArray(_) -> Index(indexExpr, NoOffset)
                      | TSComp(_) -> 
                            if not (isStaticallyZero indexExpr) then
                                (* PROBLEM. Consider this code (based on something in SPEC gcc)
                                    static const int nums[] = { 1 };
                                    struct { int *p; } *info[(sizeof nums / sizeof (int))];
                                    for (int n = 0; n < 1; ++n) info[n] = ... ;
                                   Statically, at info[n], we don't know that n is always 0.
                                   But earlier, we *did* know that "info" has only one element.
                                   There, we were reasoning about a more constrained language
                                   (the language of constant-foldable expressions).
                                   To avoid bailing here, we must omit the check and unconditionally
                                   access the singleton comp shadow value.
                                   Correctness of this is relying on this being in bounds;
                                   in crunchbound, this means our indexing checks on local
                                   arrays via checkLocalBounds. Consider the following.
                                   e.g.
                                   int *access_idx(int idx)
                                   {
                                     static int *localps[] = { ... };
                                     return localps[idx];
                                   }
                                   We test idx against the bounds of localps.
                                   If it succeeds, we load+return localps[idx] 
                                   *together with* its bounds, localbound_localps[idx].
                                   We're worried about localbound_localps[idx] overflowing.
                                   If it did, the first check would have failed.
                                   There is a compiler-visible dependency from the localps load
                                   to the result of the checkLocalBounds check. So we should be okay.
                                 *)
                                (* failwith "singleton shadow but non-zero shadow expr" *)
                                NoOffset
                           else NoOffset
                      | _ -> failwith "bad shadow var type"
                )
                in
                (Var(shadowVi), offs)
            | _ -> failwith "local lvalue not a Var"

    (* Tell whether a pointer expression has a stored shadowable behind it,
     * i.e. would do a load of a shadowed value if we deref'd it.
     * It's the caller's job to handle local versus non-local lvalues specially. *)
    method getStoredShadowableLvalForPtrExp ptrE =
        match ptrE with
            Lval(lh, lo) -> Some(lh, lo)
            | StartOf(Mem(memExp), NoOffset) ->
                (* This means &( ( *memExp )[0] )
                 * i.e. memExp is a pointer to array
                 * and our expression is getting a pointer to the first element.
                 * by loading that pointer from memExp,
                 * if memExp is itself an lvalue a.k.a. memExp a stored pointer. *)
                (* memExp points to an array which we make decay to ptr. *)
                (* Example (from LBM): calling     *dstGrid, 
                 * where dstGrid is a pointer to an array of double.
                 * Here *dstGrid has type double[],
                 * so the expression is equivalent to &( *dstGrid)[0],
                 * so we get StartOf(Mem(dstGrid)).
                 * What is the loaded-from address?
                 * It's clearly dstGrid, i.e. "dstGrid" holds a pointer whose
                 * shadow we might have stored at shadow(dstGrid).
                 * So it's memExp, right?
                 *)
                (* This just changes the type of memExp from pointer-to-array to pointer-to-element;
                 * the stored pointer lval is still memExp *)
                self#getStoredShadowableLvalForPtrExp memExp
            | (* casts? *)
                CastE(castT, subE) ->
                    (* OVERRIDEME: in crunchbound type-aware mode, we discard this value and recompute *)
                    self#getStoredShadowableLvalForPtrExp subE
            | _ -> None

    method ensureShadowLocalLval ptrExpr shadowDescrForPtrExpr =
        match !currentFunc with
            None -> failwith "making shadow local when not inside function"
          | Some(f) ->
        match shadowDescrForPtrExpr with
            ShadowLocalLval(blv) -> blv
          | ShadowFreshFromRvals(_)
             -> let bTemp = Cil.makeTempVar f ~name:("__cil_" ^ shadowString ^ "rv_") shadowType
                in
                (Var(bTemp), NoOffset)
          | ShadowFetch(_) -> 
                let bTemp = Cil.makeTempVar f ~name:("__cil_" ^ shadowString ^ "fetched_") shadowType
                in
                (Var(bTemp), NoOffset)

    method virtual makeStoreHelperCall : Cil.exp -> Cil.lval -> shadow_for_expr -> Cil.location -> Cil.instr list

    method localShadowUpdateInstrs slv valE shadowDescr loc = 
        match shadowDescr with
            ShadowFreshFromRvals(exprs) ->
                [makeCallToMakeShadow (Some(slv)) exprs valE loc]
          | ShadowLocalLval(otherSlv) ->
                (* avoid cyclic comparison *)
                let sameVi = match (slv, otherSlv) with
                    ((Var(vi1), NoOffset), (Var(vi2), NoOffset)) when vi1.vname = vi2.vname -> true
                  | ((Var(vi1), Index(Const(CInt64(n1, IInt, None)), NoOffset)), 
                            (Var(vi2), Index(Const(CInt64(n2, IInt, None)), NoOffset)))
                                when vi1.vname = vi2.vname && n1 = n2 -> true
                  | _ -> slv == otherSlv
                in
                if sameVi then [] else [Set( slv, Lval(otherSlv), loc )]
          | ShadowFetch(lf) -> (* fetchOol complication is in here *) makeCallToFetchShadow slv lf valE loc

    method containedShadowedPrimitiveExprsForExpr e =
        let rec enumerateShadowedPrimitiveOffsetsForT t = match t with
            TVoid(attrs) -> []
          | TInt(_, _)
          | TFloat(_, _)
          | TPtr(_, _) -> if primitiveTypeIsShadowed t then [NoOffset] else []
          | TArray(at, None, attrs) -> failwith "asked to enumerate shadowed offsets for unbounded array type"
          | TArray(at, Some(boundExpr), attrs) -> 
                (* For each offset that yields a shadowed thing,
                 * prepend it with Index(n), 
                 * and copy/repeat for all n in the range of the array. *)
                let arraySize = match constInt64ValueOfExpr boundExpr with
                    Some n -> n
                  | None -> failwith "enumerating shadowed offsets for non-constant array bound"
                in
                let rec intsUpTo start endPlusOne = 
                    if start >= endPlusOne then [] else start :: (intsUpTo (start+1) endPlusOne)
                in
                let elementShadowableOffsets = enumerateShadowedPrimitiveOffsetsForT at
                in
                let arrayIndices = intsUpTo 0 (Int64.to_int arraySize)
                in
                (* copy the list of offsets, once
                 * for every index in the range of the array *)
                List.flatten (List.map (fun offset -> List.map (fun i ->
                    Index(makeIntegerConstant (Int64.of_int i), offset)
                ) arrayIndices) elementShadowableOffsets)
          | TFun(_, _, _, _) -> failwith "asked to enumerate shadowed offsets for incomplete (function) type"
          | TNamed(ti, attrs) -> enumerateShadowedPrimitiveOffsetsForT ti.ttype
          | TComp(ci, attrs) -> 
                (* For each field, recursively collect the offsets
                 * then prepend Field(fi) to each. We prepend the same fi,
                 * for all offsets yielded by a given field,
                 * then move on to the next field. *)
                List.fold_left (fun acc -> fun fi -> 
                    let thisFieldOffsets = enumerateShadowedPrimitiveOffsetsForT fi.ftype
                    in
                    let prepended = List.map (fun offs -> 
                        Field(fi, offs)
                    ) thisFieldOffsets
                    in
                    prepended @ acc
                ) [] ci.cfields
          | TEnum(ei, attrs) -> []
          | TBuiltin_va_list(attrs) -> []
        in
        let offsets = enumerateShadowedPrimitiveOffsetsForT (Cil.typeOf e)
        in
        match e with
            Lval(someHost, someOff) -> 
                List.map (fun off -> 
                    let newOff = offsetFromList (
                        (offsetToList someOff) @ (offsetToList off)
                    ) in
                    let _ = if offsets != [NoOffset] then
                        debug_print 1 ("Expression `" ^ (expToString e) ^ "' contains a shadowable field: `" ^ 
                            (expToString (Lval(someHost, newOff))) ^ "'\n")
                        else ()
                    in
                    Lval(someHost, newOff)
                ) offsets
         |  _ when isCompOrArray (Cil.typeOf e) -> 
                failwith ("internal error: did not expect array or struct type: " ^ 
                    (typToString (Cil.typeOf e)))
         |  _ when primitiveTypeIsShadowed (Cil.typeOf e) -> [e]
         |  _ -> []

    (* let containedVoidPointerExprsForExpr e = 
        List.filter (fun ptrE -> isVoidPtrType (Cil.typeOf e))
            (containedPointerExprsForExpr ~forceIncludeVoidPointers:true e) *)
    (* OVERRIDEME *)

    method mapForAllShadowsInExpr : 'a. (Cil.exp -> shadow_for_expr -> Cil.exp -> 'a) -> Cil.exp -> 'a list =
        fun forOne outerE ->
        List.mapi (fun i -> fun valExp -> 
            let shadowExp = self#shadowDescrForExpr valExp
            in
            forOne valExp shadowExp (makeIntegerConstant (Int64.of_int i))
        ) (self#containedShadowedPrimitiveExprsForExpr outerE)

    method concatMapForAllShadowsInExprList: 'a. (Cil.exp -> shadow_for_expr -> Cil.exp -> 'a) -> (Cil.exp list) -> 'a list =
        fun f es ->
        List.flatten (
            List.map (
                fun e -> self#mapForAllShadowsInExpr f e
            ) es
        )

    (* In what way is this "non-local"?
     * It's that the *destination* is non-local.
     * We ensure that the source expression
     * is a local/temporary having a local shadow var. *)
    method doNonLocalShadowableStoreInstr valE storeLv (sd: shadow_for_expr) l =
        (* __store_ptr_nonlocal(dest, written_val, maybe_known_shadow) *)
        (* What do we do if we have to fetch the shadow of the stored value?
         * This means that we don't *locally* know the shadow of the stored
         * value. And the value need have no relationship to the pointer
         * currently stored at the address. And the value we're storing is
         * probably *local*, i.e. coming from a temporary. So the right 
         * solution is probably to be eager about fetching the shadow, if we're
         * storing a shadowed value. Is this an optional mode? HMM, yes, we can do this.
         * If we turn makeInvalidShadowFun into a make-invalid-or-fetch-fast?
         * THEN must distinguish makeInvalid from receiveArg
         *)
        let slv = self#ensureShadowLocalLval valE sd
        in
        let (shadowExpr, preInstrs) = (Lval(slv), self#localShadowUpdateInstrs slv valE sd l)
        in
        preInstrs @ self#makeStoreHelperCall shadowExpr storeLv sd l

    method makeShadowSpaceInitializerCalls (initializerLocation: Cil.location) initializedShadowableLv initializationValExpr : Cil.instr list =
        (* The instructions we need to generate are like doNonLocalShadowableStoreInstr,
         * except that we always do a slow fetch. *)
        let sd = self#shadowDescrForExpr initializationValExpr
        in
        (* We want to allow clients to omit the initializer if they know that
         * not doing it is equivalent to using a null value. *)
        (* OVERRIDEME: if isStaticallyNullPinter initializationValExpr then []
        else *)
        self#doNonLocalShadowableStoreInstr initializationValExpr initializedShadowableLv
            sd initializerLocation

    method prependShadowSpaceInitializer initializerLocation (initializedShadowableLv: Cil.lval) (maybeInitializationValExpr: Cil.exp option) =
        let instrsToPrepend = match maybeInitializationValExpr with
            Some(initializationValExpr) ->
                self#makeShadowSpaceInitializerCalls initializerLocation
                    (initializedShadowableLv: Cil.lval) initializationValExpr
          | None -> self#makeShadowSpaceInitializerCalls initializerLocation
                    (initializedShadowableLv: Cil.lval) (defaultShadowValueForType (Cil.typeOf (Lval(initializedShadowableLv))))
        in
        if instrsToPrepend = [] then ()
        else
        initFunc.sbody <- { battrs = []; bstmts = [{
                labels = [];
                skind = (let origInstrList = match (List.hd initFunc.sbody.bstmts).skind with
                    Instr(orig) -> orig
                    | _ -> failwith "initializer function is not a single Instr statement"
                    in Instr(instrsToPrepend @ origInstrList));
                sid = 0;
                succs = [];
                preds = []
            }]}

    initializer
        (* Create a constructor function to hold the shadow write instructions
         * we are creating. This is a bit like CIL's "global initializer", which
         * we deliberately avoid using because CIL wants to put the call to it
         * in main(), which would be silly... we should use constructors instead. *)
        begin
            initFunc.svar <- findOrCreateFunc enclosingFile ("__libcrunch_crunch" ^ shadowString ^ "_init" )
                (TFun(TVoid([]), Some([]), false, [Attr("constructor", [AInt(101)])]))
                ;
            initFunc.svar.vstorage <- Static;
            initFunc.sbody <- { battrs = []; bstmts = [{
                labels = [];
                skind = Instr([]);
                sid = 0;
                succs = [];
                preds = []
            }]};
            enclosingFile.globals <- enclosingFile.globals @ [GFun(initFunc, initFunc.svar.vdecl)]
        end

    method vblock (b: block) : block visitAction = 
        currentBlock := Some(b);
        DoChildren

    method vstmt (outerS : stmt) : stmt visitAction = 
        match !currentFunc with
            None -> (* statement outside function? *) DoChildren
          | Some f ->
        (* We need to instrument returns. *)
        match outerS.skind with 
            Return(Some(returnExp), loc) -> (
                (* We have to pass some shadow value(s) back -- but only if the caller
                 * is instrumented. Can the inline function take care of that?
                 * No because if our return type is a struct, we have to do
                 * multiple pushes. *)
                ChangeDoChildrenPost(outerS, fun s -> match s.skind with
                    Return(Some(returnExp), loc) ->
                    begin
                    match !currentFuncCallerIsInstFlag with
                        None -> failwith "internal error: did not create caller-is-instrumented flag"
                      | Some callerIsInstrumentedFlagVar ->
                    (* OVERRIDEME: pure helpers require special handling here *)
                    let mkReturnShadowBlock rs instrs = {
                        battrs = [];
                        bstmts = [{
                            labels = [];
                            skind = Instr(instrs);
                            sid = 0;
                            succs = [];
                            preds = []
                        }; {
                            labels = [];
                            skind = rs;
                            sid = 0;
                            succs = [];
                            preds = []
                        }]
                    }
                    in
                    let mkReturnNoShadowBlock rs instrs = {
                        battrs = [];
                        bstmts = let justReturn = { labels = []; skind = rs; sid = 0; succs = []; preds = [] } in
                        if instrs = [] then [justReturn] else [{
                            labels = [];
                            skind = Instr(instrs);
                            sid = 0;
                            succs = [];
                            preds = []
                        }; justReturn ]
                    }
                    in
                    let (rs, shadowReturnInstrList) =
                    (s.skind, 
                    self#mapForAllShadowsInExpr (fun valExp -> fun shadowExp -> fun offsetExp ->
                        match shadowExp with
                            ShadowLocalLval(slv) -> 
                                Call( None,
                                (Lval(Var((helperFunctions#getPushLocalResultShadow ()).svar),NoOffset)),
                                [
                                    Lval(Var(callerIsInstrumentedFlagVar), NoOffset);
                                    Lval(slv);
                                ],
                                loc
                                )
                          | ShadowFreshFromRvals(ses) ->
                                Call( None,
                                (Lval(Var((helperFunctions#getPushResultShadowManifest ()).svar),NoOffset)),
                                [
                                    Lval(Var(callerIsInstrumentedFlagVar), NoOffset);
                                    valExp
                                ] @ ses,
                                loc
                                )
                          | ShadowFetch(LoadedFrom(lh,lo)) ->
                                Call( None,
                                (Lval(Var((helperFunctions#getFetchAndPushResultShadow ()).svar),NoOffset)),
                                [
                                    Lval(Var(callerIsInstrumentedFlagVar), NoOffset);
                                    valExp;
                                    mkAddrOf (lh,lo);
                                    descriptorExprForShadowableExpr valExp
                                ],
                                loc
                                )
                          | ShadowFetch(_) ->
                                Call( None,
                                (Lval(Var((helperFunctions#getFetchAndPushResultShadow ()).svar),NoOffset)),
                                [
                                    Lval(Var(callerIsInstrumentedFlagVar), NoOffset);
                                    valExp;
                                    nullPtr;
                                    descriptorExprForShadowableExpr valExp
                                ],
                                loc
                                )
                        ) returnExp
                    )
                    in
                    (* PROBLEM. Goto and Switch make use of 'ref statements'. 
                     * This is completely stupid, but too hard to fix right now.
                     * The consequence is that we can't just move labels from one
                     * statement to another with impunity. we have to make sure
                     * that whatever statement record gets the labels
                     * is the same OCaml object 
                     * as originally had them. 
                     * We do this by mutating s. 
                     * Effectively, 's' is updated from being a Return statement
                     * into being an If statement. It keeps the same labels. *)
                    match !currentFuncCallerIsInstFlag with 
                        None -> failwith "internal error: have not created caller-is-instrumented flag"
                     |  Some(callerIsInstVar) ->
                    let testForUninstCallerExpression = 
                        UnOp(LNot, Lval(Var(callerIsInstVar), NoOffset), boolType)
                    in
                    s.skind <- If(
                                testForUninstCallerExpression, 
                                (* true  *) mkReturnNoShadowBlock rs shadowReturnInstrList,
                                (* false *) mkReturnShadowBlock rs shadowReturnInstrList,
                            loc);
                    s
                end
                | _ -> s (* after change, not a Return of Some of *)
                ) (* end ChangeDoChildrenPost *)
            ) (* end Return case (outer) *)
       | _ -> DoChildren

    method vglob (g: global) : global list visitAction = 
        match g with 
            GVar(gvi, gii, loc) when self#exprNeedsShadow (Lval(Var(gvi), NoOffset)) ->
             if self#varIsOurs gvi then SkipChildren
             else (
                (debug_print 1 ("Saw global needing "^ shadowString ^ ", name `" ^ gvi.vname ^ "'\n"));
                List.iter (fun containedShadowedExpr -> 
                            match containedShadowedExpr with
                                Lval(lh, loff) ->
                                    self#prependShadowSpaceInitializer
                                        loc
                                        (lh, loff) None
                                | _ -> failwith "error: statically initializing a non-lvalue"
                        ) (self#containedShadowedPrimitiveExprsForExpr (Lval(Var(gvi), NoOffset)))
                        ;
                        (debug_print 1 ("Finished writing zero-init shadowables; writing non-zero-init ones for `" ^ gvi.vname ^ "'\n"));
                        let createCalls lv initExpr = 
                            if self#exprNeedsShadow (Lval(lv))
                            then
                                self#prependShadowSpaceInitializer
                                    loc 
                                    lv (Some(initExpr))
                            else ()
                        in
                        let _ = mapOverInitializedLvalues createCalls (Var(gvi), NoOffset) gii.init loc
                        in ()
                ;
                DoChildren)
            | _ -> DoChildren

    method vfunc (f: fundec) : fundec visitAction = 
        currentFunc := Some(f);
        currentFuncCallerIsInstFlag := None;
        shadowLocals := VarinfoMap.empty;
        tempLoadExprs := VarinfoMap.empty;
        (* (debug_print 1 ("CIL dump of function `" ^ f.svar.vname ^ "': ");
        Cil.dumpBlock (new plainCilPrinterClass) stderr 0 f.sbody;
        debug_print 1 "\n"); *)
        (* Do our own scan for AddrOf and StartOf. 
        * The CIL one is not trustworthy, because any array subexpression
        * has internally been tripped via a StartOf (perhaps now rewritten? FIXME)
        * meaning it contains false positives. *)
        let tempAddressTakenLocalNames = ref []
        in
        let _ = visitCilBlock (new addressTakenVisitor tempAddressTakenLocalNames) f.sbody
        in
        debug_print 1 ("Address-taken locals in `" ^ f.svar.vname ^ "' : [" ^ (
            List.fold_left (fun l -> fun r -> l ^ (if l = "" then "" else ", ") ^ r) "" !tempAddressTakenLocalNames
            ^ "]\n"));
        currentFuncAddressTakenLocalNames := !tempAddressTakenLocalNames
        ;
        (* Don't instrument our own (liballocs/libcrunch) functions that get -include'd. *)
        if self#varIsOurs f.svar then (currentFunc := None; currentFuncCallerIsInstFlag := None; SkipChildren)
        else
        let currentFuncCallerIsInstFlagVar = Cil.makeTempVar f ~name:"__caller_is_inst_" boolType
        in 
        currentFuncCallerIsInstFlag := Some(currentFuncCallerIsInstFlagVar);
        (
            let varNeedsLocalShadow = (fun v -> self#varinfoIsLocal v)
            in
            let maybeCreateShadow = fun vi -> 
                let shadowT = self#shadowTForT vi.vtype
                in
                match shadowT with
                    Some(st) -> 
                        debug_print 1 ("Creating shadow for local " ^ 
                            vi.vname ^ "; shadow has type " ^ (typToString st) ^ "\n");
                        let created = self#getOrCreateShadowLocal vi
                        in
                        Some(shadowT, created)
                  | None -> None
            in
            let _ = ignore (List.map maybeCreateShadow (List.filter varNeedsLocalShadow f.slocals))
            in
            let formalsNeedingShadows =  (List.filter (fun fvi -> 
                match self#shadowTForT fvi.vtype
                    with Some(_) -> true
                  | _ -> false
            ) f.sformals)
            in
            let _ = ignore (List.map maybeCreateShadow (List.filter varNeedsLocalShadow f.sformals))
            in
            (* The shadowTs were added by side-effect. 
             * What we haven't done is initialise the ones that correspond to formals.
             * Unlike locals, formals are already valid before initialisation.
             * We initialise them to an invalid shadow value.
             * Unfortunately this might depend on their actual value.
             * So we need to make a call to a helper.
             * We unroll all these calls. *)
            (* We reverse the formals, to generate an r-to-l order just like the caller
             * uses. Then we reverse the whole lot, because we're popping not pushing. 
             * Complication: actually we're peeking. So what offset are we peeking at?
             * The concatMapBlahBlah function doesn't give us what we want because it
             * only does offsets within a single argument. Instead, abstract over the 
             * offset and then assign when we have the whole list. *)
            let (getFunForOne : Cil.exp -> shadow_for_expr -> Cil.exp -> int -> Cil.instr) = fun valExp -> fun shadowExp -> fun offsetExp ->
                match shadowExp with
                    ShadowLocalLval(Var(svar), soffset) ->
                        fun idx -> Call(Some(Var(svar), soffset),
                            (Lval(Var((helperFunctions#getPeekArgumentShadow ()).svar),NoOffset)),
                            [
                                (* really do it? only if cookie was okay *)
                                Lval(Var(currentFuncCallerIsInstFlagVar), NoOffset);
                                (* offset on stack *)
                                makeIntegerConstant (Int64.of_int (idx)); (* DON'T adjust for the 
                                  cookie -- the inline function has to do it, because
                                  shadows are not necessarily single words in size. *)
                                (*  const void *ptr, the pointer value (for makeInvalidShadow) *)
                                valExp;
                                Const(CStr((expToString valExp) ^ ", offset " ^ (expToCilString offsetExp)))
                            ],
                            svar.vdecl (* loc *)
                        )
                 | ShadowFetch(LoadedFrom(lh,lo)) ->
                        fun idx -> Call(None,
                            (Lval(Var((helperFunctions#getPeekAndShadowStoreArgumentShadow ()).svar),NoOffset)),
                            [
                                (* really do it? only if cookie was okay *)
                                Lval(Var(currentFuncCallerIsInstFlagVar), NoOffset);
                                (* address of the stored pointer *)
                                mkAddrOf (lh,lo);
                                (* offset on stack of the shadow that was passed *)
                                makeIntegerConstant (Int64.of_int (idx)) (* DON'T adjust for the 
                                  cookie -- the inline function has to do it, because
                                  shadows are not necessarily single words in size. *);
                                (*  const void *ptr, the pointer value (for makeInvalidShadow) *)
                                valExp;
                                descriptorExprForShadowableExpr valExp;
                                (* the debugging string for the shadow stack tracer *)
                                Const(CStr((expToString valExp) ^ ", offset " ^ (expToCilString offsetExp)))
                            ],
                            (match lh with
                                (Var(vi)) -> vi.vdecl (* loc *)
                              | _ -> failwith "address-taken formal but a Mem lvalue"
                            )
                        )
                 | _ -> failwith "internal error: formal parameter lacks local or shadow-space shadow values"
            in
            let makeLv = (fun vi -> Lval(Var(vi), NoOffset))
            in
            let formalShadowInitFunReverseList = self#concatMapForAllShadowsInExprList
                getFunForOne (List.map makeLv (List.rev formalsNeedingShadows))
            in
            let formalShadowInitList = List.mapi (fun i -> fun f -> f i)
                (List.rev formalShadowInitFunReverseList)
            in
            let writeCallerInstFlag
             = [Call(Some(Var(currentFuncCallerIsInstFlagVar), NoOffset),
                Lval(Var((helperFunctions#getTweakArgumentShadowCookie ()).svar), NoOffset), 
                [CastE(voidConstPtrType, mkAddrOf (Var(f.svar), NoOffset))], instrLoc !currentInst)]
            in
            f.sbody <- { 
                battrs = f.sbody.battrs; 
                bstmts = {
                    labels = [];
                    skind = Instr(writeCallerInstFlag @ formalShadowInitList);
                    sid = 0;
                    succs = [];
                    preds = [] 
                } :: f.sbody.bstmts
            }
            ;
            ChangeDoChildrenPost(f, fun x -> currentFunc := None; currentFuncCallerIsInstFlag := None; x)
        )

  method vinst (outerI: instr) : instr list visitAction = begin
    currentInst := Some(outerI); (* used from vexpr *)
    ChangeDoChildrenPost([outerI], fun changedInstrs ->
        (* At this point, we've done our thing on the contained expressions.
         * Various queueInstrs have happened and the final instruction is a simplified
         * version of outerI, i.e. referencing temporaries assigned from expressions that 
         * have been hoisted up, checked, etc.
         * 
         * So the last instruction should be derived from outerI, and should be doing
         * the Set or Asm or Call that the user code intended. As a sanity check,
         * test that it really does.
         *)
        let finalI = List.nth changedInstrs ((List.length changedInstrs) - 1)
        in
        let _ = begin match (finalI, outerI) with
            (Set(_, _, _), Set(_, _, _)) -> ()
           | (Call(_, _, _, _), Call(_, _, _, _)) -> ()
           | (Asm(_, _, _, _, _, _), Asm(_, _, _, _, _, _)) -> ()
           | _ -> failwith "internal: outerI did not match finalI"
        end
        in
        let instrsPlusShadowUpdates = begin
            let f = match !currentFunc with
                Some(af) -> af
              | None -> failwith "instruction outside function"
            in
            match finalI with
                Set((lhost, loff), e, l) ->
                    let containedShadowedLvsL = List.map (fun e -> match e with
                        Lval(lh, lo) -> (lh, lo)
                      | _ -> failwith "impossible: lval yielded non-lv"
                    ) (self#containedShadowedPrimitiveExprsForExpr (Lval(lhost, loff)))
                    in
                    let containedShadowedExprsR = self#containedShadowedPrimitiveExprsForExpr e
                    in
                    let shadowableAssignments = List.combine containedShadowedLvsL containedShadowedExprsR
                    in
                    let getInstrsToPrepend ((writtenToLh, writtenToLo), writtenE) : instr list = (
                        (* We might be writing one or more shadowables on the lhs. 
                         *      If we're keeping a shadow, we need to write some shadow value.
                         *      Sometimes we have to fetch it;
                         *      Sometimes we can copy it;
                         *      Sometimes we can *create* it ... depending on rules
                         *)
                        let shadowDescr = self#shadowDescrForExpr writtenE
                        in
                        if self#hostIsLocal writtenToLh
                        then (
                            debug_print 1 ("Saw write to a local non-void pointer lval: " ^ (
                                lvalToString (writtenToLh, writtenToLo)
                            ) ^ ", so updating its shadow\n")
                            ;
                            self#localShadowUpdateInstrs (self#shadowLvalForLocalLval (writtenToLh, writtenToLo))
                                writtenE shadowDescr (instrLoc !currentInst)
                        )
                        else (
                            debug_print 1 ("Saw write to a non-local shadowable lval: " ^ (
                                lvalToString (writtenToLh, writtenToLo)
                            ) ^ ", so calling out the shadow-store hook\n")
                            ;
                            self#doNonLocalShadowableStoreInstr writtenE
                                  (writtenToLh, writtenToLo) shadowDescr (instrLoc !currentInst)
                        )
                    )
                    in
                    let instrsToPrepend = List.flatten (List.map getInstrsToPrepend shadowableAssignments)
                    @
                    (if (*!voidPtrHasShadow*) true then []
                    else (* OVERRIDEME *) [] )
                    (* This is a non-local write of a void* value. Problem: it may actually
                     * be storing to a non-void*-holding lvalue, i.e. one that should have
                     * shadow. We need to store something; doing nothing risks leaving stale
                     * shadow behind. We'd like to store the shadow appropriate for the type it
                     * *will* be used at, but can't predict this; as a "safe" default, we can
                     * store invalid shadow. *)
                    in begin
                    debug_print 1 "Queueing some instructions\n";
                    instrsToPrepend @ changedInstrs (* @ instrsToAppend *)
                    end
              | Call(olv, calledE, es, l) -> 
                  (* Don't instrument calls to our own (liballocs/libcrunch) functions that get -include'd. *)
                  (* Also don't instrument calls to __builtin_va_arg. *)
                  if (match calledE with Lval(Var(fvi), NoOffset)
                   when (self#varIsOurs fvi || fvi.vname = "__builtin_va_arg") -> true
                    | _ -> false)
                  then changedInstrs
                  else
                    let passesShadows = List.fold_left (fun acc -> fun argExpr -> 
                        acc || (* isNonVoidPointerType (Cil.typeOf argExpr) *)
                                   not (list_empty (self#containedShadowedPrimitiveExprsForExpr argExpr))
                    ) false es
                    in      
                    let returnsShadows = match olv with Some(lv) -> 
                        (* isNonVoidPointerType (Cil.typeOf (Lval(lv))) *)
                        not (list_empty (self#containedShadowedPrimitiveExprsForExpr (Lval(lv))))
                      | None -> false
                    in
                    let realCalledE, newChangedInstrs = (* OVERRIDEME: pure helper callees *)(calledE, changedInstrs)
                    in            
                    let shadowSpExpr = match findGlobalVarInFile "__shadow_sp" enclosingFile with
                                Some(sspv) -> Lval(Var(sspv), NoOffset)
                              | None -> failwith "internal error: did not find shadow stack pointer"
                    in
                    let (maybeSavedShadowStackPtr, shadowSaveInstrs)
                     = if passesShadows || returnsShadows then 
                        let vi = Cil.makeTempVar f ~name:"__shadow_stack_ptr" ulongPtrType
                        in
                        (Some(vi), [Set((Var(vi), NoOffset), shadowSpExpr, instrLoc !currentInst)])
                        else (None, [])
                    in
                    begin
                        let shadowPassInstructions = if (not passesShadows) then [] else (
                            let callForOneOffset valExpr shadowExpr offsetExpr = 
                                debug_print 1 ("Pushing shadow for shadowable-contained-in-argument expr " ^ (expToString valExpr) ^ "\n");
                                match shadowExpr with
                                    ShadowLocalLval(slv) -> 
                                        Call( None,
                                        (Lval(Var((helperFunctions#getPushLocalArgumentShadow ()).svar),NoOffset)),
                                        [
                                            Lval(slv)
                                        ],
                                        instrLoc !currentInst
                                        )
                                  | ShadowFreshFromRvals(ses) ->
                                        Call( None,
                                        (Lval(Var((helperFunctions#getPushArgumentShadowManifest ()).svar),NoOffset)),
                                        [
                                            valExpr
                                        ] @ ses,
                                        instrLoc !currentInst
                                        )
                                  | ShadowFetch(LoadedFrom(lh,lo)) ->
                                        Call( None,
                                        (Lval(Var((helperFunctions#getFetchAndPushArgumentShadow ()).svar),NoOffset)),
                                        [
                                            valExpr;
                                            mkAddrOf (lh,lo);
                                            descriptorExprForShadowableExpr valExpr
                                        ],
                                        instrLoc !currentInst
                                        )
                                  | ShadowFetch(_) ->
                                        Call( None,
                                        (Lval(Var((helperFunctions#getFetchAndPushArgumentShadow ()).svar),NoOffset)),
                                        [
                                            valExpr;
                                            nullPtr;
                                            descriptorExprForShadowableExpr valExpr
                                        ],
                                        instrLoc !currentInst
                                        )
                            in
                            self#concatMapForAllShadowsInExprList callForOneOffset (List.rev es) (* push r to l! *)
                        )
                        in
                        let cookieStackAddrVar = ref None
                        in
                        let calleeLval = match realCalledE with
                                    Lval(lv) -> lv
                                  | _ -> failwith "internal error: calling a non-lvalue"
                        in
                        let shadowPushCookieInstructions = 
                            if passesShadows || returnsShadows then (
                            let spVar = Cil.makeTempVar f ~name:"__cookie_stackaddr" ulongPtrType
                            in
                            spVar.vattr <- [Attr("unused", [])];
                            cookieStackAddrVar := Some(spVar);
                            [Call(None, 
                                Lval(Var((helperFunctions#getPushArgumentShadowCookie ()).svar), NoOffset),
                                [CastE(voidConstPtrType, mkAddrOf calleeLval)], instrLoc !currentInst);
                             (* also remember the cookie stackaddr *)
                             Set((Var(spVar), NoOffset), shadowSpExpr, instrLoc !currentInst)
                            ]
                            ) else []
                        in
                        let returnShadowPeekOrStoreInstructions = 
                          (* OVERRIDEME: pure helper*) (
                          match olv with
                            None -> []
                          | Some(lhost, loff) -> (
                                let writeOneLocal shadowLval valExpr _ offsetExpr = 
                                [Call(
                                    Some(shadowLval),
                                    (Lval(Var((helperFunctions#getPeekResultShadow ()).svar),NoOffset)),
                                    [
                                        (* really? only if *)
                                        (match !cookieStackAddrVar with
                                            Some(vi) ->
                                                BinOp(Ne, Lval(Mem(Lval(Var(vi), NoOffset)), NoOffset),
                                                    CastE(ulongType, mkAddrOf calleeLval), boolType)
                                          | None -> failwith "error: no saved shadow stack pointer for cookie"
                                        )
                                          ;
                                        (* offset? *)
                                        offsetExpr; 
                                        (*  const void *ptr *)
                                        valExpr (* Lval(lhost, loff) *);
                                        Const(CStr("call to " ^ (expToString calledE)))
                                    ],
                                    instrLoc !currentInst
                                )]
                                in
                                let callsForOneLocal valExpr blah offsetExpr = 
                                    writeOneLocal (self#shadowLvalForLocalLval (lhost, loff)) valExpr blah offsetExpr
                                in
                                let callsForOneNonLocal valExpr blah offsetExpr =
                                    let svi = Cil.makeTempVar f ~name:"__shadow_return_" shadowType
                                    in
                                    let lvExpr = (Lval(lhost, loff)) in
                                    (* write to a local temp, then do the shadow-space store *)
                                    (writeOneLocal (Var(svi), NoOffset) valExpr blah offsetExpr) @ (
                                     self#doNonLocalShadowableStoreInstr
                                        (* shadowable value *) lvExpr
                                        (* where it's been/being stored *) (lhost, loff)
                                        (* its shadow *) (ShadowLocalLval(Var(svi), NoOffset))
                                        (instrLoc !currentInst)
                                    )
                                in
                                if not (list_empty (self#containedShadowedPrimitiveExprsForExpr (Lval(lhost, loff))))
                                then (
                                    debug_print 1 ("Saw call writing to [one or more] shadowable lval: " ^ (
                                        lvalToString (lhost, loff)
                                    ) ^ "\n")) else ();
                                let instrFunc = if self#hostIsLocal lhost
                                    then (
                                        debug_print 1 "Local, so updating/invalidating its shadow.\n";
                                        callsForOneLocal
                                    ) else (
                                        debug_print 1 "Host is not local\n";
                                        callsForOneNonLocal
                                    )
                                in
                                (* we're popping/peeking, so reverse *)
                                List.flatten (List.rev (self#mapForAllShadowsInExpr instrFunc (Lval(lhost, loff))))
                            )
                        )
                     in
                        let shadowCleanupInstructions = (
                            if passesShadows || returnsShadows then (
                              match maybeSavedShadowStackPtr with
                                Some(spvi) -> [Call( None,
                                                (Lval(Var((helperFunctions#getCleanupShadowStack ()).svar),NoOffset)),
                                                [
                                                    (* ptr *)
                                                    Lval(Var(spvi), NoOffset)
                                                ],
                                                (instrLoc !currentInst)
                                                )]
                              | None -> failwith "internal error: didn't save shadow stack pointer"
                            ) else []
                        )
                        in
                        shadowSaveInstrs @ 
                            shadowPassInstructions @ 
                            shadowPushCookieInstructions @
                            newChangedInstrs @ 
                            returnShadowPeekOrStoreInstructions @ 
                            shadowCleanupInstructions
                end
              | (* Asm(attrs, instrs, locs, u, v, l) -> *) _ -> changedInstrs
        end (* let instrsWithCachedShadowUpdates = begin ... *)
        in instrsPlusShadowUpdates
  ) (* end ChangeDoChildrenPost *)
  end

  method vlval outerLv =
        currentLval := Some(outerLv);
        DoChildren (* OVERRIDEME *)
        
  method vexpr (outerE: exp) : exp visitAction =
    debug_print 1 (("Visiting expression: " ^ (expToString outerE)) ^ "\n");
    debug_print 1 (("CIL form: " ^ (expToCilString outerE)) ^ "\n");
    (* let l = instrLoc !currentInst in *)
    match !currentFunc with
        None -> (* expression outside function *) SkipChildren
      | Some(f) -> 
            DoChildren (* OVERRIDEME *)
end
