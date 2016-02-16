(* Copyright (c) 2014--16,
 *  Stephen Kell        <stephen.kell@cl.cam.ac.uk>
 *
 * and based on logwrites.ml, which is 
 *
 * Copyright (c) 2001-2002, 
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

open Cil
open Pretty
open Map
open Str
open Cilallocs
module E = Errormsg
module H = Hashtbl

type helperFunctionsRecord = {
 mutable fetchBoundsInl : fundec; 
 mutable fetchBoundsOol : fundec; 
 mutable makeBounds : fundec; 
 mutable pushLocalArgumentBounds : fundec; 
 mutable pushArgumentBoundsBaseLimit : fundec; 
 mutable fetchAndPushArgumentBounds : fundec;
 mutable pushArgumentBoundsCookie : fundec; 
 mutable tweakArgumentBoundsCookie : fundec;
 mutable peekArgumentBounds : fundec; 
 mutable pushLocalResultBounds : fundec; 
 mutable pushResultBoundsBaseLimit : fundec; 
 mutable fetchAndPushResultBounds : fundec;
 mutable peekResultBounds : fundec; 
 mutable cleanupBoundsStack : fundec; 
 mutable makeInvalidBounds : fundec; 
 mutable checkDerivePtr : fundec; 
 mutable primaryCheckDerivePtr : fundec; 
 mutable detrap : fundec; 
 mutable checkLocalBounds : fundec; 
 mutable storePointerNonLocal : fundec;
 mutable primarySecondaryTransition : fundec
}

let instrLoc (maybeInst : Cil.instr option) =
   match maybeInst with 
   Some(i) -> Cil.get_instrLoc i
 | None -> locUnknown

let varinfoIsLocal vi currentFuncAddressTakenLocalNames = not vi.vglob && 
    ( let isAT = (List.mem vi.vname currentFuncAddressTakenLocalNames)
      in
        (if isAT then debug_print 0 ("Local var " ^ vi.vname ^ " would count as local " ^ 
        "but is address-taken\n") else ())
        ;
        not isAT
    )

let hostIsLocal lhost currentFuncAddressTakenLocalNames = 
    match lhost with
        Var(vi) -> varinfoIsLocal vi currentFuncAddressTakenLocalNames
          | Mem(_) -> false

let boundsTAsNumber maybeBoundsT gs = 
    let bounds_t = findStructTypeByName gs "__libcrunch_bounds_s"
    in
    match maybeBoundsT with
        None -> 
            (* Zero bounds objects. *)
            Int64.of_int 0
      | Some(TComp(ci, attrs)) when ci.cname = "__libcrunch_bounds_s" ->
            (* A singleton bounds_t. *)
            Int64.of_int 1
      | Some(TArray(TComp(ci, attrs), Some(boundExpr), [])) when ci.cname = "__libcrunch_bounds_s" ->
            (* An array of bounds_t. *)
            begin match constInt64ValueOfExpr boundExpr with
                Some(x) -> x
              | None -> failwith "bounds array type does not have constant bound"
            end
      | _ -> failwith "not a bounds type (asNumber)"

let boundsTTimesN maybeBoundsT (n : int64) gs =
    debug_print 0 ((Int64.to_string n) ^ " times bounds type " ^ 
        (match maybeBoundsT with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        " is: "); flush stderr;
    let bounds_t = findStructTypeByName gs "__libcrunch_bounds_s"
    in
    let prod = match maybeBoundsT with
        None -> None
      | Some(TComp(ci, attrs)) when ci.cname = "__libcrunch_bounds_s" ->
            if n = Int64.of_int 1 then Some(bounds_t)
            else Some(TArray(bounds_t, Some(Const(CInt64(n, IInt, None))), []))
      | Some(TArray(TComp(ci, attrs), Some(boundExpr), [])) when ci.cname = "__libcrunch_bounds_s" ->
            (* Okay, now we have n times as many. *)
            Some(
                TArray(bounds_t, 
                        Some(Const(CInt64(Int64.mul n (match constInt64ValueOfExpr boundExpr with
                            Some(m) -> m
                          | None -> failwith "array bound is not constant (*)"), IInt, None))),
                      []
                )
            )
      | _ -> failwith "not a bounds type (*)"
    in
    debug_print 0 ((match prod with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        "\n"); flush stderr; 
    prod

let boundsTPlusN maybeBoundsT (n : int64) gs =
    debug_print 0 ((Int64.to_string n) ^ " plus bounds type " ^ 
        (match maybeBoundsT with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        " is: "); flush stderr;
    let bounds_t = findStructTypeByName gs "__libcrunch_bounds_s"
    in
    let sum = match maybeBoundsT with
        None -> if n = Int64.of_int 0 then None
                else if n = Int64.of_int 1 then Some(bounds_t)
                else Some(TArray(bounds_t, Some(makeIntegerConstant n), []))
      | Some(TComp(ci, attrs)) when ci.cname = "__libcrunch_bounds_s" -> 
            if n = Int64.of_int 0 then Some(bounds_t)
            else Some(TArray(bounds_t, Some(makeIntegerConstant ((Int64.add n (Int64.of_int 1)))), []))
      | Some(TArray(TComp(ci, attrs), Some(boundExpr), [])) when ci.cname = "__libcrunch_bounds_s" ->
            (* Okay, now we have n more. *)
            Some(TArray(TComp(ci, attrs), 
                Some(makeIntegerConstant(Int64.add n (match constInt64ValueOfExpr boundExpr with
                        Some(m) -> m
                      | None -> failwith "array bound is not constant (+)"
                    ))),
                []
            ))
      | _ -> failwith "not a bounds type (+)"
    in
    debug_print 0 ((match sum with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        "\n");
    sum

let boundsTPlusBoundsT maybeBoundsT1 maybeBoundsT2 gs =
    let bounds_t = findStructTypeByName gs "__libcrunch_bounds_s"
    in
    let sum = match maybeBoundsT2 with
        None -> boundsTPlusN maybeBoundsT1 (Int64.of_int 0) gs
      | Some(TComp(ci, attrs)) when ci.cname = "__libcrunch_bounds_s" -> 
            boundsTPlusN maybeBoundsT1 (Int64.of_int 1) gs
      | Some(TArray(TComp(ci, attrs), Some(boundExpr), [])) when ci.cname = "__libcrunch_bounds_s" ->
            let m = (match constInt64ValueOfExpr boundExpr with
                Some n -> n
              | None -> failwith "internal error: bounds type has unbounded array"
            )
            in
            boundsTPlusN maybeBoundsT1 m gs
      | _ -> failwith "not a bounds type (T plus T)"
    in
    debug_print 0 ("Sum of bounds types " ^ 
        (match maybeBoundsT1 with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        " and " ^ 
        (match maybeBoundsT2 with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        " is " ^
        (match sum with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        "\n");
    sum

let rec boundsTForT (t : Cil.typ) gs = 
    let bounds_t = findStructTypeByName gs "__libcrunch_bounds_s"
    in
    (* We want to produce either nothing, or a single bounds_t, or 
     * a one-dimensional array of bounds_ts. 
     * This is basically because structs are heterogeneous -- they might contain
     * an array of m pointers and then an array of n pointers. 
     * There's no way to preserve this structure while still keeping,
     * a single type for the whole structure's bounds (i.e. without
     * creating a new struct type mirroring the input struct type;
     * we could also have done this). *)
    let maybeBoundsT = match t with
        TVoid(attrs) ->  failwith "asked bounds type for void"
      | TInt(ik, attrs) -> None
      | TFloat(fk, attrs) -> None
      | TPtr(pt, attrs) -> (match (Cil.typeSig pt) with TSBase(TVoid(_)) -> None | _ -> Some(bounds_t))
      | TArray(at, None, attrs) -> failwith "asked bounds type for unbounded array type"
      | TArray(at, Some(boundExpr), attrs) -> 
            boundsTTimesN (boundsTForT at gs) (match constInt64ValueOfExpr boundExpr with
                    Some n -> n
                  | None -> failwith "getting bounds type for non-constant array bounds"
                ) gs
      | TFun(_, _, _, _) -> failwith "asked bounds type for incomplete (function) type"
      | TNamed(ti, attrs) -> boundsTForT ti.ttype gs
      | TComp(ci, attrs) -> 
            List.fold_left (fun x -> fun y -> boundsTPlusBoundsT x y gs) None (
                List.map (fun fi -> boundsTForT fi.ftype gs) ci.cfields
            )
      | TEnum(ei, attrs) -> None
      | TBuiltin_va_list(attrs) -> None
    in
    debug_print 0 ("Bounds type for " ^ 
        (typToString t) ^ 
        " is " ^ (match maybeBoundsT with Some(boundsT) -> typToString boundsT | _ -> "none") ^ 
        "\n");
    maybeBoundsT

let rec boundsIndexExprForOffset (startIndexExpr: Cil.exp) (offs : Cil.offset) (host : Cil.lhost) (prevOffsets : Cil.offset) gs = 
    debug_print 0 ("Hello from boundsIndexExprForOffset\n");
    let bounds_t = findStructTypeByName gs "__libcrunch_bounds_s"
    in
    let indexExpr = match offs with
        Field(fi, nextOffset) -> 
            debug_print 0 ("Hit Field case\n");
            let compTypeBeingIndexed = Cil.typeOf (Lval(host, prevOffsets))
            in
            let rec addEarlierFields = fun acc -> fun someFis -> (
                debug_print 0 ("In addEarlierFields\n");
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
            debug_print 0 ("Found " ^ (string_of_int (List.length earlierFields)) ^ "\n");
            (* Add up all the boundsTs for *earlier* fields in the struct. *)
            let fieldOffsetAsBoundsT = List.fold_left 
                (fun x -> fun y -> boundsTPlusBoundsT x y gs) 
                None 
                (List.map (fun fi -> boundsTForT fi.ftype gs) earlierFields)
            in
            let fieldOffset = boundsTAsNumber fieldOffsetAsBoundsT gs
            in
            debug_print 0 ("Recursing on nextOffset in (ignore host): " ^ (lvalToString (host, nextOffset)) ^ "\n");
            boundsIndexExprForOffset
                (BinOp(PlusA, startIndexExpr, makeIntegerConstant fieldOffset, intType))
                nextOffset host (offsetAppend prevOffsets (Field(fi, NoOffset))) gs
      | Index(intExp, nextOffset) -> 
            debug_print 0 ("Hit Index case\n");
            (* We're indexing into an array at intExp (which we've already 
             * checked is in bounds).
             *
             * Example:
             * 
             *    x[i][j].p
             * 
             * We want the expression for i.
             * First we recursively compute the offset that we'd want if i were zero.
             * Then we add i times the array size for Cil.typeOf (x[i]), 
             * i.e. the number of bounds objects that the whole [j].p tail accounts for.
             *)
            let offsetUpToHere = offsetAppend prevOffsets (Index(intExp, NoOffset))
            in
            debug_print 0 ("Recursing on nextOffset in (ignore host): " ^ (lvalToString (host, nextOffset)) ^ "\n");
            let zeroIndexExpr = boundsIndexExprForOffset startIndexExpr nextOffset
                host offsetUpToHere gs
            in
            (BinOp(PlusA, zeroIndexExpr, ( 
                (BinOp(Mult, intExp, (
                    let elemT = Cil.typeOf (Lval(host, offsetUpToHere))
                    in
                    let maybeElemTBoundsT = boundsTForT elemT gs
                    in
                    makeIntegerConstant (boundsTAsNumber maybeElemTBoundsT gs)
                ), intType))
            ), intType))
      | NoOffset -> startIndexExpr
    in
    debug_print 0 ("Bounds index expression for indexing " ^ 
        (lvalToString (host, prevOffsets)) ^ " giving " ^ 
        (lvalToString (host, offsetFromList ((offsetToList prevOffsets) @ (offsetToList offs)))) ^ 
        " is " ^ (expToString indexExpr) ^ "\n");
    indexExpr

let makeBoundsLocal vi bt enclosingFunction enclosingFile = 
    Cil.makeTempVar enclosingFunction ~name:("__cil_localbound_" ^ vi.vname ^ "_") bt

let makeSizeOfPointeeType ptrExp = 
    let pointeeT = Cil.typeOf (Lval(Mem(ptrExp), NoOffset))
    in
    let rec decayPointeeArrayT pt = match pt with
      | TArray(at, _, _) -> decayPointeeArrayT at
      | TNamed(ti, attrs) -> decayPointeeArrayT ti.ttype
      | _ -> pt
    in
    SizeOf(decayPointeeArrayT pointeeT)

let getOrCreateBoundsLocal vi enclosingFunction enclosingFile (boundsLocals : Cil.varinfo VarinfoMap.t ref) = 
    debug_print 0 ("Ensuring we have bounds local for " ^ vi.vname ^ "\n");
    try  
        let found = VarinfoMap.find vi !boundsLocals
        in 
        debug_print 0 ("Already exists\n");
        found
    with Not_found -> 
     debug_print 0 ("Creating new bounds local for local " ^ vi.vname ^ "\n");
     let maybe_bt = boundsTForT vi.vtype enclosingFile.globals 
     in
     let bt = match maybe_bt with
            Some(bt) -> bt
          | None -> failwith ("creating bounds local for a type that doesn't need it: " ^ 
                typToString vi.vtype ^ "\n")
     in
     let newLocalVi = makeBoundsLocal vi bt enclosingFunction enclosingFile 
     in
     let newBoundsLocalsMap = (VarinfoMap.add vi newLocalVi !boundsLocals)
     in
     boundsLocals := newBoundsLocalsMap;
     newLocalVi

let boundsLvalForLocalLval (boundsLocals : Cil.varinfo VarinfoMap.t ref) enclosingFunction enclosingFile ((lh, loff) : lval) : lval =
    debug_print 0 "Hello from boundsLvalForLocalLval\n";
    let gs = enclosingFile.globals
    in
    match lh with
        Var(local_vi) -> 
            debug_print 0 "Hello from Var case\n";
            let boundsVi = getOrCreateBoundsLocal local_vi enclosingFunction enclosingFile boundsLocals 
            in 
            let indexExpr = boundsIndexExprForOffset zero loff (Var(local_vi)) NoOffset gs
            in
            debug_print 0 ("Bounds index expr is `" ^ (expToString indexExpr) ^ "'\n");
            let offs = (
                match Cil.typeSig (Cil.typeOf (Lval(Var(boundsVi), NoOffset))) with
                    TSArray(_) -> Index(indexExpr, NoOffset)
                  | TSComp(_) -> 
                        if not (isStaticallyZero indexExpr) then 
                                failwith "singleton bounds but non-zero bounds expr"
                       else NoOffset
                  | _ -> failwith "bad bounds var type"
            )
            in
            (Var(boundsVi), offs)
        | _ -> failwith "local lvalue not a Var"

let makeCallToMakeBounds outputLval baseExpr limitExpr loc makeBoundsFun =
    Call( outputLval,
            (Lval(Var(makeBoundsFun.svar),NoOffset)),
            [
                (*  unsigned long ptr *)
                CastE(ulongType, baseExpr)    (* i..e the *value* of the pointer we just wrote *)
            ;   (* unsigned long limit *)
                CastE(ulongType, limitExpr)
            ],
            loc
        )
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

let getLoadedFromAddr someE tempLoadExprMap currentFuncAddressTakenLocalNames = 
    (* This is simpler than it seems. 
     * If the pointer expression is some lvalue l, 
     * and it's not local,
     * then it's a pointer being "loaded" from &l.
     * We then simplify &l, which might give us something simple
     * like *p if our pointer is **p
     * (rather than &**p). 
     * Non-lvalue pointers that are not derived from other pointers
     * are always StartOf or AddrOf, which we handle separately.
     *)
    debug_print 0 ("Getting loaded-from addr for ptr expr: " ^ (expToCilString someE) ^ "\n");
    match someE with
    |   Lval(loadHost, loadOffs) 
            when not (hostIsLocal loadHost currentFuncAddressTakenLocalNames) -> 
            debug_print 0 ("It's a non-local lval\n");
            Some(simplifyPtrExprs (mkAddrOf (loadHost, loadOffs)))
    |   Lval(Var(vi), loadOffs) -> (
          (* We might also be dealing with a temporary whose original loaded-from expression
           * was saved. *)
          try
               let foundE = VarinfoMap.find vi tempLoadExprMap
               in
               debug_print 0 ("Found a saved loaded-from expr: " ^ (
                match foundE with Some(fe) -> expToString fe | None -> "(saved None)") ^ "\n");
               foundE
          with Not_found -> 
                debug_print 0 ("Didn't find a saved loaded-from expr\n");
                None
          )
    |   StartOf(Mem(memExp), NoOffset) -> (* memExp points to an array which we make decay to ptr. *)
            (* Example (from LBM): calling     *dstGrid, 
             * where dstGrid is a pointer to an array of double.
             * Here *dstGrid has type double[],
             * so the expression is equivalent to &( *dstGrid)[0],
             * so we get StartOf(Mem(dstGrid)).
             * What is the loaded-from address?
             * It's clearly dstGrid, i.e. "dstGrid" holds a pointer whose
             * bounds we might have stored at shadow(dstGrid).
             *)
            Some(memExp) (* FIXME: is this always an lvalue? might need to recurse? *)
    |   _ -> 
        debug_print 0 ("It's not an lvalue\n");
        None

type bounds_expr = 
    BoundsLval of lval
  | BoundsBaseLimitRvals of (Cil.exp * Cil.exp)
  | MustFetch of Cil.exp option (* the loaded-from address of the pointer whose bounds
                                 * we're getting, *or* of another pointer which necessarily
                                 * has the same bounds (i.e. in the same object). *)


let rec boundsExprForExpr e currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs = 
    if isStaticallyNullPtr e then BoundsBaseLimitRvals(zero, one)
    else
    let simplifiedPtrExp = simplifyPtrExprs e in
    debug_print 0 ("Getting bounds expr for expr " ^ (expToString simplifiedPtrExp) ^ "\n");
    match simplifiedPtrExp with 
        Lval(Var(vi), offs) when varinfoIsLocal vi currentFuncAddressTakenLocalNames -> 
            BoundsLval(localLvalToBoundsFun (Var(vi), offs))
      | _ ->
    debug_print 0 ("Expr has no local bounds\n");
    let maybeLoadedFromExpr = getLoadedFromAddr simplifiedPtrExp tempLoadExprs currentFuncAddressTakenLocalNames in
    let rec offsetContainsField offs = match offs with
            NoOffset -> false
          | Field(fi, _) -> true
          | Index(intExp, rest) -> offsetContainsField rest
    in
    let handleAddrOfVarOrField someHost someOffset = 
        debug_print 0 "Handling AddrOf(var or field)...\n";
        let rec splitLeadingIndexesAndReverse revAccIndexes offlist = match offlist with
            [] -> (revAccIndexes, [])
          | Field(fi, ign)     :: more -> 
                (* not an Index, so stop now *)
                (revAccIndexes, List.rev offlist)
          | Index(intExp, ign) :: more -> 
                splitLeadingIndexesAndReverse (Index(intExp, ign) :: revAccIndexes) more
          | NoOffset :: _ -> failwith "impossible: NoOffset in offset list"
        in
        let splitTrailingIndexes offlist = splitLeadingIndexesAndReverse [] (List.rev offlist)
        in
        let (trailingIndexes, leadingOthers) = splitTrailingIndexes (offsetToList someOffset)
        in
        let indexedLval = (someHost, offsetFromList leadingOthers)
        in
        let indexedExpr = Lval(indexedLval)
        in
        let indexedType = Cil.typeOf indexedExpr
        in
        (* let lastOffsetIsAnIndex = match (List.rev offlist) with
            Index(_, _) -> true
          | _ -> false
        in *)
        let rec multiplyAccumulateArrayBounds acc t = 
            match t with
              | TArray(at, None, attrs) -> 
                    (* failwith ("accumulating array bounds through unbounded array type: " ^ (typToString t)) *)
                    (* this can happen -- see notes below about AddrOf/StartOf localarr/globlarr. *)
                    raise Not_found
              | TArray(at, Some(boundExpr), attrs) -> begin
                    match constInt64ValueOfExpr boundExpr with
                            Some n -> multiplyAccumulateArrayBounds (Int64.mul acc n) at
                          | None -> failwith "getting array bounds for non-constant array bounds"
                end
              | TNamed(ti, attrs) -> multiplyAccumulateArrayBounds acc ti.ttype
              | _ -> (acc, t)
        in
        let (arrayElementCount, arrayElementT) = multiplyAccumulateArrayBounds (Int64.of_int 1) indexedType
        in
        (* NOTE: to avoid introducing pointer arithmetic that we will later redundantly check, 
         * we instead do our arithmetic in the integer domain. *)
        let baseExpr = CastE(ulongType, mkAddrOrStartOf indexedLval)
        in
        let limitExpr = BinOp(PlusA, baseExpr, BinOp(
            Mult, makeIntegerConstant arrayElementCount, (SizeOf(arrayElementT)), ulongType), 
            ulongType)
        in
        BoundsBaseLimitRvals(baseExpr, limitExpr)
    in
    match simplifiedPtrExp with
        Lval(Var(someVi), someOffset) when hostIsLocal (Var(someVi)) currentFuncAddressTakenLocalNames -> 
            (* - it's an also-local pointer (perhaps a subobject of a local struct/array)
             *      => we can copy the bounds *)
            let (sourceBoundsLval : lval) = localLvalToBoundsFun (Var(someVi), someOffset)
            in
            BoundsLval(sourceBoundsLval)
      | BinOp(PlusPI, Lval(Var(someVi), someOffset), someIntExp, somePtrT) 
        when hostIsLocal (Var(someVi)) currentFuncAddressTakenLocalNames ->
            (* This is a simple adjustment of a locally cached-bounds ptr, 
             * rather than a straight copy. 
             * 
             * By construction, we've just checked the adjustment. If it wasn't
             * legit, we created a trapped pointer.
             * 
             * Either way, the bounds of the adjusted pointer are always identical 
             * to the bounds of the pre-adjusted pointer. So just copy them. *)
            let (sourceBoundsLval : lval) = localLvalToBoundsFun (Var(someVi), someOffset)
            in
            BoundsLval(sourceBoundsLval)
      | AddrOf(Var(someVi), someOffset)  -> (
        debug_print 0 ("Expr " ^ (expToString e) ^ " takes addr of var or field (case 1)\n");
        try handleAddrOfVarOrField (Var(someVi)) someOffset
        with Not_found -> MustFetch(None)
        )
        (* - we're taking the address of a local variable, global variable or subobject.
                => bounds are implied by the address value and the type of the object.
           - or we're taking the address of a subobject or array element 
             of a local or global variable
                => similar, just a bit more complex:

          - strip any Index()s from the tail of the offset:
              the address of the host with NoOffset is the base ptr, 
              and the type of that thing is the element type.
          - re-descend through the Index()s, multiplying up the array size (if any)
          - the bound is the accumulated array size times the element size

          NOTE that by definition, here the host is *not* local, because it's address-taken..
          The offset may be NoOffset.
          
          NOTE also that someOffset might be out-of-bounds! That's okay; 
          the bounds we make will reflect that. HMM, but will the *pointer* we make
          be trapped?
        *)
      | StartOf(Var(someVi), someOffset) -> (
            (* This is similar but not identical to the above. 
             * In particular, since the variable is an array, 
             * we can't just use its type to get its bounds. 
             * Instead, 
             *  - we might not have an the array size in the type
             *     e.g. extern char *ss[];
             *  - we might have one but not *trust* it...
             * 
             * ... the latter is because casts to pointers-to-sized-arrays
             * can happen. In that case, the user could manufacture a
             * bound just by doing, say
             * 
             *    char ( *ptr )[42] = (char ( * )[42]) p;
             *    p[41];
             * 
             * So what to do?
             * And might we also get this problem with AddrOf?
             * Several cases:
             *     &localarr[41]   -- localarr is a var, so use var type -- *we trust it*, it's local
             *     &localp[41]     -- localp   is a ptr, so use ptr *bounds* to get bounds
             *     &globlarr[41]   -- globlarr is a var; might not have a bound, but if so we can trust it (check at link time)
             *     &globlp[41]     -- globalp  is a ptr for which we don't have bounds
             * 
             * -- the cast problem only happens in the ptr case. 
             * And in the ptr case we are already tracking bounds (for the ptr).
             * If we did a cast, we also created a local temporary.
             * HMM. Time to write two test cases:
             *    - one that we trap the "manufactured bounds" case;
             *    - another that container_of works as expected.
             * 
             * Another question: does CIL generate AddrOf(Var(arrayVar), Index(someIndex))?
             * Yes, I think it can.
             * So the localarr and globlarr cases fall under "AddrOf", not "StartOf".
             * 
             * So, short answer: both this case and the one above need to handle the 
             * "no array bound available" case. We handle untrustworthy bounds by
             * other means (CHECK).
             *)
            debug_print 0 ("Expr " ^ (expToString e) ^ " takes addr of var or field (case 2)\n");
            try handleAddrOfVarOrField (Var(someVi)) (offsetFromList (
               (offsetToList someOffset) @ [Index(zero, NoOffset)]
            ))
            with Not_found -> MustFetch(None)
        )
      | AddrOf(Mem(memExp), someOffset) 
        when offsetContainsField someOffset ->
            (* We're taking the address of a field inside a heap object, possibly then
             * applying some indexing. 
             * The type of the field always gives us the bound. 
             * This works much like the variable-subobject case. 
             * Note that offsets never deref! So we're always staying within
             * the same object. *) (
              debug_print 0 ("Expr " ^ (expToString e) ^ " takes addr of var or field (case 3)\n");
              handleAddrOfVarOrField (Mem(memExp)) someOffset
            )
            (* ELSE
             * - If the offset is empty or only indexes, 
             *   it's an unconstrained expression -- say (&( *p)) or (&( p[0])) or (&( p[j][k])).
             *   -- The bounds of the derived pointer are the same as those of p.
             *      We know those already, iff p is a local var.
             *      It might be! 
             *      (We're not taking the address of p, but of a thing it points to.)
             *)
      | StartOf(Mem(memExp), someOffset) 
        when offsetContainsField someOffset -> (
             debug_print 0 ("Expr " ^ (expToString e) ^ " takes addr of var or field (case 4)\n");
             handleAddrOfVarOrField (Mem(memExp)) (offsetFromList (
               (offsetToList someOffset) @ [Index(zero, NoOffset)]
             ))
        )
      | AddrOf(Mem(Lval(Var(lvi), varOff)) (* what about nested lvals in here? *), addrOff)
        when varinfoIsLocal lvi currentFuncAddressTakenLocalNames ->
            (* We're dereferencing a local pointer and then taking the address of 
             * the object we find there, or a subobject of it. By pattern-match semantics, 
             * someOffset does not contain any Field components, so the subobject is
             * just an array element. *) 
            if offsetContainsField addrOff then failwith "internal error: offset contains field"
            else
            (* Therefore, the bounds are simply those of the local pointer. 
             * FIXME: are we simply sidestepping the checks that the indexing is in-bounds? *)
            let sourceBoundsLval = localLvalToBoundsFun (Var(lvi), varOff)
            in
            BoundsLval(sourceBoundsLval)
            (* For the question about nested lvalues, above: by definition, thanks to CIL,
             * these have no side effect, so they won't be updating any bounds themselves,
             * so we don't need to intercept them.
             * So, in fact, it doesn't matter what's in memExp. We're selecting a 
             * subobject of it, so the bounds are determined by the *type* of the 
             * subobject being selected. *)
      | StartOf(Mem(Lval(Var(lvi), varOff)) (* what about nested lvals in here? *), startOff)
        when varinfoIsLocal lvi currentFuncAddressTakenLocalNames ->
            (* We're dereferencing a local pointer and then taking the array-start address of 
             * the object we find there, or a subobject of it. By pattern-match semantics, 
             * someOffset does not contain any Field components, so the subobject is just
             * an array element. *) 
            if offsetContainsField startOff then failwith "internal error: offset contains field"
            else
            (* Again, the bounds are simply those of the local pointer. 
             * FIXME again: are we simply sidestepping the checks that the indexing is in-bounds? *)
            let sourceBoundsLval = localLvalToBoundsFun (Var(lvi), varOff)
            in
            BoundsLval(sourceBoundsLval)
            (* Ditto / see above. *)
      | CastE(targetT, subE) when getConcreteType (Cil.typeSig (Cil.typeOf subE)) = getConcreteType (Cil.typeSig targetT) ->
            (* If we're only const-modifying the type, we can recurse. 
               NOTE: SoftBound differs here! It always recurses here.
               It *may* return MustFetch, although it will only fast-fetch
               from the shadow space. *)
            boundsExprForExpr subE currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs
      | Const(CStr(s)) ->  (* we can write down the bounds for string literals straight away *)
            BoundsBaseLimitRvals(CastE(ulongType, Const(CStr(s))), CastE(ulongType, BinOp(PlusPI, Const(CStr(s)), makeIntegerConstant (Int64.of_int (String.length s)), Cil.typeOf e)))
      | Const(CWStr(s)) -> (* same for wide string literals *)
            BoundsBaseLimitRvals(CastE(ulongType, Const(CWStr(s))), CastE(ulongType, BinOp(PlusPI, Const(CWStr(s)), makeIntegerConstant (Int64.of_int (List.length s)), Cil.typeOf e)))

            (* Casts? Literal pointers? 
             * HMM. Literal pointers always need their bounds retrieving, because
             * by definition we don't know what exists there.
             * We can, however, make an exception for NULL pointers (and also -1).
             * 
             * Casts should be handled by __fetch_bounds, because we don't
             * know the type of the underlying object (it's not local, by definition,
             * because it's address-taken). We'd hope that the compiler's subexpression
             * elimination can remove repeated __fetch_bounds calls.
             *)
            (* Note that if we're loading a pointer stored on the heap, we
             * can do nothing; we have to fetch its bounds.
             * 
             * In the example
             * 
             *    p = q + 1
             * 
             * ... we've already checked that "q + 1" is valid at the point where
             * we do the write. PROBLEM: we've split this across a function call,
             * __check_derive_ptr. HMM. Perhaps this transformation hasn't happened yet?
             *)
      | _ -> MustFetch(maybeLoadedFromExpr)

let checkInLocalBounds enclosingFile enclosingFunction helperFunctions uniqtypeGlobals localHost (prevOffsets : Cil.offset list) intExp currentInst = 
    debug_print 0 ("Making local bounds check for indexing expression " ^ 
        (expToString (Lval(localHost, offsetFromList prevOffsets))) ^ 
        " by index expression " ^ (expToString (intExp)) ^ "\n");
    (* To avoid leaking bad pointers when writing to a shared location, 
     * we make the assignment to a temporary, then check the temporary,
     * then copy from the temporary to the actual target. *)
    let loc = instrLoc currentInst in
    (* What's the bound of the local? *)
    [
        (* Test the intExp against the bound *)
        let rec getArrayBound t = match t with
          | TArray(at, None, attrs) -> failwith "asked local bound for unbounded array type"
          | TArray(at, Some(boundExpr), attrs) -> 
                (match constInt64ValueOfExpr boundExpr with
                    Some n -> n
                  | None -> failwith "getting local bound for non-constant-length array"
                )
          | TNamed(ti, attrs) -> getArrayBound ti.ttype
          | _ -> failwith "getArrayBound called on something not an array type"
        in
        let bound = getArrayBound (Cil.typeOf (Lval(localHost, offsetFromList prevOffsets)))
        in
        (* If we're trying to go out-of-bounds, do what? Rather than faff with CIL
         * blocks/statements, just call an inline function. *)
        Call( None,
            (Lval(Var(helperFunctions.checkLocalBounds.svar),NoOffset)),
            [
              (* index *)
              intExp;
              (* limit *)
              Const(CInt64(bound, IInt, None))
            ],
            loc
        )
    ]
    
let mightBeTrappedPointer ptrE =
(* FIXME: want to canonicalise the Lval and AddrOf expressions first,
 * to avoid *&*&... -style mixups. *)
match ptrE with
    (* Just rule out some common definitely-not cases for now. *)
      (* address-of Vars are fresh from the compiler, so can't be trapped...
       * address-of Mems might actually be wrapping arbitrary lvals *)
    | AddrOf(Var(vi), loff) -> false
    | StartOf(Var(vi), loff) -> false
      (* handle cast-of-addressof specially*)
    | CastE(castToT, AddrOf(Var(vi), _)) -> false
    | CastE(castToT, StartOf(_, _)) -> false
    | Lval(lhost, loff) -> true
    | UnOp(op, subE, subT) -> true
    | BinOp(op, subE1, subE2, subT) -> true
    | CastE(castToT, castE) -> true
    | AddrOf(lhost, loff) -> true
    | StartOf(lhost, loff) -> true
    | _ -> true


let hoistAndCheckAdjustment enclosingFile enclosingFunction helperFunctions uniqtypeGlobals ptrExp intExp localLvalToBoundsFun currentFuncAddressTakenLocalNames currentInst tempLoadExprs = 
    (* To avoid leaking bad pointers when writing to a shared location, 
     * we make the assignment to a temporary, then check the temporary,
     * then copy from the temporary to the actual target. *)
    let loc = instrLoc currentInst in
    let exprTmpVar = Cil.makeTempVar ~name:"__cil_adjexpr_" enclosingFunction (typeOf ptrExp) in
    let boundsForAdjustedExpr = boundsExprForExpr ptrExp currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs
    in
    (* remember where the pointer we're adjusting was loaded from, if it was loaded *)
    let _ = match boundsForAdjustedExpr with 
        MustFetch(Some(loadedFromE)) -> 
            tempLoadExprs := VarinfoMap.add exprTmpVar (Some(loadedFromE)) !tempLoadExprs
       | _ -> ()
    in
    let checkResultVar = Cil.makeTempVar ~name:"__cil_boundscheck_" enclosingFunction boolType in
    (* Since exprTmpVar is a non-address-taken local pointer, we might want it to have 
     * local bounds. Only create them if we don't have to fetch them (i.e. to propagate
     * local bounds info that we already have, not to early grab bounds info for expressions
     * that we don't have bounds for). *)
    let exprTmpBoundsVar = begin match localLvalToBoundsFun (Var(exprTmpVar), NoOffset) with
            (Var(bvi), NoOffset) -> bvi
          | lv -> failwith ("unexpected lval: " ^ (lvalToString lv))
    end
    in
      (exprTmpVar, 
            (* if we just created local bounds for the temporary, initialise them *)
            (
                (* We either copy the bounds, if they're local, 
                 * or we emit a make_bounds Call to initialise them,
                 * or we emit a make_invalid_bounds Call to dummy-initialise them.
                 * Note that we never emit an out-of-line fetch call; we're lazy about fetching. *)
                match boundsForAdjustedExpr with
                    BoundsBaseLimitRvals(baseExpr, limitExpr) ->
                        [makeCallToMakeBounds (Some(Var(exprTmpBoundsVar), NoOffset))
                            baseExpr limitExpr loc helperFunctions.makeBounds]
                  | BoundsLval(blv) -> 
                         [Set( (Var(exprTmpBoundsVar), NoOffset), Lval(blv), loc )]
                  | MustFetch(maybeLoadedFromE) -> 
                        [Call( Some(Var(exprTmpBoundsVar), NoOffset),
                               (Lval(Var(helperFunctions.fetchBoundsInl.svar),NoOffset)),
                               [
                                   (*  const void *ptr *)
                                   ptrExp; 
                                   (* where did we load it from? *)
                                   match maybeLoadedFromE with
                                       Some(ptrE) -> CastE(voidPtrPtrType, ptrE)
                                     | None -> CastE(voidPtrPtrType, nullPtr)
                               ],
                               loc
                        )]
            )
            @
            [
        (* Recall the form of a check for p = q + 1: 
         * 
         * temp = e;
         * // check new pointer value against adjusted pointer
         * result = __check_derive_ptr(q+1, q, t, &temp, &p_bounds, &q_bounds); 
         * /* does: p_bounds = q_bounds; */    // p now points into the same object as q
         * /* does: substitute trap value on output */  // output may go out of bounds
         * /* does: rewrite trap value on input */  // adjustment may bring trap value back *in* bounds
         * assert(check);
         * p = temp;
         * 
         * We want the common case, where a pointer is incremented in a loop
         * within the same object,
         * to result in only a single check remaining in the optimised output.
         * Will this work?
         * We will make a bounds variable for the local pointer; the pointer is "locally fat".
         * On each increment, its bounds will be unchanged. Will the compiler be able to see this?
         * p = p + 1
         * will do
         * p_bounds = p_bounds
         * so yes, I think so.
         * BUT the loop might not be bounded! Even dynamically, on entry!
         * So a check inside the loop will remain... HMM. BUT ONLY against p_bounds!
         * There is no metadata query in the loop body.
         * Worth reading Moessenboeck's bounds checking optimisation at this point.
         *)
      (* first enqueue an assignment of the whole expression to exprTmpVar *)
      Set( (Var(exprTmpVar), NoOffset), BinOp(PlusPI, ptrExp, intExp, Cil.typeOf ptrExp), loc );
      (* next enqueue the check call *)
      Call( (* Some((Var(exprTmpVar), NoOffset)) *) (* None *) Some(Var(checkResultVar), NoOffset), (* return value dest *)
            (Lval(Var(helperFunctions.checkDerivePtr.svar),NoOffset)),  (* lvalue of function to call *)
            [ 
              (* const void **p_derived *)
              CastE(voidConstPtrPtrType, mkAddrOf (Var(exprTmpVar), NoOffset))
            ;
              (* const void *derivedfrom *)
              ptrExp
            ;
              (* __libcrunch_bounds_t *derivedfrom_bounds *)
              ( match boundsForAdjustedExpr with
                    BoundsLval(lv) -> mkAddrOf lv
                  | _ -> 
                        (* We just created a variable and stored some bounds to it
                         * (invalid if we're MustFetch, valid if we have the rvals).
                         * So just pass that temporary's address. (Although we created
                         * it to store the *derived* pointer's bounds, by definition
                         * these must be the same as the derived-from bounds, and that's
                         * what we initialised it with if we init'd it at all.) *)
                        mkAddrOf (Var(exprTmpBoundsVar), NoOffset)
              )
              (* struct uniqtype *t *)  (* the pointed-*TO* type of the pointer *)
            ; (let pointeeUniqtypeVar = 
                let pointeeTs = match (exprConcreteType ptrExp) with
                        TSPtr(ts, _) -> ts
                      | _ -> failwith "recipient is not a pointer"
                 in ensureUniqtypeGlobal pointeeTs enclosingFile uniqtypeGlobals
              in
              mkAddrOf (Var(pointeeUniqtypeVar), NoOffset)
              )
            ; makeSizeOfPointeeType ptrExp
            ],
            loc
      )
    ])
    
let pointeeUniqtypeGlobalPtr e enclosingFile uniqtypeGlobals =
    (* care: if we just wrote a T*, it's the type "t" that we need to pass to fetchbounds *)
    let ptrT = exprConcreteType e
    in
    let ts = match ptrT with 
        TSPtr(targetTs, _) -> targetTs
      | _ -> failwith "fetching bounds for a non-pointer"
    in
    let uniqtypeGlobal = ensureUniqtypeGlobal ts enclosingFile uniqtypeGlobals
    in
    mkAddrOf (Var(uniqtypeGlobal), NoOffset)

(* We're writing some pointer value to a *local* pointer, hence also 
 * to a local bounds var. *)
let makeBoundsWriteInstruction ~doFetchOol enclosingFile enclosingFunction currentFuncAddressTakenLocalNames helperFunctions uniqtypeGlobals (justWrittenLval : lval) writtenE derivedFromE (localLvalToBoundsFun : lval -> lval) currentInst tempLoadExprs =
    let loc = instrLoc currentInst in
    (* Here we do the case analysis: 
     * do we copy bounds, 
     *       make them ourselves, 
     *    or fetch them? *)
    debug_print 0 ("Making bounds write instructions... matching writtenE: " ^ expToString writtenE ^ "\n");
    (* We only get called if we definitely want to update the bounds for 
     * justWrittenLval.
     * It follows that justWrittenLval is a pointer
     * for which we are locally caching bounds.
     * 
     * The main problem is: do we infer the bounds,
     * or do we have to call liballocs to fetch them?
     * 
     * We can infer the bounds if... *)
    let destBoundsLval = localLvalToBoundsFun justWrittenLval
    in
    begin match boundsExprForExpr writtenE currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs with 
        BoundsLval(sourceBoundsLval) ->
            Set(destBoundsLval, Lval(sourceBoundsLval), loc)
      | BoundsBaseLimitRvals(baseExpr, limitExpr) ->
            makeCallToMakeBounds (Some(destBoundsLval)) baseExpr limitExpr loc helperFunctions.makeBounds
      | MustFetch(maybeLoadedFromE) -> 
            if maybeLoadedFromE = None && doFetchOol then
                Call( Some(localLvalToBoundsFun justWrittenLval),
                        (Lval(Var(helperFunctions.fetchBoundsOol.svar),NoOffset)),
                        [
                            (* const void *ptr *)
                            Lval(justWrittenLval)     (* i..e the *value* of the pointer we just wrote *)
                        ;   (* const void *derived_ptr *)
                            Lval(justWrittenLval)     (* i..e the *value* of the pointer we just wrote *)
                        ;   (* struct uniqtype *t *)
                            pointeeUniqtypeGlobalPtr (Lval(justWrittenLval)) enclosingFile uniqtypeGlobals
                        (* ; *)  (* what's the size of that thing? *)
                            (* makeSizeOfPointeeType (Lval(justWrittenLval)) *)
                        ],
                        loc
                    )
            else Call( Some(localLvalToBoundsFun justWrittenLval),
                   (Lval(Var(helperFunctions.fetchBoundsInl.svar),NoOffset)),
                   [
                       (*  const void *ptr *)
                       Lval(justWrittenLval);
                       (* where did we load it from? *)
                       match maybeLoadedFromE with
                           Some(ptrE) -> CastE(voidPtrPtrType, ptrE)
                         | None -> CastE(voidPtrPtrType, nullPtr)
                   ],
                   loc
            )
    end

(* How do we deal with Stephen D's "unspecified values" problem?
 * Here he is referring to how uninitialised memory takes unspecified values, 
 * which are allowed to be copied around. An unspecified pointer value might 
 * get copied around and later used, without adjustment and hence without
 * check. To close off this loophole, we need to force the initialization
 * of any pointer local, and any heap location that might be interpreted
 * as a pointer. For the former, we require a CIL pass (FIXME: write this). 
 * For the latter, it's a bit trickier: even structs with one pointer field 
 * need initializing. Perhaps we can write a smart zeroer which dynamically 
 * interprets a uniqtype and zeroes a block of them. Maybe we should compute
 * pointer masks for each uniqtype, since this would help us in the GC use-case
 * also. Pointer masks for unbounded-length uniqtypes need some care. *)


(* What do we keep bounds for?
 * Note that not everything that gets indexed/adjusted is a variable.
 *
 *       p = q->r + 1;
 * 
 * We can't cache the bounds of q->r -- we'd have to invalidate them
 * whenever q is updated, through any alias. 
 * 
 * We just take the hit of checking the global structures for these cases.
 * 
 * This means we might be better off with a four-argument __check_derive_ptr:
 * 
 * __check_derive_ptr(derived_ptr, derived_from_ptr, &derived_from_ptr_bounds, t)
 * 
 * where we use the final argument, t, the type of the derived ptr,
 * if we have no bounds to derive from -- we go and look it up.
 * If we do have bounds to derive from, we just use that.
 * 
 * The derived ptr bounds may or may not be cached locally, depending on
 * what we're writing them into.
 * 
 * scanForAdjustedLocalsVisitor is trying to figure out which locals need
 * bounds. Short answer:
 * 
 * - any local non-address-taken pointer that is adjusted
 * - in fact, any pointer *within* a non-address-taken local
          *if* that pointer is subject to address arithmetic.
          -- this gets into alias analysis, it seems, even though we don't
             consider address-taken locals, because of the possibility of 
             non-static indexing of local arrays. 
          -- BUT non-static indexing will get rewritten into indexing a temporary
             local pointer
          -- SO we don't have to worry about that
          -- BUT we have to do that rewriting *first*? vsimpleaddr seems like a good idea
             once more.
          -- OR can we rely on ChangeDoChildrenPost to do the right thing? 
             I don't think so, because we might get a local that is
 * 
 * What's the invariant we want
 * at the point where we do the bounds checking?
 * 
 * All non-constant indexing of local non-a-t'n arrays (at any depth) has been transformed
 * into a simple check against the locally known bounds.
 *
 * All indexing of other arrays and pointers has been transformed into pointer arithmetic.
 * 
 * All constant indexing of a local non-a-t'n array is checked statically (FIXME).
 * 
 * This is better than if we just said "if a local array is indexed by a non-constant expression,
 * treat it as address-taken, i.e. don't cache bounds".
 *)


(* It's important not to confuse two things here.
 * 
 * - address calculations (pointer arith or array indexing)
         that need checking;
 * 
 * - local pointers that have cached bounds.
 * 
 * We lift address calculations into temporaries, and check them. 
 * To avoid creating too many temporaries, we do the following:
 *
 * - lift address-calculation temporaries on demand;
 * 
 * - don't lift calculations of addresses which are statically 
 * safe.
 *)
 
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
end (* class *)


 
class crunchBoundBasicVisitor = fun enclosingFile -> 
                               object(self)
  inherit nopCilVisitor
  val assertFailFunDec = findOrCreateExternalFunctionInFile enclosingFile "__assert_fail" (TFun(voidType, 
                            Some [ 
                            ("assertion", charConstPtrType, []);
                            ("file", charConstPtrType, []);
                            ("line", uintType, []);
                            ("function", charConstPtrType, [])
                             ], 
                            false, []))
  
  (* Will fill these in during initializer *) 
  val mutable helperFunctions = {
     fetchBoundsInl = emptyFunction "__fetch_bounds_inl";
     fetchBoundsOol = emptyFunction "__fetch_bounds_ool";
     makeBounds = emptyFunction "__make_bounds";
     pushLocalArgumentBounds = emptyFunction "__push_local_argument_bounds";
     pushArgumentBoundsBaseLimit = emptyFunction "__push_argument_bounds_base_limit";
     fetchAndPushArgumentBounds = emptyFunction "__fetch_and_push_argument_bounds";
     pushArgumentBoundsCookie = emptyFunction "__push_argument_bounds_cookie";
     tweakArgumentBoundsCookie = emptyFunction "__tweak_argument_bounds_cookie";
     peekArgumentBounds = emptyFunction "__peek_argument_bounds";
     pushLocalResultBounds = emptyFunction "__push_local_result_bounds";
     pushResultBoundsBaseLimit = emptyFunction "__push_result_bounds_base_limit";
     fetchAndPushResultBounds = emptyFunction "__fetch_and_push_result_bounds";
     peekResultBounds = emptyFunction "__peek_result_bounds";
     cleanupBoundsStack = emptyFunction "__cleanup_bounds_stack";
     makeInvalidBounds = emptyFunction "__libcrunch_make_invalid_bounds";
     checkDerivePtr = emptyFunction "__full_check_derive_ptr";
     primaryCheckDerivePtr = emptyFunction "__primary_check_derive_ptr";
     detrap = emptyFunction "__libcrunch_detrap";
     checkLocalBounds = emptyFunction "__check_local_bounds";
     storePointerNonLocal = emptyFunction "__store_pointer_nonlocal";
     primarySecondaryTransition = emptyFunction "__primary_secondary_transition"
  }
  
  initializer
    (* according to the docs for pushGlobal, non-types go at the end of globals --
     * but if we do this, our function definition appears at the end, which is wrong.
     * So just put it at the front -- seems to work.
     * ARGH. Actually, it needs to go *after* the assertFailFun, on which it depends,
     * to avoid implicit declaration problems. So we split the list at this element, 
     * then build a new list. *)
    let uniqtypePtrType = TPtr(findStructTypeByName enclosingFile.globals "uniqtype", [])
    in
    let boundsType = findStructTypeByName enclosingFile.globals "__libcrunch_bounds_s"
    in
    let boundsPtrType = TPtr(boundsType, [])
    in
    helperFunctions.fetchBoundsOol <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__fetch_bounds_ool" (TFun(boundsType, 
                            Some [ 
                                   ("ptr", voidConstPtrType, []);
                                   ("derived_ptr", voidConstPtrType, []);
                                   ("t", TPtr(findStructTypeByName enclosingFile.globals "uniqtype", []), []) (* ;
                                   ("t_sz", ulongType, []) *)
                                 ], 
                            false, []))
    ;
    helperFunctions.fetchBoundsInl <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__fetch_bounds_inl" (TFun(boundsType, 
                            Some [ 
                                   ("ptr", voidConstPtrType, []);
                                   ("maybe_loaded_from", voidPtrPtrType, [])
                                 ], 
                            false, []))
    ;

    helperFunctions.makeBounds <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__make_bounds" (TFun(boundsType, 
                            Some [ 
                                   ("base", ulongType, []);
                                   ("limit", ulongType, [])
                                 ], 
                            false, []))
    ;

    helperFunctions.pushLocalArgumentBounds <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__push_local_argument_bounds" (TFun(voidType, 
                            Some [ 
                                   ("bounds", boundsType, []);
                                 ], 
                            false, []))
    ;

    helperFunctions.pushArgumentBoundsBaseLimit <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__push_argument_bounds_base_limit" (TFun(voidType, 
                            Some [ 
                                   ("ptr", voidConstPtrType, []);
                                   ("base", ulongType, []);
                                   ("limit", ulongType, []);
                                 ], 
                            false, []))
    ;

    helperFunctions.fetchAndPushArgumentBounds <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__fetch_and_push_argument_bounds" (TFun(voidType, 
                            Some [ 
                                   ("ptr", voidConstPtrType, []);
                                   ("maybe_loaded_from", voidPtrPtrType, [])
                                 ], 
                            false, []))
    ;

    helperFunctions.pushArgumentBoundsCookie <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__push_argument_bounds_cookie" (TFun(voidType, 
                            Some [ 
                                   ("callee", voidConstPtrType, [])
                                 ], 
                            false, []))
    ;

    helperFunctions.tweakArgumentBoundsCookie <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__tweak_argument_bounds_cookie" (TFun(boolType, 
                            Some [ 
                                   ("callee", voidConstPtrType, [])
                                 ], 
                            false, []))
    ;

    helperFunctions.peekArgumentBounds <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__peek_argument_bounds" (TFun(boundsType, 
                            Some [ 
                                   ("really", boolType, []);
                                   ("offset", ulongType, []);
                                   ("ptr", voidConstPtrType, [])
                                 ], 
                            false, []))
    ;
    
    helperFunctions.pushLocalResultBounds <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__push_local_result_bounds" (TFun(voidType, 
                            Some [
                                   ("really", boolType, []);
                                   ("bounds", boundsType, []);
                                   ("ptr", voidConstPtrType, [])
                                 ], 
                            false, []))
    ;
    
    helperFunctions.pushResultBoundsBaseLimit <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__push_result_bounds_base_limit" (TFun(voidType, 
                            Some [ 
                                   ("really", boolType, []);
                                   ("ptr", voidConstPtrType, []);
                                   ("base", ulongType, []);
                                   ("limit", ulongType, [])
                                 ], 
                            false, []))
    ;
    
    helperFunctions.fetchAndPushResultBounds <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__fetch_and_push_result_bounds" (TFun(voidType, 
                            Some [ 
                                   ("really", boolType, []);
                                   ("ptr", voidConstPtrType, [])
                                 ], 
                            false, []))
    ;
    
    helperFunctions.peekResultBounds <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__peek_result_bounds" (TFun(voidType, 
                            Some [ 
                                   ("p_bounds", boundsPtrType, []);
                                   ("n", ulongType, [])
                                 ], 
                            false, []))
    ;
    
    helperFunctions.cleanupBoundsStack <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__cleanup_bounds_stack" (TFun(voidType, 
                            Some [ 
                                    ("really", boolType, []);
                                    ("saved_ptr", voidPtrType, [])
                                 ], 
                            false, []))
    ;

    helperFunctions.makeInvalidBounds <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__libcrunch_make_invalid_bounds" (TFun(boundsType, 
                            Some [ 
                                   ("ptr", voidConstPtrType, [])
                                 ], 
                            false, []))
    ;
    helperFunctions.primaryCheckDerivePtr <- findOrCreateExternalFunctionInFile
                            enclosingFile "__primary_check_derive_ptr" (TFun(voidPtrType, 
                            Some [ ("p_derived", voidConstPtrPtrType, []);
                                   ("derivedfrom", voidConstPtrType, []); 
                                   ("derivedfrom_bounds", boundsPtrType, []); 
                                   ("t", uniqtypePtrType, []); 
                                   ("t_sz", ulongType, [])
                                 ], 
                            false, [])) 
    ;
    helperFunctions.checkDerivePtr <- findOrCreateExternalFunctionInFile
                            enclosingFile "__full_check_derive_ptr" (TFun(voidPtrType, 
                            Some [ ("p_derived", voidConstPtrPtrType, []);
                                   ("derivedfrom", voidConstPtrType, []); 
                                   ("derivedfrom_bounds", boundsPtrType, []); 
                                   ("t", uniqtypePtrType, []); 
                                   ("t_sz", ulongType, [])
                                 ], 
                            false, [])) 
    ;
    helperFunctions.detrap <- findOrCreateExternalFunctionInFile
                            enclosingFile "__libcrunch_detrap" (TFun(ulongType, 
                            Some [ ("ptr", voidPtrType, []) ], 
                            false, [])) 
    ;
    helperFunctions.checkLocalBounds <- findOrCreateExternalFunctionInFile
                            enclosingFile "__libcrunch_check_local_bounds" (TFun(intType, 
                            Some [ ("ptr", intType, []);
                                   ("limit", intType, [])
                            ], 
                            false, [])) 
    ;
    helperFunctions.storePointerNonLocal <- findOrCreateExternalFunctionInFile
                            enclosingFile "__store_pointer_nonlocal" (TFun(voidType, 
                            Some [ ("dest", voidPtrPtrType, []);
                                   ("val", voidPtrType, []);
                                   ("val_bounds", boundsType, []);
                                   ("val_pointee_t", uniqtypePtrType, [])
                            ], 
                            false, []))
    ;
    helperFunctions.primarySecondaryTransition <- findOrCreateExternalFunctionInFile
                            enclosingFile "__primary_secondary_transition" (TFun(voidType, 
                            Some [],
                            false, []))


  val currentInst : instr option ref = ref None
  val currentFunc : fundec option ref = ref None
  val currentLval : lval option ref = ref None
  val currentBlock : block option ref = ref None
  val currentFuncAddressTakenLocalNames : string list ref = ref [] 

end

let isCompOrArray t = match Cil.typeSig t with
    TSArray(_, _, _) -> true
  | TSComp(_, _, _) -> true
  | _ -> false

let containedPointerExprsForExpr e = 
    let rec enumeratePointerYieldingOffsetsForT t = match t with
        TVoid(attrs) -> []
      | TInt(ik, attrs) -> []
      | TFloat(fk, attrs) -> []
      | TPtr(pt, attrs) -> 
        (match (Cil.typeSig pt) with 
            TSBase(TVoid(_)) -> [] 
            | _ -> [NoOffset]
        )
      | TArray(at, None, attrs) -> failwith "asked to enumerate ptroffs for unbounded array type"
      | TArray(at, Some(boundExpr), attrs) -> 
            (* For each offset that yields a pointer in the element type,
             * prepend it with Index(n), 
             * and copy/repeat for all n in the range of the array. *)
            let arraySize = match constInt64ValueOfExpr boundExpr with
                Some n -> n
              | None -> failwith "enumerating ptroffs for non-constant array bounds"
            in
            let rec intsUpTo start endPlusOne = 
                if start >= endPlusOne then [] else start :: (intsUpTo (start+1) endPlusOne)
            in
            let elementPtrOffsets = enumeratePointerYieldingOffsetsForT at
            in
            let arrayIndices = intsUpTo 0 (Int64.to_int arraySize)
            in
            (* copy the list of offsets, once
             * for every index in the range of the array *)
            List.flatten (List.map (fun offset -> List.map (fun i ->
                Index(makeIntegerConstant (Int64.of_int i), offset)
            ) arrayIndices) elementPtrOffsets)
      | TFun(_, _, _, _) -> failwith "asked to enumerate ptroffs for incomplete (function) type"
      | TNamed(ti, attrs) -> enumeratePointerYieldingOffsetsForT ti.ttype
      | TComp(ci, attrs) -> 
            (* For each field, recursively collect the offsets
             * then prepend Field(fi) to each. We prepend the same fi,
             * for all offsets yielded by a given field,
             * then move on to the next field. *)
            List.fold_left (fun acc -> fun fi -> 
                let thisFieldOffsets = enumeratePointerYieldingOffsetsForT fi.ftype
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
    let offsets = enumeratePointerYieldingOffsetsForT (Cil.typeOf e)
    in
    match e with
        Lval(someHost, someOff) -> 
            List.map (fun off -> 
                let newOff = offsetFromList (
                    (offsetToList someOff) @ (offsetToList off)
                ) in
                let _ = if offsets != [NoOffset] then
                    debug_print 0 ("Expression `" ^ (expToString e) ^ "' contains a pointer: `" ^ 
                        (expToString (Lval(someHost, newOff))) ^ "'\n")
                    else ()
                in
                Lval(someHost, newOff)
            ) offsets
     |  _ when isCompOrArray (Cil.typeOf e) -> 
            failwith ("internal error: did not expect array or struct type: " ^ 
                (typToString (Cil.typeOf e)))
     |  _ when isNonVoidPointerType (Cil.typeOf e) -> [e]
     |  _ -> []

let mapForAllPointerBoundsInExpr (forOne : Cil.exp -> bounds_expr -> Cil.exp -> 'a) (outerE : Cil.exp) currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs (* : 'a list *) = 
    List.mapi (fun i -> fun ptrExp -> 
        let boundExp = boundsExprForExpr ptrExp currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs
        in
        forOne ptrExp boundExp (makeIntegerConstant (Int64.of_int i))
    ) (containedPointerExprsForExpr outerE)
    
let concatMapForAllPointerBoundsInExprList f es currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs = 
        List.flatten (List.map (fun e -> mapForAllPointerBoundsInExpr f e currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs) es)

let doNonLocalPointerStoreInstr ptrE storeLv be l enclosingFile enclosingFunction uniqtypeGlobals helperFunctions = 
    (* __store_ptr_nonlocal(dest, written_val, maybe_known_bounds) *)
    let boundsType = try findStructTypeByName enclosingFile.globals "__libcrunch_bounds_s"
      with Not_found -> failwith "strange: __libcrunch_bounds_s not defined"
    in
    (* What do we do if we have to fetch the bounds of the stored value?
     * This means that we don't *locally* know the bounds of the stored
     * value. And the value need have no relationship to the pointer
     * currently stored at the address. And the value we're storing is
     * probably *local*, i.e. coming from a temporary. So the right 
     * solution is probably to be eager about fetching bounds, if we're
     * storing bounds. Is this an optional mode? HMM, yes, we can do this.
     * If we turn makeInvalidBoundsFun into a make-invalid-or-fetch-fast?
     * THEN must distinguish makeInvalid from receiveArg
     *)
    let (boundsExpr, preInstrs) = match be with
            BoundsLval(blv) -> (Lval(blv), [])
          | BoundsBaseLimitRvals(eb, el)
             -> let bTemp = Cil.makeTempVar enclosingFunction ~name:"__cil_boundsrv_" boundsType
                in
                let blv = (Var(bTemp), NoOffset)
                in
                (Lval(blv), 
                 [makeCallToMakeBounds (Some(blv)) eb el l (helperFunctions.makeBounds)]
                )
          | MustFetch(maybeLoadedFromE) -> 
                let bTemp = Cil.makeTempVar enclosingFunction ~name:"__cil_boundsrv_" boundsType
                in
                (Lval(Var(bTemp), NoOffset), 
                 [Call( Some(Var(bTemp), NoOffset),
                   (Lval(Var(helperFunctions.fetchBoundsInl.svar),NoOffset)),
                   [
                       (*  const void *ptr *)
                       ptrE;
                       (* where did we fetch this from? *)
                       (match maybeLoadedFromE with
                       Some(loadedFromPtrE) -> CastE(voidPtrPtrType, loadedFromPtrE)
                     | None -> CastE(voidPtrPtrType, nullPtr));
                      
                   ],
                   l
                )]
                )
    in
    preInstrs @ 
    [Call( None, 
          Lval(Var(helperFunctions.storePointerNonLocal.svar), NoOffset), 
          [ 
            CastE(voidConstPtrPtrType, mkAddrOf storeLv) ;
            ptrE ;
            boundsExpr;
            pointeeUniqtypeGlobalPtr ptrE enclosingFile uniqtypeGlobals
          ],
          l
    )]

let stringStartswith s pref = 
  if (String.length s) >= (String.length pref) 
  then (String.sub s 0 (String.length pref)) = pref 
  else false

let instrIsCheck checkDeriveFun instr = match instr with
    Call(_, Lval(Var(funvar), NoOffset), _, _) when funvar == checkDeriveFun -> true
  | _ -> false

class crunchBoundVisitor = fun enclosingFile -> 
                               object(self)
  inherit (crunchBoundBasicVisitor enclosingFile)

  (* Remember the set of __uniqtype objects for which we've created a global
   * weak extern. *)
  val uniqtypeGlobals : Cil.global UniqtypeMap.t ref = ref UniqtypeMap.empty

  (* Remember the mapping from locals to their bounds. We zap this on each
   * vfunc. *)
  val boundsLocals : Cil.varinfo VarinfoMap.t ref = ref VarinfoMap.empty
  
  (* Remember the mapping from pointer-adjusted expressions to the location
   * where the original pointer was loaded from, if any. We zap this on each
   * vfunc. *)
  val tempLoadExprs : (Cil.exp option) VarinfoMap.t ref = ref VarinfoMap.empty
  
  (* In each function we *may* create a flag to remember whether we detected, at 
   * entry, that the caller was instrumented. We use this to decide whether to
   * pass bounds back or not. *)
  val currentFuncCallerIsInstFlag : Cil.varinfo option ref = ref None

  method vblock (b: block) : block visitAction = 
      currentBlock := Some(b);
      DoChildren
      
  method vstmt (outerS : stmt) : stmt visitAction = 
      match !currentFunc with
          None -> (* statement outside function? *) DoChildren
        | Some f ->
      let localLvalToBoundsFun = boundsLvalForLocalLval boundsLocals f enclosingFile
      in 
      (* We need to instrument returns. *)
      match outerS.skind with 
        Return(Some(returnExp), loc) -> (
            (* We have to pass some bounds back -- but only if the caller
             * is instrumented. Can the inline function take care of that?
             * No because if our return type is a struct, we have to do
             * multiple pushes. *)
            ChangeDoChildrenPost(outerS, fun s -> match s.skind with
                Return(Some(returnExp), loc) ->
                begin
                match !currentFuncCallerIsInstFlag with
                    None -> failwith "internal error: did not create caller-is-instrumented flag"
                  | Some callerIsInstrumentedFlagVar ->
                let boundsReturnInstrList = mapForAllPointerBoundsInExpr (fun ptrExp -> fun boundExp -> fun offsetExp ->
                    match boundExp with
                        BoundsLval(blv) -> 
                            Call( None,
                            (Lval(Var(helperFunctions.pushLocalResultBounds.svar),NoOffset)),
                            [
                                Lval(Var(callerIsInstrumentedFlagVar), NoOffset);
                                Lval(blv)
                            ],
                            loc
                            )
                      | BoundsBaseLimitRvals(baseRv, limitRv) ->
                            Call( None,
                            (Lval(Var(helperFunctions.pushResultBoundsBaseLimit.svar),NoOffset)),
                            [
                                Lval(Var(callerIsInstrumentedFlagVar), NoOffset);
                                ptrExp;
                                baseRv;
                                limitRv
                            ],
                            loc
                            )
                      | MustFetch(maybeLoadedFromE) ->
                            Call( None,
                            (Lval(Var(helperFunctions.fetchAndPushResultBounds.svar),NoOffset)),
                            [
                                Lval(Var(callerIsInstrumentedFlagVar), NoOffset);
                                ptrExp;
                                match maybeLoadedFromE with
                                  Some(ptrE) -> CastE(voidPtrPtrType, ptrE)
                                | None -> CastE(voidPtrPtrType, nullPtr)
                            ],
                            loc
                            )
                ) returnExp !currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs
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
                let sReturnKind = s.skind
                in
                let returnWithBoundsBlock = {
                    battrs = [];
                    bstmts = [{
                        labels = [];
                        skind = Instr(boundsReturnInstrList);
                        sid = 0;
                        succs = [];
                        preds = []
                    }; {
                        labels = [];
                        skind = sReturnKind;
                        sid = 0;
                        succs = [];
                        preds = []
                    }]
                }
                in
                let ordinaryReturnBlock = {
                    battrs = [];
                    bstmts =  [{
                        labels = [];
                        skind = sReturnKind;
                        sid = 0;
                        succs = [];
                        preds = []
                    }]
                }
                in
                match !currentFuncCallerIsInstFlag with 
                    None -> failwith "internal error: have not created caller-is-instrumented flag"
                 |  Some(callerIsInstVar) ->
                let testForUninstCallerExpression = 
                    UnOp(LNot, Lval(Var(callerIsInstVar), NoOffset), boolType)
                in
                s.skind <- If(
                            testForUninstCallerExpression, 
                            (* true  *) ordinaryReturnBlock,
                            (* false *) returnWithBoundsBlock,
                        loc);
                s
            end
            | _ -> s (* after change, not a Return of Some of *)
            ) (* end ChangeDoChildrenPost *)
        ) (* end Return case (outer) *)
   | _ -> DoChildren

  method vfunc (f: fundec) : fundec visitAction = 
      currentFunc := Some(f);
      currentFuncCallerIsInstFlag := None;
      boundsLocals := VarinfoMap.empty;
      tempLoadExprs := VarinfoMap.empty;
      let localLvalToBoundsFun = boundsLvalForLocalLval boundsLocals f enclosingFile
      in 
      let boundsType = try findStructTypeByName enclosingFile.globals "__libcrunch_bounds_s"
        with Not_found -> failwith "strange: __libcrunch_bounds_s not defined"
      in
      (debug_print 0 ("CIL dump of function `" ^ f.svar.vname ^ "': ");
       Cil.dumpBlock (new plainCilPrinterClass) stderr 0 f.sbody;
       debug_print 0 "\n");
      (* Do our own scan for AddrOf and StartOf. 
       * The CIL one is not trustworthy, because any array subexpression
       * has internally been tripped via a StartOf (perhaps now rewritten? FIXME)
       * meaning it contains false positives. *)
      let tempAddressTakenLocalNames = ref []
      in
      let _ = visitCilBlock (new addressTakenVisitor tempAddressTakenLocalNames) f.sbody
      in
      debug_print 0 ("Address-taken locals in `" ^ f.svar.vname ^ "' : [" ^ (
        List.fold_left (fun l -> fun r -> l ^ (if l = "" then "" else ", ") ^ r) "" !tempAddressTakenLocalNames
      ^ "]\n"));
      currentFuncAddressTakenLocalNames := !tempAddressTakenLocalNames
      ;
      (* Don't instrument our own (liballocs/libcrunch) functions that get -include'd. *)
      if stringStartswith f.svar.vname "__liballocs_" 
          || stringEndsWith f.svar.vdecl.file "libcrunch_cil_inlines.h"
          then (currentFunc := None; currentFuncCallerIsInstFlag := None; SkipChildren)
      else
          let currentFuncCallerIsInstFlagVar = Cil.makeTempVar f ~name:"__caller_is_inst_" boolType
          in 
          currentFuncCallerIsInstFlag := Some(currentFuncCallerIsInstFlagVar);
          (* We've done computeFileCFG, so no need to do the CFG info thing *)
          (* Figure out which pointer locals also need cached bounds. 
          
             - a pointer local var gets a local bounds var
             
             - an local array of pointer gets an array of bounds
             
                  ** WHAT about out-of-bounds access to local arrays?
                     GAH.
                     AHA. We're okay. I think.
                     If we're accessing a local array, 
                     we know its bound statically.
                     We might have to insert a dynamic check that the index is in bounds.
                     This is *not* the same as a full bounds check.
                     This mini-check suffices to ensure our bounds are accessed correctly too.
             
             - same for arrays of arrays. FLATTEN the array.
             
             - what about a pointer inside a local struct?
             
                  ** HMM. Ideally we would create a mirror struct, locally.
                     Or, more simply, can we get away with "arrays of bounds" for any depth?
                     Yes. This seems sensible.
                     
                     PROBLEM: if *any* array element is used, the *whole* array will be retained
                     by the optimiser.
             
             What about address-taken locals?
             These should *not* get bounds, because
             they might be written to by other code, 
             making the cached bounds inconsistent.
             
             What about variable-length locals?
             These should be promoted to alloca()'d chunks,
             meaning they are automatically address-taken,
             meaning they also don't get bounds.
             Of course, like all uses of the address-taken check,
             we're giving up opportunities to cache bounds
             in the case where the address doesn't actually escape.
           *)
          (
            let varNeedsBounds = (fun v -> varinfoIsLocal v !currentFuncAddressTakenLocalNames)
            in
            let maybeCreateBounds = fun vi -> 
                let boundsT = boundsTForT vi.vtype enclosingFile.globals
                in
                match boundsT with
                    Some(bt) -> 
                        debug_print 0 ("Creating bounds for local " ^ 
                            vi.vname ^ "; bounds have type " ^ (typToString bt) ^ "\n");
                        let created = getOrCreateBoundsLocal vi f enclosingFile boundsLocals
                        in
                        Some(boundsT, created)
                  | None -> None
            in
            let localBoundsTsToCreate = List.map maybeCreateBounds (List.filter varNeedsBounds f.slocals)
            in
            let formalsNeedingBounds =  (List.filter varNeedsBounds f.sformals)
            in
            let formalBoundsTsToCreate = List.map maybeCreateBounds formalsNeedingBounds
            in
            (* The boundsTs were added by side-effect. 
             * What we haven't done is initialise the ones that correspond to formals.
             * Unlike locals, formals are already valid before initialisation.
             * We initialise them to an invalid bounds value.
             * Unfortunately this might depend on their actual value.
             * So we need to make a call to a helper.
             * We unroll all these calls. *)
            (* We reverse the formals, to generate an r-to-l order just like the caller
             * uses. Then we reverse the whole lot, because we're popping not pushing. 
             * Complication: actually we're peeking. So what offset are we peeking at?
             * The concatMapBlahBlah function doesn't give us what we want because it
             * only does offsets within a single argument. Instead, abstract over the 
             * offset and then assign when we have the whole list. *)
            let formalBoundsInitFunReverseList = concatMapForAllPointerBoundsInExprList (fun ptrExp -> fun boundExp -> fun offsetExp ->
                match boundExp with
                    BoundsLval(Var(bvar), boffset) ->
                    fun idx -> Call(Some(Var(bvar), boffset),
                        (Lval(Var(helperFunctions.peekArgumentBounds.svar),NoOffset)),
                        [
                            (* really do it? only if cookie was okay *)
                            Lval(Var(currentFuncCallerIsInstFlagVar), NoOffset);
                            (* offset on stack *)
                            makeIntegerConstant (Int64.of_int (idx)) (* DON'T adjust for the 
                              cookie -- the inline function has to do it, because
                              bounds are not necessarily single words in size. *);
                            (*  const void *ptr, the pointer value (for makeInvalidBounds) *)
                            ptrExp
                        ],
                        bvar.vdecl (* loc *)
                    )
              | _ -> failwith "internal error: formal parameter lacks bounds"
            ) (List.map (fun vi -> Lval(Var(vi), NoOffset)) (List.rev formalsNeedingBounds))
                !currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs
            in
            let formalBoundsInitList = List.mapi (fun i -> fun f -> f i) 
                (List.rev formalBoundsInitFunReverseList)
            in
            let writeCallerInstFlag
             = [Call(Some(Var(currentFuncCallerIsInstFlagVar), NoOffset), 
                Lval(Var(helperFunctions.tweakArgumentBoundsCookie.svar), NoOffset), 
                [mkAddrOf (Var(f.svar), NoOffset)], instrLoc !currentInst)]
            in
            f.sbody <- { 
                battrs = f.sbody.battrs; 
                bstmts = {
                    labels = [];
                    skind = Instr(writeCallerInstFlag @ formalBoundsInitList);
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
    let boundsType = try findStructTypeByName enclosingFile.globals "__libcrunch_bounds_s"
      with Not_found -> failwith "strange: __libcrunch_bounds_s not defined"
    in
    let isWithCachedBoundsUpdates = begin
        let f = match !currentFunc with
            Some(af) -> af
          | None -> failwith "instruction outside function"
        in
        (* 
            INITIALIZATION OF A LOCAL WITH-BOUNDS POINTER MUST INITIALIZE ITS BOUNDS.
            If we're initializing from something non-local, 
            for an initial load from the heap, say, 
            we use an invalid bounds value.
            But if we're taking the address of a local array,
            we should write its bounds precisely and immediately.
            If we're taking the address of a field within some object, 
            perhaps across a deref'd pointer, we must also write its bounds.

            WRITES TO A LOCAL WITH-BOUNDS POINTER MUST COPY ITS BOUNDS.
            The same as above applies.
         *)
        let localLvalToBoundsFun = boundsLvalForLocalLval boundsLocals f enclosingFile
        in 
        match outerI with
            Set((lhost, loff), e, l) ->
                let instrsToAppend =
                    (* We might be writing a non-void pointer on the lhs. 
                     *      If we're keeping bounds for that pointer, we need to write some bounds.
                     *      Sometimes we have to fetch those bounds;
                     *      Sometimes we can copy them;
                     *      Sometimes we can *create* them 
                     *          -- if it's taking the address of a variable (local or global);
                     *          -- if it's selecting a subobject from a variable
                     *          -- if it's selecting a subobject via a pointer we have bounds for.
                     *)
                    (* FIXME: handle struct assignments *)
                    if isNonVoidPointerType (Cil.typeOf (Lval(lhost, loff)))
                        && hostIsLocal lhost !currentFuncAddressTakenLocalNames
                    then (
                        debug_print 0 ("Saw write to a local non-void pointer lval: " ^ (
                            lvalToString (lhost, loff)
                        ) ^ ", so updating its bounds\n")
                        ;
                        [makeBoundsWriteInstruction ~doFetchOol:false enclosingFile f !currentFuncAddressTakenLocalNames helperFunctions uniqtypeGlobals (lhost, loff) e e (* <-- derivedFrom *) localLvalToBoundsFun !currentInst !tempLoadExprs]
                    )
                    else if isNonVoidPointerType (Cil.typeOf (Lval(lhost, loff)))
                        && not (hostIsLocal lhost !currentFuncAddressTakenLocalNames)
                    then (
                        debug_print 0 ("Saw write to a non-local pointer lval: " ^ (
                            lvalToString (lhost, loff)
                        ) ^ ", so calling out the bounds-store hook\n")
                        ;
                        (* Again, depending on the written expression, 
                         * we can fetch, copy or create the bounds. 
                         * If it's fetch, 
                         * we don't actually do the fetch -- we pass invalid bounds,
                         * and we let the inline function do the fetch. In
                         * most configurations, we actually won't do the fetch
                         * in that case.
                         * PERHAPS we can optimise: if the dest already stores
                         * bounds that contain the overwritten value, we assume
                         * that those bounds are good for the new value?
                         * YES, I think this is sane. It might break if we're changing the
                         * type of the pointer stored at the target location.
                         * This means that storage allocations should clear the
                         * relevant bounds areas. Hmm. If we succeed in this being
                         * unnecessary for most allocations (only the hot ones), this
                         * will be very reasonable.
                         * HMM. Might be too clever; "just fetch" is sane if fetches
                         * are cheap.
                         *)
                        let boundsExpr = boundsExprForExpr e !currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs
                        in
                        doNonLocalPointerStoreInstr e (lhost, loff) (boundsExpr) l enclosingFile f uniqtypeGlobals helperFunctions
                    )
                    else []
                in begin
                debug_print 0 "Queueing some instructions\n";
                [outerI] @ instrsToAppend
                end
          | Call(olv, calledE, es, l) -> 
              (* Don't instrument our own (liballocs/libcrunch) functions that get -include'd. *)
              if (match calledE with Lval(Var(fvi), NoOffset) when stringStartswith fvi.vname "__liballocs_" 
                  || stringEndsWith fvi.vdecl.file "libcrunch_cil_inlines.h"
                  -> true
                | _ -> false)
              then [outerI]
              else
                let passesBounds = List.fold_left (fun acc -> fun argExpr -> 
                    acc || (* isNonVoidPointerType (Cil.typeOf argExpr) *)
                               not (list_empty (containedPointerExprsForExpr argExpr))
                ) false es
                in
                let returnsBounds = match olv with Some(lv) -> 
                    (* isNonVoidPointerType (Cil.typeOf (Lval(lv))) *)
                    not (list_empty (containedPointerExprsForExpr (Lval(lv))))
                  | None -> false
                in
                let boundsSpExpr = match findGlobalVarInFile "__bounds_sp" enclosingFile with
                            Some(bspv) -> Lval(Var(bspv), NoOffset)
                          | None -> failwith "internal error: did not find bounds stack pointer"
                in
                let (maybeSavedBoundsStackPtr, boundsSaveInstrs)
                 = if passesBounds || returnsBounds then 
                    let vi = Cil.makeTempVar f ~name:"__bounds_stack_ptr" ulongPtrType
                    in
                    (Some(vi), [Set((Var(vi), NoOffset), boundsSpExpr, instrLoc !currentInst)])
                    else (None, [])
                in
                begin
                    let boundsPassInstructions = if (not passesBounds) then [] else (
                        let callForOneOffset ptrExpr boundsExpr offsetExpr = 
                            debug_print 0 ("Pushing bounds for pointer-contained-in-argument expr " ^ (expToString ptrExpr) ^ "\n");
                            match boundsExpr with
                                BoundsLval(blv) -> 
                                    Call( None,
                                    (Lval(Var(helperFunctions.pushLocalArgumentBounds.svar),NoOffset)),
                                    [
                                        Lval(blv)
                                    ],
                                    instrLoc !currentInst
                                    )
                              | BoundsBaseLimitRvals(baseRv, limitRv) ->
                                    Call( None,
                                    (Lval(Var(helperFunctions.pushArgumentBoundsBaseLimit.svar),NoOffset)),
                                    [
                                        ptrExpr;
                                        baseRv;
                                        limitRv
                                    ],
                                    instrLoc !currentInst
                                    )
                              | MustFetch(maybeLoadedFromE) ->
                                    Call( None,
                                    (Lval(Var(helperFunctions.fetchAndPushArgumentBounds.svar),NoOffset)),
                                    [
                                        ptrExpr;
                                        match maybeLoadedFromE with
                                          Some(ptrE) -> CastE(voidPtrPtrType, ptrE)
                                        | None -> CastE(voidPtrPtrType, nullPtr)
                                    ],
                                    instrLoc !currentInst
                                    )
                        in
                        concatMapForAllPointerBoundsInExprList callForOneOffset (List.rev es) (* push r to l! *)
                            !currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs
                    )
                    in
                    let cookieStackAddrVar = ref None
                    in
                    let calleeLval = match calledE with
                                Lval(lv) -> lv
                              | _ -> failwith "internal error: calling a non-lvalue"
                    in
                    let boundsPushCookieInstructions = 
                        if passesBounds || returnsBounds then (
                        let spVar = Cil.makeTempVar f ~name:"__cookie_stackaddr" ulongPtrType
                        in
                        cookieStackAddrVar := Some(spVar);
                        [Call(None, 
                            Lval(Var(helperFunctions.pushArgumentBoundsCookie.svar), NoOffset), 
                            [mkAddrOf calleeLval], instrLoc !currentInst);
                         (* also remember the cookie stackaddr *)
                         Set((Var(spVar), NoOffset), boundsSpExpr, instrLoc !currentInst)
                        ]
                        ) else []
                    in
                    let returnBoundsPeekOrStoreInstructions = 
                      match olv with
                        None -> []
                      | Some(lhost, loff) -> 
                            let destT = Cil.typeOf (Lval(lhost, loff))
                            in
                            if not (list_empty (containedPointerExprsForExpr (Lval(lhost, loff))))
                            then (
                                debug_print 0 ("Saw call writing to a non-void pointer lval: " ^ (
                                    lvalToString (lhost, loff)
                                ) ^ "\n");
                                let writeOne boundsLval ptrExpr _ offsetExpr = 
                                    Call(
                                        Some(boundsLval),
                                        (Lval(Var(helperFunctions.peekResultBounds.svar),NoOffset)),
                                        [
                                            (* really? only if *)
                                            (match !cookieStackAddrVar with
                                                Some(vi) ->
                                                    BinOp(Ne, Lval(Mem(Lval(Var(vi), NoOffset)), NoOffset),
                                                        CastE(ulongType, mkAddrOf calleeLval), boolType)
                                              | None -> failwith "error: no saved bounds stack pointer for cookie"
                                            )
                                              ;
                                            (* offset? *)
                                            (* makeIntegerConstant (Int64.of_int 1); *)
                                            offsetExpr; 
                                            (* match boundsTForT destT enclosingFile.globals with
                                                Some(TArray(at, maybeArrayBoundExpr, _)) -> 
                                                    (match maybeArrayBoundExpr with
                                                        Some(arrayBoundExpr) -> 
                                                          (match constInt64ValueOfExpr arrayBoundExpr with
                                                            Some n -> makeIntegerConstant n
                                                          | None -> failwith "getting bounds type for non-constant array bounds"
                                                          )
                                                        | None -> failwith "no array bound when one expected"
                                                    )
                                                | Some(_) -> makeIntegerConstant (Int64.of_int 1)
                                                | None -> failwith "no bounds type when one expected"
                                            *)
                                            (*  const void *ptr *)
                                            ptrExpr (* Lval(lhost, loff) *)
                                        ],
                                        instrLoc !currentInst
                                    )
                                in
                                let callsForOneLocal ptrExpr blah offsetExpr = 
                                    [writeOne (localLvalToBoundsFun (lhost, loff)) ptrExpr blah offsetExpr]
                                in
                                let callsForOneNonLocal ptrExpr blah offsetExpr =
                                    let bvi = Cil.makeTempVar f ~name:"__bounds_return_" boundsType
                                    in
                                    (writeOne (Var(bvi), NoOffset) ptrExpr blah offsetExpr) :: (
                                     doNonLocalPointerStoreInstr 
                                        (* pointer value *) (Lval(lhost, loff)) 
                                        (* where it's been/being stored *) (lhost, loff)
                                        (* its bounds *) (BoundsLval(Var(bvi), NoOffset))
                                        l enclosingFile f uniqtypeGlobals helperFunctions
                                    )
                                in
                                if hostIsLocal lhost !currentFuncAddressTakenLocalNames then (
                                    debug_print 0 "Local, so updating/invalidating its bounds.\n";
                                    (* we're popping/peeking, so reverse *)
                                    List.flatten (List.rev (mapForAllPointerBoundsInExpr callsForOneLocal (Lval(lhost, loff)) !currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs))
                                ) else ( (* non-local -- writing into the "heap"! *)
                                debug_print 0 "Host is not local\n";
                                List.flatten (List.rev (mapForAllPointerBoundsInExpr callsForOneNonLocal (Lval(lhost, loff)) !currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs))
                                )
                            ) (* end if pointer list non-empty *)
                            else (* no pointers *) []
                    in
                    let boundsCleanupInstructions = (
                        if passesBounds || returnsBounds then (
                          match maybeSavedBoundsStackPtr with
                            Some(spvi) -> [Call( None,
                                            (Lval(Var(helperFunctions.cleanupBoundsStack.svar),NoOffset)),
                                            [
                                                (* ptr *)
                                                Lval(Var(spvi), NoOffset)
                                            ],
                                            (instrLoc !currentInst)
                                            )]
                          | None -> failwith "internal error: didn't save bounds stack pointer"
                        ) else []
                    )
                    in
                    boundsSaveInstrs @ 
                        boundsPassInstructions @ 
                        boundsPushCookieInstructions @
                        [outerI] @ 
                        returnBoundsPeekOrStoreInstructions @ 
                        boundsCleanupInstructions
            end
          | (* Asm(attrs, instrs, locs, u, v, l) -> *) _ -> [outerI]
    end
    in
    ChangeDoChildrenPost(isWithCachedBoundsUpdates, fun is -> is)
    end
    
  method vlval outerLv = 
        currentLval := Some(outerLv);
        let theFunc = match !currentFunc with
            None -> failwith "lvalue outside function"
          | Some(f) -> f
        in
        ChangeDoChildrenPost(outerLv, fun lv -> 
            let (initialHost, initialOff) = lv in
            (* Now we've visited all child expressions of the lvalue, 
             * we need to re-jig any indexing within the lvalue itself. 
             * We can't use voffs because it doesn't let us change the lvalue
             * itself. 
             * Instead, we turn the lvalue into a list
             * and then process it piecewise.
             * Imagine an lvalue
             * 
             *       o[i][j].x.y[k].v
             * 
             * -- where each of [i], [j] and [k] might require hoisting.
             * 
             * To handle the last bit of indexing, 
             * we need to hoist out the pointer q = o[i][j].x.y
             * 
             * and then check the operation r = q + k
             * 
             * then substitute the lvalue (Mem(r), NoOffset).
             * 
             * We can skip this if k is statically known and in-bounds.
             * Actually we need to process the list *head*-first.
             *)
             let offsetList = offsetToList initialOff
             in
             let rec hoistIndexing ol lhost offsetsOkayWithoutCheck origHost prevOffsetList = 
                 match ol with 
                     [] -> (lhost, offsetFromList offsetsOkayWithoutCheck)
                   | NoOffset :: rest -> failwith "impossible: NoOffset in offset list"
                   | Field(fi, ign) :: rest -> 
                         hoistIndexing rest lhost (offsetsOkayWithoutCheck @ [Field(fi, ign)]) lhost (prevOffsetList @ [Field(fi, ign)])
                   | Index(intExp, ign) :: rest -> 
                         let isPossiblyOOB = 
                             (* Try to get a bound *)
                             let maybeBound = match (lhost, offsetsOkayWithoutCheck) with
                                    (Var(vi), _) -> 
                                        let prefixLval = (Var(vi), offsetFromList offsetsOkayWithoutCheck)
                                        in 
                                        begin match (Cil.typeSig (Cil.typeOf (Lval(prefixLval)))) with
                                            TSArray(elTs, maybeB, _) -> maybeB
                                          | _ -> failwith ("indexing non-array-typed lvalue: " ^
                                            (lvalToString prefixLval) ^ 
                                            ", type " ^ (typToString (Cil.typeOf (Lval(prefixLval)))))
                                        end
                                   | _ -> None
                              in
                              match maybeBound with
                                None -> 
                                    (* No bound on the array. 
                                     * We let statically-zero bounds through without check
                                     * *if* the array is not inside a struct 
                                     * i.e. is not a flexible array member "[]".
                                     * for which the [0] selector might not be valid. *)
                                    let isSelectingOnFlexibleArrayMember
                                     = match (List.rev offsetsOkayWithoutCheck) with
                                            Field(fi, _) :: rest -> true
                                          | _ -> false
                                    in
                                    (* isPossiblyOOB = *)
                                    isSelectingOnFlexibleArrayMember 
                                        || (not (isStaticallyZero intExp))
                              | Some(bound) -> match constInt64ValueOfExpr intExp with
                                    Some(intValue) -> 
                                        intValue < (Int64.of_int 0) || intValue >= bound
                                  | None ->
                                    (* This means we couldn't simplify the expression
                                     * to a constant. We really want to dynamically
                                     * check that `intExp' is within the `bound'. This is
                                     * simpler than a call to __fetch_bounds because
                                     * we know exactly where the object was allocated.
                                     * In fact we *don't* want to invoke __fetch_bounds,
                                     * because we need to evaluate this simpler check in order
                                     * to access the locally cached bounds which __fetch_bounds
                                     * will want. So,
                                     * 
                                     *  - if the lvalue we're within is local,
                                     *    then
                                     *  - do a simple test on intExp, and 
                                     *    if it's in bounds, we can proceed
                                     *    (and retrieve the cached bounds,
                                     *    if necessary -- e.g. if we can have
                                     *    an array of non-address-taken local pointers,
                                     *    and are doing arithmetic on elements of the array,
                                     *    using indexing expressions that are non-constant.
                                     *    So we *do* care about this case. 
                                     *    (We had said: HMM. What about variably-indexed locals?
                                     *    That's exactly the case we're considering!)
                                     *
                                     *    What if the check fails? i.e. we're indexing
                                     *    a local array by an out-of-bounds amount.
                                     *    Then we should fail *before* we access the bound.
                                     *    The "right" failure is to segfault on a trapped
                                     *    rep of the calculated address.
                                     *    Perhaps more helpful would be to trap the address
                                     *    of the local.
                                     *)
                                    if hostIsLocal origHost !currentFuncAddressTakenLocalNames then
                                        (* Emit a simple check that we're in-bounds.
                                         * This is sufficient to ensure that 
                                         * access to the cached bounds locals,
                                         * even using the variable intExp,
                                         * is safe. *)
                                        let checkInstrs = checkInLocalBounds 
                                            enclosingFile theFunc
                                            helperFunctions
                                            uniqtypeGlobals 
                                            (* localHost *) origHost
                                            prevOffsetList
                                            (* intExp *) intExp
                                            !currentInst
                                        in
                                        self#queueInstr checkInstrs;
                                        false (* i.e. *not* possibly OOB, once we've checked. *)
                                    else true
                            in
                            if not isPossiblyOOB then
                                (* simple recursive call *)
                                hoistIndexing rest lhost (offsetsOkayWithoutCheck @ [Index(intExp, ign)]) lhost (prevOffsetList @ [Index(intExp, ign)])
                            else 
                                (* if we started with x[i].rest, 
                                 * make a temporary to hold &x + i, 
                                 * check that,
                                 * then recurse
                                 * with current lvalue *(temp).rest *)
                                let localLvalToBoundsFun = boundsLvalForLocalLval boundsLocals theFunc enclosingFile
                                in
                                let ptrExp =  (Cil.mkAddrOrStartOf (lhost, offsetFromList offsetsOkayWithoutCheck))
                                in
                                let (tempVar, checkInstrs) = hoistAndCheckAdjustment 
                                    enclosingFile theFunc
                                    helperFunctions uniqtypeGlobals 
                                    ptrExp
                                    (* intExp *) intExp
                                    (* localLvalToBoundsFun *) localLvalToBoundsFun
                                    !currentFuncAddressTakenLocalNames
                                    !currentInst
                                    tempLoadExprs
                                in
                                (
                                    self#queueInstr checkInstrs;
                                    hoistIndexing rest (Mem(Lval(Var(tempVar), NoOffset))) [] lhost (prevOffsetList @ [Index(intExp, ign)])
                                )
             in
             let eventualLval = hoistIndexing offsetList initialHost [] initialHost []
             in
             eventualLval
        )

  method vexpr (outerE: exp) : exp visitAction = 
    debug_print 0 (("Visiting expression: " ^ (expToString outerE)) ^ "\n");
    debug_print 0 (("CIL form: " ^ (expToCilString outerE)) ^ "\n");
    match !currentFunc with
        None -> (* expression outside function *) SkipChildren
      | Some(f) -> 
        ChangeDoChildrenPost(outerE, fun e -> 
          (* When we get here, indexing in lvalues has been rewritten. 
           * Also, we've run on any expressions nested within those lvalues.
           * First, de-trap pointers that are differenced. *)
          match e with
              BinOp(MinusPP, e1, e2, t) when mightBeTrappedPointer e1 || mightBeTrappedPointer e2 ->
                let e1MightBeTrapped = mightBeTrappedPointer e1
                in
                let e2MightBeTrapped = mightBeTrappedPointer e2
                in
                (* When differencing pointers of distinct type, what type denominates? *)
                let unifyPointerTargetTypes pt1 pt2 = 
                    match (pt1, pt2) with
                        (TPtr(t1, _), TPtr(t2, _)) when t1 = t2 -> t1
                      | (TPtr(TVoid(_), _), TPtr(TInt(IChar, _), _)) -> TInt(IChar, [])
                      | (TPtr(TVoid(_), _), TPtr(TInt(IUChar, _), _)) -> TInt(IUChar, [])
                      | (TPtr(TVoid(_), _), TPtr(TInt(ISChar, _), _)) -> TInt(ISChar, [])
                      | (TPtr(TInt(IChar, _), _),  TPtr(TVoid(_), _)) -> TInt(IChar, [])
                      | (TPtr(TInt(IUChar, _), _), TPtr(TVoid(_), _)) -> TInt(IUChar, [])
                      | (TPtr(TInt(ISChar, _), _), TPtr(TVoid(_), _)) -> TInt(ISChar, [])
                      | (TPtr(TInt(IChar, _), _),  TPtr(TInt(ISChar, _), _)) -> TInt(IChar, [])
                      | (TPtr(TInt(IChar, _), _),  TPtr(TInt(IUChar, _), _)) -> TInt(IChar, [])
                      | (TPtr(TInt(ISChar, _), _), TPtr(TInt(IChar, _), _)) -> TInt(IChar, [])
                      | (TPtr(TInt(ISChar, _), _),  TPtr(TInt(IUChar, _), _)) -> TInt(IChar, [])
                      | (TPtr(TInt(IUChar, _), _),  TPtr(TInt(IChar, _), _)) -> TInt(IChar, [])
                      | (TPtr(TInt(IUChar, _), _),  TPtr(TInt(ISChar, _), _)) -> TInt(IChar, [])
                      | (_, _) -> failwith "impossible: differencing non-unifiable pointer types"
                in
                (* De-trap both pointer expressions, if necessary. 
                 * If it's not necessary to detrap them, we just cast them to ulongType,
                 * because we do the arithmetic in the integer domain. *)
                let e1ToUse = 
                    if not e1MightBeTrapped then CastE(ulongType, e1)
                    else (
                        let tmpVarL = Cil.makeTempVar f ~name:"__cil_detrapL_" ulongType in
                        self#queueInstr [
                            Call( Some(Var(tmpVarL), NoOffset), 
                                  Lval(Var(helperFunctions.detrap.svar), NoOffset), 
                                  [ e1 ],
                                  instrLoc !currentInst
                            )
                        ];
                        Lval(Var(tmpVarL), NoOffset)
                    )
                in
                let e2ToUse = 
                    if not e2MightBeTrapped then CastE(ulongType, e2)
                    else (
                        let tmpVarR = Cil.makeTempVar f ~name:"__cil_detrapR_" ulongType in
                        self#queueInstr [Call( Some(Var(tmpVarR), NoOffset), 
                                  Lval(Var(helperFunctions.detrap.svar), NoOffset),
                                  [ e2 ],
                                  instrLoc !currentInst
                            );
                        ];
                        Lval(Var(tmpVarR), NoOffset)
                    )
                in
                BinOp(Div, BinOp(MinusA, e1ToUse, e2ToUse, ulongType), 
                    SizeOf (unifyPointerTargetTypes (Cil.typeOf e1) (Cil.typeOf e2)),
                    (* ptrdiff_t *) longType)
          | CastE(t, subex) -> 
              let subexTs = getConcreteType(Cil.typeSig(Cil.typeOf(subex)))
              in 
              let targetTs = getConcreteType(Cil.typeSig(t))
              in
              if (tsIsPointer subexTs) && (not (tsIsPointer targetTs)) 
                && (mightBeTrappedPointer subex)
                (* Making an integer from a pointer: need to de-trap.
                 * Go via a helper -- also handy for tracing this?
                 * ACTUALLY will make life more difficult for tracing
                 * escape of pointerness, but do it anyway. *)
                then
                let tmpVar = Cil.makeTempVar ~name:"__cil_detrap_" f ulongType in
                    self#queueInstr [
                        Call( Some(Var(tmpVar), NoOffset), 
                              Lval(Var(helperFunctions.detrap.svar), NoOffset), 
                              [ subex ],
                              instrLoc !currentInst
                        )
                    ]
                    ;
                    CastE(t, Lval(Var(tmpVar), NoOffset))
                else e
          (* Now we just need to handle pointer arithmetic. 
           * We let it stand if we're at top level; otherwise we hoist it
           * to another temporary. *)
        | _ -> (let maybeAdjustment = begin match e with
              |  BinOp(PlusPI, ptrExp, intExp, t) -> Some(ptrExp, intExp)
              |  BinOp(IndexPI, ptrExp, intExp, t) -> Some(ptrExp, intExp)
              |  BinOp(MinusPI, ptrExp, intExp, t) -> Some(ptrExp, UnOp(Neg, intExp, Cil.typeOf intExp))
              |  _ -> None
          end in match maybeAdjustment with
            None -> begin
                debug_print 0 ("Leaving expression alone because it does no pointer arithmetic: " ^ (expToString e) ^ "\n");
                e
            end
          | Some(ptrExp, intExp) -> 
                if isStaticallyZero intExp then begin
                    debug_print 0 ("Leaving expression alone because its adjustment is always zero: " ^ (expToString e) ^ "\n");
                    e
                end
                else begin
                    debug_print 0 ("Not top-level, so rewrite to use temporary\n");
                    flush stderr;
                    let tempVar, checkInstrs = hoistAndCheckAdjustment enclosingFile f
                                    helperFunctions uniqtypeGlobals 
                                    (* ptrExp *) ptrExp
                                    (* intExp *) intExp
                                    (* localLvalToBoundsFun *) (boundsLvalForLocalLval boundsLocals f enclosingFile)
                                    !currentFuncAddressTakenLocalNames
                                    !currentInst
                                    tempLoadExprs
                    in
                    (
                        self#queueInstr checkInstrs;
                        (Lval(Var(tempVar), NoOffset))
                    )
                end
        )
        (* Fix up occurrences of "&*blah" *)
        (*
        match fixedE with
            AddrOf((Mem(ptrExp), NoOffset)) -> ptrExp
          | _ -> fixedE
          *)
    ) (* end ChangeDoChildrenPost *)
end

class checkStatementLabelVisitor = fun labelPrefix -> 
                                   fun checkDeriveFun ->
                                object(self)
  inherit nopCilVisitor

  val mutable labelCounter = 0
  
  method vstmt (outerS : stmt) : stmt visitAction = 
      (* We split statements of kind Instrs so that 
       * if they're doing a pointer derivation check, 
       * they *only* do that, and they have a unique label
       * of a name generated from labelPrefix. *)
      let mkLabelIdent = fun fragment -> 
        let ident = "__crunchbound_" ^ fragment ^ "_" ^ (string_of_int labelCounter)
        in
        labelCounter <- labelCounter + 1;
        ident
      in
      match outerS.skind with
            |  Instr(is) -> ChangeDoChildrenPost(outerS, fun s ->
                    match is with
                        [] -> s
                      | firstI :: moreIs ->
                            (* Here we want a split so that check calls are in a separate run. *)
                            let rec maybeSplitInstrs (rev_acc : instr list list) (cur : instr list) instrs = 
                                match instrs with
                                  [] -> List.rev (cur :: rev_acc)
                                | x :: more when instrIsCheck checkDeriveFun x ->
                                      debug_print 0 "Saw a check\n";
                                      (* accumulate a singleton, then start a new run of instrs *)
                                      let new_singleton = [x]
                                      in
                                      let acc_with_cur = cur :: rev_acc
                                      in
                                      let new_rev_acc = new_singleton :: acc_with_cur
                                      in
                                      maybeSplitInstrs new_rev_acc [] more
                                | x :: more -> (* it's some other instr *)
                                      let new_cur = cur @ [x]
                                      in
                                      maybeSplitInstrs rev_acc new_cur more
                            in
                            let accRuns = maybeSplitInstrs [] [] is
                            in
                            debug_print 0 ("Split an Instr sequence into " ^ (string_of_int (List.length accRuns)) ^ "\n");
                            (* Okay, now we have the runs, make a block. *)
                            {
                                labels = s.labels;
                                skind = Block(
                                      let b = 
                                      mkBlock (List.map (fun accRun -> {
                                        labels = (if List.length accRun = 1 
                                            && instrIsCheck checkDeriveFun (List.hd accRun)
                                            then
                                                let loc = match List.hd accRun with
                                                    Call(_, _, _, loc) -> loc
                                                  | _ -> failwith "impossible: check not a call"
                                                in
                                                [Label(mkLabelIdent labelPrefix, loc, false)]
                                            else []
                                        );
                                        skind = Instr(accRun);
                                        sid = 0;
                                        succs = [];
                                        preds = []
                                    }) accRuns)
                                    in
                                    Cil.dumpBlock (new defaultCilPrinterClass) Pervasives.stderr 0 b;
                                    b
                                    );
                                sid = s.sid;
                                succs = s.succs;
                                preds = s.preds;
                            }
               ) (* end ChangeDoChildrenPost *)
         (* |   Return(maybeE, loc)
            |   Goto(stmtRef, loc)
            |   Break(loc)
            |   Continue(loc)
            |   If(testExpr, blkTrue, blkFalse, loc)
            |   Switch(testExpr, stmtBlock, stmtList, loc)
            |   Loop(blk, loc, unusedContinueLabel, unusedBreakLabel)
            |   Block(blk)
            |   TryFinally(blkTry, blkFinally, loc)
            |   TryExcept(blk, instrsWithExprs, loc)       
      *)
            | _ -> DoChildren
end

class primaryToSecondaryJumpVisitor = fun fullCheckFun
                                    -> fun primaryCheckFun
                                    -> fun findStmtByLabel
                                    -> fun enclosingFile
                                    -> object(self)
  inherit (crunchBoundBasicVisitor enclosingFile)

  method vstmt (outerS: stmt) : stmt visitAction = 
       (* Either it's a single call to a check function, or it doesn't 
        * include a check function at all. *)
       match outerS.skind with
           Instr(is) -> 
               let isCheckStmt = List.length is = 1 && instrIsCheck fullCheckFun (List.hd is)
               in 
               if not isCheckStmt then DoChildren
               else 
                   let newStatements = [
                    (* We do the check instr as usual, *except* calling the primary check function. *)
                    { labels = outerS.labels;
                      skind = Instr([match (List.hd is) with
                            Call(out, _, [arg1; arg2; arg3; arg4; arg5], loc) ->
                                Call(out, Lval(Var(primaryCheckFun), NoOffset), 
                                    [Lval(mkMem arg1 NoOffset); arg2; Lval(mkMem arg3 NoOffset); arg5], loc)
                          | _ -> failwith ("impossible: check is not a check (" ^ instToString (List.hd is) ^ ")")
                      ]);
                      sid = outerS.sid;
                      succs = outerS.succs;
                      preds = outerS.preds;
                    };
                    (* We're expecting that the check statement is labelled with a "full check" *)
                    let secondaryCheckLabelIdent = match outerS.labels with
                        [Label(ident, _, _)] ->
                            debug_print 0 ("Found label ident: " ^ ident ^ "\n");
                            let prefix = String.sub ident 0 (String.length "__crunchbound_primary_check")
                            in
                            let suffix = (
                                if String.length ident > String.length ("__crunchbound_primary_check")
                                then String.sub ident (String.length ("__crunchbound_primary_check")) (String.length ident - String.length ("__crunchbound_primary_check"))
                                else "")
                            in
                            debug_print 0 ("Prefix, suffix: " ^ prefix ^ ", " ^ suffix ^ "\n");
                            "__crunchbound_full_check" ^ suffix
                      | _ -> failwith "check statement does not have a check label"
                    in 
                    (* We then test the return value of the call. *)
                    let (checkVarinfo, loc) = match is with 
                        [Call(Some(Var(vi), NoOffset), Lval(Var(funvar), NoOffset), args, loc)]
                             when funvar == fullCheckFun
                                -> (vi, loc)
                      | _ -> failwith "check has no output var"
                    in
                    { labels = [];
                      skind = If(UnOp(LNot, Lval(Var(checkVarinfo), NoOffset), intType), 
                        mkBlock [{ labels = [];
                          skind = Block(mkBlock [
                            { labels = [];
                              skind = Instr([Call(None, 
                                Lval(Var(helperFunctions.primarySecondaryTransition.svar), NoOffset), 
                                [], 
                                loc)]);
                              sid = 0; succs = []; preds = []
                            };
                            { labels = [];
                              skind = Goto(findStmtByLabel secondaryCheckLabelIdent, loc);
                              sid = 0; succs = []; preds = []
                            }
                          ]);
                          sid = 0; succs = []; preds = []
                        }],
                        mkBlock [],
                        loc);
                      sid = 0;
                      succs = [];
                      preds = [];
                    }
                ]
                in
                ChangeDoChildrenPost(outerS, fun s -> {s with labels = []; skind = Block(mkBlock newStatements)})
         | _ -> DoChildren
end

class findStmtByNameVisitor = fun stmtName -> fun foundStmt -> object(self) inherit nopCilVisitor
    method vstmt (stmt: stmt) : stmt visitAction = begin
        if List.exists (fun l -> match l with
            Label(x, _, _) -> x = stmtName
          | _ -> false
        ) stmt.labels
        then (
            foundStmt := Some(stmt);
            SkipChildren
        )
        else DoChildren
    end
end (* class *)

(* A visitor that makes a deep copy of a block. Based on Cil's copyFunctionVisitor, 
 * but we don't want to copy variables. Unlike that visitor, we also need to rename labels. *)
class copyBlockVisitor = fun (outputBlock : block option ref) -> fun labelPrefix -> object (self)
  inherit nopCilVisitor

  val toplevelBlock = ref None

  (* Keep here a list of statements to be patched *)
  val patches : stmt list ref = ref []
  (* Keep a mapping from statements to their copies *)
  val stmtmap : (int, stmt) Hashtbl.t = Hashtbl.create 113
  val sid = ref 0 (* Will have to assign ids to statements *)

  (* Replace statements. PROBLEM: goto targets are represented by a 
   * stmt ref, which needs to point into the copy. *)
  method vstmt (s: stmt) : stmt visitAction = 
    s.sid <- !sid; sid := !sid + 1;
    let prefixLabels ls = List.map (fun l -> match l with
        Label(ident, loc, flag) -> Label(labelPrefix ^ ident, loc, flag)
      | _ -> l) ls
    in
    (* make a copy of the statement, renaming any labels. *)
    let s' = {s with sid = s.sid; labels = prefixLabels s.labels} in
    H.add stmtmap s.sid s'; (* remember the copied statement, by its sid *)
    (* if we have a Goto or a Switch remember them to fixup at end *)
    (match s'.skind with
      (Goto _ | Switch _) -> patches := s' :: !patches
    | _ -> ());
    (* Do the children *)
    ChangeDoChildrenPost (s', fun x -> x)

  (* Copy blocks since they are mutable *)
  method vblock (outerB: block) = 
    let copiedB = {outerB with bstmts = outerB.bstmts}
    in
    if !toplevelBlock = None then
        toplevelBlock := Some(copiedB)
    else ();
    ChangeDoChildrenPost (copiedB, fun b -> (
        (* Do fixup and output if we're *finishing* with the top-level block.  *)
        (match !toplevelBlock with
            (* We always *have* a top-level block, but only if it's us, i.e. we're
             * the top-level activation of vblock, do we want to do anything. *)
            Some(b') when b' == copiedB ->
              let findStmt (i: int) = 
                try Hashtbl.find stmtmap i 
                with Not_found -> failwith "bug: cannot find the copy of stmt"
              in
              let patchstmt (s: stmt) = 
                match s.skind with
                  Goto (sr, l) -> 
                    let sr' = ref (findStmt !sr.sid) in
                    s.skind <- Goto (sr',l)
                | Switch (e, body, cases, l) -> 
                    s.skind <- Switch (e, body, 
                                       List.map (fun cs -> findStmt cs.sid) cases, l)
                | _ -> ()
              in
              (* for all goto or switch statements we saw, 
               * patch their references *)
              List.iter patchstmt !patches; 
              outputBlock := Some(b)
        | _ -> ()   (* None case, i.e. we are not finishing the top-level block. *)
        ) ; b
    ))

  method vinst (i: instr) = 
    let i' = match i with
        Set(lv, e, loc) -> Set(lv, e, loc)
      | Call(maybeLv, f, args, loc) -> Call(maybeLv, f, args, loc)
      | Asm(attrs, instrs, locs, u, v, l) -> Asm(attrs, instrs, locs, u, v, l)
    in
    ChangeDoChildrenPost ([i'], fun x -> x)
end

class primarySecondarySplitVisitor = fun enclosingFile -> 
                                     object(self)
  inherit (crunchBoundBasicVisitor enclosingFile)
  
  method vfunc (f: fundec) : fundec visitAction = 
      (* The idea here is to duplicate the function body in two.
       * In the top half, we turn "full" checks into "primary only" checks.
       * In the bottom half, we leave them be, but label them.
       * After each top-half check we insert a test; if it fails, jump to the bottom-half label. 
       * The top-half transformation entails splitting up
       * a list of statements to include a test-and-maybe-jump in the middle.
       * 
       * First we visit the block with our special block visitor that rejigs
       * the statements so that check instructions come in a statement by themselves.
       *)
      (* Now we can visit the statements *)
      let copiedBlock = ref None
      in
      debug_print 0 ("Copying body of function " ^ f.svar.vname ^ "\n");
      let secondaryBody = (
          visitCilBlock (new copyBlockVisitor copiedBlock "__bottomhalf_") f.sbody;
          match !copiedBlock with
              Some(b) -> b
            | None -> failwith "couldn't copy function body"
      )  
      in
      debug_print 0 ("Labelling primary checks in function " ^ f.svar.vname ^ " original body\n");
      f.sbody <- visitCilBlock (new checkStatementLabelVisitor "primary_check" helperFunctions.checkDerivePtr.svar) f.sbody;
      debug_print 0 ("After visit, original body is as follows\n");
      Cil.dumpBlock (new defaultCilPrinterClass) Pervasives.stderr 0 f.sbody;
      debug_print 0 ("Labelling full checks in copied body of function " ^ f.svar.vname ^ "\n");
      let labelledSecondaryBody = visitCilBlock (new checkStatementLabelVisitor "full_check" helperFunctions.checkDerivePtr.svar) secondaryBody in
      let findStmtByLabel = fun ident -> (
          let found = ref None
          in
          (visitCilBlock (new findStmtByNameVisitor ident found) labelledSecondaryBody;
          match !found with
              None -> debug_print 0 ("Label not found: " ^ ident ^ "\n"); raise Not_found
            | Some(stmt) -> ref stmt
          )
      )
      in 
      debug_print 0 ("After visit, copied body is as follows\n");
      Cil.dumpBlock (new defaultCilPrinterClass) Pervasives.stderr 0 labelledSecondaryBody;
      debug_print 0 ("... and original body is as follows\n");
      Cil.dumpBlock (new defaultCilPrinterClass) Pervasives.stderr 0 f.sbody;
      debug_print 0 ("Inserting primary/secondary jumps in function " ^ f.svar.vname ^ "\n");
      f.sbody <- visitCilBlock (new primaryToSecondaryJumpVisitor helperFunctions.checkDerivePtr.svar helperFunctions.primaryCheckDerivePtr.svar findStmtByLabel enclosingFile) f.sbody;
      debug_print 0 ("Finally, function body is as follows\n");
      Cil.dumpBlock (new defaultCilPrinterClass) Pervasives.stderr 0 f.sbody;
      debug_print 0 ("Finished with function " ^ f.svar.vname ^ "\n");
      (* Our new body is a Block containing both *)
      f.sbody <- {
        battrs = f.sbody.battrs;
        bstmts = [
            { labels = []; skind = Block(f.sbody);       sid = 0; succs = []; preds = [] };
            { labels = []; skind = Block(labelledSecondaryBody); sid = 0; succs = []; preds = [] }
        ]
      };
      DoChildren

end

let feature : Feature.t = 
  { fd_name = "crunchbound";
    fd_enabled = false;
    fd_description = "dynamic bounds checking of pointer indexing and arithmetic";
    fd_extraopt = [];
    fd_doit = 
    (function (fl: file) -> 
      debug_print 1 ("command line args are:\n"
       ^ (String.concat ", " (Array.to_list Sys.argv) ) );
      let tpFunVisitor = new crunchBoundVisitor fl in
      visitCilFileSameGlobals tpFunVisitor fl;
      let splitVisitor = new primarySecondarySplitVisitor fl in
      visitCilFileSameGlobals splitVisitor fl
      );
    fd_post_check = true;
  } 

let () = Feature.register feature
