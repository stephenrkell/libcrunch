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

let voidPtrHasBounds = ref false
let noPointerTypeInfo = ref false
let skipSecondarySplit = ref false

type helperFunctionsRecord = {
 mutable fetchBoundsInl : fundec; 
 mutable fetchBoundsOol : fundec; 
 mutable fetchBoundsFull : fundec; 
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
 mutable storePointerNonLocalViaVoidptrptr : fundec;
 mutable primarySecondaryTransition : fundec;
 mutable checkDeref: fundec
}

let stringStartsWith s pref = 
  if (String.length s) >= (String.length pref) 
  then (String.sub s 0 (String.length pref)) = pref 
  else false

let varIsOurs vi = stringStartsWith vi.vname "__liballocs_" 
             || stringStartsWith vi.vname "__libcrunch_" 
              || stringEndsWith vi.vdecl.file "libcrunch_cil_inlines.h"

let instrLoc (maybeInst : Cil.instr option) =
   match maybeInst with 
   Some(i) -> Cil.get_instrLoc i
 | None -> locUnknown

let rec uncastExpr e = 
    match e with
        CastE(castT, subE) -> uncastExpr subE
        | _ -> e

let varinfoIsLocal vi currentFuncAddressTakenLocalNames = not vi.vglob && 
    ( let isAT = (List.mem vi.vname currentFuncAddressTakenLocalNames)
      in
        (if isAT then () (* debug_print 1 ("Local var " ^ vi.vname ^ " would count as local " ^ 
        "but is address-taken\n")*) else ())
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
    (* debug_print 1 ((Int64.to_string n) ^ " times bounds type " ^ 
        (match maybeBoundsT with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        " is: "); flush stderr; *)
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
    (* debug_print 1 ((match prod with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        "\n"); flush stderr; *)
    prod

let boundsTPlusN maybeBoundsT (n : int64) gs =
    (* debug_print 1 ((Int64.to_string n) ^ " plus bounds type " ^ 
        (match maybeBoundsT with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        " is: "); flush stderr; *)
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
    (* debug_print 1 ((match sum with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        "\n"); *)
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
    (* debug_print 1 ("Sum of bounds types " ^ 
        (match maybeBoundsT1 with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        " and " ^ 
        (match maybeBoundsT2 with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        " is " ^
        (match sum with Some(boundsT) -> typToString boundsT | _ -> "(none)") ^ 
        "\n"); *)
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
      | TPtr(pt, attrs) -> (match (Cil.typeSig pt) with 
            TSBase(TVoid(_)) -> if !voidPtrHasBounds then Some(bounds_t) else None 
            | _ -> Some(bounds_t))
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
    debug_print 1 ("Bounds type for " ^ 
        (typToString t) ^ 
        " is " ^ (match maybeBoundsT with Some(boundsT) -> typToString boundsT | _ -> "none") ^ 
        "\n");
    maybeBoundsT

let typeNeedsBounds t enclosingFile = 
    match boundsTForT t enclosingFile.globals with
        Some(_) -> true
      | None -> false

let isPointerTypeNeedingBounds t enclosingFile = 
    isPointerType t &&
    match boundsTForT t enclosingFile.globals with
        Some(_) -> true
      | None -> false

let exprNeedsBounds expr enclosingFile =
    typeNeedsBounds (Cil.typeOf expr) enclosingFile

let castNeedsFreshBounds sourceE targetT enclosingFile =
    let sourceT = Cil.typeOf sourceE in
    if (!noPointerTypeInfo && isPointerType sourceT) then false else
    let (sourceTS, targetTS) = (Cil.typeSig sourceT, Cil.typeSig targetT) in
    let sourceConcrete = getConcreteType (decayArrayToCompatiblePointer sourceTS) in
    let targetConcrete = getConcreteType (decayArrayToCompatiblePointer targetTS) in
    typeNeedsBounds targetT enclosingFile
    (* And the bounds are not trivial... *)
    && (not (isStaticallyNullPtr sourceE) && not (isStaticallyZero sourceE))
    (* AND either the cast-from expr does not have bounds, or it has different bounds *)
    && (
        (sourceConcrete <> targetConcrete
        && (* possibly-different sizes? *)
            (not (tsIsPointer targetTS && tsIsPointer sourceTS && 
                let sourcePointeeT = match unrollType sourceT with TPtr(x, _) -> x | _ -> failwith "impossible" in
                let targetPointeeT = match unrollType targetT with TPtr(x, _) -> x | _ -> failwith "impossible" in
                sizeOf sourcePointeeT = sizeOf targetPointeeT))
        )
        || (not (typeNeedsBounds sourceT enclosingFile))
    )

let rec boundsIndexExprForOffset (startIndexExpr: Cil.exp) (offs : Cil.offset) (host : Cil.lhost) (prevOffsets : Cil.offset) gs = 
    debug_print 1 ("Hello from boundsIndexExprForOffset\n");
    let bounds_t = findStructTypeByName gs "__libcrunch_bounds_s"
    in
    let indexExpr = match offs with
        Field(fi, nextOffset) -> 
            debug_print 1 ("Hit Field case\n");
            let compTypeBeingIndexed = Cil.typeOf (Lval(host, prevOffsets))
            in
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
            (* Add up all the boundsTs for *earlier* fields in the struct. *)
            let fieldOffsetAsBoundsT = List.fold_left 
                (fun x -> fun y -> boundsTPlusBoundsT x y gs) 
                None 
                (List.map (fun fi -> boundsTForT fi.ftype gs) earlierFields)
            in
            let fieldOffset = boundsTAsNumber fieldOffsetAsBoundsT gs
            in
            debug_print 1 ("Recursing on nextOffset in (ignore host): " ^ (lvalToString (host, nextOffset)) ^ "\n");
            boundsIndexExprForOffset
                (BinOp(PlusA, startIndexExpr, makeIntegerConstant fieldOffset, intType))
                nextOffset host (offsetAppend prevOffsets (Field(fi, NoOffset))) gs
      | Index(intExp, nextOffset) -> 
            debug_print 1 ("Hit Index case\n");
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
            debug_print 1 ("Recursing on nextOffset in (ignore host): " ^ (lvalToString (host, nextOffset)) ^ "\n");
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
    debug_print 1 ("Bounds index expression for indexing " ^ 
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
    debug_print 1 ("Ensuring we have bounds local for " ^ vi.vname ^ "\n");
    try  
        let found = VarinfoMap.find vi !boundsLocals
        in 
        debug_print 1 ("Already exists\n");
        found
    with Not_found -> 
     debug_print 1 ("Creating new bounds local for local " ^ vi.vname ^ "\n");
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
    debug_print 1 ("Hello from boundsLvalForLocalLval, lval " ^ (lvalToString (lh, loff)) ^ "\n");
    let gs = enclosingFile.globals
    in
    match lh with
        Var(local_vi) -> 
            debug_print 1 ("Hello from Var case, name " ^ local_vi.vname ^ "\n");
            let boundsVi = getOrCreateBoundsLocal local_vi enclosingFunction enclosingFile boundsLocals 
            in 
            let indexExpr = boundsIndexExprForOffset zero loff (Var(local_vi)) NoOffset gs
            in
            debug_print 1 ("Bounds index expr is `" ^ (expToString indexExpr) ^ "'\n");
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

type bounds_expr = 
    BoundsLval of lval
  | BoundsBaseLimitRvals of (Cil.exp * Cil.exp)
  | MustFetch of Cil.lval option (* the loaded-from address of the pointer whose bounds
                                 * we're getting, *or* of another pointer which necessarily
                                 * has the same bounds (i.e. in the same object). *)

(* Tell whether a pointer expression has a stored pointer behind it, i.e. does a "pointer load".
 * It's the caller's job to handle local versus non-local lvalues specially. *)
let rec getStoredPtrLvalForPtrExp ptrE enclosingFile =
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
             * bounds we might have stored at shadow(dstGrid).
             * So it's memExp, right?
             *)
            (* This just changes the type of memExp from pointer-to-array to pointer-to-element;
             * the stored pointer lval is still memExp *)
            getStoredPtrLvalForPtrExp memExp enclosingFile
            (* NOTE that StartOf with an non-NoOffset offset would be an
             * adjustment, so we'll have turned it into the NoOffset case
             * plus a later adjustment. Even in SoftBound mode we do this
             * refactoring; we just do nothing to check the adjustments. *)
        | (* casts? *)
            CastE(castT, subE) ->
                if !noPointerTypeInfo && isPointerTypeNeedingBounds (Cil.typeOf subE) enclosingFile then
                    (* strip the casts off *)
                    getStoredPtrLvalForPtrExp subE enclosingFile
                else None
        | _ -> None

let ptrExpIsStoredPtrLval ptrE enclosingFile = 
    getStoredPtrLvalForPtrExp ptrE enclosingFile <> None

let rec boundsDescrForExpr e currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs wholeFile = 
    if isStaticallyNullPtr e then BoundsBaseLimitRvals(zero, one)
    else
    let simplifiedPtrExp = if !noPointerTypeInfo then uncastExpr (simplifyPtrExprs e) else (simplifyPtrExprs e) in
    debug_print 1 ("Getting bounds expr for expr " ^ (expToString simplifiedPtrExp) ^ "\n");
    match simplifiedPtrExp with 
        Lval(Var(vi), offs) when varinfoIsLocal vi currentFuncAddressTakenLocalNames -> 
            BoundsLval(localLvalToBoundsFun (Var(vi), offs))
      | (* for pointer arithmetic, just use the raw input pointer's bounds *) 
        BinOp(PlusPI, ptrE, intE, ptrT) -> boundsDescrForExpr ptrE currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs wholeFile
      |  BinOp(MinusPI, ptrE, intE, ptrT) -> boundsDescrForExpr ptrE currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs wholeFile
            
      | _ ->
    debug_print 1 ("Expr has no local bounds\n");
    let maybeLoadedFromLv = (
        (* getLoadedFromLv simplifiedPtrExp tempLoadExprs currentFuncAddressTakenLocalNames *) 
        debug_print 1 ("Getting loaded-from addr for ptr expr: " ^ (expToCilString simplifiedPtrExp) ^ "\n");
        match getStoredPtrLvalForPtrExp simplifiedPtrExp wholeFile with
        |   Some(loadHost, loadOffs) ->
                if not (hostIsLocal loadHost currentFuncAddressTakenLocalNames) 
                then (
                    debug_print 1 ("It's a non-local lval\n");
                    Some((loadHost, loadOffs))
                ) else (* the host is local -- this will never be hit! *) 
                    failwith ("impossible: exp " ^ (expToString simplifiedPtrExp)
                        ^ " did not hit varinfoislocal but also stores a pointer locally " ^ 
                        (lvalToString (loadHost, loadOffs)))
        | None -> None
    ) in
    let rec offsetContainsField offs = match offs with
            NoOffset -> false
          | Field(fi, _) -> true
          | Index(intExp, rest) -> offsetContainsField rest
    in
    let handleAddrOfVarOrField someHost someOffset = 
        debug_print 1 "Handling AddrOf(var or field)...\n";
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
        (* let _ = output_string stderr ("indexedExpr is " ^ (expToString indexedExpr) ^ "\n") in let _ = flush stderr in *)
        let indexedType = Cil.typeOf indexedExpr
        in
        (* let lastOffsetIsAnIndex = match (List.rev offlist) with
            Index(_, _) -> true
          | _ -> false
        in *)
        let (arrayElementCount, arrayElementT) = multiplyAccumulateArrayBounds (Int64.of_int 1) indexedType
        in
        (* NOTE: to avoid introducing pointer arithmetic that we will later redundantly check, 
         * we instead do our arithmetic in the integer domain. *)
        let baseExpr = CastE(ulongType, mkAddrOrStartOf indexedLval)
        in
        let limitExpr = BinOp(PlusA, baseExpr, BinOp(
            Mult, 
            makeIntegerConstant arrayElementCount, 
            (if tsIsUndefinedType (Cil.typeSig arrayElementT) wholeFile then one else SizeOf(arrayElementT)), 
            ulongType),
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
        debug_print 1 ("Expr " ^ (expToString e) ^ " takes addr of var or field (case 1)\n");
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
            debug_print 1 ("Expr " ^ (expToString e) ^ " takes addr of var or field (case 2)\n");
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
              debug_print 1 ("Expr " ^ (expToString e) ^ " takes addr of var or field (case 3)\n");
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
             debug_print 1 ("Expr " ^ (expToString e) ^ " takes addr of var or field (case 4)\n");
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
      | CastE(targetT, subE) 
            when not (castNeedsFreshBounds subE targetT wholeFile) ->
            (* E.g. if we're only const-modifying the type, we can recurse. 
               NOTE: SoftBound differs here! It always recurses here if the source is a pointer type.
               SoftBound *may* return (its equivalent of) MustFetch, although fast-fetch
               from the shadow space is the most fetching it can do. *)
            boundsDescrForExpr subE currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs wholeFile
      | Const(CStr(s)) ->  (* Ae can write down the bounds for string literals straight away.
                            * As usual, avoid introducing new pointer arithmetic; work in the
                            * integer (ulongType) domain. *)
            BoundsBaseLimitRvals(CastE(ulongType, Const(CStr(s))), 
                BinOp(PlusA, CastE(ulongType, Const(CStr(s))), 
                makeIntegerConstant (Int64.of_int (1 + String.length s)), 
                ulongType))
      | Const(CWStr(s)) -> (* same for wide string literals *)
            BoundsBaseLimitRvals(CastE(ulongType, Const(CWStr(s))), 
                BinOp(PlusA, 
                    CastE(ulongType, Const(CWStr(s))), 
                    (* FIXME: this is broken for escape characters in wide strings.
                     * CIL should help us by interpreting the escape chars, but doesn't. *)
                    BinOp(Mult, makeIntegerConstant (Int64.of_int (1 + List.length s)), SizeOf(!Cil.wcharType), intType),
                    ulongType))

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
      | _ -> (debug_print 1 ("Expr " ^ (expToString e) ^ " hit default MustFetch case, " ^
                (match maybeLoadedFromLv with None -> "without" | _ -> "with") ^ " a loaded-from lvalue\n");
                MustFetch(maybeLoadedFromLv))

let checkInLocalBounds enclosingFile enclosingFunction helperFunctions uniqtypeGlobals localHost (prevOffsets : Cil.offset list) intExp currentInst = 
    debug_print 1 ("Making local bounds check for indexing expression " ^ 
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
    | Const(_) -> false
    | _ -> true

let pointeeUniqtypeGlobalPtrGivenPtrT ptrT enclosingFile uniqtypeGlobals =
    let ts = match ptrT with 
        TSPtr(targetTs, _) -> targetTs
      | _ -> failwith "fetching bounds for a non-pointer"
    in
    let uniqtypeGlobal = ensureUniqtypeGlobal ts enclosingFile uniqtypeGlobals
    in
    mkAddrOf (Var(uniqtypeGlobal), NoOffset)

let pointeeUniqtypeGlobalPtr e enclosingFile uniqtypeGlobals =
    (* care: if we just wrote a T*, it's the type "t" that we need to pass to fetchbounds *)
    let ptrT = exprConcreteType e
    in
    pointeeUniqtypeGlobalPtrGivenPtrT ptrT enclosingFile uniqtypeGlobals

let ensureBoundsLocalLval ptrExpr boundsDescrForPtrExpr enclosingFunction boundsType =
    match boundsDescrForPtrExpr with
        BoundsLval(blv) -> blv
      | BoundsBaseLimitRvals(eb, el)
         -> let bTemp = Cil.makeTempVar enclosingFunction ~name:"__cil_boundsrv_" boundsType
            in
            (Var(bTemp), NoOffset)
      | MustFetch(_) -> 
            let bTemp = Cil.makeTempVar enclosingFunction ~name:"__cil_boundsfetched_" boundsType
            in
            (Var(bTemp), NoOffset)

let boundsUpdateInstrs blv ptrExp boundsDescr
    helperFunctions loc enclosingFile pointeeUniqtypeExpr = 
    match boundsDescr with
        BoundsBaseLimitRvals(baseExpr, limitExpr) ->
            [makeCallToMakeBounds (Some(blv))
                baseExpr limitExpr loc helperFunctions.makeBounds]
      | BoundsLval(otherBlv) ->
            (* avoid cyclic comparison *)
            let sameVi = match (blv, otherBlv) with
                ((Var(vi1), NoOffset), (Var(vi2), NoOffset)) when vi1.vname = vi2.vname -> true
              | ((Var(vi1), Index(Const(CInt64(n1, IInt, None)), NoOffset)), 
                        (Var(vi2), Index(Const(CInt64(n2, IInt, None)), NoOffset)))
                            when vi1.vname = vi2.vname && n1 = n2 -> true
              | _ -> blv == otherBlv
            in
            if sameVi then [] else [Set( blv, Lval(otherBlv), loc )]
      | MustFetch(maybeLoadedFromLv) -> 
            [Call( Some(blv),
                   (Lval(Var(helperFunctions.fetchBoundsInl.svar),NoOffset)),
                   [
                       (*  const void *ptr *)
                       ptrExp; 
                       (* where did we load it from? *)
                       (match maybeLoadedFromLv with
                           Some(lh, lo) -> CastE(voidPtrPtrType, mkAddrOf (lh,lo))
                         | None -> CastE(voidPtrPtrType, nullPtr));
                       (* what's the pointee type? *)
                       pointeeUniqtypeExpr
                   ],
                   loc
            )]

let maybeGetNoinlinePureHelper fvi enclosingFile =
    let name = "__crunchbound_pure_helper_" ^ fvi.vname in
    findFun name enclosingFile.globals

let getNoinlinePureHelper fvi enclosingFile =
    match findFun ("__crunchbound_pure_helper_" ^ fvi.vname) enclosingFile.globals with
        Some(f) -> f
      | None -> failwith ("internal error: did not find noinline pure helper for " ^
            fvi.vname)
       
let createNoinlinePureHelper fvi enclosingFile createdHelpers callingFunction =
    let name = "__crunchbound_pure_helper_" ^ fvi.vname in
    let found = maybeGetNoinlinePureHelper fvi enclosingFile in
    match found with Some(x) -> (debug_print 1 ("Already found helper: " ^ name ^ "\n"); x)
      | None ->
        let fatPtrType = try findStructTypeByName enclosingFile.globals "__libcrunch_ptr_with_bounds_s"
          with Not_found -> failwith "strange: __libcrunch_ptr_with_bounds_s not defined"
        in
        let fatPtrCompinfo = match fatPtrType with TComp(ci, _) -> ci
          | _ -> failwith "strange: __libcrunch_ptr_with_bounds_s not a composite type"
        in
        let (origRetT, origArgsT, origIsVa, origTattrs) = match fvi.vtype with
            TFun(retT, argsT, isVa, tAttrs) -> (retT, argsT, isVa, tAttrs)
          | _ -> failwith "creating helper for something not of function type"
        in
        let f = findOrCreateNewFunctionInFile 
                enclosingFile name (TFun(fatPtrType, origArgsT, origIsVa, origTattrs)) fvi.vdecl
                (* insert before the function that called us *)
                (fun g -> match g with GFun(dec, loc) when dec == callingFunction -> true
                        | _ -> false);
        in
        f.svar.vglob <- false;
        f.svar.vstorage <- Static;
        (* Did we just do find, or create? *)
        match f.sbody.bstmts with
            stmt :: stmts -> failwith ("internal error: helper we just created has non-empty body")
          | [] ->
                let foundThisTime = findFun name enclosingFile.globals
                in
                let _ = match foundThisTime with None -> failwith "just created helper but it doesn't exist"
                  | Some(_) -> ()
                in
                f.svar.vglob <- false;
                f.sformals <- (
                    match origArgsT with None -> []
                  | Some(argTriples) -> 
                    List.mapi (fun idx -> fun (_, t, _) -> makeFormalVar f ("arg" ^ (string_of_int idx)) t) argTriples
                );
                (* We keep the "pure" attribute, but make us noinline. FIXME: why? It just feels right. *)
                f.svar.vattr <- addAttribute (Attr("noinline", [])) fvi.vattr;
                let _ = debug_print 1 ("pure callee has attributes " ^ (attrsToString fvi.vattr) ^ "\n") in
                let _ = debug_print 1 ("wrapper now has attributes " ^ (attrsToString f.svar.vattr) ^ "\n") in
                (* We create a simple wrapper body
                   which populates a temporary
                   instrument that;
                   and then fix up the return site
                   to pass the bounds in-band.
                   Since the pointer is an unsigned long,
                   this won't be treated as a bounds-returning call.
                   We still pass bounds as usaul.
                   We can't fix up the return quite yet. *)
                let tmpVi = Cil.makeTempVar ~name:"__ptr_retval_" f origRetT in
                let instrs = [
                    Call(Some(Var(tmpVi), NoOffset),
                         Lval(Var(fvi), NoOffset),
                         List.map (fun x -> Lval(Var(x), NoOffset)) f.sformals,
                         fvi.vdecl
                        )
                ] in
                (* When we actually instrument this call, we will need 
                 * the varinfo of the pointer temporary, so we can get its bounds.
                 * We can create the fat pointer ourselves, and do the two Sets.
                 * So just try to return tmpVi for now. *)
                let retStmt = (Return(Some(Lval(Var(tmpVi), NoOffset)), fvi.vdecl))
                in
                f.sbody <- mkBlock [{ labels = []; skind = Instr(instrs); sid = 0; succs = []; preds = [] };
                                    { labels = []; skind = retStmt; sid = 0; succs = []; preds = [] }]
                ;
                createdHelpers := (f, callingFunction) :: !createdHelpers;
                debug_print 1 ("Created noinline helper " ^ name ^ "\n");
                f


let hoistAndCheckAdjustment ~isBeingDerefed enclosingFile enclosingFunction helperFunctions uniqtypeGlobals ptrExp intExp checkEvenIfStaticallyZero localLvalToBoundsFun currentFuncAddressTakenLocalNames currentInst tempLoadExprs = 
    (* To avoid leaking bad pointers when writing to a shared location, 
     * we make the assignment to a temporary, then check the temporary,
     * then copy from the temporary to the actual target. *)
    (* The argument isBeingDerefed, if set, means that we're derefing.
     * We may or may not be emulating SoftBound, but 
     * we're called from a context where the created pointer will be immediately dereferenced.
     * We always create the deref check if so; it's up to the #defines to enable/disable
     # the deref check and the adjust check.
     *)
    let loc = instrLoc currentInst in
    let exprTmpVar = Cil.makeTempVar ~name:"__cil_adjexpr_" enclosingFunction (typeOf ptrExp) in
    let boundsDescrForAdjustedExpr = boundsDescrForExpr ptrExp currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs enclosingFile
    in
    let _ = match boundsDescrForAdjustedExpr with 
        MustFetch(Some(loadedFromLv)) -> 
            tempLoadExprs := VarinfoMap.add exprTmpVar (Some(loadedFromLv)) !tempLoadExprs
       | _ -> ()
    in
    (* Since exprTmpVar is a non-address-taken local pointer, we expect it to have 
     * local bounds. Calling localLvalToBoundsFun will ensure they exist. *)
    let exprTmpBoundsVar = begin match localLvalToBoundsFun (Var(exprTmpVar), NoOffset) with
            (Var(bvi), NoOffset) -> bvi
          | lv -> failwith ("unexpected lval: " ^ (lvalToString lv))
    end
    in
    let res = (exprTmpVar, 
            (* We have just created local bounds for the temporary, so initialise them *)
            (
                (* We either copy the bounds, if they're local, 
                 * or we emit a make_bounds Call to initialise them,
                 * or we emit a make_invalid_bounds Call to dummy-initialise them.
                 * Note that we never emit an out-of-line fetch call;
                 * in a fully instrumented program we simply shouldn't need it, and
                 * in partially instrumented cases we're lazy about doing the fetch. *)
                boundsUpdateInstrs (Var(exprTmpBoundsVar), NoOffset) ptrExp boundsDescrForAdjustedExpr 
                   helperFunctions 
                   loc enclosingFile (pointeeUniqtypeGlobalPtr ptrExp enclosingFile uniqtypeGlobals)
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
      Set( (Var(exprTmpVar), NoOffset), BinOp(PlusPI, ptrExp, intExp, Cil.typeOf ptrExp), loc )
      ] @
      (* next enqueue the check call, assuming we're not statically zero *)
      (if (not (isStaticallyZero intExp)) || checkEvenIfStaticallyZero then
      [let checkResultVar = Cil.makeTempVar ~name:"__cil_boundscheck_" enclosingFunction boolType in
      (checkResultVar.vattr <- [Attr("unused", [])];
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
            ( match boundsDescrForAdjustedExpr with
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
       )) ] else []) @ (if isBeingDerefed then [
       Call(None,
            (Lval(Var(helperFunctions.checkDeref.svar),NoOffset)),
            [ Lval(Var(exprTmpVar), NoOffset)
            ; match boundsDescrForAdjustedExpr with
                    BoundsLval(lv) -> Lval(lv)
                  | _ -> 
                        (Lval(Var(exprTmpBoundsVar), NoOffset))
            ], loc
       )] else [])
    ) (* end pair *)
    in
    let resTmp, resInstrs = res
    in
    let _ = debug_print 1 ("Finished hoistAndCheckAdjustment; instrs are [" ^ 
        ( List.fold_left (fun s -> fun xi -> s ^ (instToString xi) ^ ",") "" resInstrs )^ "]\n") in let _ = flush stderr in
    res

let hoistCast enclosingFile enclosingFunction helperFunctions uniqtypeGlobals castFromExp castExp castToTS localLvalToBoundsFun currentFuncAddressTakenLocalNames currentInst tempLoadExprs = 
    let loc = instrLoc currentInst in
    let exprTmpVar = Cil.makeTempVar ~name:"__cil_castexpr_" enclosingFunction (typeOf castExp) in
    (* Since exprTmpVar is a non-address-taken local pointer, we expect it to have 
     * local bounds. *)
    let exprTmpBoundsVar = begin match localLvalToBoundsFun (Var(exprTmpVar), NoOffset) with
            (Var(bvi), NoOffset) -> bvi
          | lv -> failwith ("unexpected lval: " ^ (lvalToString lv))
    end
    in
    (* If we're emulating SoftBound, we still get called -- but only for casts that create 
     * pointers from integers. Get this case out of the way first. *)
    if !noPointerTypeInfo then (exprTmpVar,
        [makeCallToMakeBounds (Some(Var(exprTmpBoundsVar), NoOffset)) zero zero loc helperFunctions.makeBounds]
    )
    else
    (* By definition, a pointer created by a bounds-changing cast was not loaded from anywhere; 
     * it is newly created. *)
    let boundsForCastExpr = boundsDescrForExpr castExp currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs enclosingFile
    in
    let _ = match boundsForCastExpr with 
        MustFetch(None) -> ()
      | _ -> failwith ("internal error: boundsDescrForExpr on type-changing cast `" ^ 
        (expToString castExp) ^ 
        "', CIL form `" ^ (expToCilString castExp) ^
        "' did not say MustFetch(None)")
    in
    let res = (exprTmpVar, 
            (* We've just created local bounds for the temporary, so initialise them.
             * But do it after we assign to the temporary, so that we pick up
             * what trumptr left behind in the cache, hopefully. *)
            [
                Set( (Var(exprTmpVar), NoOffset), castExp, loc );
                Call( Some(Var(exprTmpBoundsVar), NoOffset),
                               (Lval(Var(helperFunctions.fetchBoundsFull.svar),NoOffset)),
                               [
                                   (*  const void *ptr *)
                                   (Lval(Var(exprTmpVar), NoOffset));
                                   (* derived_ptr is the same *)
                                   (Lval(Var(exprTmpVar), NoOffset));
                                   (* where did we load it from? *)
                                   CastE(voidPtrPtrType, nullPtr);
                                   (* what's the type we're asking about? *)
                                   pointeeUniqtypeGlobalPtr castExp enclosingFile uniqtypeGlobals
                               ],
                               loc
                        )
            ]
      )
    in
    let resTmp, resInstrs = res
    in
    let _ = debug_print 1 ("Finished hoistCast; instrs are [" ^ 
        ( List.fold_left (fun s -> fun xi -> s ^ (instToString xi) ^ ",") "" resInstrs )^ "]\n") in let _ = flush stderr in
    res

(* We're writing some pointer value to a *local* pointer, hence also 
 * to a local bounds var. *)
let makeBoundsWriteInstruction ~doFetchOol enclosingFile enclosingFunction currentFuncAddressTakenLocalNames helperFunctions uniqtypeGlobals (writtenToLval : lval) writtenE derivedFromE (localLvalToBoundsFun : lval -> lval) currentInst tempLoadExprs =
    let loc = instrLoc currentInst in
    (* Here we do the case analysis: 
     * do we copy bounds, 
     *       make them ourselves, 
     *    or fetch them? *)
    debug_print 1 ("Making bounds write instructions... matching writtenE: " ^ expToString writtenE ^ "\n");
    (* We only get called if we definitely want to update the bounds for 
     * writtenToLval.
     * It follows that writtenToLval is a pointer
     * for which we are locally caching bounds.
     * 
     * The main problem is: do we infer the bounds,
     * or do we have to call liballocs to fetch them?
     * 
     * We can infer the bounds if... *)
    let destBoundsLval = localLvalToBoundsFun writtenToLval
    in
    begin match boundsDescrForExpr writtenE currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs enclosingFile with 
        BoundsLval(sourceBoundsLval) ->
            Set(destBoundsLval, Lval(sourceBoundsLval), loc)
      | BoundsBaseLimitRvals(baseExpr, limitExpr) ->
            makeCallToMakeBounds (Some(destBoundsLval)) baseExpr limitExpr loc helperFunctions.makeBounds
      | MustFetch(maybeLoadedFromLv) -> 
            if maybeLoadedFromLv = None && doFetchOol then
                Call( Some(localLvalToBoundsFun writtenToLval),
                        (Lval(Var(helperFunctions.fetchBoundsOol.svar),NoOffset)),
                        [
                            (* const void *ptr *)
                            writtenE     (* i..e the *value* of the pointer we just wrote *)
                        ;   (* const void *derived_ptr *)
                            writtenE     (* i..e the *value* of the pointer we just wrote *)
                        ;   (* struct uniqtype *t *)
                            pointeeUniqtypeGlobalPtr (Lval(writtenToLval)) enclosingFile uniqtypeGlobals
                        (* ; *)  (* what's the size of that thing? *)
                            (* makeSizeOfPointeeType (Lval(writtenToLval)) *)
                        ],
                        loc
                    )
            else Call( Some(localLvalToBoundsFun writtenToLval),
                   (Lval(Var(helperFunctions.fetchBoundsInl.svar),NoOffset)),
                   [
                       (*  const void *ptr *)
                       writtenE;
                       (* where did we load it from? *)
                       (match maybeLoadedFromLv with
                           Some(lh,lo) -> CastE(voidPtrPtrType, mkAddrOf (lh,lo))
                         | None -> CastE(voidPtrPtrType, nullPtr));
                        (* what's the pointee type? *)
                        pointeeUniqtypeGlobalPtr (Lval(writtenToLval)) enclosingFile uniqtypeGlobals
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
     fetchBoundsOol = emptyFunction (if !noPointerTypeInfo then "__fetch_bounds_ool_via_dladdr" else "__fetch_bounds_ool");
     fetchBoundsFull = emptyFunction "__fetch_bounds_full";
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
     storePointerNonLocalViaVoidptrptr = emptyFunction "__store_pointer_nonlocal_via_voidptrptr";
     primarySecondaryTransition = emptyFunction "__primary_secondary_transition";
     checkDeref = emptyFunction "__check_deref";
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
    helperFunctions.fetchBoundsInl <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__fetch_bounds_inl" (TFun(boundsType, 
                            Some [ 
                                   ("ptr", voidConstPtrType, []);
                                   ("maybe_loaded_from", voidPtrPtrType, []);
                                   ("t", TPtr(findStructTypeByName enclosingFile.globals "uniqtype", []), [])
                                 ], 
                            false, []))
    ;
    helperFunctions.fetchBoundsOol <- findOrCreateExternalFunctionInFile 
                            enclosingFile (if !noPointerTypeInfo then "__fetch_bounds_ool_via_dladdr" 
                                else "__fetch_bounds_ool") (TFun(boundsType, 
                            Some [ 
                                   ("ptr", voidConstPtrType, []);
                                   ("derived_ptr", voidConstPtrType, []);
                                   ("t", TPtr(findStructTypeByName enclosingFile.globals "uniqtype", []), []) (* ;
                                   ("t_sz", ulongType, []) *)
                                 ], 
                            false, []))
    ;
    helperFunctions.fetchBoundsFull <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__fetch_bounds_full" (TFun(boundsType, 
                            Some [ 
                                   ("ptr", voidConstPtrType, []);
                                   ("derived_ptr", voidConstPtrType, []);
                                   ("maybe_loaded_from", voidPtrPtrType, []);
                                   ("t", TPtr(findStructTypeByName enclosingFile.globals "uniqtype", []), []) (* ;
                                   ("t_sz", ulongType, []) *)
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
                                   ("bounds", boundsType, [])
                                 ], 
                            false, []))
    ;

    helperFunctions.pushArgumentBoundsBaseLimit <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__push_argument_bounds_base_limit" (TFun(voidType, 
                            Some [ 
                                   ("ptr", voidConstPtrType, []);
                                   ("base", ulongType, []);
                                   ("limit", ulongType, [])
                                ], 
                            false, []))
    ;

    helperFunctions.fetchAndPushArgumentBounds <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__fetch_and_push_argument_bounds" (TFun(voidType, 
                            Some [ 
                                   ("ptr", voidConstPtrType, []);
                                   ("maybe_loaded_from", voidPtrPtrType, []);
                                   ("t", TPtr(findStructTypeByName enclosingFile.globals "uniqtype", []), [])
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
                                   ("ptr", voidConstPtrType, []);
                                   ("debugstr", charConstPtrType, [])
                                ], 
                            false, []))
    ;
    
    helperFunctions.pushLocalResultBounds <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__push_local_result_bounds" (TFun(voidType, 
                            Some [
                                   ("really", boolType, []);
                                   ("bounds", boundsType, []);
                                   ("ptr", voidConstPtrType, []);
                                 ], 
                            false, []))
    ;
    
    helperFunctions.pushResultBoundsBaseLimit <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__push_result_bounds_base_limit" (TFun(voidType, 
                            Some [ 
                                   ("really", boolType, []);
                                   ("ptr", voidConstPtrType, []);
                                   ("base", ulongType, []);
                                   ("limit", ulongType, []);
                                 ],
                            false, []))
    ;
    
    helperFunctions.fetchAndPushResultBounds <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__fetch_and_push_result_bounds" (TFun(voidType, 
                            Some [ 
                                   ("really", boolType, []);
                                   ("ptr", voidConstPtrType, []);
                                   ("t", TPtr(findStructTypeByName enclosingFile.globals "uniqtype", []), []);
                                 ], 
                            false, []))
    ;
    
    helperFunctions.peekResultBounds <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__peek_result_bounds" (TFun(voidType, 
                            Some [ 
                                   ("p_bounds", boundsPtrType, []);
                                   ("n", ulongType, []);
                                   ("debugstr", charConstPtrType, [])
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
    helperFunctions.storePointerNonLocalViaVoidptrptr <- findOrCreateExternalFunctionInFile
                            enclosingFile "__store_pointer_nonlocal_via_voidptrptr" (TFun(voidType, 
                            Some [ ("dest", voidConstPtrPtrType, []);
                                   ("val", voidConstPtrType, []);
                                   ("val_bounds", boundsType, []);
                                   ("val_pointee_t", uniqtypePtrType, [])
                            ], 
                            false, []))
    ;
    helperFunctions.primarySecondaryTransition <- findOrCreateExternalFunctionInFile
                            enclosingFile "__primary_secondary_transition" (TFun(voidType, 
                            Some [],
                            false, []))
    ;
    helperFunctions.checkDeref <- findOrCreateExternalFunctionInFile
                            enclosingFile "__check_deref" (TFun(voidType, 
                            Some [ ("ptr", voidConstPtrType, []);
                                   ("bounds", boundsType, [])
                            ],
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
            TSBase(TVoid(_)) -> if !voidPtrHasBounds then [NoOffset] else []
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
                    debug_print 1 ("Expression `" ^ (expToString e) ^ "' contains a pointer: `" ^ 
                        (expToString (Lval(someHost, newOff))) ^ "'\n")
                    else ()
                in
                Lval(someHost, newOff)
            ) offsets
     |  _ when isCompOrArray (Cil.typeOf e) -> 
            failwith ("internal error: did not expect array or struct type: " ^ 
                (typToString (Cil.typeOf e)))
     |  _ when (
            ((not !voidPtrHasBounds) && isNonVoidPointerType (Cil.typeOf e))
        || (!voidPtrHasBounds && isPointerType (Cil.typeOf e))) -> [e]
     |  _ -> []

let mapForAllPointerBoundsInExpr (forOne : Cil.exp -> bounds_expr -> Cil.exp -> 'a) (outerE : Cil.exp) currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs (* : 'a list *) wholeFile = 
    List.mapi (fun i -> fun ptrExp -> 
        let boundExp = boundsDescrForExpr ptrExp currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs wholeFile 
        in
        forOne ptrExp boundExp (makeIntegerConstant (Int64.of_int i))
    ) (containedPointerExprsForExpr outerE)
    
let concatMapForAllPointerBoundsInExprList f es currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs wholeFile = 
        List.flatten (List.map (fun e -> mapForAllPointerBoundsInExpr f e currentFuncAddressTakenLocalNames localLvalToBoundsFun tempLoadExprs wholeFile) es)

let doNonLocalPointerStoreInstr ~useVoidPP ptrE pointeeUniqtypeExpr storeLv be l enclosingFile enclosingFunction uniqtypeGlobals helperFunctions = 
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
    let blv = ensureBoundsLocalLval ptrE be enclosingFunction boundsType
    in
    let (boundsExpr, preInstrs) = (Lval(blv), boundsUpdateInstrs blv ptrE be 
                        helperFunctions l enclosingFile pointeeUniqtypeExpr)
    in
    preInstrs @ 
    [Call( None, 
          Lval(Var(
                if useVoidPP then helperFunctions.storePointerNonLocalViaVoidptrptr.svar
                             else helperFunctions.storePointerNonLocal.svar), NoOffset), 
          [ 
            CastE(voidConstPtrPtrType, mkAddrOf storeLv) ;
            ptrE ;
            boundsExpr;
            (* What's the type we claim? Make it a no-op *)
            pointeeUniqtypeExpr
          ],
          l
    )]

let instrIsCheck checkDeriveFun instr = match instr with
    Call(_, Lval(Var(funvar), NoOffset), _, _) when funvar == checkDeriveFun -> true
  | _ -> false

let makeShadowBoundsInitializerCalls helperFunctions enclosingFunc initializerLocation initializedPtrLv initializationValExpr wholeFile uniqtypeGlobals =
    (* The instructions we need to generate are like doNonLocalPointerStoreInstr,
     * except that we always do a slow fetch. *)
    (* __store_ptr_nonlocal(dest, written_val, maybe_known_bounds) *)
    let boundsType = try findStructTypeByName wholeFile.globals "__libcrunch_bounds_s"
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
    let be = boundsDescrForExpr initializationValExpr
        (* currentFuncAddressTakenLocalNames *) [] 
        (* localLvalToBoundsFun *) (fun _ -> failwith "no local lvals!")
        (* tempLoadExprs *) VarinfoMap.empty
        wholeFile
    in
    let (boundsExpr, preInstrs) = match be with
            BoundsLval(blv) -> (Lval(blv), [])
          | BoundsBaseLimitRvals(eb, el)
             -> let bTemp = Cil.makeTempVar enclosingFunc ~name:"__cil_boundsrv_" boundsType
                in
                let blv = (Var(bTemp), NoOffset)
                in
                (Lval(blv), 
                 [makeCallToMakeBounds (Some(blv)) eb el initializerLocation (helperFunctions.makeBounds)]
                )
          | MustFetch(maybeLoadedFromLv) -> 
                let bTemp = Cil.makeTempVar enclosingFunc ~name:"__cil_boundsfetched_" boundsType
                in
                (Lval(Var(bTemp), NoOffset), 
                 [Call( Some(Var(bTemp), NoOffset),
                   (Lval(Var(helperFunctions.fetchBoundsOol.svar),NoOffset)),
                   [
                       (*  const void *ptr *)
                       initializationValExpr;
                       (* where did we fetch this from? *)
                       CastE(voidPtrPtrType, nullPtr);
                       (* pointee type *)
                       pointeeUniqtypeGlobalPtr initializationValExpr wholeFile uniqtypeGlobals
                   ],
                   initializerLocation
                )]
                )
    in
    let instrsToAppend = preInstrs @ 
    [Call( None, 
          Lval(Var(helperFunctions.storePointerNonLocal.svar), NoOffset), 
          [ 
            CastE(voidConstPtrPtrType, mkAddrOf initializedPtrLv) ;
            initializationValExpr ;
            boundsExpr;
            pointeeUniqtypeGlobalPtr initializationValExpr wholeFile uniqtypeGlobals
          ],
          initializerLocation
    )]
    in
    instrsToAppend

let rec initializationExprMightPointToArray e = 
    let _ = debug_print 1 ("Might init expr " ^ (expToCilString e) ^ " point to or into an array? ") in
    let res = 
    match (simplifyPtrExprs e) with
    |BinOp(PlusPI, subE1, subE2, subT) -> initializationExprMightPointToArray subE1
    |BinOp(MinusPI, subE1, subE2, subT) -> initializationExprMightPointToArray subE1
    |BinOp(_, subE1, subE2, subT) -> false
    |CastE(subT, subE) -> (* Hmm? *) false
    |AddrOf(lhost, offs) -> (* if we're taking the address of an array element, then yes *)
        (* If we remove the last offset, *)
        begin
        let revOffList = List.rev (offsetToList offs) in 
            match revOffList with
                off :: more -> begin
                    match Cil.typeSig (Cil.typeOf (Lval(lhost, offsetFromList (List.rev  more)))) with
                        TSArray(_, _, _) -> true
                      | _ -> false
                    end
              | [] -> false (* we don't have AddrOf on array hosts -- should be StartOf *)
        end
    |StartOf(_, offs) -> (* must be of array type *) true
    |Lval(Var(vi), offs) ->
          (* We really want to recurse on the initialization expr of that particular location.
           * For now, be safe. *)
          true
    | Const(CStr(s)) -> true
    | Const(CWStr(s)) -> true
    | _ -> false
    in
    let _ = debug_print 1 (if res then "yes\n" else "no\n") in
    res

let prependShadowBoundsInitializer helperFunctions initFunc initializerLocation initializedPtrLv initializationValExpr wholeFile uniqtypeGlobals =
    if isStaticallyNullPtr initializationValExpr then ()
    else 
    let instrsToPrepend = makeShadowBoundsInitializerCalls helperFunctions initFunc initializerLocation initializedPtrLv initializationValExpr wholeFile uniqtypeGlobals
    in
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

class crunchBoundVisitor = fun enclosingFile ->
                           fun noinlineHelpers ->
                               object(self)
  inherit (crunchBoundBasicVisitor enclosingFile)
  
  val initFunc : fundec = emptyFunction "__libcrunch_crunchbound_init" 

  initializer
    (* Create a constructor function to hold the bounds write instructions
     * we are creating. This is a bit like CIL's "global initializer", which
     * we deliberately avoid using because CIL wants to put the call to it
     * in main(), which would be silly... we should use constructors instead. *)
    begin
        initFunc.svar <- findOrCreateFunc enclosingFile "__libcrunch_crunchbound_init" 
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

  (* Remember the set of __uniqtype objects for which we've created a global
   * weak extern. *)
  val uniqtypeGlobals : Cil.global UniqtypeMap.t ref = ref UniqtypeMap.empty

  (* Remember the mapping from locals to their bounds. We zap this on each
   * vfunc. *)
  val boundsLocals : Cil.varinfo VarinfoMap.t ref = ref VarinfoMap.empty
  
  (* Remember the mapping from pointer-adjusted expressions to the location
   * where the original pointer was loaded from, if any. We zap this on each
   * vfunc. Each loaded-from addr is latched into its own temporary var, because
   * the lvalue's meaning might change from where we do the load to where
   * we use the bounds (consider "e = e->next" -- we want to fetch the bounds
   * of e, loaded from "e->next"). FIXME: I'm not convinced this is necessary,
   * so long as we fetch bounds (of e->next) before we do the actual program
   * statement (modifying e). *)
  val tempLoadExprs : (Cil.lval option) VarinfoMap.t ref = ref VarinfoMap.empty
  
  (* In each function we *may* create a flag to remember whether we detected, at 
   * entry, that the caller was instrumented. We use this to decide whether to
   * pass bounds back or not. *)
  val currentFuncCallerIsInstFlag : Cil.varinfo option ref = ref None

  method vblock (b: block) : block visitAction = 
      currentBlock := Some(b);
      DoChildren
      
  method vstmt (outerS : stmt) : stmt visitAction = 
      let fatPtrType = try findStructTypeByName enclosingFile.globals "__libcrunch_ptr_with_bounds_s"
        with Not_found -> failwith "strange: __libcrunch_ptr_with_bounds_s not defined"
      in
      let fatPtrCompinfo = match fatPtrType with TComp(ci, _) -> ci
        | _ -> failwith "strange: __libcrunch_ptr_with_bounds_s not a composite type"
      in
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
                (* Are we in a noinline helper to a pure bounds-returning function? If so, 
                 * what happens is a bit different. *)
                let (helperFns, _) = unzip noinlineHelpers in
                let mkReturnBoundsBlock rs instrs = {
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
                let mkReturnNoBoundsBlock rs instrs = {
                    battrs = [];
                    bstmts = let justReturn = {
                        labels = [];
                        skind = rs;
                        sid = 0;
                        succs = [];
                        preds = []
                    } in
                    if instrs = [] then [justReturn] else [{
                        labels = [];
                        skind = Instr(instrs);
                        sid = 0;
                        succs = [];
                        preds = []
                    }; justReturn ]
                }
                in
                let (rs, boundsReturnInstrList) =
                if List.mem f helperFns then
                    let tmpFatPointerVi = Cil.makeTempVar ~name:"__fatptr_retval_" f fatPtrType in
                    let returnBoundsInstrs = match returnExp with Lval(Var(tmpPointerVar), NoOffset) ->
                        [
                            (* set the pointer one *)
                            Set((Var(tmpFatPointerVi), Field(List.hd fatPtrCompinfo.cfields, NoOffset)),
                                CastE(ulongType, returnExp), loc);
                            (* set the bounds one *)
                            Set((Var(tmpFatPointerVi), Field(List.hd (List.tl fatPtrCompinfo.cfields), NoOffset)),
                                Lval(localLvalToBoundsFun (Var(tmpPointerVar), NoOffset)), loc)
                        ]
                        | _ -> failwith "noinline helper has returnExp not a simple lvalue"
                    in
                    (* return the fat pointer, not the original one *)
                    let rs = Return(Some(Lval(Var(tmpFatPointerVi), NoOffset)), loc)
                    in
                    (* we always return bounds *)
                    (rs, returnBoundsInstrs)
                else
                (s.skind, 
                mapForAllPointerBoundsInExpr (fun ptrExp -> fun boundExp -> fun offsetExp ->
                    match boundExp with
                        BoundsLval(blv) -> 
                            Call( None,
                            (Lval(Var(helperFunctions.pushLocalResultBounds.svar),NoOffset)),
                            [
                                Lval(Var(callerIsInstrumentedFlagVar), NoOffset);
                                Lval(blv);
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
                      | MustFetch(maybeLoadedFromLv) ->
                            Call( None,
                            (Lval(Var(helperFunctions.fetchAndPushResultBounds.svar),NoOffset)),
                            [
                                Lval(Var(callerIsInstrumentedFlagVar), NoOffset);
                                ptrExp;
                                (match maybeLoadedFromLv with
                                  Some(lh,lo) -> CastE(voidPtrPtrType, mkAddrOf (lh,lo))
                                | None -> CastE(voidPtrPtrType, nullPtr));
                                pointeeUniqtypeGlobalPtr ptrExp enclosingFile uniqtypeGlobals
                            ],
                            loc
                            )
                    ) returnExp !currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs enclosingFile
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
                            (* true  *) mkReturnNoBoundsBlock rs boundsReturnInstrList,
                            (* false *) mkReturnBoundsBlock rs boundsReturnInstrList,
                        loc);
                s
            end
            | _ -> s (* after change, not a Return of Some of *)
            ) (* end ChangeDoChildrenPost *)
        ) (* end Return case (outer) *)
   | _ -> DoChildren

  method vglob (g: global) : global list visitAction = 
    match g with 
        GVar(gvi, gii, loc) when exprNeedsBounds (Lval(Var(gvi), NoOffset)) enclosingFile ->
         if varIsOurs gvi then SkipChildren
         else (
            (debug_print 1 ("Saw global needing bounds, name `" ^ gvi.vname ^ "'\n"));
            List.iter (fun containedPtrExpr -> 
                        match containedPtrExpr with
                            Lval(lh, loff) ->
                                prependShadowBoundsInitializer
                                    helperFunctions initFunc loc
                                    (lh, loff) (CastE(voidPtrType, zero)) enclosingFile
                                    uniqtypeGlobals
                            | _ -> failwith "error: statically initializing a non-lvalue"
                    ) (containedPointerExprsForExpr (Lval(Var(gvi), NoOffset)))
                    ;
                    (debug_print 1 ("Finished writing zero-init bounds; writing non-zero-init ones for `" ^ gvi.vname ^ "'\n"));
                    let createCalls lv initExpr = 
                        if exprNeedsBounds (Lval(lv)) enclosingFile 
                        then
                            prependShadowBoundsInitializer
                                helperFunctions initFunc loc 
                                lv initExpr enclosingFile 
                                uniqtypeGlobals
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
      boundsLocals := VarinfoMap.empty;
      tempLoadExprs := VarinfoMap.empty;
      let localLvalToBoundsFun = boundsLvalForLocalLval boundsLocals f enclosingFile
      in 
      let boundsType = try findStructTypeByName enclosingFile.globals "__libcrunch_bounds_s"
        with Not_found -> failwith "strange: __libcrunch_bounds_s not defined"
      in
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
      if varIsOurs f.svar then (currentFunc := None; currentFuncCallerIsInstFlag := None; SkipChildren)
      else
          let currentFuncCallerIsInstFlagVar = Cil.makeTempVar f ~name:"__caller_is_inst_" boolType
          in 
          currentFuncCallerIsInstFlag := Some(currentFuncCallerIsInstFlagVar);
      (* Handle (static) variables that have global initializers.
       * GAH. This is hard because static variables are local.
       * We can't take their address from outside of the scope that declares them.
       * So we have to make our initializer be a run-once block at the start
       * of the function. BLARGH. That's annoying, but we can do it.
       * Let's only do it if we have such a static, though.
       * 
       * ACTUALLY: CIL doesn't have local statics! They get promoted to globals.
       *)
      (*
      let staticsNeedingBoundsInitialization = 
      List.map (fun localvi -> localvi.vstorage = Static && 
            (* this counts pointer-containing structs etc.*)
            exprNeedsBounds (Lval(Var(localvi), NoOffset)) enclosingFile) f.slocals
      in
      if (not (0 = List.length staticsNeedingBoundsInitialization)) then
         let initFlag = Cil.makeTempVar f ~name:"__done_static_pointer_bounds_init_" boolType in
         initFlag.vstorage <- Static;
         f.sbody <- {
            battrs = f.sbody.battrs;
            bstmts = [{ labels = []
                      ; skind = If(UnOp(LNot, Lval(Var(initFlag), NoOffset), boolType), mkBlock [{ labels = [];
                          skind = Instr([Set((Var(initFlag), NoOffset), one, f.svar.vdecl)]
                            @ List.fold_left (fun instrs -> fun aVar -> 
                                if (aVar.vstorage <> Static
                                     || not (exprNeedsBounds (Lval(Var(aVar), NoOffset)) enclosingFile))
                                then instrs
                                else instrs @ (List.flatten (
                                    (* HACK. To make sure we catch everything,
                                     * first we map over all contained pointer exprs 
                                     *      for all static locals,
                                     *      and store the bounds of the null pointer.
                                     * Then for each contained pointer that is initialized, we
                                     * output a write something.
                                     *)
                                    List.map (fun containedPtrExpr -> 
                                        match containedPtrExpr with
                                            Lval(lh, loff) ->
                                                makeShadowBoundsInitializerCalls
                                                helperFunctions initFunc f.svar.vdecl
                                                (lh, loff) (CastE(voidPtrType, zero)) enclosingFile 
                                                uniqtypeGlobals
                                            | _ -> failwith "error: statically initializing a non-lvalue"
                                    ) (containedPointerExprsForExpr (Lval(Var(aVar), NoOffset)))
                                    @
                                    (
                                    let createCalls lv initExpr = 
                                        if exprNeedsBounds (Lval(lv)) enclosingFile 
                                        then
                                            makeShadowBoundsInitializerCalls 
                                                helperFunctions initFunc f.svar.vdecl
                                                lv initExpr enclosingFile
                                                uniqtypeGlobals
                                        else []
                                    in
                                    mapOverInitializedLvalues createCalls (Var(aVar), NoOffset) aVar.vinit.init aVar.vdecl
                                    ))
                                )
                            ) [] f.slocals
                          )
                        ; sid = 0
                        ; succs = []
                        ; preds = []
                        }], mkBlock [], f.svar.vdecl)
                      ; sid = 0
                      ; succs = []
                      ; preds = []
                      }] @ f.sbody.bstmts
         }
      else ();
    *)
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
                        debug_print 1 ("Creating bounds for local " ^ 
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
                            ptrExp;
                            Const(CStr((expToString ptrExp) ^ ", offset " ^ (expToCilString offsetExp)))
                        ],
                        bvar.vdecl (* loc *)
                    )
              | _ -> failwith "internal error: formal parameter lacks bounds"
            ) (List.map (fun vi -> Lval(Var(vi), NoOffset)) (List.rev formalsNeedingBounds))
                !currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs enclosingFile
            in
            let formalBoundsInitList = List.mapi (fun i -> fun f -> f i) 
                (List.rev formalBoundsInitFunReverseList)
            in
            let writeCallerInstFlag
             = [Call(Some(Var(currentFuncCallerIsInstFlagVar), NoOffset), 
                Lval(Var(helperFunctions.tweakArgumentBoundsCookie.svar), NoOffset), 
                [CastE(voidConstPtrType, mkAddrOf (Var(f.svar), NoOffset))], instrLoc !currentInst)]
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
    let fatPtrType = try findStructTypeByName enclosingFile.globals "__libcrunch_ptr_with_bounds_s"
      with Not_found -> failwith "strange: __libcrunch_ptr_with_bounds_s not defined"
    in
    let fatPtrCompinfo = match fatPtrType with TComp(ci, _) -> ci
      | _ -> failwith "strange: __libcrunch_ptr_with_bounds_s not a composite type"
    in
    ChangeDoChildrenPost([outerI], fun changedInstrs ->
        (* At this point, hoistAndCheck has done its thing on the expressions.
         * So various queueInstrs have happened and the final instruction is a simplified
         * version of outerI, i.e. referencing temporaries assigned from expressions that 
         * have been hoisted up and checked.
         * 
         * So the last instruction should be derived from outerI, and should be doing
         * the Set or Asm or Call that the user code intended. As a sanity check,
         * test that it really does
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
        let instrsWithCachedBoundsUpdates = begin
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
            match finalI with
                Set((lhost, loff), e, l) ->
                    let instrsToPrepend, instrsToAppend =
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
                        if isPointerTypeNeedingBounds (Cil.typeOf (Lval(lhost, loff))) enclosingFile
                            && hostIsLocal lhost !currentFuncAddressTakenLocalNames
                        then (
                            debug_print 1 ("Saw write to a local non-void pointer lval: " ^ (
                                lvalToString (lhost, loff)
                            ) ^ ", so updating its bounds\n")
                            ;
                            ([makeBoundsWriteInstruction ~doFetchOol:false enclosingFile f !currentFuncAddressTakenLocalNames helperFunctions uniqtypeGlobals (lhost, loff) e e (* <-- derivedFrom *) localLvalToBoundsFun !currentInst !tempLoadExprs],
                            [])
                        )
                        else if isPointerTypeNeedingBounds (Cil.typeOf (Lval(lhost, loff))) enclosingFile
                            && not (hostIsLocal lhost !currentFuncAddressTakenLocalNames)
                        then (
                            debug_print 1 ("Saw write to a non-local pointer lval: " ^ (
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
                            let boundsExpr = boundsDescrForExpr e !currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs enclosingFile
                            in
                            (doNonLocalPointerStoreInstr ~useVoidPP:false e (pointeeUniqtypeGlobalPtr e enclosingFile uniqtypeGlobals) 
                                  (lhost, loff) (boundsExpr) l enclosingFile f uniqtypeGlobals helperFunctions, 
                            [])
                        )
                        else if isPointerType (Cil.typeOf (Lval(lhost, loff)))
                            && not (hostIsLocal lhost !currentFuncAddressTakenLocalNames) then (
                            (* We're storing to a void* lvalue, i.e. some pointer type that
                             * does *not* need bounds.
                             * 
                             * But PROBLEM: if it's polymorphic code, the actual pointer might be 
                             * alive elsewhere with a different type, e.g. int*.
                             * So 
                             * (1) we need to store something; doing nothing risks leaving stale
                             *        bounds behind;
                             * (2) we'd like to store the bounds appropriate for the type it
                             *        *will* be used at, but can't predict this;
                             * (3) as a "safe" default, we can store invalid bounds;
                             * (4) we could grub around in the cache to "guess" the typed
                             *        bounds and store those;
                             * (5) that's *only* safe to do if we also instrument the shadow-load
                             *        code with a type-check, which we could do with
                             *        pointer-XORing. 
                             *
                             * We *could* look up the declared type of the stored-to object,
                             * via liballocs, but that would be slow.
                             *
                             * Can we assume that the stored-to object is in the cache?
                             * We might expect that (void** ) &obj has been done recently,
                             * meaning the allocation "obj" perhaps ought to be cached.
                             * In fact YES:
                             * we currently do __fetch_bounds_full for cast expressions,
                             * and this will only be optimised away if we don't pass the
                             * cast expressions via the shadow stack or store them.
                             * 
                             * (1) these casts-to-void** don't generate a check in trumptr,
                             * (2) argument pushes do only inline fetches,
                             *    which don't help since the pointer was not loaded from anywhere;
                             * (3) no arithmetic/indexing was done on the void** value
                             * 
                             * ... we *still* seem to eagerly fetch bounds. This is a bit
                             * disconcerting, but it helps us here.
                             * 

                             * So if I do   f((void** ) &p), 
                             * what happens
                             * and what should happen?
                             * We immediately calculate bounds for (void** ) &p and push them.
                             * What are they?
                             * They are the same as for (int* ) &p since
                             * bounds care only about sizes, not types.
                             * However, a side-effect is that the actual type of the storage of p,
                             * i.e. the allocation whose address we're writing,
                             * should be in the cache,
                             * *if* it was obtained by void**-casting some other address.
                             * 
                             * PROBLEM though: if I'm just doing
                             *   *destLoc = somePtrToVoid;
                             * I will *not* have bounds for somePtrToVoid!
                             * i.e. if the cast happened outside our procedure....
                             *
                             * What about the allocation we're writing *to*?
                             * That is actually the best authority, consistent with 
                             * __can_hold_pointer() checking.
                             * We should check the cache for that too.
                             * 
                             * FIXME: add the XOR-based load safeguard.
                             *)
                            (*let grubbedUniqtypeExpr = (Lval(Var(grubbedTemp), NoOffset)) in
                            [
                                Call((Var(grubbedTemp), NoOffset),
                                    Lval(Var(helperFunctions.getCachedPointeeType.svar), NoOffset),
                                    [
                                        e
                                    ],
                                    l
                                )
                            ] @ *)
                            (* How do we want it to generate the bounds? 
                             * If uncasting has done some good, we can use that to generate
                             * the bounds.
                             * If it hasn't, we want to use cache-grubbing for that too.
                             * There's no way to say "fetch from cache only" as a boundsExpr value,
                             * so pass invalid bounds and live with it.
                             * NOTE that all of this is not a problem in SoftBound mode, because
                             * even a void* has bounds then.
                             *)
                            ( let uncastedSrcExpr = uncastExpr e in
                              let uncastDoesGood = typeNeedsBounds (Cil.typeOf uncastedSrcExpr) enclosingFile
                              in
                            (doNonLocalPointerStoreInstr ~useVoidPP:true e 
                                (if uncastDoesGood then pointeeUniqtypeGlobalPtr uncastedSrcExpr enclosingFile uniqtypeGlobals
                                else (CastE(TPtr(findStructTypeByName enclosingFile.globals "uniqtype", []), nullPtr)))
                                (lhost, loff)
                                (if uncastDoesGood
                                    then 
                                        let loadedFromLv = (
                                            debug_print 1 ("Getting loaded-from addr for ptr expr: " ^ (expToCilString uncastedSrcExpr) ^ "\n");
                                            match (getStoredPtrLvalForPtrExp (simplifyPtrExprs uncastedSrcExpr) enclosingFile) with
                                            |   Some(Var(vi), loadOffs)
                                                    when (hostIsLocal (Var(vi)) !currentFuncAddressTakenLocalNames) ->
                                                    (* It's given us back a local lvalue
                                                     * as a "loaded from" lvalue.
                                                     * This is normally begging the question,
                                                     * i.e. we don't know where the local was
                                                     * loaded from, so make it None.
                                                     * But it might be a temporary 
                                                     * whose original loaded-from expression
                                                     * was saved. *)
                                                    (try
                                                         let (foundLv : lval option) = VarinfoMap.find vi !tempLoadExprs
                                                         in
                                                         debug_print 1 ("Found a saved loaded-from expr: " ^ (
                                                             match foundLv with Some(lv) -> lvalToString lv | None -> "(saved None)") ^ "\n");
                                                         foundLv
                                                    with Not_found -> 
                                                          debug_print 1 ("Didn't find a saved loaded-from expr\n");
                                                          None
                                                    )
                                            |  x -> x
                                        )
                                        in 
                                        (MustFetch(loadedFromLv))
                                    else (BoundsBaseLimitRvals(zero, zero))
                                )
                                l enclosingFile f uniqtypeGlobals helperFunctions
                            , [])
                            )
                        )
                        else ([], [])
                    in begin
                    debug_print 1 "Queueing some instructions\n";
                    instrsToPrepend @ changedInstrs @ instrsToAppend
                    end
              | Call(olv, calledE, es, l) -> 
                  (* Don't instrument calls to our own (liballocs/libcrunch) functions that get -include'd. *)
                  if (match calledE with Lval(Var(fvi), NoOffset) when varIsOurs fvi -> true
                    | _ -> false)
                  then changedInstrs
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
                    let needCallViaNoinlinePureHelper = if olv = None || (not returnsBounds) then false
                    else (match calledE with
                        (Lval(Var(fvi), NoOffset)) ->
                            (filterAttributes "aconst" fvi.vattr <> [] (* HACK: why "aconst"? very CILly *)||
                            filterAttributes "pure" fvi.vattr <> []) &&
                            (* don't call the helpers from within the helpers *)
                            (let (helperFundecs, _) = unzip noinlineHelpers in
                             not (List.mem f helperFundecs))
                      | _ -> false
                    )
                    in
                    let calleeAndInstrsViaHelper fvi =
                        debug_print 1 ("Saw call to pure/const bounds-returning function called: " ^ 
                        expToString calledE ^ "\n");
                        match Cil.typeSig fvi.vtype with
                            TSFun(TSPtr(_, _), _, _, _) -> (
                                (* Okay. The static-noinline helper wrapper function
                                 * has already been created. What we need to do is
                                 * (1) replace the call to the function
                                 * with the call to the helper.
                                 * (2) unpack the fat pointer it returns, so that
                                 * the pointer gets assigned to the intended return lvalue
                                 * and the bounds get assigned to that lvalue's bounds var.
                                 * - make this call call that instead
                                 * - it will return a __libcrunch_ptr_with_bounds_t fatptr,
                                 * - we need to manipulate changedInstrs
                                 *     so that we assign fatptr.p to olv
                                 *     and fatptr.bounds to wherever we would put the returned bounds.
                                 *)
                                let tmpRet = Cil.makeTempVar f ~name:"__fatptr_ret" fatPtrType in
                                let noinlineHelper = getNoinlinePureHelper fvi enclosingFile in
                                match olv with
                                    None -> failwith "internal error: noinline helper but no return value or return value ignored"
                                  | Some(olhost, oloff) -> (
                                    let ptrFieldLv = Var(tmpRet), Field(List.hd fatPtrCompinfo.cfields, NoOffset) in
                                    let boundsFieldLv = Var(tmpRet), Field(List.hd (List.tl fatPtrCompinfo.cfields), NoOffset) in
                                    let actualCalledE = Lval(Var(noinlineHelper.svar), NoOffset)
                                    in
                                    (actualCalledE,
                                    [Call(Some(Var(tmpRet), NoOffset), actualCalledE, es, l);
                                     Set((olhost, oloff), CastE(voidPtrType, Lval(ptrFieldLv)), l)
                                     ] @ 
                                    let boundsRval = Lval(boundsFieldLv) in
                                    if hostIsLocal olhost !currentFuncAddressTakenLocalNames then (
                                        [Set(localLvalToBoundsFun (olhost, oloff), boundsRval, l)]
                                    ) else (
                                        [Set(localLvalToBoundsFun (olhost, oloff), boundsRval, l)] @ (
                                         doNonLocalPointerStoreInstr ~useVoidPP:false
                                            (* pointer value *) (CastE(voidPtrType, Lval(ptrFieldLv)))
                                            (* type of where it's being stored? *)
                                            (pointeeUniqtypeGlobalPtr (Lval(olhost, oloff)) enclosingFile uniqtypeGlobals)
                                            (* where it's been/being stored *) (olhost, oloff)
                                            (* its bounds *) (BoundsLval(boundsFieldLv))
                                            l enclosingFile f uniqtypeGlobals helperFunctions
                                        )
                                    )
                                    )
                                ) (* end Some(olhost, oloff) *)
                                )
                          | _ -> (calledE, changedInstrs)
                    in
                    let realCalledE, newChangedInstrs = 
                        if needCallViaNoinlinePureHelper then 
                            match calledE with
                                (Lval(Var(fvi), NoOffset)) -> calleeAndInstrsViaHelper fvi
                              | _ -> (calledE, changedInstrs)
                        else (calledE, changedInstrs)
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
                                debug_print 1 ("Pushing bounds for pointer-contained-in-argument expr " ^ (expToString ptrExpr) ^ "\n");
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
                                  | MustFetch(maybeLoadedFromLv) ->
                                        Call( None,
                                        (Lval(Var(helperFunctions.fetchAndPushArgumentBounds.svar),NoOffset)),
                                        [
                                            ptrExpr;
                                            (match maybeLoadedFromLv with
                                              Some(lh,lo) -> CastE(voidPtrPtrType, mkAddrOf (lh,lo))
                                            | None -> CastE(voidPtrPtrType, nullPtr));
                                            pointeeUniqtypeGlobalPtr ptrExpr enclosingFile uniqtypeGlobals
                                        ],
                                        instrLoc !currentInst
                                        )
                            in
                            concatMapForAllPointerBoundsInExprList callForOneOffset (List.rev es) (* push r to l! *)
                                !currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs enclosingFile
                        )
                        in
                        let cookieStackAddrVar = ref None
                        in
                        let calleeLval = match realCalledE with
                                    Lval(lv) -> lv
                                  | _ -> failwith "internal error: calling a non-lvalue"
                        in
                        let boundsPushCookieInstructions = 
                            if passesBounds || returnsBounds then (
                            let spVar = Cil.makeTempVar f ~name:"__cookie_stackaddr" ulongPtrType
                            in
                            spVar.vattr <- [Attr("unused", [])];
                            cookieStackAddrVar := Some(spVar);
                            [Call(None, 
                                Lval(Var(helperFunctions.pushArgumentBoundsCookie.svar), NoOffset), 
                                [CastE(voidConstPtrType, mkAddrOf calleeLval)], instrLoc !currentInst);
                             (* also remember the cookie stackaddr *)
                             Set((Var(spVar), NoOffset), boundsSpExpr, instrLoc !currentInst)
                            ]
                            ) else []
                        in
                        let returnBoundsPeekOrStoreInstructions = 
                          if needCallViaNoinlinePureHelper then []
                          else
                          match olv with
                            None -> []
                          | Some(lhost, loff) -> 
                                let destT = Cil.typeOf (Lval(lhost, loff))
                                in
                                if not (list_empty (containedPointerExprsForExpr (Lval(lhost, loff))))
                                then (
                                    debug_print 1 ("Saw call writing to a non-void pointer lval: " ^ (
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
                                                ptrExpr (* Lval(lhost, loff) *);
                                                Const(CStr("call to " ^ (expToString calledE)))
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
                                        let lvExpr = (Lval(lhost, loff)) in
                                        (writeOne (Var(bvi), NoOffset) ptrExpr blah offsetExpr) :: (
                                         doNonLocalPointerStoreInstr ~useVoidPP:false
                                            (* pointer value *) lvExpr
                                            (pointeeUniqtypeGlobalPtr lvExpr enclosingFile uniqtypeGlobals)
                                            (* where it's been/being stored *) (lhost, loff)
                                            (* its bounds *) (BoundsLval(Var(bvi), NoOffset))
                                            l enclosingFile f uniqtypeGlobals helperFunctions
                                        )
                                    in
                                    if hostIsLocal lhost !currentFuncAddressTakenLocalNames then (
                                        debug_print 1 "Local, so updating/invalidating its bounds.\n";
                                        (* we're popping/peeking, so reverse *)
                                        List.flatten (List.rev (mapForAllPointerBoundsInExpr callsForOneLocal (Lval(lhost, loff)) !currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs enclosingFile))
                                    ) else ( (* non-local -- writing into the "heap"! *)
                                    debug_print 1 "Host is not local\n";
                                    List.flatten (List.rev (mapForAllPointerBoundsInExpr callsForOneNonLocal (Lval(lhost, loff)) !currentFuncAddressTakenLocalNames localLvalToBoundsFun !tempLoadExprs enclosingFile))
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
                            newChangedInstrs @ 
                            returnBoundsPeekOrStoreInstructions @ 
                            boundsCleanupInstructions
                end
              | (* Asm(attrs, instrs, locs, u, v, l) -> *) _ -> changedInstrs
        end (* let instrsWithCachedBoundsUpdates = begin ... *)
        in instrsWithCachedBoundsUpdates
  ) (* end ChangeDoChildrenPost *)
  end

  val underAddrOf = ref false
    
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
        let localLvalToBoundsFun = boundsLvalForLocalLval boundsLocals theFunc enclosingFile
        in
        let boundsType = try findStructTypeByName enclosingFile.globals "__libcrunch_bounds_s"
          with Not_found -> failwith "strange: __libcrunch_bounds_s not defined"
        in
        (* latch underAddrOf on the way down, so that we remember 
         * on the way back up (ChangeDoChildrenPost) *)
        let weAreUnderAddrOf = !underAddrOf
        in
        debug_print 1 ("Descend-visiting lval " ^ (lvalToCilString outerLv) ^ "; under addr of? " ^
            (if weAreUnderAddrOf then "yes\n" else "no\n"));
        ChangeDoChildrenPost(outerLv, fun lv -> 
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
             (* Here
                - we are recursing down ol, the list of offset components
                - lhost is the working host; when we hoist a prefix, it gets replaced with a ( *temp ) expr
                - origHost is always the original host, even after hoist/replace
                - offsetsOkayWithoutCheck accumulates offsets from ol that we decided we didn't need to check
                   ... meaning the working lvalue *up to the currently-processed offset*
                       is always (lhost, offsetsOkayWithoutCheck)
                - prevOffsetList accumulates all processed offsets
                   ... meaning the original lvalue *up to the currently-processed offset* is always (origHost, prevOffsetList)
                   
                If we're emulating SoftBound we check/hoist derefs as well as indexing.
              *)
             let hoistDeref memExpr offsetsOkayWithoutCheck =
                debug_print 1 ("Hoisting deref of " ^ (expToCilString memExpr) ^ "\n");
                (* If we're working with a Mem lvalue, it means we have a deref we 
                 * want to hoist.
                 * By construction, thanks to ChangeDoChildrenPost, any nested derefs
                 * or indexing inside the memExpr will have been hoisted already.
                 * However, we could still be derefing a global, say; we can't assume
                 * it is a local lvalue.
                 * So, in general, we need to load its bounds.
                 * It's easier if we just create a fresh temporary.
                 * We *don't* create a temporary for the deref operation's output.
                 * That way, we don't have to remember any "loaded from" relationships.
                 *)
                (* Do we have a bounds local already for the thing we're derefing? *)
                let simplifiedMemExpr = foldConstants memExpr
                in
                (* If we have an expr using PlusPI, like "ptr + 1", our BoundsDescrForExpr
                 * should just use the bounds of the input pointer. *)
                let boundsDescrForMemExpr = boundsDescrForExpr simplifiedMemExpr !currentFuncAddressTakenLocalNames 
                    localLvalToBoundsFun !tempLoadExprs enclosingFile
                in
                let blv = ensureBoundsLocalLval simplifiedMemExpr boundsDescrForMemExpr theFunc boundsType
                in
                let boundsLoadInstrs = 
                    boundsUpdateInstrs blv simplifiedMemExpr boundsDescrForMemExpr 
                        helperFunctions currentLoc enclosingFile 
                        (pointeeUniqtypeGlobalPtr simplifiedMemExpr enclosingFile uniqtypeGlobals)
                in
                self#queueInstr (boundsLoadInstrs @ [Call(None,
                    (Lval(Var(helperFunctions.checkDeref.svar),NoOffset)),
                    [ simplifiedMemExpr
                    ; (* what are the bounds of memExpr? *)
                      Lval(blv)
                    ], currentLoc
                )]);
                (* the same lval *)
                (Mem(memExpr), offsetFromList offsetsOkayWithoutCheck)
             in
             let rec hoistIndexing ol lhost offsetsOkayWithoutCheck origHost prevOffsetList = 
                (* let _ = debug_print 1 ("hoist indexing on" ^ 
                    "\nlval " ^ (lvalToCilString (lhost, offsetFromList (offsetsOkayWithoutCheck @ ol))) ^
                    "\ntype " ^ (typToString (Cil.typeOf (Lval(lhost, offsetFromList (offsetsOkayWithoutCheck @ ol))))) ^
                    "\norig host " ^ (lvalToCilString (origHost, NoOffset)) ^
                    "\nprev offset list " ^ (List.fold_left (fun s -> fun ox -> s ^ ", " ^ (offsetToString ox)) "" prevOffsetList)
                    ^ "\n") in let _ = flush stderr in *)
                 match ol with 
                     [] -> (lhost, offsetFromList offsetsOkayWithoutCheck)
                   | NoOffset :: rest -> failwith "impossible: NoOffset in offset list"
                   | Field(fi, ign) :: rest -> 
                         hoistIndexing rest lhost (offsetsOkayWithoutCheck @ [Field(fi, ign)]) origHost (prevOffsetList @ [Field(fi, ign)])
                   | Index(indexExp, ign) :: rest -> 
                         let indexedType = Cil.typeOf (Lval(origHost, offsetFromList prevOffsetList))
                         in
                         let exprType = Cil.typeOf (Lval(origHost, offsetFromList (prevOffsetList @ [Index(indexExp, ign)])))
                         in
                         let (arrayElementCount, arrayElementT)
                          = multiplyAccumulateArrayBounds (Int64.of_int 1) exprType
                         in
                         let intExp = (* BinOp(Mult, indexExp, makeIntegerConstant arrayElementCount, intType) *) indexExp
                         in
                         let (isNotStaticallyInBounds, isIndexingFlexibleArray) = 
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
                                    (* FIXME: write test cases for this flexible-array and 
                                     * static-zero stuff. *)
                                    (* We're indexing on a host that is *not* 
                                     * an array with a size bound.
                                     * It might be an array with no size bound, or a non-array,
                                     * i.e. a Mem host.
                                     * In general, this indexing needs to be hoisted and checked.
                                     * If the index expr is statically zero, we will
                                     * detect this later after we've converted it to a pointer
                                     * adjustment. FIXME: actually we don't yet. ALSO,
                                     * if we do, we need to make an exception for pointers
                                     * generated from &s.arr[0] where arr is a flexible array
                                     * member, since its length might be 0.
                                     * If we're on a Mem, we will hoist the deref later anyway.
                                     * We let through statically-zero index exprs without check
                                     * *if* the array is not inside a struct 
                                     * i.e. is not a flexible array member "[]".
                                     * for which the [0] selector might not be valid. *)
                                    let isSelectingOnFlexibleArrayMember
                                     = match (List.rev offsetsOkayWithoutCheck) with
                                            Field(fi, _) :: rest -> true
                                          | _ -> false
                                    in
                                    (true, isSelectingOnFlexibleArrayMember)
                              | Some(bound) -> match constInt64ValueOfExpr intExp with
                                    Some(intValue) -> 
                                        (intValue < (Int64.of_int 0) || intValue >= bound, false)
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
                                    (* isNotStaticallyInBounds = *)
                                    if hostIsLocal origHost !currentFuncAddressTakenLocalNames then
                                        (* Emit a simple check that we're in-bounds.
                                         * This is sufficient to ensure that 
                                         * access to the cached bounds locals,
                                         * even using the variable intExp,
                                         * is safe. *)
                                        (* NOTE: what was the rationale for separating out
                                         * this case? I think it was so that non-address-taken
                                         * locals woul stay non-address-taken. Not sure whether
                                         * the fear is justified. *)
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
                                        (false, false) (* i.e. *not* possibly OOB, once we've checked. *)
                                    else (true, false)
                            in
                            if (not isNotStaticallyInBounds) then
                                (* simple recursive call *)
                                hoistIndexing rest lhost (offsetsOkayWithoutCheck @ [Index(indexExp, ign)]) origHost (prevOffsetList @ [Index(indexExp, ign)])
                            else 
                                (* if we started with x[i].rest, 
                                 * make a temporary to hold &x[0] + i, 
                                 * check that,
                                 * then recurse
                                 * with current lvalue *(temp).rest.
                                 *
                                 * SUBTLETY:
                                 * If x itself has array type, 
                                 * &x[0] will always have type T* where T is the ultimate (non-array) element type.
                                 * We need to multiply.
                                 * OR DO WE? 
                                 * Does StartOf always give us a pointer to the raw element type? 
                                 * If it gives us pointers to arrays, we're okay. And it does. *)
                                let ptrExp =  (Cil.mkAddrOrStartOf (lhost, offsetFromList offsetsOkayWithoutCheck))
                                in
                                let (tempVar, checkInstrs) = hoistAndCheckAdjustment
                                    ~isBeingDerefed:(not weAreUnderAddrOf)
                                    enclosingFile theFunc
                                    helperFunctions uniqtypeGlobals 
                                    ptrExp
                                    (* intExp *) intExp
                                    isIndexingFlexibleArray
                                    (* localLvalToBoundsFun *) localLvalToBoundsFun
                                    !currentFuncAddressTakenLocalNames
                                    !currentInst
                                    tempLoadExprs
                                in
                                let res = (
                                    self#queueInstr checkInstrs;
                                    hoistIndexing rest (Mem(Lval(Var(tempVar), NoOffset))) [] origHost (prevOffsetList @ [Index(intExp, ign)])
                                )
                                in 
                                res
             in
             (* Check the initial deref, if there is one and we're checking derefs;
              * then we have to check the indexing. *)
             let (initialHost, initialOff) = lv in
             let (postDerefHost, postDerefOff) = match initialHost with
                 Mem(memExpr) when not weAreUnderAddrOf ->
                     hoistDeref memExpr (offsetToList initialOff)
                   | _ -> lv
             in
             let finalLval = hoistIndexing (offsetToList postDerefOff) postDerefHost [] postDerefHost []
             in
             finalLval
        )

  method vexpr (outerE: exp) : exp visitAction = 
    debug_print 1 (("Visiting expression: " ^ (expToString outerE)) ^ "\n");
    debug_print 1 (("CIL form: " ^ (expToCilString outerE)) ^ "\n");
    match !currentFunc with
        None -> (* expression outside function *) SkipChildren
      | Some(f) -> 
          (* Need to remember parent lval *)
          let initialSimplifiedE = simplifyPtrExprs outerE in
          let simplifiedE = match initialSimplifiedE with
                (* HACK around CIL's bonkers encoding of __builtin_va* primitives. *)
                SizeOfE(Lval(Var(x), NoOffset)) when stringStartsWith x.vname "__builtin_"
                  -> initialSimplifiedE
            | SizeOfE(subE) -> SizeOf(Cil.typeOf subE)
            | _ -> initialSimplifiedE
          in
          let _ = match simplifiedE with
            AddrOf(lv) -> underAddrOf := true
              | StartOf(lv) -> underAddrOf := true (* necessary? *)
              | _ -> underAddrOf := false
          in  
          ChangeDoChildrenPost(simplifiedE, fun e -> 
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
                        (TPtr(t1, _), TPtr(t2, _)) when Cil.typeSig t1 = Cil.typeSig t2 -> t1
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
          | CastE(targetT, subex) ->
              let subexT = Cil.typeOf subex in 
              let subexTs = getConcreteType(Cil.typeSig(subexT)) in
              let targetTs = getConcreteType(Cil.typeSig(targetT))
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
                    CastE(targetT, Lval(Var(tmpVar), NoOffset))
                else if castNeedsFreshBounds subex targetT enclosingFile
                    then begin
                    debug_print 1 ("Cast may change bounds: source `" ^ 
                        (typsigToString (decayArrayToCompatiblePointer subexTs)) ^ 
                        "', dest `" ^ 
                        (typsigToString (decayArrayToCompatiblePointer targetTs)) ^ "'\n");
                    flush stderr;
                    let tempVar, checkInstrs = hoistCast enclosingFile f
                                    helperFunctions uniqtypeGlobals 
                                    (* castFromExp *) subex
                                    (* castExp *) e
                                    (* castToTS *) targetTs
                                    (* localLvalToBoundsFun *) (boundsLvalForLocalLval boundsLocals f enclosingFile)
                                    !currentFuncAddressTakenLocalNames
                                    !currentInst
                                    tempLoadExprs
                    in
                    (
                        self#queueInstr checkInstrs;
                        (Lval(Var(tempVar), NoOffset))
                    )
                end else e
          (* Now we just need to handle pointer arithmetic that appears in code as such.
           * We let it stand if we're at top level; otherwise we hoist it
           * to another temporary. *)
        | _ -> (* check for pointer arithmetic *)
             (let maybeAdjustment = begin match e with
              |  BinOp(PlusPI, ptrExp, intExp, t) -> Some(ptrExp, intExp)
              |  BinOp(IndexPI, ptrExp, intExp, t) -> Some(ptrExp, intExp)
              |  BinOp(MinusPI, ptrExp, intExp, t) -> Some(ptrExp, UnOp(Neg, intExp, Cil.typeOf intExp))
              |  _ -> None
          end in match maybeAdjustment with
            None -> begin
                debug_print 1 ("Leaving expression alone because it does no pointer arithmetic: " ^ (expToString e) ^ "\n");
                e
            end
          | Some(ptrExp, intExp) -> 
                if isStaticallyZero intExp then begin
                    debug_print 1 ("Leaving expression alone because its adjustment is always zero: " ^ (expToString e) ^ "\n");
                    e
                end
                else begin
                    debug_print 1 ("Not top-level, so rewrite to use temporary\n");
                    flush stderr;
                    let tempVar, checkInstrs = hoistAndCheckAdjustment ~isBeingDerefed:false 
                                    enclosingFile f
                                    helperFunctions uniqtypeGlobals 
                                    (* ptrExp *) ptrExp
                                    (* intExp *) intExp
                                    (* checkEvenIfStaticallyZero *) false
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
                                      (* debug_print 1 "Saw a check\n"; *)
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
                            (* debug_print 1 ("Split an Instr sequence into " ^ (string_of_int (List.length accRuns)) ^ "\n"); *)
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
                                    (* Cil.dumpBlock (new defaultCilPrinterClass) Pervasives.stderr 0 b; *)
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
                                    [arg1; arg2; Lval(mkMem arg3 NoOffset); arg5], loc)
                          | _ -> failwith ("impossible: check is not a check (" ^ instToString (List.hd is) ^ ")")
                      ]);
                      sid = outerS.sid;
                      succs = outerS.succs;
                      preds = outerS.preds;
                    };
                    (* We're expecting that the check statement is labelled with a "full check" *)
                    let secondaryCheckLabelIdent = match outerS.labels with
                        [Label(ident, _, _)] ->
                            (* debug_print 1 ("Found label ident: " ^ ident ^ "\n"); *)
                            let prefix = String.sub ident 0 (String.length "__crunchbound_primary_check")
                            in
                            let suffix = (
                                if String.length ident > String.length ("__crunchbound_primary_check")
                                then String.sub ident (String.length ("__crunchbound_primary_check")) (String.length ident - String.length ("__crunchbound_primary_check"))
                                else "")
                            in
                            (* debug_print 1 ("Prefix, suffix: " ^ prefix ^ ", " ^ suffix ^ "\n"); *)
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
      if varIsOurs f.svar then SkipChildren
      else
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
      (* debug_print 1 ("Copying body of function " ^ f.svar.vname ^ "\n"); *)
      let secondaryBody = (
          visitCilBlock (new copyBlockVisitor copiedBlock "__bottomhalf_") f.sbody;
          match !copiedBlock with
              Some(b) -> b
            | None -> failwith "couldn't copy function body"
      )  
      in
      (* debug_print 1 ("Labelling primary checks in function " ^ f.svar.vname ^ " original body\n"); *)
      f.sbody <- visitCilBlock (new checkStatementLabelVisitor "primary_check" helperFunctions.checkDerivePtr.svar) f.sbody;
      (* debug_print 1 ("After visit, original body is as follows\n");
      Cil.dumpBlock (new defaultCilPrinterClass) Pervasives.stderr 0 f.sbody; *)
      (* debug_print 1 ("Labelling full checks in copied body of function " ^ f.svar.vname ^ "\n"); *)
      let labelledSecondaryBody = visitCilBlock (new checkStatementLabelVisitor "full_check" helperFunctions.checkDerivePtr.svar) secondaryBody in
      let findStmtByLabel = fun ident -> (
          let found = ref None
          in
          (visitCilBlock (new findStmtByNameVisitor ident found) labelledSecondaryBody;
          match !found with
              None -> debug_print 1 ("Label not found: " ^ ident ^ "\n"); raise Not_found
            | Some(stmt) -> ref stmt
          )
      )
      in 
      (* debug_print 1 ("After visit, copied body is as follows\n");
      Cil.dumpBlock (new defaultCilPrinterClass) Pervasives.stderr 0 labelledSecondaryBody;
      debug_print 1 ("... and original body is as follows\n");
      Cil.dumpBlock (new defaultCilPrinterClass) Pervasives.stderr 0 f.sbody;
      debug_print 1 ("Inserting primary/secondary jumps in function " ^ f.svar.vname ^ "\n"); *)
      f.sbody <- visitCilBlock (new primaryToSecondaryJumpVisitor helperFunctions.checkDerivePtr.svar helperFunctions.primaryCheckDerivePtr.svar findStmtByLabel enclosingFile) f.sbody;
      (* debug_print 1 ("Finally, function body is as follows\n");
      Cil.dumpBlock (new defaultCilPrinterClass) Pervasives.stderr 0 f.sbody;
      debug_print 1 ("Finished with function " ^ f.svar.vname ^ "\n"); *)
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

class helperizePureCalleesVisitor = fun enclosingFile ->
                                     object(self)
  inherit (crunchBoundBasicVisitor enclosingFile)
  
  val createdHelpers = ref []

  method getCreatedHelpers () : (fundec * fundec) list = !createdHelpers (* helper function, first caller function *)

  method vfunc (f: fundec) : fundec visitAction = 
      currentFunc := Some(f);
      DoChildren

  method vinst (i: instr) : instr list visitAction = 
      let f = match !currentFunc with Some(x) -> x | None -> failwith "Instr outside function"
      in
      let fatPtrType = try findStructTypeByName enclosingFile.globals "__libcrunch_ptr_with_bounds_s"
        with Not_found -> failwith "strange: __libcrunch_ptr_with_bounds_s not defined"
      in
      let fatPtrCompinfo = match fatPtrType with TComp(ci, _) -> ci
        | _ -> failwith "strange: __libcrunch_ptr_with_bounds_s not a composite type"
      in
      match i with
          Call(maybeOlv, calledE, args, l) -> (
            match calledE with
                (Lval(Var(fvi), NoOffset)) when (
                        match maybeOlv with Some(lv) -> 
                           not (list_empty (containedPointerExprsForExpr (Lval(lv))))
                         | None -> false
                    ) (* returnsBounds *)
                    && maybeOlv <> None && (
                    filterAttributes "aconst" fvi.vattr <> [] (* HACK: why "aconst"? very CILly *)||
                    filterAttributes "pure" fvi.vattr <> [])
                    && not (let (helpers, callees) = unzip !createdHelpers in List.mem fvi (List.map (fun x -> x.svar) callees))
                    -> (
                        debug_print 1 ("helperizePureCallees saw call to pure/const bounds-returning function called: " ^ 
                        expToString calledE ^ "\n");
                        match Cil.typeSig fvi.vtype with
                            TSFun(TSPtr(_, _), _, _, _) -> (
                                (* Okay. What we need to do is
                                 * 
                                 * - get or create a static-noinline helper wrapper function
                                 * - make this call call that instead
                                 * - it will return a __libcrunch_ptr_with_bounds_t fatptr,
                                 * - we need to manipulate changedInstrs
                                 *     so that we assign fatptr.p to olv
                                 *     and fatptr.bounds to wherever we would put the returned bounds.
                                 *)
                                let tmpRet = Cil.makeTempVar f ~name:"__fatptr_ret" fatPtrType in
                                let noinlineHelper = createNoinlinePureHelper fvi enclosingFile createdHelpers f in
                                (* Now we've created the helper, it will get instrumented by crunchbound.
                                 * What to do about our call? Nothing, for now; that's the main
                                 * crunchbound pass's job. *)
                                DoChildren
                            )
                            | _ ->
                                debug_print 1 ("helperizePureCallees: return type is too complex, so just de-consting.\n");
                                fvi.vattr <- dropAttributes ["aconst"; "pure"] fvi.vattr;
                                DoChildren
                       )
                  | _ -> DoChildren
                )
          | _ -> DoChildren
end

class fixupPureCalleesVisitor = fun enclosingFile -> 
                                fun createdHelpers ->
                                     object(self)
  inherit (crunchBoundBasicVisitor enclosingFile)

  method vfunc (f: fundec) : fundec visitAction = 
      currentFunc := Some(f);
      let (helpers, callees) = unzip createdHelpers in
      if List.mem f callees then (
          f.svar.vattr <- dropAttributes ["pure"; "aconst"] f.svar.vattr;
          SkipChildren)
        else SkipChildren
end

let feature : Feature.t = 
  { fd_name = "crunchbound";
    fd_enabled = false;
    fd_description = "dynamic bounds checking of pointer indexing and arithmetic";
    fd_extraopt = [("--emulate-softbound", Arg.Unit (fun _ -> 
        voidPtrHasBounds := true; 
        noPointerTypeInfo := true;
        skipSecondarySplit := true
    ), " emulate SoftBound (no type info, ignore casts, no secondary path)")];
    fd_doit = 
    (function (fl: file) -> 
      debug_print 1 ("command line args are:\n"
       ^ (String.concat ", " (Array.to_list Sys.argv) ) ^ "\n" );
      let _ = (if !voidPtrHasBounds then 
        debug_print 1 "void* has bounds (like SoftBound)"
      else debug_print 1 "void* does not have bounds\n";
        if !noPointerTypeInfo then 
        debug_print 1 "no pointer type info assumed (like SoftBound)"
      else (debug_print 1 "using pointer type info\n");
        if !skipSecondarySplit then 
        debug_print 1 "skip secondary path (like SoftBound)"
      else (debug_print 1 "enabling secondary path\n"))
      in
      let createHelpersVisitor = new helperizePureCalleesVisitor fl
      in
      visitCilFileSameGlobals (createHelpersVisitor :> cilVisitor) fl;
      visitCilFileSameGlobals (new crunchBoundVisitor fl (createHelpersVisitor#getCreatedHelpers ())) fl;
      if (not !skipSecondarySplit) then 
        let splitVisitor = new primarySecondarySplitVisitor fl in
        visitCilFileSameGlobals splitVisitor fl
        else ();
      visitCilFileSameGlobals (new fixupPureCalleesVisitor fl (createHelpersVisitor#getCreatedHelpers ())) fl
      )
    ;
    fd_post_check = true;
  } 

let () = Feature.register feature
