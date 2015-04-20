(* Copyright (c) 2011--15,
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

let instrLoc (maybeInst : Cil.instr option) =
   match maybeInst with 
   Some(i) -> Cil.get_instrLoc i
 | None -> locUnknown

let hoistAndCheckAdjustment enclosingFile enclosingFunction checkDerivePtrInlineFun inlineAssertFun uniqtypeGlobals ptrExp intExp ptrBoundsExp lvalToBoundsFun currentInst = 
    (* To avoid leaking bad pointers when writing to a shared location, 
     * we make the assignment to a temporary, then check the temporary,
     * then copy from the temporary to the actual target. *)
    let loc = instrLoc currentInst in
    let exprTmpVar = Cil.makeTempVar enclosingFunction (typeOf ptrExp) in
    let checkTmpVar = Cil.makeTempVar enclosingFunction intType in 
    (exprTmpVar, [
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
      Call( Some((Var(checkTmpVar), NoOffset)), (* return value dest *)
            (Lval(Var(checkDerivePtrInlineFun.svar),NoOffset)),  (* lvalue of function to call *)
            [ 
              (* const void **p_derived *)
              CastE( voidPtrPtrType, mkAddrOf (Var(exprTmpVar), NoOffset))
            ;
              (* const void *derivedfrom *)
              ptrExp
            ;
              (* __libcrunch_bounds_t *opt_derivedfrom_bounds *)
              ptrBoundsExp
              (* struct uniqtype *t *)  (* the pointed-*TO* type of the pointer *)
            ; let pointeeUniqtypeVar = 
                let pointeeTs = match (exprConcreteType ptrExp) with
                        TSPtr(ts, _) -> ts
                      | _ -> failwith "recipient is not a pointer"
                 in ensureUniqtypeGlobal pointeeTs enclosingFile uniqtypeGlobals
              in
              mkAddrOf (Var(pointeeUniqtypeVar), NoOffset)
            ],
            loc
      );
      (* then enqueue the assertion about its result *)
      Call( None,
            (Lval(Var(inlineAssertFun.svar),NoOffset)),
            [
              (* arg is the check result *)
              Lval(Var(checkTmpVar), NoOffset);
              Const(CStr("__check_derive_ptr(" ^ exprTmpVar.vname ^ ", " ^ "" ^ ")"));
              Const(CStr( loc.file ));
              Const(CInt64(Int64.of_int (if loc.line == -1 then 0 else loc.line), IUInt, None));
              Const(CStr( enclosingFunction.svar.vname ))
            ],
            loc
      )
    ])

let makeBoundsUpdateInstruction enclosingFile enclosingFunction fetchBoundsFun uniqtypeGlobals justWrittenLval lvalToBoundsFun currentInst = 
    let loc = instrLoc currentInst in
    Call( (lvalToBoundsFun justWrittenLval),
            (Lval(Var(fetchBoundsFun.svar),NoOffset)),
            [
                (* const void *ptr *)
                Lval(justWrittenLval)     (* i..e the *value* of the pointer we just wrote *)
            ;   (* struct uniqtype *t *)
                mkAddrOf (Var(ensureUniqtypeGlobal (exprConcreteType (Lval(justWrittenLval))) enclosingFile uniqtypeGlobals), NoOffset)
            ],
            loc
        )

let makeBoundsWriteInstruction enclosingFile enclosingFunction fetchBoundsFun uniqtypeGlobals (justWrittenLval : lval) writtenE lvalToBoundsFun currentInst =
    let loc = instrLoc currentInst in
    (* Here we do the case analysis: 
     * do we copy bounds, 
     *       make them ourselves, 
     *    or fetch them? *)
    begin match writtenE with
        _ -> ()
    end;
    (* FIXME *)
    makeBoundsUpdateInstruction enclosingFile enclosingFunction fetchBoundsFun uniqtypeGlobals justWrittenLval lvalToBoundsFun currentInst
    

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


let varinfoIsLocal vi = (not vi.vglob) && vi.vstorage != Static

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
 * __check_derive_ptr(derived_ptr, derived_from_ptr, &opt_derived_from_ptr_bounds, t)
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
 * into pointer arithmetic.
 * 
 * Hence any indexing of a local non-a-t'n array is passable statically.
 
 * Can we just say "if a local array is indexed by a non-constant expression,
 * treat it as address-taken, i.e. don't cache bounds"? YES.
 * We handle whether an array index is statically in-bounds later and separately.
 * Therefore we don't have to worry about pulling out a pointer and keeping
 * bounds for that.
 * That *would* be a valid optimisation though -- it would let us cache more bounds.
 *)

let rec isWithinArray (loff : offset) : bool = 
    match loff with
        NoOffset -> false
      | Index(_, rest) -> true
      | Field(fi, rest) -> isWithinArray rest

let isBoundsCacheablePointer (lv : lval) : bool = 
    begin match lv with
        (Var(vi), offset) -> varinfoIsLocal vi && isPointerType (Cil.typeOf (Lval(lv)))
            && (not vi.vaddrof)
            (* FIXME: exclude only variably-indexed arrays *)
            && (not (isWithinArray offset))
      | _ -> false
    end

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

class crunchBoundVisitor = fun enclosingFile -> 
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
  val mutable inlineAssertFun = emptyFunction "__inline_assert"
  val mutable fetchBoundsInlineFun = emptyFunction "__fetch_bounds"
  val mutable checkDerivePtrInlineFun = emptyFunction "__check_derive_ptr"
  
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

    inlineAssertFun <-  findOrCreateExternalFunctionInFile
                            enclosingFile "__inline_assert" (TFun(voidType, 
                            Some [ ("cond", intType, []);
                                   ("assertion", charConstPtrType, []);
                                   ("file", charConstPtrType, []);
                                   ("line", uintType, []);
                                   ("function", charConstPtrType, [])
                            ], false, [])) 
                            ;

    fetchBoundsInlineFun <- findOrCreateExternalFunctionInFile 
                            enclosingFile "__fetch_bounds" (TFun(boundsType, 
                            Some [ 
                                   ("ptr", voidConstPtrType, []);
                                   ("t", voidConstPtrType, [])
                                 ], 
                            false, []))
    ;

    checkDerivePtrInlineFun <- findOrCreateExternalFunctionInFile
                            enclosingFile "__check_derive_ptr" (TFun(intType, 
                            Some [ ("p_derived", voidPtrPtrType, []);
                                   ("derived_from", voidPtrType, []); 
                                   ("opt_derivedfrom_bounds", boundsPtrType, []); 
                                   ("t", uniqtypePtrType, []); 
                                 ], 
                            false, [])) 

  val currentInst : instr option ref = ref None
  val currentFunc : fundec option ref = ref None
  val currentLval : lval option ref = ref None
  
  val mutable currentFunctionLocalsToCacheBoundsFor = []

  (* Remember the set of __uniqtype objects for which we've created a global
   * weak extern. *)
  val uniqtypeGlobals : Cil.global UniqtypeMap.t ref = ref UniqtypeMap.empty

  (* Remember the mapping from locals to their bounds. *)
  val boundsLocals : Cil.varinfo VarinfoMap.t ref = ref VarinfoMap.empty

  method vfunc (f: fundec) : fundec visitAction = 
      currentFunc := Some(f);
      let boundsType = try findStructTypeByName enclosingFile.globals "__libcrunch_bounds_s"
        with Not_found -> failwith "strange: __libcrunch_bounds_s not defined"
      in
      (* Don't instrument our own (liballocs/libcrunch) functions that get -include'd. *)
      let startswith s pref = 
          if (String.length s) >= (String.length pref) 
          then (String.sub s 0 (String.length pref)) = pref 
          else false
      in 
      if startswith f.svar.vname "__liballocs_" 
          || stringEndsWith f.svar.vdecl.file "libcrunch_cil_inlines.h"
          then SkipChildren
      else
          (* We've done computeFileCFG, so no need to do the CFG info thing *)
          (* Figure out which pointer locals also need cached bounds. *)
          (
            currentFunctionLocalsToCacheBoundsFor <- [] (* !seenAdjustedLocals *); 
          (* let rec makeBoundsLocals vil = match vil with
              [] -> ()
            | vi :: more -> 
                let made = makeLocalVar f ("__cil_crunchbound_" ^ vi.vname) boundsType
                in 
                boundsLocals := VarinfoMap.add vi made !boundsLocals
          in
          makeBoundsLocals currentFunctionLocalsToCacheBoundsFor; *) 
            DoChildren
          )
  
  method vinst (outerI: instr) : instr list visitAction = begin
    currentInst := Some(outerI); (* used from vexpr *)
    ChangeDoChildrenPost([outerI], fun is ->
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
        let lvalToBoundsFun lv = match lv with
          (*  (Var(vi), NoOffset) -> (Var(VarinfoMap.find vi !boundsLocals), NoOffset) *)
          | _ -> raise Not_found
        in
        match is with
            [Set(lv, e, l)] ->
                let instrsToQueue =
                    (* We might be writing a non-void pointer on the lhs. 
                     *      If we're have bounds for that pointer, we need to write some bounds.
                     *      Sometimes we have to fetch those bounds;
                     *      Sometimes we can copy them;
                     *      Sometimes we can *create* them 
                     *          -- if it's taking the address of a variable (local or global);
                     *          -- if it's selecting a subobject from a variable
                     *          -- if it's selecting a subobject via a pointer we have bounds for.
                     *)
                    if isNonVoidPointerType(Cil.typeOf (Lval(lv))) && match lv with
                            (Var(vi), NoOffset) -> List.mem vi currentFunctionLocalsToCacheBoundsFor
                              | _ -> false
                    then
                        (* Queue some instructions to write the bounds. *)
                        [makeBoundsWriteInstruction enclosingFile f fetchBoundsInlineFun uniqtypeGlobals lv e lvalToBoundsFun !currentInst]
                    else []
                in begin
                self#queueInstr instrsToQueue;
                [Set(lv, e, l)]
            end
          | [Call(olv, e, es, l)] -> begin
                (* We might be writing a pointer. 
                 * Since, if so, the pointer has come from a function call, we don't know
                 * how to get bounds for it. So we always fetch those bounds.
                 * HMM. Potentially expensive. But the cache should help.
                 *)
                    match olv with
                        None -> is
                      | Some(lv) -> 
                        if isNonVoidPointerType (Cil.typeOf (Lval(lv)))
                                && (match lv with
                                    (Var(vi), NoOffset) -> List.mem vi currentFunctionLocalsToCacheBoundsFor
                                      | _ -> false)
                            then begin
                                (* Queue some instructions to write the bounds. *)
                               self#queueInstr [
                                makeBoundsUpdateInstruction enclosingFile f fetchBoundsInlineFun uniqtypeGlobals lv lvalToBoundsFun !currentInst
                                ];
                                is
                            end
                            else is
                        
            end
          | (* Asm(attrs, instrs, locs, u, v, l) -> *) _ -> is
       )
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
             let rec hoistIndexing ol lhost offsetsOkayWithoutCheck = 
                 match ol with 
                     [] -> (lhost, offsetFromList offsetsOkayWithoutCheck)
                   | NoOffset :: rest -> failwith "impossible: NoOffset in offset list"
                   | Field(fi, ign) :: rest -> 
                         hoistIndexing rest lhost (offsetsOkayWithoutCheck @ [Field(fi, ign)])
                   | Index(intExp, ign) :: rest -> 
                         let isPossiblyOOB = 
                             (* Try to get a bound *)
                             let maybeBound = match (lhost, offsetsOkayWithoutCheck) with
                                    (Var(vi), _) -> 
                                        let prefixLval = (Var(vi), offsetFromList offsetsOkayWithoutCheck)
                                        in 
                                        begin match (Cil.typeSig (Cil.typeOf (Lval(prefixLval)))) with
                                            TSArray(elTs, maybeB, _) -> maybeB
                                          | _ -> failwith "indexing non-array-typed lvalue"
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
                              | Some(bound) -> match (foldConstants intExp) with
                                    Const(CInt64(intValue, _, _)) -> 
                                        intValue < (Int64.of_int 0) || intValue >= bound
                                  | Const(CChr(chrValue)) -> begin
                                        match charConstToInt chrValue with
                                            CInt64(intValue, _, _) -> 
                                                intValue < (Int64.of_int 0)
                                                 || intValue >= bound
                                            | _ -> true
                                        end
                                  | _ -> true
                            in
                            if not isPossiblyOOB then
                                (* simple recursive call *)
                                hoistIndexing rest lhost (offsetsOkayWithoutCheck @ [Index(intExp, ign)])
                            else 
                                (* if we started with x[i].rest, 
                                 * make a temporary to hold &x + i, 
                                 * check that,
                                 * then recurse
                                 * with current lvalue *(temp).rest *)
                                let (tempVar, checkInstrs) = hoistAndCheckAdjustment 
                                    enclosingFile theFunc
                                    checkDerivePtrInlineFun inlineAssertFun uniqtypeGlobals 
                                    (* ptrExp *) (Cil.mkAddrOrStartOf (lhost, offsetFromList offsetsOkayWithoutCheck))
                                    (* intExp *) intExp
                                    (* ptrBoundsExp *) nullPtr
                                    (* lvalToBoundsFun *) (fun someLv -> raise Not_found)
                                    !currentInst
                                in
                                (
                                    self#queueInstr checkInstrs;
                                    hoistIndexing rest (Mem(Lval(Var(tempVar), NoOffset))) []
                                )
             in
             let eventualLval = hoistIndexing offsetList initialHost []
             in
             eventualLval
        )

  method vexpr (outerE: exp) : exp visitAction = 
    output_string stderr (("Visiting expression: " ^ (expToString outerE)) ^ "\n");
    output_string stderr (("CIL form: " ^ (expToCilString outerE)) ^ "\n");
    match !currentFunc with
        None -> (* expression outside function *) SkipChildren
      | Some(f) -> 
        ChangeDoChildrenPost(outerE, fun e -> 
          (* When we get here, indexing in lvalues has been rewritten. 
           * Also, we've run on any expressions nested within those lvalues.
           * So we just need to handle pointer arithmetic. 
           * We let it stand if we're at top level; otherwise we hoist it
           * to another temporary. *)
          let maybeAdjustment = begin match e with
              |  BinOp(PlusPI, ptrExp, intExp, t) -> Some(ptrExp, intExp)
              |  BinOp(IndexPI, ptrExp, intExp, t) -> Some(ptrExp, intExp)
              |  BinOp(MinusPI, ptrExp, intExp, t) -> Some(ptrExp, UnOp(Neg, intExp, Cil.typeOf intExp))
              |  _ -> None
          end in match maybeAdjustment with
            None -> begin
                output_string stderr ("Leaving expression alone because it does no pointer arithmetic: " ^ (expToString e) ^ "\n");
                e
            end
          | Some(ptrExp, intExp) -> 
                if isStaticallyZero intExp then begin
                    output_string stderr ("Leaving expression alone because its adjustment is always zero: " ^ (expToString e) ^ "\n");
                    e
                end
                else begin
                    output_string stderr ("Not top-level, so rewrite to use temporary\n");
                    flush stderr;
                    let tempVar, checkInstrs = hoistAndCheckAdjustment enclosingFile f
                                    checkDerivePtrInlineFun inlineAssertFun uniqtypeGlobals 
                                    (* ptrExp *) ptrExp
                                    (* intExp *) intExp
                                    (* ptrBoundsExp *) nullPtr
                                    (* lvalToBoundsFun *) (fun someLv -> raise Not_found)
                                    !currentInst
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
end

(* Things I've punted on: 
 * 
 * - any caching, for now; 
 * - pre-defining the hoisted pointers, so that they can be cached
        (but are they ever written to more than once? no, but copy-propagation works);
 * - updating the cached bounds after a pointer write
 *)

let feature : Feature.t = 
  { fd_name = "crunchbound";
    fd_enabled = false;
    fd_description = "dynamic bounds checking of pointer indexing and arithmetic";
    fd_extraopt = [];
    fd_doit = 
    (function (fl: file) -> 
      (* compute CFG as needed by scanForAdjustedLocalsVisitor *)
      (* already done by vsimplemem (?) *)
      (* Cfg.computeFileCFG fl; *)
      let tpFunVisitor = new crunchBoundVisitor fl in
      debug_print 1 ("command line args are:\n"
       ^ (String.concat ", " (Array.to_list Sys.argv) ) );
      visitCilFileSameGlobals tpFunVisitor fl);
    fd_post_check = true;
  } 

let () = Feature.register feature
