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
 
(* Here we assume vsimpleaddr has run... *)
let rec indexedLvToPointerExpAndIntegerExpAndOffset lv = match lv with 
   (lh, NoOffset) -> None
 | (lh, Index(indexExp, rest)) -> begin
        match lh with
           Var(vi) ->   Some(mkAddrOf (Var(vi), NoOffset), indexExp, rest)
         | Mem(memE) -> Some(memE, indexExp, rest)
        end
 | (lh, Field(fi, rest)) -> 
       (* make a new host that incorporates the field access *)
       let new_lh = Mem( mkAddrOf( (lh, Field(fi, NoOffset)) ) )
       in 
       indexedLvToPointerExpAndIntegerExpAndOffset (new_lh, rest)

let adjustedPointerAtTopLevel ourE = match ourE with 
  | BinOp(PlusPI, ptrExp, intExp, t) -> Some ptrExp
  | BinOp(IndexPI, ptrExp, intExp, t) -> Some ptrExp (* see docs -- "probably positive" *)
  | BinOp(MinusPI, ptrExp, intExp, t) -> Some ptrExp
  | Lval(lh, loff) -> begin 
    match indexedLvToPointerExpAndIntegerExpAndOffset (lh, loff) with
        None -> None
      | Some(ptrExp, intExp, remainingOffset) -> Some(ptrExp)
    end
  |   _ -> None

let adjustsPointerAtTopLevel ourE = match adjustedPointerAtTopLevel ourE with
    Some(ptrExp) -> true
  | None -> false
  
let makeCheckDeriveInstructions enclosingFile enclosingFunction checkDerivePtrInlineFun inlineAssertFun uniqtypeGlobals derivingE receivingLval lvalToBoundsFun currentInst = 
    (* To avoid leaking bad pointers when writing to a shared location, 
     * we make the assignment to a temporary, then check the temporary,
     * then copy from the temporary to the actual target. *)
    let loc = instrLoc currentInst in
    let exprTmpVar = Cil.makeTempVar enclosingFunction (typeOf derivingE) in
    let checkTmpVar = Cil.makeTempVar enclosingFunction intType in 
    let derivedFromE = match adjustedPointerAtTopLevel derivingE with
            Some(ptrExp) -> ptrExp
          | None -> failwith "impossible: checking derivation in expr that doesn't adjust a pointer"
    in
    let targetVar = match receivingLval with 
        (Var(v), NoOffset) -> v | _ -> failwith "unexpected adjustment: assigning to array or non-local storage"
    in
    ([
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
      Set( (Var(exprTmpVar), NoOffset), derivingE, loc );
      (* next enqueue the check call *)
      Call( Some((Var(checkTmpVar), NoOffset)), (* return value dest *)
            (Lval(Var(checkDerivePtrInlineFun.svar),NoOffset)),  (* lvalue of function to call *)
            [ 
              (* const void **p_derived *)
              CastE( voidPtrPtrType, mkAddrOf (Var(exprTmpVar), NoOffset))
            ;
              (* const void *derivedfrom *)
              derivedFromE
            ;
              (* __libcrunch_bounds_t *opt_derived_bounds *)
            (*  (try mkAddrOf (lvalToBoundsFun receivingLval) 
              with Not_found -> nullPtr)
            ; *)
              (* __libcrunch_bounds_t *opt_derivedfrom_bounds *)
              (try mkAddrOf(
                match derivedFromE with 
                    Lval derivedFromLv -> lvalToBoundsFun derivedFromLv
                  | _ -> raise Not_found
                )
              with Not_found -> nullPtr)

              (* struct uniqtype *t *)  (* the pointed-*TO* type of the pointer *)
            ; let pointeeUniqtypeVar = 
                let pointeeTs = match (exprConcreteType (Lval(receivingLval))) with
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
    ], exprTmpVar)

let makeBoundsUpdateInstructions enclosingFile enclosingFunction fetchBoundsFun uniqtypeGlobals justWrittenLval lvalToBoundsFun currentInst = 
    let loc = instrLoc currentInst in
    [
      Call( None (* FIXME: check return value? or just have the function fetch noop bounds? *),
            (Lval(Var(fetchBoundsFun.svar),NoOffset)),
            [
                (* const void *ptr *)
                Lval(justWrittenLval)     (* i..e the *value* of the pointer we just wrote *)
            ;   (* struct uniqtype *t *)
                mkAddrOf (Var(ensureUniqtypeGlobal (exprConcreteType (Lval(justWrittenLval))) enclosingFile uniqtypeGlobals), NoOffset)
                (* __libcrunch_bounds_t *out *)
            ;   mkAddrOf (lvalToBoundsFun justWrittenLval)
            ],
            loc
        )
    ]

let makeBoundsWriteInstructions enclosingFile enclosingFunction fetchBoundsFun uniqtypeGlobals (justWrittenLval : lval) writtenE lvalToBoundsFun currentInst =
    let loc = instrLoc currentInst in
    (* Here we do the case analysis: 
     * do we copy bounds, 
     *       make them ourselves, 
     *    or fetch them? *)
    begin match writtenE with
        _ -> ()
    end;
    (* FIXME *)
    makeBoundsUpdateInstructions enclosingFile enclosingFunction fetchBoundsFun uniqtypeGlobals justWrittenLval lvalToBoundsFun currentInst
    

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
 *)

class scanForAdjustedLocalsVisitor = fun addressTakenLocals -> 
                                     fun seenAdjustedLocals -> 
                                     object(self) inherit nopCilVisitor
   
    (* val mutable seenAdjustedLocals = ref [] *)
    (* method vlval (outerLv: lval) : lval visitAction = ChangeDoChildrenPost(lv, fun lv -> 
        match lv with
            (Var(vi), Index(intExp, rest)) 
                when 
            -> seenAdjustedLocals := vi :: !seenAdjustedLocals; 
          | (Var(vi), _) -> 
    )
        
    method vexpr (e : expr) : expr visitAction =
       *) 
    method vinst (i: instr) : instr list visitAction = 
          (* Figure out which pointer locals also need bounds. 
           * We cache bounds for any pointer local that is adjusted. 
           * Adjustment only happens at the top level of instructions which only adjust. 
           * So scan for such instructions. 
           * WHAT IF an address-taken local escapes? 
           * Its bounds might get out-of-sync.
           * So don't maintain bounds for it.
           * It's okay if our __check_derive_ptr calls make an otherwise non-address-taken
           * local become address-taken.
           *)
        match i with
        | Set(lv, e, l) -> begin
            let adjustedPtr = adjustedPointerAtTopLevel e
            in
            match adjustedPtr with
                Some(ptrExp) -> begin match ptrExp with 
                    Lval(Var(vi), NoOffset) when (not vi.vglob) && vi.vstorage != Static
                        && (not (List.mem vi addressTakenLocals))
                        && (not (List.mem vi !seenAdjustedLocals))
                        -> seenAdjustedLocals := vi :: !seenAdjustedLocals; 
                            DoChildren
                  | _ -> DoChildren
                end
              | _ -> DoChildren
          end
        | _ -> DoChildren
end

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
    let boundsPtrType = TPtr(findStructTypeByName enclosingFile.globals "__libcrunch_bounds_s", [])
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
                            enclosingFile "__fetch_bounds" (TFun(intType, 
                            Some [ 
                                   ("ptr", voidConstPtrType, []);
                                   ("t", voidConstPtrType, []);
                                   ("p_bounds", voidConstPtrType, [])
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
  
  val mutable currentFunctionLocalsToCacheBoundsFor = []

  (* Remember the set of __uniqtype objects for which we've created a global
   * weak extern. *)
  val uniqtypeGlobals : Cil.global UniqtypeMap.t ref = ref UniqtypeMap.empty

  (* Remember the mapping from locals to their bounds. *)
  val boundsGlobals : Cil.varinfo VarinfoMap.t ref = ref VarinfoMap.empty

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
          (* Figure out which pointer locals also need bounds. *)
          let addressTakenLocals = 
            (* !(scanAddressTakenVisitor.seenAddressTakenLocals) *)
            List.filter (fun x -> varinfoIsLocal x && (not (x.vaddrof))) (f.slocals @ f.sformals)
          in
          let seenAdjustedLocals = ref []
          in 
          let scanAdjustmentsVisitor = new scanForAdjustedLocalsVisitor addressTakenLocals seenAdjustedLocals
          in
          (* our visitor only defines a non-nop action per instruction; we give it 
           * a whole function at a time. *)
          let _ = visitCilFunction scanAdjustmentsVisitor f
          in
          currentFunctionLocalsToCacheBoundsFor <- !seenAdjustedLocals; 
          let rec makeBoundsLocals vil = match vil with
              [] -> ()
            | vi :: more -> 
                let made = makeLocalVar f ("__cil_crunchbound_" ^ vi.vname) boundsType
                in 
                boundsGlobals := VarinfoMap.add vi made !boundsGlobals
          in
          makeBoundsLocals currentFunctionLocalsToCacheBoundsFor; DoChildren
  
  method vinst (i: instr) : instr list visitAction = begin
    currentInst := Some(i); (* used from vexpr *)
    let f = match !currentFunc with
        Some(af) -> af
      | None -> failwith "instruction outside function"
    in
    (* vsimpleaddr ensures for us that if there is a pointer adjustment 
     * (including indexing) happening during an instruction, it will be
     * in the top-level expression. So we need only match a few patterns,
     * and we can use ChangeTo. *)
     
    (* ALSO handle: 
     * Set( ) to a pointer lvalue 
     * (1) when rhs is an AddrOf
     * (2) when rhs is another pointer local
     * (3) when rhs is an expression that retrieves a pointer using any combination
           of derefs, casts, differences (e.g. obj->ps[p-q])
     * (4) assert that there are no other cases!
     *)
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
        (Var(vi), NoOffset) -> (Var(VarinfoMap.find vi !boundsGlobals), NoOffset)
      | _ -> raise Not_found
    in
    match i with
        Set(lv, e, l) ->
            let afterWriteInstrs = 
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
                    makeBoundsWriteInstructions enclosingFile f fetchBoundsInlineFun uniqtypeGlobals lv e lvalToBoundsFun !currentInst
                else []
            in
            let (checkDeriveInstrs, newSetInstr) = 
                if (not (adjustsPointerAtTopLevel e)) then ([], Set(lv, e, l))
                else
                    let instrs, exprTmpVar = makeCheckDeriveInstructions enclosingFile f checkDerivePtrInlineFun inlineAssertFun uniqtypeGlobals e lv lvalToBoundsFun !currentInst
                    in 
                    (instrs, Set(lv, Lval(Var(exprTmpVar), NoOffset), l))
            in
            ChangeTo (
                checkDeriveInstrs @ [newSetInstr] @ afterWriteInstrs
            )
      | Call(olv, e, es, l) -> begin
            (* We might be writing a pointer. 
             * Since, if so, the pointer has come from a function call, we don't know
             * how to get bounds for it. So we always fetch those bounds.
             * HMM. Potentially expensive. But the cache should help.
             *)
            (* Our arg exprs may *not* adjust a pointer -- those have become separate Sets.
             * Any of our expressions might create a pointer using address-of, at any depth.
             * Any of our expressions might *cast* a pointer, at any depth.
             * These are not mutually exclusive!
             *)
            if adjustsPointerAtTopLevel e
                            || (List.fold_left (||) false (List.map adjustsPointerAtTopLevel es))
            then failwith "unexpected pointer adjustment in call!"
            else
                match olv with
                    None -> SkipChildren
                  | Some(lv) -> 
                    let afterWriteInstrs = if 
                            isNonVoidPointerType(Cil.typeOf (Lval(lv))) 
                            && match lv with
                                (Var(vi), NoOffset) -> List.mem vi currentFunctionLocalsToCacheBoundsFor
                                  | _ -> false
                        then
                            (* Queue some instructions to write the bounds. *)
                           makeBoundsUpdateInstructions enclosingFile f fetchBoundsInlineFun uniqtypeGlobals lv lvalToBoundsFun !currentInst
                        else []
                    in
                    ChangeTo ( Call(olv, e, es, l) :: afterWriteInstrs )
        end
      | (* Asm(attrs, instrs, locs, u, v, l) -> *) _ -> SkipChildren
    end
end

(* We snarf the function body as a string, and also return a 
 * CIL-friendly location of where we found it. *)
let getInlineFunctionDefinition name = ("", {line = -1; file = "BLAH FIXME"; byte = 0}) (* FIXME *)

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
