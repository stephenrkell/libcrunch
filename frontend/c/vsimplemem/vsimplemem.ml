(* Copyright (c) 2014--15,
 *  Stephen Kell        <stephen.kell@cl.cam.ac.uk>
 *
 * and based on simplemem.ml, which is 
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

(*
 * Vsimplemem: Transform a program so that all memory expressions are
 * even more "simple". Introduce well-typed temporaries to hold intermediate 
 * valuesfor expressions that would normally involve more than one memory
 * reference, or any reference at a nonzero offset. 
 *
 * If vsimplemem succeeds, each lvalue should contain only one Mem()
 * constructor, and have NoOffset rhs. 
 *)
open Cil
open Feature
open Cilallocs

(* current context: where should we put our temporaries? *)
let thefunc = ref None 

(* build up a list of assignments to temporary variables *)
let assignment_list = ref []

(* turn type "int [5][5]" into "int **" *)
let rec array_to_pointer tau = 
  match unrollType tau with
    TArray(dest,_,al) -> TPtr(array_to_pointer dest,al)
  | _ -> tau

(* create a temporary variable in the current function *)
let make_temp tau = 
  let tau = array_to_pointer tau in 
  match !thefunc with
    Some(fundec) -> makeTempVar fundec ~name:("mem_") tau
  | None -> failwith "vsimplemem: temporary needed outside a function"

(* Separate loffsets into "scalar addition parts" and "memory parts".
   What does this mean? Indexing is a memory part, and field selection
   is a scalar addition. Why does it make sense to separate the two? 
   Because if we have a run of field parts, say o.f.g.h.i[2], 
   we can leave the "f.g.h" as-is and skip straight to the i[2] part.
   Our function handle_loffset is only called on memory parts. *)
let rec separate_loffsets lo = 
  match lo with
    NoOffset -> (NoOffset, NoOffset)
  | Field(fi,rest) -> 
      let (scalarParts, memoryParts) = separate_loffsets rest in
      (Field(fi, scalarParts), memoryParts)
  | Index(_) -> (NoOffset, lo)

(* Recursively decompose the lvalue so that what is under a "Mem()"
 * constructor is put into a temporary variable. *)
let rec handle_lvalue (lh,lo) = 
  let scalarParts, memoryParts = separate_loffsets lo in 
  match lh with
    Var(vi) -> 
      handle_loffset (lh, scalarParts) memoryParts
  | Mem(Lval(Var(_),NoOffset)) ->
      (* special case to avoid generating "tmp = ptr;" *)
      handle_loffset (lh, scalarParts) memoryParts 
  | Mem(e) -> 
      begin
        let new_vi = make_temp (typeOf e) in
        assignment_list := (Set((Var(new_vi),NoOffset),e,!currentLoc)) 
          :: !assignment_list ;
        handle_loffset (Mem(Lval(Var(new_vi),NoOffset)),NoOffset) lo
      end
and handle_loffset lv lo = 
  (* lv's offset part is always scalar-only or NoOffset; lo 
   * can be anything (after a Mem). *)
  let lhost, _ = lv
  in
  output_string stderr ("Handling offset on lvalue " ^ (lvalToString lv) ^ "\n");
  output_string stderr ("Offset is (ignore host)" ^ (lvalToString (lhost, lo)) ^ "\n");
  match lo with
    NoOffset -> lv
    (* What's the Field case doing? It's rebuilding the 
       Field part of the lvalue, then recursing on the next offset. 
       Why? Not sure. *)
  | Field(f,o) -> handle_loffset (addOffsetLval (Field(f,NoOffset)) lv) o
  | Index(exp,o) -> 
        (* was: handle_loffset (addOffsetLval (Index(exp,NoOffset)) lv) o *)
        (* Make a temporary that has the address of lhost[exp], and recurse on o *)
        (* FIXME: want a similar special case to the above, to avoid generating & * blah *)
        (* intermediate lvalue, zero-indexed *)
        let tempHost, tempOff = addOffsetLval (Index(exp, NoOffset)) lv
        in
        output_string stderr ("Temp lvalue is " ^ (lvalToString (tempHost, tempOff)) ^ "\n");
        let derefType = typeOf (Lval(tempHost, tempOff))
        in
        let tempType = TPtr(
            (match o with Index(_) -> TArray(derefType, None, []) | _ -> derefType)
            , [])
        in
        let tempPtr = make_temp tempType 
        in
        let newAddrOf = mkAddrOf (tempHost, tempOff)
        in 
        output_string stderr ("Made temporary of type " ^ (typToString tempType) 
        ^ " to hold " ^ (expToString newAddrOf) ^ "\n");
        let newAssignment = Set((Var(tempPtr),NoOffset), newAddrOf ,!currentLoc)
        in
        assignment_list := newAssignment :: !assignment_list ;
        (* problem: if `'o' is Index, because the entire lvalue was an array 
         * before -- say q[1][2], then it's now a pointer,
         * say ( *p)[2] where p == &q[1]. 
         * CIL doesn't let us Index *p. Or does it?
         * temphost, tempoff is the expression q[1].
         * tempPtr gets the value &q[1]. 
         * We should make sure this has pointer-to-array type if o is Index(_).
         * It should do! unless mkAddrOf does array-to-pointer decay.
         *)
        let tempLval : lval = (Var(tempPtr), NoOffset)
        in
        let derefLval : lval = (Mem(Lval(tempLval)), NoOffset)
        in
        output_string stderr ("Replaced lvalue portion " ^ (lvalToString (lhost, Index(exp, NoOffset)))
        ^ " with " ^ (lvalToString derefLval) ^ "\n");
        handle_loffset derefLval o

(* the transformation is implemented as a Visitor *)
class vsimpleVisitor = object 
  inherit nopCilVisitor

  method vfunc fundec = (* we must record the current context *)
    thefunc := Some(fundec) ;
    DoChildren

  (* The point of ChangeToChildrenPost is to visit children in bottom-up order, 
   * rather than top down. In an expression tree containing lvalues at various
   * levels, the lower-down ones will have been rewritten before we see them.
   * This means their temporaries will be earlier in the assignment list, so
   * will be available for use. Moreover, in the lvalue we see, the subexpressions 
   * which generated those temporaries have been replaced by references to the 
   * temporaries themselves. So there is "no interesting deeper structure",
   * by construction. *)
  method vlval lv = ChangeDoChildrenPost(lv,
      (fun lv -> handle_lvalue lv))

  method vexpr outerE = ChangeDoChildrenPost(outerE, 
    fun e -> 
        (* simplify expressions consisting of mkaddrof a Mem lvalue *)
        match e with
            AddrOf((Mem(ptrExp), NoOffset)) -> ptrExp
          | _ -> e
    )

  method unqueueInstr () = 
      let result = List.rev !assignment_list in
      assignment_list := [] ;
      result 
end

(* Main entry point: apply the transformation to a file *)
let vsimplemem (f : file) =
  try 
    visitCilFileSameGlobals (new vsimpleVisitor) f;
    f
  with e -> Printf.printf "Exception in Vsimplemem.vsimplemem: %s\n"
    (Printexc.to_string e) ; raise e

let feature = 
  { fd_name = "vsimpleMem";
    fd_enabled = false;
    fd_description = "simplify all memory expressions" ;
    fd_extraopt = [];
    fd_doit = (function (f: file) -> ignore (vsimplemem f)) ;
    fd_post_check = true;
  } 

let () = Feature.register feature
