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
 * Vsimpleaddr: Transform a program so that all address-yielding expressions are
 * even more "simple". Introduce temporaries to hold intermediate values 
 * for expressions that would normally involve more than one address computation.
 *
 * Address derivations are:
 * 
 * - pointer arithmetic. In CIL this is 
 * - indexing, such as in a[42] or &a[12] -- in both cases, we perform pointer
 * arithmetic (and in the former we also dereference).
 * 
 * Address derivations are *not*: 
 * 
 * - the address-of operator &: somewhat paradoxically, this does not actually
 * create a new pointer by itself; indexing does that. If I do `&a', then since
 * `a' must already be in scope, it already has a valid address.
 * 
 * Tricky cases:
 * 
 * - taking the address of a zero-length array: this needs to become a trap value.
 * We deal with this in crunchbound; here we just want to simplify the address
 * derivations.
 * 
 * - array-to-pointer decay: again, this might need to become a trap value, but
 * we handle it in crunchbound.
 *)
open Cil
open Feature
open Cilallocs

let expToCilString e                  = (Pretty.sprint 80 (printExp (new plainCilPrinterClass) () e))

(* current context: where should we put our temporaries? *)
let thefunc = ref None 

let theinst = ref None

(* turn "int a[5][5]" into "int ** temp" *)
let rec array_to_pointer tau = 
  match unrollType tau with
    TArray(dest,_,al) -> TPtr(array_to_pointer dest,al)
  | _ -> tau

let rec firstIndex offset = match offset with 
      NoOffset -> None
    | Field(fi, rest) -> firstIndex rest
    | Index(indexExp, rest) -> Some(indexExp)

let offsetIncludesIndex offset = match firstIndex offset with 
      None -> false
      | Some(_) -> true

(* create a temporary variable in the current function *)
let make_temp tau = 
  let tau = array_to_pointer tau in 
  match !thefunc with
    Some(fundec) -> makeTempVar fundec ~name:("addr_") tau
  | None -> failwith "vsimpleaddr: temporary needed outside a function"

(* the transformation is implemented as a Visitor *)
class vsimpleaddrVisitor = object(self)
  inherit nopCilVisitor

  method vfunc fundec = (* we must record the current context *)
    thefunc := Some(fundec) ;
    DoChildren
    
  method vinst inst = (* we must record the current context *)
    theinst := Some(inst) ;
    DoChildren
    
  method vlval outerLv = ChangeDoChildrenPost(outerLv, fun (host, off) -> 
      output_string stderr (("Visiting lvalue: " ^ (lvalToString outerLv)) ^ "\n");
      output_string stderr (("CIL form: " ^ (lvalToCilString outerLv)) ^ "\n");
      match off with
          NoOffset -> (host, off)
        | Field(fi, rest) -> 
              (* By construction, rest does not do any indexing or pointer adjustment.  *)
              (host, off)
        | Index(intExp, rest) ->
            if (isStaticallyZero intExp) then
                (
                    output_string stderr ("Leaving lvalue alone because it does no indexing: " ^ (lvalToString outerLv) ^ "\n");
                    (host, off)
                )
            else (
              output_string stderr ("Hoisting indexing out of lvalue: " ^ (lvalToString outerLv) ^ "\n");
              (* by construction, rest does not do any indexing. *)
              (* Make the adjusted pointer as a temporary, then make us Mem. *)
              let tempType = TPtr(Cil.typeOf (Lval(host, Index(intExp, NoOffset))), [])
              in
              let newTemp = make_temp tempType
              in
              let tempExpr = 
                  let ptrExp = match host with
                  Mem(ptrExp) -> ptrExp
                | Var(varinfo) -> mkAddrOrStartOf (host, NoOffset)
                in
                BinOp(PlusPI, ptrExp, intExp, tempType)
              in 
              self#queueInstr [
                (Set((Var(newTemp), NoOffset), tempExpr, !currentLoc))
              ];
              (* if we started with x[i].rest, we now have &x + i in our temp, and 
               * we need to rewrite the whole lvalue as *(&x + i).rest. *)
              (Mem(Lval(Var(newTemp), NoOffset)), rest)
            )
    )

  method vexpr (outerE: exp) : exp visitAction = ChangeDoChildrenPost(outerE, fun e -> 
      let fixedE = (begin
          output_string stderr (("Visiting expression: " ^ (expToString outerE)) ^ "\n");
          output_string stderr (("CIL form: " ^ (expToCilString outerE)) ^ "\n");
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
                else
                    match !theinst with
                    None -> failwith "expression without instruction"
                  | Some(Set((Var(_), NoOffset), assignedE, _)) when (assignedE == e) -> begin
                        output_string stderr ("Leaving expression alone because it's the top-level rhs in an assignment: " ^ (expToString e) ^ "\n");
                        e
                    end
                  | Some(_) -> begin
                        output_string stderr ("Not top-level, so rewrite to use temporary\n");
                        flush stderr;
                        let new_vi = make_temp (typeOf e) in begin
                            (* queue the assignment to the temporary *)
                            output_string stderr ("New temporary takes expression " ^ (expToString e) ^ "\n");
                            self#queueInstr [
                                (Set((Var(new_vi),NoOffset),e,!currentLoc))
                            ];
                            Lval(Var(new_vi), NoOffset)
                        end
                    end
        end)
        in
        match fixedE with
            AddrOf((Mem(ptrExp), NoOffset)) -> ptrExp
          | _ -> fixedE
      )
end

(* Main entry point: apply the transformation to a file *)
let vsimpleaddr (f : file) =
  try 
    visitCilFileSameGlobals (new vsimpleaddrVisitor) f;
    f
  with e -> Printf.printf "Exception in Vsimpleaddr.vsimpleaddr: %s\n"
    (Printexc.to_string e) ; raise e

let feature = 
  { fd_name = "vsimpleAddr";
    fd_enabled = false;
    fd_description = "simplify all address derivations" ;
    fd_extraopt = [];
    fd_doit = (function (f: file) -> ignore (vsimpleaddr f)) ;
    fd_post_check = true;
  } 

let () = Feature.register feature
