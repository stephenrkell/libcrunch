(* Copyright (c) 2011,
 *  Stephen Kell        <stephen.kell@cs.ox.ac.uk>
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

open List
open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

class dumpAllocsVisitor = object
  inherit nopCilVisitor

  method vinst (i: instr) : instr list visitAction = 
    match i with 
      Call(Some lv, f, args, l) -> begin
        (* Check if we need to output *)
        match f with 
          Lval(Var(v), NoOffset) when v.vglob -> 
              match v.vtype with
                  TFun(returnType, optParamList, isVarArgs, attrs) ->
                      if mem v.vname ["malloc"; "calloc"; "realloc"] then
                          (* print something out *)
                          let msg = Pretty.sprint 80 
                              (Pretty.dprintf "Found an allocation function %a\n" d_lval (Var(v), NoOffset)) in
                          prerr_string msg;
                          (* Here we need to identify the size argument and
                             then do either some hacky pattern matching
                             or a recursive function on the expr structure:
                             Sizeof T lets us terminate
                             Sizeof V also lets us terminate
                             Mul where an arg is a Sizeof lets us terminate *)
                          SkipChildren
                      else SkipChildren
                | _ -> SkipChildren
             
        | _ -> SkipChildren
      end 
    | _ -> SkipChildren

end

let feature : featureDescr = 
  { fd_name = "dumpallocs";
    fd_enabled = ref false;
    fd_description = "print information about allocation sites";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) -> 
      let daVisitor = new dumpAllocsVisitor in
      Cfg.computeFileCFG f;
      computeAEs f;
      visitCilFileSameGlobals daVisitor f);
    fd_post_check = true;
  } 

