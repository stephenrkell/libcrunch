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

class ptrIntArithVisitor = fun enclosingFile -> 
                               object(self)
  inherit nopCilVisitor

  method vexpr (outerE: exp) : exp visitAction = 
      (* Consider a nest of (PlusPI|MinusPI) expressions.
       * It must be a linear nest, not a tree, because only the left subexpression is a pointer.
       * The innermost expression is a pointer expression that is *not* 
       *    computed by PlusPI/MinusPI. Call this the input pointer expression (IPE).
       * We can rewrite such a nest as a single PlusPI operation,
       * where all other nodes have been rewritten to do arithmetic in the integer domain.
       * For example,
       
               PlusPI(PlusPI(ipe, 4), 4)
        
       * becomes
       
               PlusPI(ipe, PlusII(4, 4)).
       
       * That's all there is to it! We just do it recursively, using ChangeDoChildrenPost.
       * One trick is that on the way down we rewrite all MinusPIs into PlusPIs.
       * And ditto for IndexPI. RECALL that IndexPI is semantically equivalent to PlusPI.
       *)
       ChangeDoChildrenPost(
        begin
            match outerE with
                BinOp(MinusPI, subEP, subEI, t) -> 
                    BinOp(PlusPI, subEP, UnOp(Neg, subEI, Cil.typeOf subEI), t)
              | BinOp(IndexPI, subEP, subEI, t) -> 
                    BinOp(PlusPI, subEP, subEI, t)
              | _ -> outerE
        end, fun e ->
           match e with
               BinOp(PlusPI, BinOp(PlusPI, subEP, subEI1, ptrTInner), subEI2, ptrTOuter) ->
                   (* We can rewrite this into a single PlusPI *)
                   BinOp(PlusPI, subEP, BinOp(PlusA, subEI1, subEI2, Cil.typeOf subEI1), ptrTOuter)
          | _ -> e
        )
end

let feature : Feature.t = 
  { fd_name = "ptrintarith";
    fd_enabled = false;
    fd_description = "do pointer arithmetic in the integer domain as far as possible";
    fd_extraopt = [];
    fd_doit = 
    (function (fl: file) -> 
      debug_print 1 ("command line args are:\n"
       ^ (String.concat ", " (Array.to_list Sys.argv) ) );
      let piaFunVisitor = new ptrIntArithVisitor fl in
      visitCilFileSameGlobals piaFunVisitor fl
      );
    fd_post_check = true;
  } 

let () = Feature.register feature
