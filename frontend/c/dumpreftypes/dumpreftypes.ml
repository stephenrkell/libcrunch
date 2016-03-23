(* Copyright (c) 2016,
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

open Unix
open List
open Str
open Pretty
open Cil
open Feature
open Cilallocs
module E = Errormsg
module H = Hashtbl

let isAlwaysInline vi = 
    List.fold_left (fun acc -> fun attr -> acc || 
        match attr with Attr("always_inline", _) -> true | _ -> false
    ) false vi.vattr

let rec decayArrayInTypesig ts = match ts with
   TSArray(tsig, optSz, attrs) -> decayArrayInTypesig tsig (* recurse for multidim arrays *)
 | _ -> ts

class dumpRefTypesVisitor = fun (fl: Cil.file) -> object(self)
  inherit nopCilVisitor
  
  (* where we will write our deftype/reftype data *)
  val refOutChannel : out_channel option ref = ref None
  val defOutChannel : out_channel option ref = ref None
  
  (* at construction time, open the output file *)
  initializer 
    let reftypeFileName = fl.fileName ^ ".reftypes" in
    (refOutChannel := try begin
         let chan = open_out reftypeFileName in
         Some(chan)
      end 
      with _ ->
        raise (Arg.Bad ("Cannot open file " ^ reftypeFileName))
    );
    (* DO we really need deftypes? it duplicates debugging info, 
     * but might be a nice summary all the same. *)
    let deftypeFileName = fl.fileName ^ ".deftypes" in
    (defOutChannel := try begin
         let chan = open_out deftypeFileName in
         Some(chan)
      end 
      with _ ->
        raise (Arg.Bad ("Cannot open file " ^ reftypeFileName))
    )

  method vvdec (v: varinfo) : varinfo visitAction = 
      (* could be because of a GVar, GVarDecl, GFunc, function type varinfo, 
       * function def formal or function def local. *)
      match (!refOutChannel, !defOutChannel) with
       (Some (refChan), Some (defChan)) ->
          if ( (* v.vused *) v.vglob && not (isAlwaysInline v) && v.vstorage != Static) then
              
              (* TODO 1: for reference types, only include things that are "vreferenced"
                 after running Rmtmps.removeUnusedTemps
               *) 
              let chan = if v.vstorage = Extern then refChan else defChan
              in 
              (output_string chan (v.vname ^ "\t" ^ (barenameFromSig (Cil.typeSig (v.vtype))) ^ "\n"))
              (* Print a line consisting of its linkage name followed by its uniqtype name *)
              ; flush chan; SkipChildren
          else SkipChildren
       | _ -> failwith "could not open output channel(s)"
     
end (* class dumpAllocsVisitor *)

let feature : Feature.t = 
  { fd_name = "dumpreftypes";
    fd_enabled = false;
    fd_description = "print information about referenced globals' types";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) -> 
      let drtVisitor = new dumpRefTypesVisitor f in
      (* Cfg.computeFileCFG f;
      computeAEs f; *)
      visitCilFileSameGlobals drtVisitor f);
    fd_post_check = true;
  } 

let () = Feature.register feature
