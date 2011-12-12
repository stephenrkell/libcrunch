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

open Unix
open List
open Str
open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

(* HACKed realpath for now: *)
let abspath f =
   if String.get f 0 = '/' then f else (getcwd ()) ^ "/" ^ f

(* Return the typesig of the type that e is calculating (a multiple of) the size of,
   if any. 
   We understand multiplications. 
   We also want to detect buffer/string-size calculations, which do not use sizeof. 
*)
let rec sizeExprHasNoSizeof (e: exp) =
  match e with 
 | Const(c) -> true
 | Lval((Var(v),o)) -> true
 | Lval((Mem(ex),o)) -> sizeExprHasNoSizeof ex
 | SizeOf(t) -> false
 | SizeOfE(ex) -> false
 | SizeOfStr(s) -> false
 | AlignOf(t) -> true
 | AlignOfE(t) -> true
 | UnOp(u, e1, t) -> sizeExprHasNoSizeof e1
 | BinOp(b, e1, e2, t) -> (sizeExprHasNoSizeof e1) && (sizeExprHasNoSizeof e2)
 | CastE(t, ex) -> sizeExprHasNoSizeof ex
 | AddrOf((Var(v),o)) -> true
 | AddrOf((Mem(ex),o)) -> sizeExprHasNoSizeof ex
 | StartOf((Var(v),o)) -> true
 | StartOf((Mem(ex),o)) -> sizeExprHasNoSizeof ex

(* FIXME: split this into a "toplevel" that does the HasNoSizeof check,
   and a recursive part which recurses *without* recursively doing the
   HasNoSizeof check. *)
let rec getSizeExpr (e: exp) =
  if sizeExprHasNoSizeof e then Some(typeSig charType)
  else begin
  match e with
   |  BinOp(Mult, e1, e2, t) -> begin
         match (getSizeExpr e1) with
           | Some(s1) -> Some(s1)
           | None -> begin match (getSizeExpr e2) with
              | Some(s2) -> Some(s2)
              | None -> None
              end
         end
   |  SizeOf(t) -> Some(typeSig t)
   |  SizeOfE(e) -> Some(typeSig (typeOf e))
   |  SizeOfStr(s) -> Some(typeSig charType)
   | _ -> None
  end

(*   |  SizeOf(t) -> Some(Pretty.sprint 80 (d_typsig () (typeSig t)))
   |  SizeOfE(e) -> Some(Pretty.sprint 80 (d_typsig () (typeSig (typeOf e))))
   |  SizeOfStr(s) -> Some(Pretty.sprint 80 (d_typsig () (typeSig charType))) *)

let rec try_match vname pattern =
    try if (search_forward (regexp pattern) (vname) 0) >= 0
        then true
        else false
    with Not_found -> false
   
(* Work out whether this call is an allocation call. If it is,
   return Some(fn, optionalTypeSig)
   where fn is the function varinfo
   and optionalTypeSig is the type signature we inferred was being allocated, if we managed it *)
let rec getAllocExpr (i: instr) (f: varinfo) (arglist: exp list) =
  match f.vname with
    | "malloc" -> Some (f.vname, getSizeExpr (nth arglist 0))
    | "calloc" -> Some (f.vname, getSizeExpr (nth arglist 1))
    | "realloc" -> Some (f.vname, getSizeExpr (nth arglist 1))
    | _ -> if (length arglist > 0) then begin
         let guessedSizeArg = 
          if try_match f.vname "calloc" then Some(nth arglist 1)
          else if try_match f.vname "realloc" then Some(nth arglist  1)
          else if try_match f.vname "[aA][lL][lL][oO][cC]" then Some(nth arglist 0)
          else None
        in match guessedSizeArg with
             Some(a) -> Some(f.vname, getSizeExpr a)
           | None -> None
      end 
      else None
      
(* HACK: copied from trumptr *)
(* This effectively embodies our "default specification" for C code
 * -- it controls what we assert in "__is_a" tests, and
 * needs to mirror what we record for allocation sites in dumpallocs *)
let rec getEffectiveType tsig =
 match tsig with
   TSArray(tsig, optSz, attrs) -> getEffectiveType tsig
 | TSPtr(tsig, attrs) -> TSPtr(getEffectiveType tsig, []) (* stays a pointer, but discard attributes *)
 | TSComp(isSpecial, name, attrs) -> TSComp(isSpecial, name, [])
 | TSFun(returnTs, argsTss, isSpecial, attrs) -> TSFun(returnTs, argsTss, isSpecial, [])
 | TSEnum(enumName, attrs) -> TSEnum(enumName, [])
 | TSBase(TVoid(attrs)) -> TSBase(TVoid([]))
 | TSBase(TInt(kind,attrs)) -> TSBase(TInt(kind, []))
 | TSBase(TFloat(kind,attrs)) -> TSBase(TFloat(kind, []))
 | _ -> tsig
 
(* stolen from StackOverflow:  http://stackoverflow.com/questions/1584758/
   -- eventually want to change to use Ocaml Batteries Included *)
let trim str =   if str = "" then "" else   let search_pos init p next =
    let rec search i =
      if p i then raise(Failure "empty") else
      match str.[i] with
      | ' ' | '\n' | '\r' | '\t' -> search (next i)
      | _ -> i
    in
    search init   in   let len = String.length str in   try
    let left = search_pos 0 (fun i -> i >= len) (succ)
    and right = search_pos (len - 1) (fun i -> i < 0) (pred)
    in
    String.sub str left (right - left + 1)   with   | Failure "empty" -> "" ;;

let rec stringFromSig tsig = (* = Pretty.sprint 80 (d_typsig () (getEffectiveType ts)) *)
 match tsig with
   TSArray(tsig, optSz, attrs) -> "impossible"
 | TSPtr(tsig, attrs) -> "^" ^ (stringFromSig tsig)
 | TSComp(isSpecial, name, attrs) -> name
 | TSFun(returnTs, argsTss, isSpecial, attrs) -> "()=>" ^ (stringFromSig tsig)
 | TSEnum(enumName, attrs) -> enumName
 | TSBase(TVoid(attrs)) -> "void"
 | TSBase(TInt(kind,attrs)) -> trim (Pretty.sprint 80 (d_ikind () kind))
 | TSBase(TFloat(kind,attrs)) -> trim (Pretty.sprint 80 (d_fkind () kind))
 | _ -> "impossible"

(* I so do not understand Pretty.dprintf *)
let printAllocFn fileAndLine chan funvar allocExpr =
   output_string chan fileAndLine;
   let msg = Pretty.sprint 80 
       (Pretty.dprintf  "\t%a\t"
            d_lval (Var(funvar), NoOffset)) 
   in
   output_string chan (msg ^ allocExpr ^ "\n")
              
class dumpAllocsVisitor = fun (fl: Cil.file) -> object(self)
  inherit nopCilVisitor
  
  (* where we will write our alloc data *)
  val outChannel : out_channel option ref = ref None
  
  (* at construction time, open the output file *)
  initializer 
    let allocFileName = fl.fileName ^ ".allocs" in
    outChannel := try begin
         let chan = open_out allocFileName in
         (* output_string chan ("run initializer, opened " ^ allocFileName); *)
         Some(chan)
      end 
      with _ ->
        raise (Arg.Bad ("Cannot open file " ^ allocFileName))

  method vinst (i: instr) : instr list visitAction = 
    match i with 
      Call(Some lv, f, args, l) -> begin
        (* Check if we need to output *)
        match f with 
          Lval(Var(v), NoOffset) when v.vglob -> begin
              match v.vtype with
               | TFun(returnType, optParamList, isVarArgs, attrs) -> 
                   (* Where to write our output? We want the .allocs to be output 
                      right alongside the .c file (say) that does the allocation.
                      PROBLEM 1: this varies, because we're reading a .i file, i.e.
                      preprocessed temporary, that is NOT NECESSARILY IN THE SAME DIRECTORY
                      the C file.
                      PROBLEM 2: allocs might be in a header file, in which case we
                      won't have permission to write the output.
                      Our solution to both is a non-solution. We create the .allocs file
                      wherever the .i is .
                      This makes sense because our allocs data is a per translation unit thing
                      (e.g. could conceivably be different for two different compilations
                      both including the same header file, so can't write a single ".allocs"
                      for that header).
                    *)
                   let chan = match !outChannel with
                    | Some(s) -> s
                    | None    -> Pervasives.stderr
                   in
                   let fileAndLine = (abspath l.file) ^ "\t" ^ (string_of_int l.line) 
                   in
                   begin
                      (* Here we need to identify the size argument and
                         then do either some hacky pattern matching
                         or a recursive function on the expr structure:
                         Sizeof T lets us terminate
                         Sizeof V also lets us terminate
                         Mul where an arg is a Sizeof lets us terminate *)
                     match (getAllocExpr i v args) with
                        Some(fn1, Some(ts)) -> printAllocFn fileAndLine chan v (stringFromSig ts); SkipChildren
                     |  Some(fn2, None) -> printAllocFn fileAndLine chan v "(unknown)"; SkipChildren
                     |  _ -> SkipChildren (* this means it's not an allocation function *)
                   end
                | _ -> SkipChildren
             end
        | _ -> SkipChildren
      end 
    | _ -> SkipChildren

end (* class dumpAllocsVisitor *)

let feature : featureDescr = 
  { fd_name = "dumpallocs";
    fd_enabled = ref false;
    fd_description = "print information about allocation sites";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) -> 
      let daVisitor = new dumpAllocsVisitor f in
      (* Cfg.computeFileCFG f;
      computeAEs f; *)
      visitCilFileSameGlobals daVisitor f);
    fd_post_check = true;
  } 

