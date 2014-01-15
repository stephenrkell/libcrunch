(* Copyright (c) 2011--13,
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

open Pretty
open Cil
open Map
module NamedTypeMap = Map.Make(String)
module E = Errormsg
module H = Hashtbl

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
    
(* David Park at Stanford points out that you cannot take the address of a
 * bitfield in GCC. *)

(* Returns true if the given lvalue offset ends in a bitfield access. *) 
let rec is_bitfield lo = match lo with
  | NoOffset -> false
  | Field(fi,NoOffset) -> not (fi.fbitfield = None)
  | Field(_,lo) -> is_bitfield lo
  | Index(_,lo) -> is_bitfield lo 

(* CIL doesn't give us a const void * type builtin, so we define one. *)
let voidConstPtrType = TPtr(TVoid([Attr("const", [])]),[])

let rec canonicalizeBaseTypeStr s = 
 (* generated from a table maintained in srk's libcxxgen  *)
if (s = "signed char" or s = "char" or s = "char signed" or  false) then "signed char"
else if (s = "unsigned char" or s = "char unsigned" or  false) then "unsigned char"
else if (s = "short int" or s = "short" or s = "int short" or  false) then "short int"
else if (s = "short unsigned int" or s = "unsigned short" or s = "short unsigned" or s = "unsigned short int" or s = "int unsigned short" or s = "int short unsigned" or s = "unsigned int short" or s = "short int unsigned" or  false) then "short unsigned int"
else if (s = "int" or s = "signed" or s = "signed int" or s = "int signed" or  false) then "int"
else if (s = "unsigned int" or s = "unsigned" or s = "int unsigned" or  false) then "unsigned int"
else if (s = "long int" or s = "long" or s = "int long" or s = "signed long int" or s = "int signed long" or s = "int long signed" or s = "long signed int" or s = "signed int long" or s = "long signed" or s = "signed long" or  false) then "long int"
else if (s = "unsigned long int" or s = "int unsigned long" or s = "int long unsigned" or s = "long unsigned int" or s = "unsigned int long" or s = "long unsigned" or s = "unsigned long" or  false) then "unsigned long int"
else if (s = "long long int" or s = "long long" or s = "long int long" or s = "int long long" or s = "long long signed" or s = "long signed long" or s = "signed long long" or s = "long long int signed" or s = "long long signed int" or s = "long signed long int" or s = "signed long long int" or s = "long int long signed" or s = "long int signed long" or s = "long signed int long" or s = "signed long int long" or s = "int long long signed" or s = "int long signed long" or s = "int signed long long" or s = "signed int long long" or  false) then "long long int"
else if (s = "long long unsigned int" or s = "long long unsigned" or s = "long unsigned long" or s = "unsigned long long" or s = "long long int unsigned" or s = "long unsigned long int" or s = "unsigned long long int" or s = "long int long unsigned" or s = "long int unsigned long" or s = "long unsigned int long" or s = "unsigned long int long" or s = "int long long unsigned" or s = "int long unsigned long" or s = "int unsigned long long" or s = "unsigned int long long" or  false) then "long long unsigned int"
else if (s = "float" or  false) then "float"
else if (s = "double" or  false) then "double"
else if (s = "long double" or s = "double long" or  false) then "long double"
else if (s = "bool" or  false) then "bool"
else if (s = "wchar_t" or  false) then "wchar_t"
  else s

(* HACK: pasted from dumpallocs *)
let rec stringFromSig tsig = (* = Pretty.sprint 80 (d_typsig () (getConcreteType ts)) *)
 match tsig with
   TSArray(tsig, optSz, attrs) -> "impossible"
 | TSPtr(tsig, attrs) -> "^" ^ (stringFromSig tsig)
 | TSComp(isSpecial, name, attrs) -> name
 | TSFun(returnTs, argsTss, isSpecial, attrs) -> "()=>" ^ (stringFromSig returnTs)
 | TSEnum(enumName, attrs) -> enumName
 | TSBase(TVoid(attrs)) -> "void"
 | TSBase(TInt(kind,attrs)) -> canonicalizeBaseTypeStr (trim (Pretty.sprint 80 (d_ikind () kind)))
 | TSBase(TFloat(kind,attrs)) -> canonicalizeBaseTypeStr (trim (Pretty.sprint 80 (d_fkind () kind)))
 | _ -> "impossible" 

(* Return an expression that evaluates to the address of the given lvalue.
 * For most lvalues, this is merely AddrOf(lv). However, for bitfields
 * we do some offset gymnastics. 
 *)
let addr_of_lv (lh,lo) = 
  if is_bitfield lo then begin
    (* we figure out what the address would be without the final bitfield
     * access, and then we add in the offset of the bitfield from the
     * beginning of its enclosing comp *) 
    let rec split_offset_and_bitfield lo = match lo with 
      | NoOffset -> failwith "trumptr: impossible" 
      | Field(fi,NoOffset) -> (NoOffset,fi)
      | Field(e,lo) ->  let a,b = split_offset_and_bitfield lo in 
                        ((Field(e,a)),b)
      | Index(e,lo) ->  let a,b = split_offset_and_bitfield lo in
                        ((Index(e,a)),b)
    in 
    let new_lv_offset, bf = split_offset_and_bitfield lo in
    let new_lv = (lh, new_lv_offset) in 
    let enclosing_type = TComp(bf.fcomp, []) in 
    let bits_offset, bits_width = 
      bitsOffset enclosing_type (Field(bf,NoOffset)) in
    let bytes_offset = bits_offset / 8 in 
    let lvPtr = mkCast ~e:(mkAddrOf (new_lv)) ~newt:(charPtrType) in
    (BinOp(PlusPI, lvPtr, (integer bytes_offset), ulongType))
  end else (AddrOf (lh,lo))

(* This effectively embodies our "default specification" for C code
 * -- it controls what we assert in "__is_a" tests, and
 * needs to mirror what we record for allocation sites in dumpallocs *)
let rec getConcreteType tsig =
 match tsig with
   TSArray(tsig, optSz, attrs) -> getConcreteType tsig
 | TSPtr(tsig, attrs) -> TSPtr(getConcreteType tsig, []) (* stays a pointer, but discard attributes *)
 | TSComp(isSpecial, name, attrs) -> TSComp(isSpecial, name, [])
 | TSFun(returnTs, argsTss, isSpecial, attrs) -> TSFun(returnTs, argsTss, isSpecial, [])
 | TSEnum(enumName, attrs) -> TSEnum(enumName, [])
 | TSBase(TVoid(attrs)) -> TSBase(TVoid([]))
 | TSBase(TInt(kind,attrs)) -> TSBase(TInt(kind, []))
 | TSBase(TFloat(kind,attrs)) -> TSBase(TFloat(kind, []))
 | _ -> tsig

  
  (* CIL "expressions" are defined to be side-effect-free. Side-effecting
     operations are pushed into "instructions". Since we want to insert
     a call to assert(), which is side-effecting, we need to visit instructions.
     For each instruction, we find all the casts in any expressions its
     contains, and do the following
     - declare a temporary
     - introduce a new instruction which computes the expression,
       and assigns the result the  
       
     PROBLEM: pointer expressions that are not side-effecting
     may be nested directly inside a Cil.stmt, rather than being
     in an instr? YES, e.g. return statements.
     
     SO maybe it will suffice to
     - visit expressions, but 
     - use the visitor's "enqueue instructions" support
       ... to enqueue some instructions before the current instruction?
       --- i.e. replace a cast expression with a reference to a temporary...
       ... and insert the definition + assertion *before* the containing instr.
   *)
class trumPtrExprVisitor = fun enclosingFile -> 
                           fun enclosingFunction -> 
                          (* fun checkFun -> *)
                          fun checkFunVar ->
                           fun assertFun -> 
                           fun assertFailFun ->
                               object(self)
  inherit nopCilVisitor
  (* Create a prototype for the check function, but don't put it in the 
   * file *)
(*  val checkFun =   
    let fdec = emptyFunction "__is_a" in
    fdec.svar.vtype <- TFun(intType, 
                            Some [ ("obj", voidPtrType, []);
                                   ("typestr", charConstPtrType, []) ], 
                            false, []);
    fdec
  val assertFailFun = (* should use findOrCreateFunc here *)
    let assertFailFunDec = emptyFunction "__assert_fail" in
    assertFailFunDec.svar.vtype <- TFun(voidType, 
                            Some [ 
                            ("assertion", charConstPtrType, []);
                            ("file", charConstPtrType, []);
                            ("line", uintType, []);
                            ("function", charConstPtrType, [])
                             ], 
                            false, []);
    assertFailFunDec *)

  val currentInst : instr option ref = ref None
  method vinst (i: instr) : instr list visitAction = begin
    currentInst := Some(i);
    DoChildren
  end
  
  (* Remember the named types we've seen, so that we can map them back to
   * source code locations.
   * HACK: for now, don't bother maintaining separate namespaces for enums, structs and unions. *)
  val namedTypesMap : location NamedTypeMap.t ref = ref NamedTypeMap.empty

  method vglob (g: global) : global list visitAction =
    match g with
      GCompTag(ci, l) -> namedTypesMap := (NamedTypeMap.add ci.cname l !namedTypesMap); DoChildren
    | GCompTagDecl(ci, l) -> namedTypesMap := (NamedTypeMap.add ci.cname l !namedTypesMap); DoChildren
    | GEnumTag(ei, l) -> namedTypesMap := (NamedTypeMap.add ei.ename l !namedTypesMap); DoChildren
    | GEnumTagDecl(ei, l) -> namedTypesMap := (NamedTypeMap.add ei.ename l !namedTypesMap); DoChildren
    | _ -> DoChildren
  
  method vexpr (e: exp) : exp visitAction = 
    match e with 
      (* Check casts, unless they only affect qualifiers we don't care about, or are casts to void* *)
      CastE(t, subex) -> if getConcreteType(Cil.typeSig(t)) = getConcreteType(Cil.typeSig(Cil.typeOf(subex))) then DoChildren else 
      if (getConcreteType(Cil.typeSig(t)) = TSPtr(TSBase(TVoid([])), [])
       or getConcreteType(Cil.typeSig(t)) = TSPtr(TSBase(TInt(IChar, [])), [])
       or getConcreteType(Cil.typeSig(t)) = TSPtr(TSBase(TInt(ISChar, [])), [])
       or getConcreteType(Cil.typeSig(t)) = TSPtr(TSBase(TInt(IUChar, [])), [])) then DoChildren else begin
          (* let () = Printf.printf "unequal typesigs %s, %s\n%!" (getConcreteType(Cil.typeSig(t))) (Cil.typeSig(Cil.typeOf(subex))) in  *)
          let location = match !currentInst with
            None -> {line = -1; file = "(unknown)"; byte = 0}
          | Some(i) -> begin match i with
            | Set(lv, e, l) -> l
            | Call(olv, e, es, l) -> l
            | Asm(attrs, instrs, locs, u, v, l) -> l
          end
          in
          match t with 
            TPtr(ptdt, attrs) -> begin
              (* enqueue the tmp var decl, assignment and assertion *)
              let exprTmpVar = Cil.makeTempVar enclosingFunction (typeOf e) in
              let checkTmpVar = Cil.makeTempVar enclosingFunction intType in 
              let concreteType = getConcreteType (typeSig ptdt) in
              let typeStr = stringFromSig concreteType in
            (* The string representation needs tweaking to make it a symname:
               - prepend "__uniqtype_" 
               - insert the defining header file, with the usual munging for slashes, hyphens, dots, and spaces
               - FIXME: fix up base types specially!
               - replace '^' with '__PTR_' 
               - replace '()=>' with '__FUN_'.
               NOTE that arrays probably shouldn't come up here.

               We can't get the declaring file/line ("location" in CIL-speak) from a typesig,
               nor from its type -- we need the global.  *)
             let symnameFromString s ts = begin
                let rec definingFile t = match t with 
                  TSComp(isSpecial, name, attrs) -> begin try
                      let l = NamedTypeMap.find name !namedTypesMap in l.file 
                      with Not_found -> output_string Pervasives.stderr ("missing decl for " ^ name ^ "\n"); "" (* raise Not_found *)
                  end
                | TSEnum(name, attrs) -> begin try
                      let l = NamedTypeMap.find name !namedTypesMap in l.file 
                      with Not_found -> output_string Pervasives.stderr ("missing decl for " ^ name ^ "\n"); "" (* raise Not_found *)
                  end
                | TSPtr(tsig, attrs) -> definingFile tsig
                | _ -> ""
                in
                let defining_filestr = definingFile ts
                in 
                let header_insert = Str.global_replace (Str.regexp "[. /-]") "_" defining_filestr in
                let ptr_replaced = Str.global_replace (Str.regexp "\\^") "__PTR_"  (Str.global_replace (Str.regexp "[. /-]") "_" s) in
                (* HACK: using escaped brackets in the regexp doesn't seem to work for replacing, 
                   so just use two dots. Will need to change this if we start to include function
                   argument types in function typestrings. *)
                let ptr_and_fun_replaced = Str.global_replace (Str.regexp "..=>") "__FUN_" ptr_replaced in
                "__uniqtype_" ^ (if String.length header_insert > 0 then string_of_int(String.length header_insert) else "") ^ header_insert ^ "_" ^ ptr_and_fun_replaced
              end in
              let symname = symnameFromString typeStr concreteType in
              self#queueInstr [
                (* first enqueue an assignment of the whole cast to exprTmpVar *)
                Set( (Var(exprTmpVar), NoOffset), e, locUnknown );
                (* next enqueue the is_a call *)
                Call( Some((Var(checkTmpVar), NoOffset)), (* return value dest *)
                      (Lval(Var(checkFunVar),NoOffset)),  (* lvalue of function to call *)
                      [ 
                        (* first arg is the expression result *)
                        Lval(Var(exprTmpVar), NoOffset);
                        (* second argument is a string computed from ptdt *)
                        Const(CStr(symname))
                      ],
                      locUnknown
                );
                (* then enqueue the assertion about its result *)
                Call( None,
                      (Lval(Var(assertFun.svar),NoOffset)),
                      [
                        (* arg is the check result *)
                        Lval(Var(checkTmpVar), NoOffset);
                        Const(CStr("__is_a(" ^ exprTmpVar.vname ^ ", \"" ^ typeStr ^ "\")"));
                        Const(CStr( location.file ));
                        Const(CInt64(Int64.of_int (if location.line == -1 then 0 else location.line), IUInt, None));
                        Const(CStr( enclosingFunction.svar.vname ))
                      ],
                      locUnknown
                )
              ];
              (* change to a reference to the decl'd tmp var *)
              ChangeTo ( CastE(t, subex) )
            end
          | _ -> DoChildren
        end 
    | _ -> DoChildren
end

class trumPtrFunVisitor = fun fl -> object
  inherit nopCilVisitor
  val mutable assertFun = let assertFunDec = emptyFunction "__inline_assert" in
    assertFunDec
  val checkFunVar = findOrCreateFunc fl "__is_a" (TFun(intType, 
                             Some [ ("obj", voidConstPtrType, []);
                                   ("typestr", charConstPtrType, []) ],
                            false, [Attr("weak", [])]))
    
   (* emptyFunction "__is_a" in
    checkFunDec.svar.vtype <- TFun(intType, 
                            None, (* will be filled later using setFunctionTypeMakeFormals*)
                            false, [Attr("weak", [])]);
    checkFunDec*)
  val assertFailFun = findOrCreateFunc fl "__assert_fail" (TFun(voidType, 
                            Some [ 
                            ("assertion", charConstPtrType, []);
                            ("file", charConstPtrType, []);
                            ("line", uintType, []);
                            ("function", charConstPtrType, [])
                             ], 
                            false, []))
  initializer
    let arg0 = makeFormalVar assertFun "cond" intType in
    let arg1 = makeFormalVar assertFun "assertion" charConstPtrType in
    let arg2 = makeFormalVar assertFun "file" charConstPtrType in
    let arg3 = makeFormalVar assertFun "line" uintType in
    let arg4 = makeFormalVar assertFun "function" charConstPtrType in
    setFunctionType assertFun (TFun(voidType, 
                            Some [ ("cond", intType, []);
                                   ("assertion", charConstPtrType, []);
                                   ("file", charConstPtrType, []);
                                   ("line", uintType, []);
                                   ("function", charConstPtrType, [])
                                    ], 
                            false, [Attr("always_inline", []); Attr("gnu_inline", [])]));
    (* setFunctionTypeMakeFormals checkFun  (TFun(intType, 
                            Some [ ("obj", voidPtrType, []);
                                   ("typestr", charConstPtrType, []) ], 
                            false, [Attr("weak", [])])); *)
    assertFun.sbody <- mkBlock (Formatcil.cStmts 
         "if (!cond) __assert_fail(assertion, file, line, function);"
         (fun n t -> makeTempVar assertFun ~name:n t)
         {line = -1; file = "BLAH FIXME"; byte = 0}
         [ ("cond", Fv arg0 );
           ("assertion", Fv arg1 );
           ("file", Fv arg2 );
           ("line", Fv arg3 );
           ("function", Fv arg4 );
           ("__assert_fail", Fv assertFailFun ) ]);
    assertFun.svar.vinline <- true;
    
    (* Don't make it static -- inline is enough. Making it static
        generates lots of spurious warnings when used from a non-static 
        inline function. *)
    (* Actually, do make it static -- C99 inlines are weird and don't eliminate
       multiple definitions the way we'd like.*)
    (* assertFun.svar.vstorage <- Static; *)
    (* ACTUALLY actually, make it extern, which plus gnu_inline above, 
       should be enough to shut up the warnings and give us a link error if 
       any non-inlined calls creep through. *)
    assertFun.svar.vstorage <- Extern;
    
    (* according to the docs for pushGlobal, non-types go at the end of globals --
     * but if we do this, our function definition appears at the end, which is wrong.
     * So just put it at the front -- seems to work.
     * ARGH. Actually, it needs to go *after* the assertFailFun, on which it depends,
     * to avoid implicit declaration problems. So we split the list at this element, 
     * then build a new list. *)
    let (preList, postList) = 
        let rec buildPre l accumPre = match l with 
            [] -> (accumPre, [])
         |  x::xs -> match x with 
              GFun (assertFailFun, _) -> (accumPre, x :: xs)
           | _ -> buildPre xs (accumPre @ [x])
         in buildPre fl.globals []
    in
    fl.globals <- preList @ [GFun(assertFun, {line = -1; file = "BLAH FIXME"; byte = 0})] @ postList

  method vfunc (f: fundec) : fundec visitAction = 
      let tpExprVisitor = new trumPtrExprVisitor fl f checkFunVar assertFun assertFailFun in
      ChangeTo(visitCilFunction tpExprVisitor f)
end


let feature : featureDescr = 
  { fd_name = "trumptr";
    fd_enabled = ref false;
    fd_description = "generation of code to assert object data types";
    fd_extraopt = [];
    fd_doit = 
    (function (fl: file) -> 
      let tpFunVisitor = new trumPtrFunVisitor fl in
      output_string Pervasives.stderr ("command line args are:\n"
       ^ (String.concat ", " (Array.to_list Sys.argv) ) );
      visitCilFileSameGlobals tpFunVisitor fl);
    fd_post_check = true;
  } 

