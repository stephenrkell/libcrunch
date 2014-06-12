(* Copyright (c) 2011--14,
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
open Str
module NamedTypeMap = Map.Make(String)

(* Module-ify Cil.tysSig *)
module CilTypeSig = struct
   type t = Cil.typsig
   let compare ts1 ts2 = String.compare (Pretty.sprint 80 (d_typsig () ts1)) (Pretty.sprint 80 (d_typsig () ts2))
end

module UniqtypeMap = Map.Make(CilTypeSig)
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
 (* 'generated' from a table maintained in srk's libcxxgen  *)
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
 

            (* The string representation needs tweaking to make it a symname:
               - prepend "__uniqtype_" 
               - insert the defining header file, with the usual munging for slashes, hyphens, dots, and spaces
               - FIXME: fix up base types specially!
               - replace '^' with '__PTR_' 
               - replace '()=>' with '__FUN_'.
               NOTE that arrays probably shouldn't come up here.

               We can't get the declaring file/line ("location" in CIL-speak) from a typesig,
               nor from its type -- we need the global.  *)

(* WORKAROUND for CIL's anonymous structure types: 
   we undo the numbering (set to 1) and hope for the best. *)
let hackTypeName s = (*if (string_match (regexp "__anon\\(struct\\|union\\|enum\\)_.*_[0-9]+$") s 0)
   then Str.global_replace (Str.regexp "_[0-9]+$") "_1" s
   else*) s

let rec barenameFromSig ts = 
 let rec labelledArgTs ts startAt =
   match ts with
     [] -> ""
  | t :: morets -> 
      let remainder = (labelledArgTs morets (startAt + 1))
      in
      "__ARG" ^ (string_of_int startAt) ^ "_" ^ (barenameFromSig t) ^ remainder
 in
 let baseTypeStr ts = 
   let rawString = match ts with 
     TInt(kind,attrs) -> (Pretty.sprint 80 (d_ikind () kind))
   | TFloat(kind,attrs) -> (Pretty.sprint 80 (d_fkind () kind))
   | TBuiltin_va_list(attrs) -> "__builtin_va_list"
   | _ -> raise(Failure ("bad base type: " ^ (Pretty.sprint 80 (Pretty.dprintf "%a" d_type ts))))
   in 
   Str.global_replace (Str.regexp "[. /-]") "_" (canonicalizeBaseTypeStr (trim rawString))
 in
 match ts with
   TSArray(tNestedSig, optSz, attrs) -> "__ARR" ^ (match optSz with Some(s) -> (string_of_int (i64_to_int s)) | None -> "0") ^ "_" ^ (barenameFromSig tNestedSig)
 | TSPtr(tNestedSig, attrs) -> "__PTR_" ^ (barenameFromSig tNestedSig)
 | TSComp(isSpecial, name, attrs) -> (hackTypeName name)
 | TSFun(returnTs, argsTss, isSpecial, attrs) -> 
      "__FUN_FROM_" ^ (labelledArgTs argsTss 0) ^ (if isSpecial then "__VA_" else "") ^ "__FUN_TO_" ^ (barenameFromSig returnTs) 
 | TSEnum(enumName, attrs) -> enumName
 | TSBase(TVoid(attrs)) -> "void"
 | TSBase(tbase) -> baseTypeStr tbase

let userTypeNameToBareName s = Str.global_replace (Str.regexp "[. /-]") "_" (canonicalizeBaseTypeStr (trim s))

let symnameFromSig ts = "__uniqtype_" ^ "" ^ "_" ^ (barenameFromSig ts)

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
let rec getConcreteType ts =
 match ts with
   TSArray(tsig, optSz, attrs) -> getConcreteType tsig
 | TSPtr(tsig, attrs) -> TSPtr(getConcreteType tsig, []) (* stays a pointer, but discard attributes *)
 | TSComp(isSpecial, name, attrs) -> TSComp(isSpecial, name, [])
 | TSFun(returnTs, argsTss, isSpecial, attrs) -> TSFun(returnTs, argsTss, isSpecial, [])
 | TSEnum(enumName, attrs) -> TSEnum(enumName, [])
 | TSBase(TVoid(attrs)) -> TSBase(TVoid([]))
 | TSBase(TInt(kind,attrs)) -> TSBase(TInt(kind, []))
 | TSBase(TFloat(kind,attrs)) -> TSBase(TFloat(kind, []))
 | _ -> ts

let findCompDefinitionInFile isStruct name wholeFile = 
    let rec findCompGlobal iss n globals = 
        match globals with
            []       -> None
        |   g :: gs  -> begin match g with
                GCompTag(ci, _) -> if ci.cstruct = isStruct && ci.cname = name then Some(g) else findCompGlobal iss n gs
              | _ -> findCompGlobal iss n gs
            end
    in
    findCompGlobal isStruct name wholeFile.globals

(* How we deal with incompletes. 
 * 
 * incompletes themselves: by default, don't check anything (treat them like void)
 *     because the name of an incomplete type is arbitrary
 *     BUT, at the user's request, allow __named_a check (TODO).
 *
 * types built out of incompletes (__PTR_incomplete, __FUN_FROM_ ptr-to-incomplete, etc.)
 *
 *     we want to check only the "shape". This is a tricky relation to 
 *     uncover in our uniqtypes. We basically need a function that can 
 *     do a deep "like a" seeing through functions, pointers and arrays,
 *     where our test type substitutes void (or empty) for the incomplete.
 *     It feels like a lot of faff for little reward, sadly. *)

let rec tsIsUndefinedType ts wholeFile = 
    let rec anyTsIsUndefined tss = match tss with
        []          -> false
      | ts1 :: more -> (tsIsUndefinedType ts1 wholeFile) || (anyTsIsUndefined more)
    in
    match ts with
        TSArray(tsig, optSz, attrs)                 -> tsIsUndefinedType tsig wholeFile
    |   TSPtr(tsig, attrs)                          -> tsIsUndefinedType tsig wholeFile
    |   TSComp(isStruct, name, attrs)               -> (findCompDefinitionInFile isStruct name wholeFile) = None
    |   TSFun(returnTs, argsTss, isVarargs, attrs)  -> tsIsUndefinedType returnTs wholeFile || anyTsIsUndefined argsTss
    |   _                                           -> false

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
 
 let matchIgnoringLocation g1 g2 = match g1 with 
    GType(ti, loc) ->        begin match g2 with GType(ti2, _)        -> ti = ti2 | _ -> false end
  | GCompTag(ci, loc) ->     begin match g2 with GCompTag(ci2, _)     -> ci = ci2 | _ -> false end
  | GCompTagDecl(ci, loc) -> begin match g2 with GCompTagDecl(ci2, _) -> ci = ci2 | _ -> false end
  | GEnumTag(ei, loc) ->     begin match g2 with GEnumTag(ei2, _)     -> ei = ei2 | _ -> false end
  | GEnumTagDecl(ei, loc) -> begin match g2 with GEnumTagDecl(ei2, _) -> ei = ei2 | _ -> false end
  | GVarDecl(vi, loc) ->     begin match g2 with GVarDecl(vi2, loc)   -> vi = vi2 | _ -> false end
  | GVar(vi, ii, loc) ->     begin match g2 with GVar(vi2, ii2, loc)  -> ((vi = vi2) (* and (ii = ii2) *)) | _ -> false end
  | GFun(f, loc) ->          begin match g2 with GFun(f2, loc)        -> f  = f2  | _ -> false end
  | GAsm(s, loc) ->          begin match g2 with GAsm(s2, loc)        -> s  = s2  | _ -> false end
  | GPragma(a, loc) ->       begin match g2 with GPragma(a2, loc)     -> a  = a2  | _ -> false end
  | GText(s) ->              begin match g2 with GText(s2)            -> s  = s2  | _ -> false end

let isFunction g = match g with
  GFun(_, _) -> true
| _ -> false

let newGlobalsList globals toAdd insertBeforePred = 
  let (preList, postList) = 
      let rec buildPre l accumPre = match l with 
          [] -> (accumPre, [])
       |  x::xs -> if (insertBeforePred x) then (accumPre, x :: xs) else buildPre xs (accumPre @ [x])
      in 
      buildPre globals []
  in
  preList @ toAdd @ postList

let getOrCreateUniqtypeGlobal m typename concreteType globals = 
  try 
      let found = UniqtypeMap.find concreteType m
      in
      let foundVar = match found with 
        GVarDecl(v, i) -> v
      | _ -> raise(Failure "unexpected state")
      in 
      (m, foundVar, globals)
  with Not_found -> 
     let newGlobal = 
       let tempGlobal = makeGlobalVar typename (TInt(IInt, [])); 
       in 
       tempGlobal.vstorage <- Extern;
       tempGlobal.vattr <- [Attr("weak", [])];
       tempGlobal
     in
     let newGlobalVarInfo = GVarDecl(newGlobal, {line = -1; file = "BLAH FIXME"; byte = 0})
     in 
     let newMap = (UniqtypeMap.add concreteType newGlobalVarInfo m)
     in 
     let newGlobals = newGlobalsList globals [newGlobalVarInfo] isFunction
     in
     (newMap, newGlobal, newGlobals)


class trumPtrExprVisitor = fun enclosingFile -> 
                           fun enclosingFunction -> 
                           fun isAInternalFunDec ->
                           fun checkArgsInternalFunDec -> 
                           fun isASInlineFun ->
                           fun isAUInlineFun ->
                           fun likeAUInlineFun ->
                           fun namedAUInlineFun ->
                           fun inlineAssertFun -> 
                               object(self)
  inherit nopCilVisitor

  val likeAStr =  try begin
    (Sys.getenv "LIBCRUNCH_USE_LIKE_A_FOR_TYPES")
  end with Not_found -> ""

  val sloppyFps = try begin
    let envstr = (Sys.getenv "LIBCRUNCH_SLOPPY_FUNCTION_POINTERS")
    in (String.length envstr) > 0
  end with Not_found -> false
  
  val likeATypeNames = try begin
    Str.split (regexp "[ \t]+") (Sys.getenv "LIBCRUNCH_USE_LIKE_A_FOR_TYPES")
  end with Not_found -> []

  val currentInst : instr option ref = ref None
  method vinst (i: instr) : instr list visitAction = begin
    currentInst := Some(i);
    (* We handle indirect calls here. *)
    if sloppyFps then begin
        match i with 
          Call(_, funExpr, args, location) -> begin
            let getFunctionPointerTypeSig fexp : Cil.typsig option = 
               match fexp with 
                 Lval(Var(v), NoOffset) -> None (* not a function pointer *)
             |   _ -> Some(typeSig (typeOf fexp))
            in
            let maybeFpts = getFunctionPointerTypeSig funExpr
            in
            match maybeFpts with
            |   Some(fpts) -> begin
                    (* For each pointer argument, we insert a check that the argument satisfies __is_a (or __like_a)
                     * with the argument that the *called* function expects to receive. How do we get at the called
                     * function's uniqtype? *)
                    let checkTmpVar = Cil.makeTempVar enclosingFunction intType in 
                    let makeCheckArgArg expr = 
                        (* If the arg type is an integer or a pointer or enum, we just pass the arg
                         * cast to a long. Otherwise it's a pass-by-value struct or union and we
                         * pass zero. *)
                        match (Cil.typeSig (Cil.typeOf expr)) with
                        | TSBase(_) -> CastE(TInt(ILong, []), expr)
                        | TSPtr(_, _) -> CastE(TInt(ILong, []), expr)
                        | TSEnum(_, _) -> CastE(TInt(ILong, []), expr)
                        | _ -> Const(CInt64((Int64.of_int 0), IInt, None))
                    in 
                    self#queueInstr [
                      (* enqueue the checkargs call *)
                      Call( Some((Var(checkTmpVar), NoOffset)), (* return value dest *)
                            (Lval(Var(checkArgsInternalFunDec),NoOffset)),  (* lvalue of function to call *)
                            [ 
                              (* first arg is the function pointer *)
                              funExpr; 
                              (* second argument is the number of args *we're* passing *)
                              Const(CInt64(Int64.of_int(List.length args), IInt, None))
                            ] @ (List.map makeCheckArgArg args) (* remaining args are the same as we're passing *),
                            locUnknown
                      );
                      (* then enqueue the assertion about its result *)
                      Call( None,
                            (Lval(Var(inlineAssertFun.svar),NoOffset)),
                            [
                              (* arg is the expression: check result == 0 *)
                              BinOp(Eq, Lval(Var(checkTmpVar), NoOffset), Const(CInt64(Int64.of_int(0), IInt, None)), Cil.intType);
                              Const(CStr("args check FIXME better message please"));
                              Const(CStr( location.file ));
                              Const(CInt64(Int64.of_int (if location.line == -1 then 0 else location.line), IUInt, None));
                              Const(CStr( enclosingFunction.svar.vname ))
                            ],
                            locUnknown
                      )
                      (* FIXME: also force a cast if the typesig of the function ptr is a ptr. 
                       * Can we do this by pretending that a pointer-returning function returns void*? *)
                    ];
                    DoChildren
                end
            |   None -> DoChildren
           end
      | _ -> DoChildren
      end
    else DoChildren
  end

  (* Remember the named types we've seen, so that we can map them back to
   * source code locations.
   * HACK: for now, don't bother maintaining separate namespaces for enums, structs and unions. *)
  val namedTypesMap : location NamedTypeMap.t ref = ref NamedTypeMap.empty
  
  (* Remember the set of __uniqtype objects for which we've created a global
   * weak extern. *)
  val uniqtypeGlobals : Cil.global UniqtypeMap.t ref = ref UniqtypeMap.empty

  method vglob (g: global) : global list visitAction =
    match g with
      GCompTag(ci, l) -> namedTypesMap := (NamedTypeMap.add ci.cname l !namedTypesMap); DoChildren
    | GCompTagDecl(ci, l) -> namedTypesMap := (NamedTypeMap.add ci.cname l !namedTypesMap); DoChildren
    | GEnumTag(ei, l) -> namedTypesMap := (NamedTypeMap.add ei.ename l !namedTypesMap); DoChildren
    | GEnumTagDecl(ei, l) -> namedTypesMap := (NamedTypeMap.add ei.ename l !namedTypesMap); DoChildren
    | _ -> DoChildren
  
  method vexpr (e: exp) : exp visitAction = 
    match e with 
      (* Check casts, unless 
         - they only affect qualifiers we don't care about
         - they are casts to void* etc. _from another pointer type_? NO, we allow these
             because they only "fail" if they don't point to valid memory, 
             which is a memory-safety issue
       *)
      CastE(t, subex) -> 
          let tsIsPointer testTs = match testTs with 
              TSPtr(_, _) -> true
            | _ -> false
          in
          let subexTs = getConcreteType(Cil.typeSig(Cil.typeOf(subex)))
          in 
          let targetTs = getConcreteType(Cil.typeSig(t))
          in
          let tsIsFunctionPointer ts = 
              match ts with
                  TSPtr(TSFun(_, _, _, _), _) -> true
                | _ -> false
          in 
          if targetTs = subexTs then DoChildren else 
          (* from any pointer (or int) to any function pointer is okay -- we check on use *)
          if (sloppyFps && (tsIsFunctionPointer targetTs)) then DoChildren else begin
          let location = match !currentInst with
            None -> {line = -1; file = "(unknown)"; byte = 0}
          | Some(i) -> begin match i with
                | Set(lv, e, l) -> l
                | Call(olv, e, es, l) -> l
                | Asm(attrs, instrs, locs, u, v, l) -> l
            end
          in
          let ts = getConcreteType(Cil.typeSig(t))
          in
          match ts with 
            TSPtr(TSBase(TVoid([])), []) (* when tsIsPointer subexTs *) -> DoChildren
          | TSPtr(TSBase(TInt(IChar, [])), []) (* when tsIsPointer subexTs *) -> DoChildren
          | TSPtr(TSBase(TInt(ISChar, [])), []) (* when tsIsPointer subexTs *) -> DoChildren
          | TSPtr(TSBase(TInt(IUChar, [])), []) (* when tsIsPointer subexTs *) -> DoChildren
          | TSPtr(ptdts, attrs) -> begin
              output_string Pervasives.stderr ("cast to typesig " ^ (Pretty.sprint 80 (d_typsig () ((* getConcreteType( *)Cil.typeSig(t) (* ) *) ))) ^ " from " ^ (Pretty.sprint 80 (d_typsig () (Cil.typeSig(Cil.typeOf(subex))))) ^ " %s needs checking!\n"); flush Pervasives.stderr; 
              (* enqueue the tmp var decl, assignment and assertion *)
              let exprTmpVar = Cil.makeTempVar enclosingFunction (typeOf e) in
              let checkTmpVar = Cil.makeTempVar enclosingFunction intType in 
              let concreteType = getConcreteType ptdts in
              let symname = symnameFromSig concreteType in
              (* FIXME: use List API! *)
              let rec findLikeA barename l = match l with 
                   [] -> None
              | x::xs -> if (userTypeNameToBareName x) = barename then Some(barename) else findLikeA barename xs
              in
              let concretePtdts = (getConcreteType ptdts) 
              in 
              let canonicalName = barenameFromSig concretePtdts
              in
              let testfunVar, testfunName = begin
                  if (tsIsUndefinedType concretePtdts enclosingFile) then
                      (namedAUInlineFun.svar, "__named_aU")
                  else match (findLikeA canonicalName likeATypeNames) with
                    None ->
                        output_string Pervasives.stderr ("not using __like_a because " ^ (Pretty.sprint 80 (d_typsig () (concretePtdts))) ^ "(" ^ canonicalName ^ ") is not in \"" ^ likeAStr ^ "\"\n"); flush Pervasives.stderr;
                        (isAUInlineFun.svar, "__is_aU")
                  | Some(_) -> 
                        output_string Pervasives.stderr ("using __like_a! because " ^ (Pretty.sprint 80 (d_typsig () (concretePtdts))) ^ "(" ^ canonicalName ^ ") is in \"" ^ likeAStr ^ "\"\n"); flush Pervasives.stderr;
                        (likeAUInlineFun.svar, "__like_aU")
              end
              in
              let (newMap, uniqtypeGlobalVar, newGlobals) = getOrCreateUniqtypeGlobal !uniqtypeGlobals symname concreteType enclosingFile.globals
              in 
              enclosingFile.globals <- newGlobals; 
              uniqtypeGlobals := newMap;
              self#queueInstr [
                (* first enqueue an assignment of the whole cast to exprTmpVar *)
                Set( (Var(exprTmpVar), NoOffset), e, locUnknown );
                (* next enqueue the is_a (or whatever) call *)
                Call( Some((Var(checkTmpVar), NoOffset)), (* return value dest *)
                      (Lval(Var(testfunVar),NoOffset)),  (* lvalue of function to call *)
                      [ 
                        (* first arg is the expression result *)
                        Lval(Var(exprTmpVar), NoOffset);
                        (* second argument is the uniqtype or the typestr *)
                        CastE(voidConstPtrType, 
                           if testfunName = "__named_aU" then 
                               Cil.mkString(barenameFromSig concreteType)
                           else Cil.mkAddrOf(Var(uniqtypeGlobalVar), NoOffset)
                        )
                      ],
                      locUnknown
                );
                (* then enqueue the assertion about its result *)
                Call( None,
                      (Lval(Var(inlineAssertFun.svar),NoOffset)),
                      [
                        (* arg is the check result *)
                        Lval(Var(checkTmpVar), NoOffset);
                        Const(CStr(testfunName ^ "(" ^ exprTmpVar.vname ^ ", " ^ (barenameFromSig concreteType) ^ ")"));
                        Const(CStr( location.file ));
                        Const(CInt64(Int64.of_int (if location.line == -1 then 0 else location.line), IUInt, None));
                        Const(CStr( enclosingFunction.svar.vname ))
                      ],
                      locUnknown
                )
              ];
              (* change to a reference to the decl'd tmp var *)
              ChangeTo ( CastE(t, subex) )
            end (* end TSPtr other cases *)
          | _ -> DoChildren
        end  (* end CastE case *)
    | _ -> DoChildren
end (* end match e *)

let makeExternalFunctionInFile fl nm proto = (* findOrCreateFunc fl nm proto *) (* NO! doesn't let us have the fundec *)
  let funDec = emptyFunction nm in
    funDec.svar.vtype <- proto;
    fl.globals <- newGlobalsList fl.globals [GVarDecl(funDec.svar, {line = -1; file = "BLAH FIXME"; byte = 0})] isFunction; 
    funDec


let makeInlineFunctionInFile fl ourFun nm proto body referencedValues = begin
   let protoArgs = match proto with 
     TFun(_, Some l, _, _) -> l
   | _ -> []
   in
   let protoWithInlineAttrs = match proto with 
     TFun(retT, args, isVarargs, attrs) -> TFun(retT, args, isVarargs, attrs @ [Attr("gnu_inline", []); Attr("always_inline", [])])
   | _ -> proto
   in
   let arglist = List.map (fun (ident, typ, attrs) -> makeFormalVar ourFun ident typ) protoArgs in 
   let () = setFunctionType ourFun protoWithInlineAttrs in
   let nameFunc =  (fun n t -> makeTempVar ourFun ~name:n t) in
   let loc = {line = -1; file = "BLAH FIXME"; byte = 0} in
   let argPatternBindings = List.map (fun ((ident, typ, attrs), arg) -> (ident, Fv arg)) (List.combine protoArgs arglist) in 
   let extPatternBindings = (* List.map (fun (ident, v) -> (ident, Fv v)) *) referencedValues in
   let madeBody = mkBlock (Formatcil.cStmts body nameFunc loc (argPatternBindings @ extPatternBindings)) in
   ourFun.sbody <- madeBody;
   ourFun.svar.vinline <- true;
   ourFun.svar.vstorage <- Extern;
    (* Don't make it static -- inline is enough. Making it static
        generates lots of spurious warnings when used from a non-static 
        inline function. *)
    (* Actually, do make it static -- C99 inlines are weird and don't eliminate
       multiple definitions the way we'd like.*)
    (* inlineAssertFun.svar.vstorage <- Static; *)
    (* ACTUALLY actually, make it extern, which plus gnu_inline above, 
       should be enough to shut up the warnings and give us a link error if 
       any non-inlined calls creep through. *)
    ourFun
  end

let loadFile fn =
  let chan = open_in fn in
  let nchars = in_channel_length chan in
  let str = String.create nchars in
  really_input chan str 0 nchars;
  close_in chan;
  (str)

let readLines fn = 
  let lines = ref [] in
  let chan = open_in fn in
  try
    while true; do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines


(* We snarf the function body as a string, and also return a 
 * CIL-friendly location of where we found it. *)
let getInlineFunctionDefinition name = ("", {line = -1; file = "BLAH FIXME"; byte = 0}) (* FIXME *)

class trumPtrFunVisitor = fun fl -> object
  inherit nopCilVisitor

  val mutable libcrunchIsInitialized = makeGlobalVar "__libcrunch_is_initialized" (TInt(IBool, [Attr("weak", [])]))
  val mutable libcrunchAbortedTypestr = makeGlobalVar "__libcrunch_aborted_typestr" (TInt(IULong, [Attr("weak", [])]))
  val mutable libcrunchBegun = makeGlobalVar "__libcrunch_begun" (TInt(IULong, [Attr("weak", [])]))

  val warnxFunDec = makeExternalFunctionInFile fl "warnx" (TFun(voidType, 
                             Some [ ("fmt", charConstPtrType, []) ],
                            true, []))

  val libcrunchGlobalInitFunDec = makeExternalFunctionInFile fl "__libcrunch_global_init" (TFun(intType, 
                             Some [],
                            false, [Attr("weak", [])]))
  
  val typestrToUniqtypeFunDec = makeExternalFunctionInFile fl "__libcrunch_typestr_to_uniqtype" (TFun(voidConstPtrType, 
                             Some [("typestr", charConstPtrType, [])],
                            false, [Attr("weak", [])]))
  
  val isAInternalFunDec = makeExternalFunctionInFile fl "__is_a_internal" (TFun(intType, 
                             Some [ ("obj", voidConstPtrType, []);
                                   ("typestr", voidConstPtrType, []) ],
                            false, [(*Attr("weak", [])*)]))
  
  val likeAInternalFunDec = makeExternalFunctionInFile fl "__like_a_internal" (TFun(intType, 
                             Some [ ("obj", voidConstPtrType, []);
                                   ("typestr", voidConstPtrType, []) ],
                            false, [(*Attr("weak", [])*)]))
  
  val namedAInternalFunDec = makeExternalFunctionInFile fl "__named_a_internal" (TFun(intType, 
                             Some [ ("obj", voidConstPtrType, []);
                                   ("typestr", voidConstPtrType, []) ],
                            false, [(*Attr("weak", [])*)]))

  val checkArgsInternalFunDec = makeExternalFunctionInFile fl "__check_args_internal" (TFun(intType, 
                             Some [ ("obj", voidConstPtrType, []);
                                   ("nargs", intType, []) ],
                            true, [(*Attr("weak", [])*)]))
  
  val assertFailFunDec = makeExternalFunctionInFile fl "__assert_fail" (TFun(voidType, 
                            Some [ 
                            ("assertion", charConstPtrType, []);
                            ("file", charConstPtrType, []);
                            ("line", uintType, []);
                            ("function", charConstPtrType, [])
                             ], 
                            false, []))
  
  (* Will fill these in during initializer*) 
  val mutable inlineAssertFun = emptyFunction "__inline_assert"
  val mutable libcrunchCheckInitFun = emptyFunction "__libcrunch_check_init"
  val mutable isAUInlineFun = emptyFunction "__is_aU"
  val mutable isASInlineFun = emptyFunction "__is_aS"
  val mutable likeAUInlineFun = emptyFunction "__like_aU"
  val mutable namedAUInlineFun = emptyFunction "__named_aU"
  

  initializer
    (* according to the docs for pushGlobal, non-types go at the end of globals --
     * but if we do this, our function definition appears at the end, which is wrong.
     * So just put it at the front -- seems to work.
     * ARGH. Actually, it needs to go *after* the assertFailFun, on which it depends,
     * to avoid implicit declaration problems. So we split the list at this element, 
     * then build a new list. *)

    inlineAssertFun <- makeInlineFunctionInFile fl inlineAssertFun "__inline_assert" (TFun(voidType, 
                            Some [ ("cond", intType, []);
                                   ("assertion", charConstPtrType, []);
                                   ("file", charConstPtrType, []);
                                   ("line", uintType, []);
                                   ("function", charConstPtrType, [])
                                    ], 
                            false, [])) "if (!%v:cond) %v:__assert_fail(%v:assertion, %v:file, %v:line, %v:function);" [("__assert_fail", (Fv assertFailFunDec.svar) )];

    libcrunchCheckInitFun <- makeInlineFunctionInFile fl libcrunchCheckInitFun "__libcrunch_check_initialized" (TFun(TInt(IInt, []), 
                            Some [], 
                            false, [])) "\
    if (/*__builtin_expect (*/ ! & %v:__libcrunch_is_initialized/*, 0)*/) \n\
    { \n\
        /* This means that we're not linked with libcrunch.  \n\
         * There's nothing we can do! */ \n\
        return -1; \n\
    } \n\
    if ( /*__builtin_expect (*/ ! %v:__libcrunch_is_initialized/*, 0)*/) \n\
    { \n\
        /* This means we haven't initialized. \n\
         * Try that now (it won't try more than once). */ \n\
        int ret = __libcrunch_global_init (); \n\
        return ret; \n\
    } \n\
     \n\
    return 0; \n\
                           " [("__libcrunch_is_initialized", (Fv libcrunchIsInitialized)); 
                              ("__libcrunch_global_init", (Fv libcrunchGlobalInitFunDec.svar)) ];
                           
    isAUInlineFun <- makeInlineFunctionInFile fl isAUInlineFun "__is_aU" (TFun(intType, 
                            Some [ ("obj", voidConstPtrType, []);
                                   ("r", voidConstPtrType, [])
                                 ], 
                            false, [])) "\
        if (!%v:obj) \n\
        { \n\
            return 1; \n\
        } \n\
        // int inited = __libcrunch_check_init (); \n\
        // if (/*__builtin_expect(*/(inited == -1)/*, 0)*/) \n\
        // { \n\
        //     return 1; \n\
        // } \n\
        \n\
        /* Null uniqtype means __is_aS got a bad typestring, OR we're not  \n\
         * linked with enough uniqtypes data. */ \n\
        if (/*__builtin_expect(*/ !r/*, 0)*/) \n\
        { \n\
           __libcrunch_begun += 1; \n\
           __libcrunch_aborted_typestr += 1; \n\
             return 1; \n\
        } \n\
        /* No need for the char check in the CIL version */ \n\
        // now we're really started \n\
        __libcrunch_begun += 1; \n\
        int ret = __is_a_internal(obj, r); \n\
        return ret;"
     [("__libcrunch_check_init", (Fv libcrunchCheckInitFun.svar)); 
      ("warnx", (Fv warnxFunDec.svar)); 
      ("__libcrunch_aborted_typestr", (Fv libcrunchAbortedTypestr)); 
      ("__libcrunch_begun", (Fv libcrunchBegun));
      ("__is_a_internal", (Fv isAInternalFunDec.svar)) ]; 
           (* %v:warnx("Aborted __is_a(%%p, %%p), reason: %%s \n", obj, r, 
                "unrecognised typename (see stack trace)"); *)

    isASInlineFun <- makeInlineFunctionInFile fl isASInlineFun "__is_aS" (TFun(intType, 
                            Some [ ("obj", voidConstPtrType, []);
                                   ("typestr", charConstPtrType, [])
                                 ], 
                            false, [])) "\
        if (!%v:obj) \n\
        { \n\
            return 1; \n\
        } \n\
        // int inited = __libcrunch_check_init (); \n\
        // if (/*__builtin_expect(*/(inited == -1)/*, 0)*/) \n\
        // { \n\
        //     return 1; \n\
        // } \n\
        \n\
        void * r = __libcrunch_typestr_to_uniqtype(typestr); \n\
        \n\
        int ret = __is_aU(obj, r);\n\
        return ret; \n\
       "
     [("__libcrunch_check_init", (Fv libcrunchCheckInitFun.svar)); 
      ("__libcrunch_typestr_to_uniqtype", (Fv typestrToUniqtypeFunDec.svar)); 
      ("__is_aU", (Fv isAUInlineFun.svar)) ]; 
           (* %v:warnx("Aborted __is_a(%%p, %%p), reason: %%s \n", obj, r, 
                "unrecognised typename (see stack trace)"); *)

    likeAUInlineFun <- makeInlineFunctionInFile fl likeAUInlineFun "__like_aU" (TFun(intType, 
                            Some [ ("obj", voidConstPtrType, []);
                                   ("r", voidConstPtrType, [])
                                 ], 
                            false, [])) "\
        if (!%v:obj) \n\
        { \n\
            return 1; \n\
        } \n\
        // int inited = __libcrunch_check_init (); \n\
        // if (/*__builtin_expect(*/(inited == -1)/*, 0)*/) \n\
        // { \n\
        //     return 1; \n\
        // } \n\
        \n\
        /* Null uniqtype means __is_aS got a bad typestring, OR we're not  \n\
         * linked with enough uniqtypes data. */ \n\
        if (/*__builtin_expect(*/ !r/*, 0)*/) \n\
        { \n\
           __libcrunch_begun += 1; \n\
           __libcrunch_aborted_typestr += 1; \n\
             return 1; \n\
        } \n\
        /* No need for the char check in the CIL version */ \n\
        // now we're really started \n\
        __libcrunch_begun += 1; \n\
        int ret = __like_a_internal(obj, r); \n\
        return ret;"
     [("__libcrunch_check_init", (Fv libcrunchCheckInitFun.svar)); 
      ("warnx", (Fv warnxFunDec.svar)); 
      ("__libcrunch_aborted_typestr", (Fv libcrunchAbortedTypestr)); 
      ("__libcrunch_begun", (Fv libcrunchBegun));
      ("__like_a_internal", (Fv likeAInternalFunDec.svar)) ]; 
           (* %v:warnx("Aborted __is_a(%%p, %%p), reason: %%s \n", obj, r, 
                "unrecognised typename (see stack trace)"); *)
    
    namedAUInlineFun <- makeInlineFunctionInFile fl namedAUInlineFun "__named_aU" (TFun(intType, 
                            Some [ ("obj", voidConstPtrType, []);
                                   ("s", voidConstPtrType, [])
                                 ], 
                            false, [])) "\
        if (!%v:obj) \n\
        { \n\
            return 1; \n\
        } \n\
        // int inited = __libcrunch_check_init (); \n\
        // if (/*__builtin_expect(*/(inited == -1)/*, 0)*/) \n\
        // { \n\
        //     return 1; \n\
        // } \n\
        \n\
        /* Null uniqtype means __is_aS got a bad typestring, OR we're not  \n\
         * linked with enough uniqtypes data. */ \n\
        if (/*__builtin_expect(*/ !s/*, 0)*/) \n\
        { \n\
           __libcrunch_begun += 1; \n\
           __libcrunch_aborted_typestr += 1; \n\
             return 1; \n\
        } \n\
        /* No need for the char check in the CIL version */ \n\
        // now we're really started \n\
        __libcrunch_begun += 1; \n\
        int ret = __named_a_internal(obj, s); \n\
        return ret;"
     [("__libcrunch_check_init", (Fv libcrunchCheckInitFun.svar)); 
      ("warnx", (Fv warnxFunDec.svar)); 
      ("__libcrunch_aborted_typestr", (Fv libcrunchAbortedTypestr)); 
      ("__libcrunch_begun", (Fv libcrunchBegun));
      ("__named_a_internal", (Fv namedAInternalFunDec.svar)) ]; 
           (* %v:warnx("Aborted __is_a(%%p, %%p), reason: %%s \n", obj, r, 
                "unrecognised typename (see stack trace)"); *)
    
    fl.globals <- newGlobalsList fl.globals [
         GVarDecl(libcrunchIsInitialized, {line = -1; file = "BLAH FIXME"; byte = 0});
         GVarDecl(libcrunchBegun, {line = -1; file = "BLAH FIXME"; byte = 0});
         GVarDecl(libcrunchAbortedTypestr, {line = -1; file = "BLAH FIXME"; byte = 0});
         (* GFun(libcrunchGlobalInitFun, {line = -1; file = "BLAH FIXME"; byte = 0}); *)
         GFun(libcrunchCheckInitFun, {line = -1; file = "BLAH FIXME"; byte = 0});
         (* GFun(warnxFun, {line = -1; file = "BLAH FIXME"; byte = 0}); *)
         (* GFun(isAInternalFun, {line = -1; file = "BLAH FIXME"; byte = 0}); *)
         (* GFun(assertFailFun, {line = -1; file = "BLAH FIXME"; byte = 0}); *)
         GFun(inlineAssertFun, {line = -1; file = "BLAH FIXME"; byte = 0});
         GFun(isAUInlineFun, {line = -1; file = "BLAH FIXME"; byte = 0}); 
         GFun(isASInlineFun, {line = -1; file = "BLAH FIXME"; byte = 0});
         GFun(likeAUInlineFun, {line = -1; file = "BLAH FIXME"; byte = 0});
         GFun(namedAUInlineFun, {line = -1; file = "BLAH FIXME"; byte = 0})
         ] 
         isFunction

  method vfunc (f: fundec) : fundec visitAction = 
      let tpExprVisitor = new trumPtrExprVisitor fl f isAInternalFunDec checkArgsInternalFunDec.svar isASInlineFun isAUInlineFun likeAUInlineFun namedAUInlineFun inlineAssertFun in
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

