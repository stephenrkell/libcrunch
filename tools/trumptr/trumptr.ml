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

open Pretty
open Cil
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
  
  method vexpr (e: exp) : exp visitAction = 
    match e with 
      (* We need to catch any use of a pointer that might cause a run-time failure.
         This is actually a bit subtle. If we do a downcast in one function, then
         pass to another function which uses it with the cast-to lvalue type, we 
         need to check the downcast even though we're not dereferencing it there
         and then. In other words, let's check all the downcasts. *)
      CastE(t, subex) -> begin
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
              let effectiveType = getEffectiveType (typeSig ptdt) in
              let typeStr = trim (Pretty.sprint 0 (d_typsig () effectiveType)) in
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
                        Const(CStr(typeStr))
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
          | _ -> SkipChildren
        end 
    | _ -> SkipChildren
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
    assertFailFunDec
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
                            false, []));
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
           ("__assert_fail", Fv assertFailFun.svar ) ]);
    assertFun.svar.vinline <- true;
    
    (* Don't make it static -- inline is enough. Making it static
        generates lots of spurious warnings when used from a non-static 
        inline function. *)
    (* Actually, do make it static -- C99 inlines are weird and don't eliminate
       multiple definitions the way we'd like.*)
    assertFun.svar.vstorage <- Static;
        
    (* according to the docs for pushGlobal, non-types go at the end of globals *)
    fl.globals <- [GFun(assertFun, 
        {line = -1; file = "BLAH FIXME"; byte = 0})] 
        (* @ [GFun(checkFun, 
        {line = -1; file = "BLAH FIXME"; byte = 0})] *)
        @fl.globals
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
      visitCilFileSameGlobals tpFunVisitor fl);
    fd_post_check = true;
  } 

