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

open Cil
open Pretty
open Map
open Str
open Cilallocs
module E = Errormsg
module H = Hashtbl

let mkCheckInstrs 
  (e : exp) (enclosingFunction: fundec) (testFunVar: varinfo) (inlineAssertFun: fundec)
  (loc : location) (checkExtraArgs : Cil.exp list) (checkArgString : string) uniqtypeGlobals currentInst
= 
  let exprTmpVar = Cil.makeTempVar enclosingFunction (typeOf e) 
  in
  let checkTmpVar = Cil.makeTempVar enclosingFunction intType 
  in 
  let instrs = 
  [
    (* first enqueue an assignment of the whole cast to exprTmpVar *)
    Set( (Var(exprTmpVar), NoOffset), e, loc )
    ;
    (* next enqueue the is_a (or whatever) call *)
    Call( Some((Var(checkTmpVar), NoOffset)), (* return value dest *)
          (Lval(Var(testFunVar),NoOffset)),  (* lvalue of function to call *)
          [ 
            (* first arg is the expression result *)
            Lval(Var(exprTmpVar), NoOffset);
            (* second argument is the uniqtype or the typestr *)
          ] @ checkExtraArgs,
          loc
    )
    ;
    (* then enqueue the assertion about its result *)
    Call( None,
          (Lval(Var(inlineAssertFun.svar),NoOffset)),
          [
            (* arg is the check result *)
            Lval(Var(checkTmpVar), NoOffset);
            Const(CStr(testFunVar.vname ^ "(" ^ exprTmpVar.vname ^ ", " ^ checkArgString ^ ")"));
            Const(CStr( loc.file ));
            Const(CInt64(Int64.of_int (if loc.line == -1 then 0 else loc.line), IUInt, None));
            Const(CStr( enclosingFunction.svar.vname ))
          ],
          loc
    )
  ]
  in (exprTmpVar, instrs)


class crunchBoundExprVisitor = fun enclosingFile -> 
                           fun enclosingFunction -> 
                           fun ensureBoundsInlineFun ->
                           fun checkDerivePtrInlineFun ->
                               object(self)
  inherit nopCilVisitor

  val currentInst : instr option ref = ref None
  
  method vinst (i: instr) : instr list visitAction = begin
    currentInst := Some(i); (* used from vexpr *)
    DoChildren
  end

  method vglob (g: global) : global list visitAction =
    match g with
    | _ -> DoChildren
  
  method vexpr (e: exp) : exp visitAction = 
    let location = match !currentInst with
      None -> {line = -1; file = "(unknown)"; byte = 0}
    | Some(i) -> begin match i with
          | Set(lv, e, l) -> l
          | Call(olv, e, es, l) -> l
          | Asm(attrs, instrs, locs, u, v, l) -> l
      end
    in
    match e with
    | _ -> DoChildren
    end (* end match e *)

(* We snarf the function body as a string, and also return a 
 * CIL-friendly location of where we found it. *)
let getInlineFunctionDefinition name = ("", {line = -1; file = "BLAH FIXME"; byte = 0}) (* FIXME *)

class crunchBoundFunVisitor = fun fl -> object
  inherit nopCilVisitor
                            
  val assertFailFunDec = findOrCreateExternalFunctionInFile fl "__assert_fail" (TFun(voidType, 
                            Some [ 
                            ("assertion", charConstPtrType, []);
                            ("file", charConstPtrType, []);
                            ("line", uintType, []);
                            ("function", charConstPtrType, [])
                             ], 
                            false, []))
  
  (* Will fill these in during initializer *) 
  val mutable inlineAssertFun = emptyFunction "__inline_assert"
  val mutable ensureBoundsInlineFun = emptyFunction "__ensure_bounds"
  val mutable checkDerivePtrInlineFun = emptyFunction "__check_derive_ptr"

  initializer
    (* according to the docs for pushGlobal, non-types go at the end of globals --
     * but if we do this, our function definition appears at the end, which is wrong.
     * So just put it at the front -- seems to work.
     * ARGH. Actually, it needs to go *after* the assertFailFun, on which it depends,
     * to avoid implicit declaration problems. So we split the list at this element, 
     * then build a new list. *)

    inlineAssertFun <-  findOrCreateExternalFunctionInFile
                            fl "__inline_assert" (TFun(voidType, 
                            Some [ ("cond", intType, []);
                                   ("assertion", charConstPtrType, []);
                                   ("file", charConstPtrType, []);
                                   ("line", uintType, []);
                                   ("function", charConstPtrType, [])
                            ], false, [])) 
                            ;

    ensureBoundsInlineFun <- findOrCreateExternalFunctionInFile 
                            fl "__ensure_bounds" (TFun(intType, 
                            Some [ ("p_bounds", voidConstPtrType, []);
                                   ("ptr", voidConstPtrType, []);
                                   ("t", voidConstPtrType, [])
                                 ], 
                            false, []))
    ;

    checkDerivePtrInlineFun <- findOrCreateExternalFunctionInFile
                            fl "__check_derive_ptr" (TFun(intType, 
                            Some [ ("p_new", voidConstPtrType, []);
                                   ("p_orig", voidConstPtrType, [])
                                 ], 
                            false, [])) 

  method vfunc (f: fundec) : fundec visitAction = 
      (* Don't instrument liballocs functions that get -include'd .
         This is actually mostly harmless, but causes warnings because
         we end up using __inline_assert before we define it. *)
      let startswith s pref = 
          if (String.length s) >= (String.length pref) then (String.sub s 0 (String.length pref)) = pref else false
      in 
      if startswith f.svar.vname "__liballocs_" then
          SkipChildren
      else
          let tpExprVisitor = new crunchBoundExprVisitor fl f ensureBoundsInlineFun checkDerivePtrInlineFun
          in
          ChangeTo(visitCilFunction tpExprVisitor f)
end

let feature : Feature.t = 
  { fd_name = "crunchbound";
    fd_enabled = false;
    fd_description = "dynamic bounds checking of pointer indexing and arithmetic";
    fd_extraopt = [];
    fd_doit = 
    (function (fl: file) -> 
      let tpFunVisitor = new crunchBoundFunVisitor fl in
      debug_print 1 ("command line args are:\n"
       ^ (String.concat ", " (Array.to_list Sys.argv) ) );
      visitCilFileSameGlobals tpFunVisitor fl);
    fd_post_check = true;
  } 

let () = Feature.register feature
