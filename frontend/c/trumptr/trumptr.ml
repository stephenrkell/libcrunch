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
                           fun isAInternalFunDec ->
                           fun checkArgsInternalFunDec -> 
                           fun isASInlineFun ->
                           fun isAUInlineFun ->
                           fun likeAUInlineFun ->
                           fun namedAUInlineFun ->
                           fun isAFunctionRefiningUInlineFun ->
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
    (* We handle check-on-use of indirect calls here. *)
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
                    (* For function pointers, we take the following approach.
                       We try the __is_a check first, because it is fast and if it passes,
                       we are okay.
                       Otherwise we do the more expensive check_args check.
                       We only report a failure if the more expensive check fails. *)
                  
                  
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
          (* from any pointer (or int) to any function pointer is okay -- we check on use 
           * -- IFF we're being sloppy. There is a significant performance penalty for
           * check-on-use in some codebases (e.g. gcc). *)
          if (sloppyFps && (tsIsFunctionPointer targetTs)) then DoChildren else begin
          let location = match !currentInst with
            None -> {line = -1; file = "(unknown)"; byte = 0}
          | Some(i) -> begin match i with
                | Set(lv, e, l) -> l
                | Call(olv, e, es, l) -> l
                | Asm(attrs, instrs, locs, u, v, l) -> l
            end
          in
          match targetTs with 
            TSPtr(TSBase(TVoid([])), []) (* when tsIsPointer subexTs *) -> DoChildren
          | TSPtr(TSBase(TInt(IChar, [])), []) (* when tsIsPointer subexTs *) -> DoChildren
          | TSPtr(TSBase(TInt(ISChar, [])), []) (* when tsIsPointer subexTs *) -> DoChildren
          | TSPtr(TSBase(TInt(IUChar, [])), []) (* when tsIsPointer subexTs *) -> DoChildren
            (* We could use our own constant folding to detect always-null pointers here. 
               But we just let the compiler do it! It will inline our __is_aU and will
               simplify it down to nothing if the pointer is null. *)
          | TSPtr(ptdts, attrs) (* when not isStaticallyNullPtr subex *) -> begin
              debug_print 1 ("cast to typesig " ^ (Pretty.sprint 80 (d_typsig () ((* getConcreteType( *)Cil.typeSig(t) (* ) *) ))) ^ " from " ^ (Pretty.sprint 80 (d_typsig () (Cil.typeSig(Cil.typeOf(subex))))) ^ " %s needs checking!\n"); flush Pervasives.stderr; 
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
                    (* If the target type is a singly indirect function pointer, 
                       we apply a more liberal test than __is_a, 
                       namely __is_a_function_refining.
                       
                       This succeeds for distinct function pointer types 
                       IFF the implicit casts done at a call site (including return)
                       ... using the cast's target type 
                       ... would always pass __is_a.
                       
                       NOTE that this is NOT the same as __check_args that we do for sloppy FPs.
                       __check_args is a check-on-use for function pointers
                       (inserted at indirect calls -- see way above in the code) 
                       whereas this is a check-on cast.
                       
                       We could roll this special function test together with __is_a,
                       but since it's a much rarer case, 
                       it seems better to keep it separate (e.g. for cache reasons). 
                       
                       Is check-on-use ever wanted?
                       YES if we want to cast to void*( * )(void * )
                            then use it with argument Foo
                            where it actually creates a Bar --
                            if we want to permissively let the fp cast go ahead, 
                            we have to check [all subsequent] function pointer uses.
                            
                       What about multiply-indirect function pointers? 
                             e.g. int( ****fn)(void* ) 
                             
                             We need to take the super-strict approach as usual here.
                     *)

              let canonicalName = barenameFromSig concretePtdts
              in
              let testfunVar, testfunName = begin
                  if (tsIsUndefinedType concretePtdts enclosingFile) then
                      (namedAUInlineFun.svar, "__named_aU")
                  else if (tsIsFunctionPointer targetTs) then
                      (isAFunctionRefiningUInlineFun.svar, "__is_a_function_refiningU")
                  else match (findLikeA canonicalName likeATypeNames) with
                    None ->
                        debug_print 1 ("not using __like_a because " ^ (Pretty.sprint 80 (d_typsig () (concretePtdts))) ^ "(" ^ canonicalName ^ ") is not in \"" ^ likeAStr ^ "\"\n"); flush Pervasives.stderr;
                        (isAUInlineFun.svar, "__is_aU")
                  | Some(_) -> 
                        debug_print 1 ("using __like_a! because " ^ (Pretty.sprint 80 (d_typsig () (concretePtdts))) ^ "(" ^ canonicalName ^ ") is in \"" ^ likeAStr ^ "\"\n"); flush Pervasives.stderr;
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

  val warnxFunDec = findOrCreateExternalFunctionInFile fl "warnx" (TFun(voidType, 
                             Some [ ("fmt", charConstPtrType, []) ],
                            true, []))

  val libcrunchGlobalInitFunDec = findOrCreateExternalFunctionInFile fl "__libcrunch_global_init" (TFun(intType, 
                             Some [],
                            false, [Attr("weak", [])]))
  
  val typestrToUniqtypeFunDec = findOrCreateExternalFunctionInFile fl "__libcrunch_typestr_to_uniqtype" (TFun(voidConstPtrType, 
                             Some [("typestr", charConstPtrType, [])],
                            false, [Attr("weak", [])]))
  
  val isAInternalFunDec = findOrCreateExternalFunctionInFile fl "__is_a_internal" (TFun(intType, 
                             Some [ ("obj", voidConstPtrType, []);
                                   ("t", voidConstPtrType, []) ],
                            false, [(*Attr("weak", [])*)]))
  
  val likeAInternalFunDec = findOrCreateExternalFunctionInFile fl "__like_a_internal" (TFun(intType, 
                             Some [ ("obj", voidConstPtrType, []);
                                   ("t", voidConstPtrType, []) ],
                            false, [(*Attr("weak", [])*)]))
  
  val namedAInternalFunDec = findOrCreateExternalFunctionInFile fl "__named_a_internal" (TFun(intType, 
                             Some [ ("obj", voidConstPtrType, []);
                                   ("typestr", voidConstPtrType, []) ],
                            false, [(*Attr("weak", [])*)]))

  val isAFunctionRefiningInternalFunDec = findOrCreateExternalFunctionInFile fl "__is_a_function_refining_internal" (TFun(intType, 
                             Some [ ("obj", voidConstPtrType, []);
                                   ("t", voidConstPtrType, []) ],
                            false, [(*Attr("weak", [])*)]))

  val checkArgsInternalFunDec = findOrCreateExternalFunctionInFile fl "__check_args_internal" (TFun(intType, 
                             Some [ ("obj", voidConstPtrType, []);
                                   ("nargs", intType, []) ],
                            true, [(*Attr("weak", [])*)]))
  
  val assertFailFunDec = findOrCreateExternalFunctionInFile fl "__assert_fail" (TFun(voidType, 
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
  val mutable isAFunctionRefiningUInlineFun = emptyFunction "__is_a_function_refiningU"
  

  initializer
    (* according to the docs for pushGlobal, non-types go at the end of globals --
     * but if we do this, our function definition appears at the end, which is wrong.
     * So just put it at the front -- seems to work.
     * ARGH. Actually, it needs to go *after* the assertFailFun, on which it depends,
     * to avoid implicit declaration problems. So we split the list at this element, 
     * then build a new list. *)

    inlineAssertFun <- (* makeInlineFunctionInFile *) findOrCreateExternalFunctionInFile
                            fl (* inlineAssertFun *) "__inline_assert" (TFun(voidType, 
                            Some [ ("cond", intType, []);
                                   ("assertion", charConstPtrType, []);
                                   ("file", charConstPtrType, []);
                                   ("line", uintType, []);
                                   ("function", charConstPtrType, [])
                                    ], 
                            false, [])) 
                            ;

    libcrunchCheckInitFun <- (* makeInlineFunctionInFile *) findOrCreateExternalFunctionInFile
                            fl (* libcrunchCheckInitFun *) "__libcrunch_check_initialized" (TFun(TInt(IInt, []), 
                            Some [], 
                            false, [])) 
    ;

    isASInlineFun <- (* makeInlineFunctionInFile *) findOrCreateExternalFunctionInFile
                            fl (* isASInlineFun *) "__is_aS" (TFun(intType, 
                            Some [ ("obj", voidConstPtrType, []);
                                   ("typestr", charConstPtrType, [])
                                 ], 
                            false, [])) 
    ; 

    likeAUInlineFun <- (* makeInlineFunctionInFile *) findOrCreateExternalFunctionInFile 
                            fl (* likeAUInlineFun *) "__like_aU" (TFun(intType, 
                            Some [ ("obj", voidConstPtrType, []);
                                   ("r", voidConstPtrType, [])
                                 ], 
                            false, [])) 
    ;

    namedAUInlineFun <- (* makeInlineFunctionInFile *) findOrCreateExternalFunctionInFile
                            fl (* namedAUInlineFun *) "__named_aU" (TFun(intType, 
                            Some [ ("obj", voidConstPtrType, []);
                                   ("s", voidConstPtrType, [])
                                 ], 
                            false, [])) 
    ;
    isAFunctionRefiningUInlineFun <- (* makeInlineFunctionInFile *) findOrCreateExternalFunctionInFile
                            fl (* namedAUInlineFun *) "__is_a_function_refiningU" (TFun(intType, 
                            Some [ ("obj", voidConstPtrType, []);
                                   ("t", voidConstPtrType, [])
                                 ], 
                            false, [])) 
    ;
    
    fl.globals <- newGlobalsList fl.globals [
         GVarDecl(libcrunchIsInitialized, {line = -1; file = "BLAH FIXME"; byte = 0});
         GVarDecl(libcrunchBegun, {line = -1; file = "BLAH FIXME"; byte = 0});
         GVarDecl(libcrunchAbortedTypestr, {line = -1; file = "BLAH FIXME"; byte = 0});
         (* GFun(libcrunchGlobalInitFun, {line = -1; file = "BLAH FIXME"; byte = 0}); *)
         (* GFun(libcrunchCheckInitFun, {line = -1; file = "BLAH FIXME"; byte = 0}); *)
         (* GFun(warnxFun, {line = -1; file = "BLAH FIXME"; byte = 0}); *)
         (* GFun(isAInternalFun, {line = -1; file = "BLAH FIXME"; byte = 0}); *)
         (* GFun(assertFailFun, {line = -1; file = "BLAH FIXME"; byte = 0}); *)
         (* GFun(inlineAssertFun, {line = -1; file = "BLAH FIXME"; byte = 0}); *)
         (* GFun(isAUInlineFun, {line = -1; file = "BLAH FIXME"; byte = 0});  *)
         (* GFun(isASInlineFun, {line = -1; file = "BLAH FIXME"; byte = 0}); *)
         (* GFun(likeAUInlineFun, {line = -1; file = "BLAH FIXME"; byte = 0}); *)
        (*  GFun(namedAUInlineFun, {line = -1; file = "BLAH FIXME"; byte = 0}) *)
        (*  GFun(isAFunctionRefiningUInlineFun, {line = -1; file = "BLAH FIXME"; byte = 0}) *)
         ] 
         isFunction

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
          let tpExprVisitor = new trumPtrExprVisitor fl f isAInternalFunDec checkArgsInternalFunDec.svar isASInlineFun isAUInlineFun likeAUInlineFun namedAUInlineFun isAFunctionRefiningUInlineFun inlineAssertFun
          in
          ChangeTo(visitCilFunction tpExprVisitor f)
end

let feature : Feature.t = 
  { fd_name = "trumptr";
    fd_enabled = false;
    fd_description = "dynamic checking of pointer casts";
    fd_extraopt = [];
    fd_doit = 
    (function (fl: file) -> 
      let tpFunVisitor = new trumPtrFunVisitor fl in
      debug_print 1 ("command line args are:\n"
       ^ (String.concat ", " (Array.to_list Sys.argv) ) );
      visitCilFileSameGlobals tpFunVisitor fl);
    fd_post_check = true;
  } 

let () = Feature.register feature
