(* Copyright (c) 2018,
 *  Stephen Kell        <stephen.kell@cl.cam.ac.uk>
 *)

open Cil
open Cilallocs
open Pretty
open Map
open Str

class allocLocalsVisitor
 = fun enclosingFile ->
   fun ignoreFiles ->
       object(self)
    inherit nopCilVisitor
 
    val replacedLocals : Cil.varinfo VarinfoMap.t ref = ref VarinfoMap.empty
    
    val allocationFunc : Cil.fundec = materialiseBuiltin "__builtin_alloca"
    val memcpyFunc : Cil.fundec = (* materialiseBuiltin "__builtin_memcpy" *)
        findOrCreateExternalFunctionInFile enclosingFile "memcpy" (TFun(voidPtrType, 
                             Some [ ("dest", voidPtrType, []);
                                    ("src", voidConstPtrType, []);
                                    ("n", ulongType, [])
                                  ],
                            false, []))

    val currentFunc : fundec option ref = ref None

    method vfunc (f: fundec) : fundec visitAction = 
        currentFunc := Some(f);
        replacedLocals := VarinfoMap.empty;
        if List.fold_left (fun acc -> fun x -> acc || stringEndsWith f.svar.vdecl.file x) 
                false ignoreFiles
        then SkipChildren
        else
        let tempAddressTakenLocalNames = ref [] in
        let _ = visitCilBlock (new addressTakenVisitor tempAddressTakenLocalNames) f.sbody in
        let localsByName : (string, Cil.varinfo * bool) Hashtbl.t ref = ref (Hashtbl.create 113) in
        let addLocal = (fun isFormal -> fun vi -> Hashtbl.add !localsByName vi.vname (vi, isFormal)) in
        List.iter (addLocal false) f.slocals;
        List.iter (addLocal true) f.sformals;
        List.iter (fun localName ->
            (* introduce and initialize pointers for each address-taken local *)
            let (vi, isFormal) = Hashtbl.find !localsByName localName in
            (* rewrite SizeOFE, as we do elsewhere *)
            let ptrTargetT = match vi.vtype with
                TArray(elT, Some(SizeOfE(sizeExp)), attrs) ->
                    TArray(elT, Some(SizeOf(Cil.typeOf sizeExp)), attrs)
              | x -> x
            in
            let ptrT = TPtr(ptrTargetT, []) in
            let ptrVi = Cil.makeTempVar f ~name:(("__cil_alloclocal" ^ "_") ^ localName ^ "_") ptrT
            in
            replacedLocals := VarinfoMap.add vi ptrVi !replacedLocals;
            self#queueInstr (
                [Call(
                    Some(Var(ptrVi), NoOffset),
                    Lval(Var(allocationFunc.svar), NoOffset),
                    [SizeOf(vi.vtype)],
                    vi.vdecl
                  )
                ] @ if isFormal then [Call(
                        None,
                        Lval(Var(memcpyFunc.svar), NoOffset),
                        [Lval(Var(ptrVi), NoOffset);
                         mkAddrOf (Var(vi), NoOffset);
                         SizeOf(vi.vtype)
                        ],
                        vi.vdecl
                    )] else
                       (* FIXME: for the sake of our own robustness,
                          delete the local (can't do this for formals) *)
                      (*let _ = f.slocals  in *) []
            )
        ) !tempAddressTakenLocalNames;
        DoChildren
    
    method vexp (outerE : Cil.exp) : Cil.exp visitAction =
        (* eliminate SizeOfE because it causes problems that we
         * amplify, by duplicating "sizeof"-based array type expressions
         * into various places (chiefly our alloca() and memcpy() calls).
         * The underlying problem is that apparently dereferencing exprs,
         *like "sizeof a->b", don't actually do derefs. It is quite hard
         * for clients / downstream passes to know that they're in
         * such a context, so rewrite as we go down. *)
        let initiallyChangedE = match outerE with
            SizeOfE(e) -> SizeOf(Cil.typeOf e)
          | x -> x
        in
        ChangeDoChildrenPost(initiallyChangedE, fun i -> i)

    method vlval ((lh,lo): Cil.lval) : Cil.lval visitAction =
        match lh with
            Var(vi) when not vi.vglob -> (
                try 
                    let ptrVi = VarinfoMap.find vi !replacedLocals in
                    ChangeTo(Mem(Lval(Var(ptrVi), NoOffset)), lo)
                with Not_found -> DoChildren
            )
           | _ -> DoChildren

end

let feature : Feature.t = 
  { fd_name = "alloclocals";
    fd_enabled = false;
    fd_description = "allocate locals";
    fd_extraopt = [];
    fd_doit = 
    (function (fl: file) -> (* Unix.sleep 10; *) (* debugging *)
      visitCilFileSameGlobals (new allocLocalsVisitor fl ["liballocs_cil_inlines.h"; "libcrunch_cil_inlines.h"; "shadowprov_helpers.h"] :> cilVisitor) fl
    );
    fd_post_check = true;
  } 

let () = Feature.register feature
