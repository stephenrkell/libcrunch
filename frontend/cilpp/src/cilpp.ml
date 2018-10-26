(* cilpp -- a simple CIL driver that replaces the C preprocessor.
 *
 * First we preprocess into a temporary file;
 * we pass through to cpp all our arguments except for any following "-o".
 * Then we run CIL and output to the intended -o file.
 *)
open Printf
open Unix
open Feature
module D = Dynlink
external mkstemp: string -> Unix.file_descr * string = "caml_mkstemp"
external mkstemps: string -> int -> Unix.file_descr * string = "caml_mkstemps"

type extra_arg =
    OutputFile
  | Plugin
  | Driver
  | DependencyOutputFile

let () =
    let minusOPos = ref None in
    let saveTemps = ref false in
    let seenStd = ref None in
    let readingExtraArg = ref None in
    let originalOutfile = ref None in
    let driver = ref None in
    let seenMd = ref false in
    let seenMf = ref false in
    let depsOutputFile = ref None in
    let cilPluginsToLoadReverse = ref [] in
    let cilPassesToRunReverse = ref [] in
    let (newTempFd, newTempName) = mkstemps "/tmp/tmp.XXXXXX.cpp.i" 6 in
    let rewrittenArgs = List.flatten (List.mapi (fun i -> fun arg ->
        (* PROBLEM: different versions of cpp behave subtly differently,
         * especially w.r.t. their default version.
         * For example, cpp-4.9 defaults to gnu89 mode,
         * whereas cpp-7.2 defaults to gnu99 mode.
         * If we see code like this:
         * #define U blah
         * U"x"
         *
         * ... this will get preprocessed differently (unicode literals in gnu99).
         * This is basically the code's fault, but it can cause surprises
         * e.g. if the client compiler was gcc-4.9, it was expecting to invoke
         * cpp-4.9 where the problem above would not arise. Ideally we would
         * invoke the same version of cpp that the compiler would invoke. To that
         * end, our wrapper scrapes the executable of its parent pid and passes
         * that as a -driver option. Alternatively the user may specify -std=xxx
         * explicitly. If we don't get either of those, we fail early rather than
         * do something subtly wrong. *)
        if i = 0 then [] (* we fill "cpp" or whatever later *) else
        match arg with
          | "-o" -> (minusOPos := Some(i); readingExtraArg := Some(OutputFile); [arg])
          | "-save-temps" -> saveTemps := true; [] (* i.e. accept -Wp,-save-temps; compiler doesn't grok it*)
          | "-driver" -> (readingExtraArg := Some(Driver); [])
          | "-plugin" -> (readingExtraArg := Some(Plugin); [])
                (* NOTE that the driver adds an extra arg to -MD, and indeed that's the point of
                 * -MD as opposed to -MF. We might have -MF in the mix too, though.
                 * We delete both and then reinstate them. We want to avoid using -MD
                 * for real because we want to re-run the driver -E, and the combination
                 * of -MD, -E and -o changes the meaning of -MD. So it will not do what
                 * the user intended. I *think* -M -MF <filename>, when passed to the preprocessor,
                 * will always get that *assuming* they didn't actually pass all three of
                 * -E, -MD and -o to the driver. FIXME: handle that case if it isn't already.
                 * NOTE that the use of -MD without -MF is already handled by the driver, which
                 * generates the extra argument to -MF. So CHECK whether the other works too. *)
          | "-MD" -> (seenMd := true; readingExtraArg := Some(DependencyOutputFile); [])
          | "-MMD" -> (seenMd := true; readingExtraArg := Some(DependencyOutputFile); [])
          | "-MF" -> (seenMf := true; readingExtraArg := Some(DependencyOutputFile); [])
          | "-MMF" -> (seenMf := true; readingExtraArg := Some(DependencyOutputFile); [])
          | s when String.length s > String.length "-fpass-"
                && String.sub s 0 (String.length "-fpass-") = "-fpass-" ->
                let passName = String.sub s (String.length "-fpass-") (String.length s - String.length "-fpass-")
                in cilPassesToRunReverse := passName :: !cilPassesToRunReverse; []
          | s when String.length s > String.length "-std="
                && String.sub s 0 (String.length "-std=") = "-std=" ->
                (seenStd := Some(s); [s])
          | _ -> (
            let replacement = match !readingExtraArg with
                None -> [arg]
              | Some(Driver) -> driver := Some(arg); []
              | Some(OutputFile) -> originalOutfile := Some(arg); [newTempName]
              | Some(Plugin) -> cilPluginsToLoadReverse := arg :: !cilPluginsToLoadReverse; []
              | Some(DependencyOutputFile) -> (if !depsOutputFile = None
                    then depsOutputFile := Some(arg) else (); [])
            in
            readingExtraArg := None; replacement
          )
        ) (Array.to_list Sys.argv))
    in
    let cppCommandPrefix = match !seenStd with
        None -> (
            match !driver with
                None -> failwith "cilpp needs a -std=xxx option or a -driver"
              | Some(d) ->
                    d :: ["-E"] (* This -E may be redundant, but leave it for good measure *)
            )
      | Some(_) -> ["cpp"] (* -std= should do the trick *)
    in
    let depsArgs = match !depsOutputFile with
            None -> []
          | Some(depsFile) -> ["-M"; "-MF"; depsFile]
    in
    let outArgs = match !minusOPos with
        None -> (* there was no -o, so add one *) [ "-o"; newTempName ]
      | _ -> []
    in
    let allArgs = cppCommandPrefix @ rewrittenArgs @ outArgs @ depsArgs
    in
    (* FIXME: we have left the fd open *)
    match fork () with
        | 0 -> (try execvp (List.hd allArgs) (Array.of_list allArgs)
            with Unix_error(err, _, _) ->
                output_string Pervasives.stderr ("cannot exec cpp: " ^ (error_message err) ^ "\n");
                exit 255
          )
        | childPid ->
            let pid, status = wait () in
            match status with
                | WEXITED 255 -> ()
                | WEXITED 0 -> ()
                | WEXITED status ->
                    failwith "cpp exited with nonzero code";
                | WSIGNALED signal ->
                    failwith "cpp killed by signal";
                | WSTOPPED signal ->
                    failwith "cpp stopped";
    ;
    (* Okay, run CIL; we need the post-preprocessing line directive style *)
    Cil.lineDirectiveStyle := Some Cil.LinePreprocessorOutput;
    let initialCilFile = Frontc.parse newTempName () in
    let (chan, str) = match !originalOutfile with
            None -> Pervasives.stdout, "(stdout)"
          | Some(fname) -> (Pervasives.open_out fname, fname)
    in
    (* do passes *)
    List.iter Feature.loadWithDeps (List.rev !cilPluginsToLoadReverse);
    let features = List.rev !cilPassesToRunReverse in
    List.iter Feature.enable features;
    (* Errormsg.verboseFlag := true; *)
    let currentCilFile = initialCilFile in
    (* HACKED based on CIL's main.ml:
     * Scan all the registered features and, if they are 
     * enabled then run them on the current file *)
    List.iter
      (fun fdesc -> 
        if fdesc.Feature.fd_enabled then begin
          if !Errormsg.verboseFlag then 
            ignore (Errormsg.log "Running CIL feature %s (%s)\n" 
                      fdesc.Feature.fd_name fdesc.Feature.fd_description);
          try
          (* Run the feature, and see how long it takes. *)
          Stats.time fdesc.Feature.fd_name
            fdesc.Feature.fd_doit currentCilFile
          with Not_found -> (output_string Pervasives.stderr ("CIL pass " ^ fdesc.Feature.fd_name ^ " raised Not_found!\n"); raise Not_found);
          (* See if we need to do some checking *)
          if !Cilutil.doCheck && fdesc.Feature.fd_post_check then begin
            ignore (Errormsg.log "CIL check after %s\n" fdesc.Feature.fd_name);
            if not (Check.checkFile [] currentCilFile) && !Cilutil.strictChecking then begin
              Errormsg.error ("Feature \"%s\" left CIL's internal data "
                       ^^"structures in an inconsistent state. "
                       ^^"(See the warnings above)") fdesc.Feature.fd_name
            end
          end
        end)
      (Feature.list_registered ());
    Cil.printerForMaincil := Cil.defaultCilPrinter;
    (* We are not printing for CIL input *)
    Cil.print_CIL_Input := false;
    let _ = Cil.dumpFile Cil.defaultCilPrinter chan str currentCilFile
    in
    (* delete temporary file unless -save-temps *)
    if !saveTemps then () else Unix.unlink newTempName
