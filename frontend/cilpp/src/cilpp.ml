(* cilpp -- a simple CIL driver that replaces the C preprocessor.
 *
 * First we preprocess into a temporary file;
 * we pass through to cpp all our arguments except for any following "-o".
 * Then we run CIL and output to the intended -o file.
 *)
open Printf
open Unix
external mkstemp: string -> Unix.file_descr * string = "caml_mkstemp"

let () =
    let minusOPos = ref None in
    let originalOutfile = ref None in
    let (newTempFd, newTempName) = mkstemp "/tmp/tmp.XXXXXX" in
    let newArgs = Array.mapi (fun i -> fun arg -> 
        if i = 0 then "cpp" else
        match arg with
          | "-o" -> (minusOPos := Some(i); arg)
          | _ -> (
            match !minusOPos with
                Some(pos) when pos + 1 = i -> (
                    originalOutfile := Some(arg);
                    newTempName
                    )
                  | _ -> arg
          )
        ) Sys.argv
    in
    (* FIXME: we have left the fd open *)
    match fork () with
        | 0 -> (try execvp newArgs.(0) newArgs
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
    (* FIXME: do passes according to environment variables *)
    let (chan, str) = match !originalOutfile with
            None -> Pervasives.stdout, "(stdout)"
          | Some(fname) -> (Pervasives.open_out fname, fname)
    in
    let _ = Cil.dumpFile Cil.defaultCilPrinter chan str initialCilFile
    in
    (* FIXME: delete temporary file! *)
    ()
