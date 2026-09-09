let target = Build_config.target
let target_parts = String.split_on_char '-' target
let target_is_windows = List.mem "windows" target_parts
let target_is_linux_musl =
  List.mem "linux" target_parts && List.mem "musl" target_parts
let toolchain_target =
  if target = "x86_64-linux-gnu" then "native-native-gnu" else target

type paths =
  { ll_path : string
  ; s_path : string
  ; code_obj_path : string
  ; exe_path : string
  ; c_path : string
  ; c_obj_path : string
  }

let devnull = if Sys.win32 then "NUL" else "/dev/null"

let find_executable name =
  let sep = if Sys.win32 then ';' else ':' in
  let exts = if Sys.win32 then [ ".exe"; ".cmd"; ".bat"; "" ] else [ "" ] in
  match Sys.getenv_opt "PATH" with
  | None -> None
  | Some path ->
      String.split_on_char sep path
      |> List.find_map (fun dir ->
             if dir = "" then None
             else
               List.find_map
                 (fun ext ->
                   let c = Filename.concat dir (name ^ ext) in
                   if Sys.file_exists c && not (Sys.is_directory c) then
                     if Sys.win32 then Some c
                     else
                       (try Unix.access c [ Unix.X_OK ]; Some c
                        with Unix.Unix_error _ -> None)
                   else None)
                 exts)

let zig_path = lazy (find_executable "zig")

let missing_toolchain_message =
  Printf.sprintf
    "xic: cannot build an executable for %s because a usable Zig executable was not found on PATH\n\
     xic: install Zig, use --emit=bytes -c for an object, or use --dump=asm,ir"
    target

let run ?(quiet = false) args =
  match Lazy.force zig_path with
  | None -> 127
  | Some zig ->
      let argv = Array.of_list args in
      let sink =
        if quiet then Some (Unix.openfile devnull [ Unix.O_WRONLY ] 0) else None
      in
      let out = match sink with Some fd -> fd | None -> Unix.stdout in
      let err = match sink with Some fd -> fd | None -> Unix.stderr in
      let result =
        try
          let pid = Unix.create_process zig argv Unix.stdin out err in
          let _, status = Unix.waitpid [] pid in
          (match status with Unix.WEXITED c -> c | _ -> 1)
        with Unix.Unix_error (error, call, path) ->
          Printf.eprintf "xic: could not execute Zig: %s (%s %s)\n"
            (Unix.error_message error) call path;
          126
      in
      (match sink with Some fd -> (try Unix.close fd with Unix.Unix_error _ -> ()) | None -> ());
      result

let write_file path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let write_bytes path contents =
  let oc = open_out_bin path in
  output_bytes oc contents;
  close_out oc

let write_ir path ir = write_file path ir

let remove_if_exists path = if Sys.file_exists path then Sys.remove path

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    let parent = Filename.dirname dir in
    if parent <> dir then mkdir_p parent;
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let with_ext path ext = Filename.remove_extension path ^ ext

let has_suffix_ci path suffix =
  Filename.check_suffix (String.lowercase_ascii path) suffix

let executable_path base =
  if target_is_windows && not (has_suffix_ci base ".exe") then base ^ ".exe"
  else base

let build_paths input output =
  match output with
  | None ->
      let ll = with_ext input ".ll" in
      { ll_path = ll
      ; s_path = with_ext ll ".s"
      ; code_obj_path = with_ext ll ".o"
      ; exe_path = executable_path (Filename.remove_extension ll)
      ; c_path = with_ext ll ".inline.c"
      ; c_obj_path = with_ext ll ".inline.o"
      }
  | Some out ->
      if has_suffix_ci out ".ll" || has_suffix_ci out ".s" then
        let base = Filename.remove_extension out in
        { ll_path = with_ext out ".ll"
        ; s_path = with_ext out ".s"
        ; code_obj_path = with_ext out ".o"
        ; exe_path = executable_path base
        ; c_path = with_ext out ".inline.c"
        ; c_obj_path = with_ext out ".inline.o"
        }
      else
        let exe = executable_path out in
        { ll_path = with_ext exe ".ll"
        ; s_path = with_ext exe ".s"
        ; code_obj_path = with_ext exe ".o"
        ; exe_path = exe
        ; c_path = with_ext exe ".inline.c"
        ; c_obj_path = with_ext exe ".inline.o"
        }

let object_output_path input output =
  match output with
  | Some out -> out
  | None -> with_ext input ".o"

let require_toolchain () =
  match Lazy.force zig_path with
  | Some _ -> Ok ()
  | None -> Error missing_toolchain_message

let cache_dir () =
  let base =
    match Sys.getenv_opt "LOCALAPPDATA" with
    | Some d -> d
    | None -> (
        match Sys.getenv_opt "XDG_CACHE_HOME" with
        | Some d -> d
        | None -> Filename.get_temp_dir_name ())
  in
  Filename.concat (Filename.concat base "xi-ml") "runtime"

let ensure_runtime_lib () =
  let cache = cache_dir () in
  mkdir_p cache;
  let key = Digest.to_hex (Digest.string Runtime_embed.runtime_lib) in
  let ext = if target_is_windows then "lib" else "a" in
  let lib = Filename.concat cache (Printf.sprintf "xi_runtime.%s.%s" key ext) in
  if Sys.file_exists lib then Ok lib
  else begin
    let tmp = Printf.sprintf "%s.tmp.%d" lib (Unix.getpid ()) in
    remove_if_exists tmp;
    write_file tmp Runtime_embed.runtime_lib;
    (try Sys.rename tmp lib with Sys_error _ -> ());
    remove_if_exists tmp;
    if Sys.file_exists lib then Ok lib
    else Error "auto-link failed: could not materialise embedded runtime library"
  end

let lib_flags libraries =
  let seen = Hashtbl.create 8 in
  List.filter_map
    (fun name ->
      if name = "" || Hashtbl.mem seen name then None
      else begin
        Hashtbl.add seen name ();
        Some ("-l" ^ name)
      end)
    libraries

let library_dir_flags directories =
  List.concat_map (fun dir -> [ "-L"; dir ]) directories

let subsystem_flags = function
  | Some "windows" when target_is_windows -> [ "-Wl,--subsystem,windows" ]
  | _ -> []

let opt_flag ~size = if size then "-Oz" else "-O2"

let compile_inline_c ~size ~include_dirs paths source =
  write_file paths.c_path source;
  let code =
    run
      ([ "zig"; "cc"; "-target"; toolchain_target; opt_flag ~size; "-std=c11" ]
       @ List.concat_map (fun dir -> [ "-I"; dir ]) include_dirs
       @ [ "-c"; paths.c_path; "-o"; paths.c_obj_path ])
  in
  if code = 0 then Ok paths.c_obj_path
  else Error "inline C compilation failed (zig cc)"

type timings =
  { generation : float
  ; object_generation : float
  ; c_compilation : float option
  ; linking : float
  }

let timed f =
  let started = Clock.now () in
  let result = f () in
  (result, Clock.now () -. started)

let build_native ?(libraries = []) ?(library_dirs = []) ?(link_files = [])
    ?subsystem ?(size = false) ?inline_c ?(inline_c_include_dirs = [])
    ?code_object ?(keep_intermediates = false) ~code_path paths =
  match ensure_runtime_lib () with
  | Error _ as e -> e
  | Ok lib ->
      let generation_started = Clock.now () in
      let c_worker =
        match inline_c with
        | None -> None
        | Some source ->
            Some
              (Domain.spawn (fun () ->
                 timed (fun () ->
                   compile_inline_c ~size
                     ~include_dirs:inline_c_include_dirs paths source)))
      in
      let object_code, object_generation =
        match code_object with
        | Some produce_bytes ->
            let bytes = produce_bytes () in
            let (), elapsed =
              timed (fun () -> write_bytes paths.code_obj_path bytes)
            in
            (0, elapsed)
        | None ->
            timed (fun () ->
              run
                ([ "zig"; "cc"; "-target"; toolchain_target; opt_flag ~size ]
                 @ (if has_suffix_ci code_path ".ll" then [ "-Wno-override-module" ] else [])
                 @ [ "-ffunction-sections"; "-fdata-sections"
                   ; "-c"; code_path; "-o"; paths.code_obj_path
                   ]))
      in
      let c_result =
        match c_worker with
        | None -> Ok ([], None)
        | Some worker ->
            let result, elapsed = Domain.join worker in
            (match result with
             | Ok obj -> Ok ([ obj ], Some elapsed)
             | Error _ as error -> error)
      in
      let generation = Clock.now () -. generation_started in
      if object_code <> 0 then
        Error "auto-link failed while generating the code object (zig cc)"
      else match c_result with
      | Error _ as error -> error
      | Ok (c_objects, c_compilation) ->
      let code, linking =
        timed (fun () ->
          run
          ([ "zig"; "cc"; "-target"; toolchain_target; opt_flag ~size ]
           @ (if target_is_linux_musl then [ "-static" ] else [])
           @ [
              "-ffunction-sections"; "-fdata-sections"; "-Wl,--gc-sections"
            ; paths.code_obj_path
            ]
          @ c_objects
          @ [ lib; "-o"; paths.exe_path ]
          @ link_files
          @ library_dir_flags library_dirs
          @ lib_flags libraries
          @ subsystem_flags subsystem))
      in
      if code = 0 then begin
        remove_if_exists (with_ext paths.exe_path ".pdb");
        if not keep_intermediates then begin
          remove_if_exists paths.c_path;
          remove_if_exists paths.c_obj_path;
          remove_if_exists paths.code_obj_path
        end;
        Ok { generation; object_generation; c_compilation; linking }
      end
      else Error "auto-link failed during final link (zig cc)"
