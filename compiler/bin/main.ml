(* xic (OCaml) CLI: lex -> parse -> analyze -> LLVM IR. *)

let render_diagnostics filename source diags =
  String.concat "\n"
    (List.map (fun d -> Xic.Diagnostic.format filename source d) diags)

let now = Xic.Clock.now

external peak_rss_bytes : unit -> int64 = "xic_peak_rss_bytes"

let print_memory_stats () =
  let bytes = peak_rss_bytes () in
  if Int64.compare bytes 0L > 0 then
    Printf.eprintf "\nMemory\nPeak compiler RSS  %12.2f MiB\n"
      (Int64.to_float bytes /. 1048576.)
  else
    Printf.eprintf "\nMemory\nPeak compiler RSS  %12s\n" "unavailable"

let timed f =
  let started = now () in
  let result = f () in
  (result, now () -. started)

let timed_if enabled f = if enabled then timed f else f (), 0.

type lowering_timings =
  { ir_preparation : float
  ; build_metadata : float
  ; lir_lowering : float
  ; lir_setup_and_declarations : float
  ; lir_function_analysis : float
  ; lir_ast_and_helper_generation : float
  ; lir_module_finalization : float
  ; lir_validation : float
  ; lir_optimization : float
  ; native_i64_promotion : float
  ; inline_c_emission : float
  }

type parse_timings =
  { root_parsing : float
  ; include_loading : float
  ; include_parsing : float
  ; source_annotation : float
  ; namespace_rewriting : float
  ; expansion_bookkeeping : float
  ; constant_folding : float
  }

let parse_time timings =
  timings.root_parsing
  +. timings.include_loading
  +. timings.include_parsing
  +. timings.source_annotation
  +. timings.namespace_rewriting
  +. timings.expansion_bookkeeping

let optimization_time timings = timings.constant_folding

let empty_parse_timings () =
  { root_parsing = 0.
  ; include_loading = 0.
  ; include_parsing = 0.
  ; source_annotation = 0.
  ; namespace_rewriting = 0.
  ; expansion_bookkeeping = 0.
  ; constant_folding = 0.
  }

let print_timing_child branch label seconds =
  Printf.eprintf "  %s %-24s %12.3f ms\n"
    branch label (seconds *. 1000.)

let print_parse_timings timings =
  print_timing_child "├─" "Root parsing" timings.root_parsing;
  print_timing_child "├─" "Include loading" timings.include_loading;
  print_timing_child "├─" "Included parsing" timings.include_parsing;
  print_timing_child "├─" "Source annotation" timings.source_annotation;
  print_timing_child "├─" "Namespace rewriting" timings.namespace_rewriting;
  print_timing_child "└─" "Expansion bookkeeping" timings.expansion_bookkeeping

let print_optimization_timings ?native_planning ?(external_pipelines = [])
    ?lir_optimization ?native_i64_promotion timings =
  let timed_children =
    [ "Constant folding", timings.constant_folding, None ]
    @ (match native_i64_promotion with
       | None -> []
       | Some seconds ->
           [ "Native-i64 promotion", seconds,
             Some "before LIR generation" ])
    @ (match lir_optimization with
       | None -> []
       | Some seconds ->
           [ "LIR optimization", seconds,
             Some "interleaved with LIR lowering" ])
    @ (match native_planning with
       | None -> []
       | Some (seconds, owner) ->
           [ "Native backend planning", seconds, Some ("during " ^ owner) ])
  in
  List.iteri
    (fun index (label, seconds, note) ->
      let is_last =
        index = List.length timed_children - 1
        && external_pipelines = []
      in
      let branch = if is_last then "└─" else "├─" in
      Printf.eprintf "  %s %-24s %12.3f ms"
        branch label (seconds *. 1000.);
      (match note with
       | Some text -> Printf.eprintf "  (%s)" text
      | None -> ());
      Printf.eprintf "\n")
    timed_children;
  List.iteri
    (fun index (label, owner) ->
      let branch =
        if index = List.length external_pipelines - 1 then "└─" else "├─"
      in
      Printf.eprintf
        "  %s %-24s timing is inseparable from %s\n" branch label owner)
    external_pipelines

let print_lir_lowering_timings timings =
  let child branch label seconds =
    Printf.eprintf "  %s %s %-21s %12.3f ms\n"
      "│" branch label (seconds *. 1000.)
  in
  child "├─" "Setup & declarations" timings.lir_setup_and_declarations;
  child "├─" "Function analysis" timings.lir_function_analysis;
  child "├─" "AST & helper generation" timings.lir_ast_and_helper_generation;
  child "├─" "Module finalization" timings.lir_module_finalization;
  child "└─" "Validation" timings.lir_validation

let analyze_source ~measure_details filename source =
  let tokens, lex_diags = Xic.Lexer.lex source in
  let lex_diags =
    List.map
      (fun (d : Xic.Lexer.diagnostic) ->
        Xic.Diagnostic.make d.code d.message d.span)
      lex_diags
  in
  let parsed =
    if lex_diags <> [] then Error (lex_diags, empty_parse_timings ())
    else
      let parsed, root_parsing =
        timed_if measure_details (fun () -> Xic.Parser.parse tokens)
      in
      match parsed with
      | Error diags ->
          Error
            (diags, { (empty_parse_timings ()) with root_parsing })
      | Ok program ->
          let program, root_annotation =
            timed_if measure_details (fun () ->
              Xic.Module_loader.annotate_source_origins
                program filename source)
          in
          let expanded, expansion_timings =
            Xic.Module_loader.expand_source_includes_with_timings
              ~measure:measure_details program filename
          in
          let timings_before_fold =
            { root_parsing
            ; include_loading = expansion_timings.source_loading
            ; include_parsing = expansion_timings.include_parsing
            ; source_annotation =
                root_annotation +. expansion_timings.source_annotation
            ; namespace_rewriting = expansion_timings.namespace_rewriting
            ; expansion_bookkeeping = expansion_timings.bookkeeping
            ; constant_folding = 0.
            }
          in
          match expanded with
          | Error diags -> Error (diags, timings_before_fold)
          | Ok program ->
              (* Constant folding sits between include expansion and analysis,
                 as in the reference: the folded tree is what gets type-checked,
                 so a folded literal is typed as the literal it became.

                 Note this deliberately does not affect --dump=ast /
                 --dump=ast-expanded, which exist to diff the parser and the
                 module loader against their ports and so must stay raw. The
                 reference's --dump=ast does show the folded tree; ours does
                 not, and our dump format differs from it anyway. *)
              let program, constant_folding =
                timed_if measure_details
                  (fun () -> Xic.Fold.fold_program program)
              in
              Ok
                (program,
                 { timings_before_fold with constant_folding })
  in
  match parsed with
  | Error (diags, parse_timings) -> Error (diags, parse_timings, 0.)
  | Ok (program, parse_timings) ->
      let sem, semantic_time = timed (fun () -> Xic.Semantic.analyze program) in
      if sem.Xic.Semantic.diagnostics <> [] then
        Error (sem.Xic.Semantic.diagnostics, parse_timings, semantic_time)
      else
        Ok ((program, sem), parse_timings, semantic_time)

let compile_analyzed ?on_func ~gc_mode (program, sem) =
  let ir_sem, ir_preparation =
    timed (fun () -> Xic.Semantic.to_ir_result sem)
  in
  let (libraries, subsystem, inline_c_include_dirs), build_metadata =
    timed (fun () ->
      let libraries =
        List.map (fun (l : Xic.Ast.link_decl) -> l.library) program.Xic.Ast.links
      in
      (* `#subsystem` — last one wins; default is a console app. *)
      let subsystem =
        List.fold_left
          (fun _ (s : Xic.Ast.subsystem_decl) -> Some s.name)
          None program.Xic.Ast.subsystems
      in
      let origins =
        List.filter_map
          (fun (p : Xic.Ast.c_preamble_decl) -> p.origin)
          program.Xic.Ast.c_preambles
        @ List.filter_map
            (fun (f : Xic.Ast.inline_c_fn_decl) -> f.origin)
            program.Xic.Ast.c_functions
      in
      let inline_c_include_dirs =
        List.fold_left
          (fun dirs path ->
            let dir = Filename.dirname path in
            if List.mem dir dirs then dirs else dirs @ [ dir ])
          [] origins
      in
      (libraries, subsystem, inline_c_include_dirs))
  in
  (* Lir.Malformed means lowering produced an invalid low-level value. That is a
     compiler bug, not a user error, but it must still come out as a diagnostic:
     an uncaught OCaml exception gives the user a bare "Fatal error" and no way
     to work around it. (This wrapper used to live in ll_parse.ml and was lost
     when that file was deleted.) *)
  let (lir, lir_timings), lir_lowering_with_optimization =
    timed (fun () ->
      try Xic.Ir.generate_lir_with_optimization_timing
            ?on_func ~gc_mode program ir_sem with
      | Xic.Lir.Malformed msg ->
          prerr_endline ("xic: internal error building the low-level IR: " ^ msg);
          prerr_endline "xic: please report this internal compiler error.";
          exit 1)
  in
  let lir_lowering =
    max 0.
      (lir_lowering_with_optimization
       -. lir_timings.native_i64_promotion
       -. lir_timings.lir_optimization)
  in
  let inline_c, inline_c_emission =
    timed (fun () -> Xic.C_embed.generate program)
  in
  let timings =
    { ir_preparation
    ; build_metadata
    ; lir_lowering
    ; lir_setup_and_declarations = lir_timings.setup_and_declarations
    ; lir_function_analysis = lir_timings.function_analysis
    ; lir_ast_and_helper_generation = lir_timings.ast_and_helper_generation
    ; lir_module_finalization = lir_timings.module_finalization
    ; lir_validation = lir_timings.validation
    ; lir_optimization = lir_timings.lir_optimization
    ; native_i64_promotion = lir_timings.native_i64_promotion
    ; inline_c_emission
    }
  in
  ((lir, libraries, subsystem, inline_c, inline_c_include_dirs), timings)

(* Lir -> x86_64 assembly. `Ir` builds the Lir directly, so nothing parses text
   in between.

   The backend refuses to guess on anything outside the subset `Ir` is known to
   emit, because a wrong lowering would show up as a miscompiled program rather
   than a build failure. Surface that as a compiler error naming the construct,
   and point at the working backend. *)
let generate_asm lir =
  let supported =
    Xic.Build_config.target = "x86_64-windows-gnu"
    || Xic.Build_config.target = "x86_64-linux-musl"
    || Xic.Build_config.target = "x86_64-linux-gnu"
  in
  if not supported then begin
    prerr_endline
      ("xic: native assembly emission is unavailable for target "
       ^ Xic.Build_config.target ^ "; use --emit=llvm.");
    exit 1
  end;
  try Xic.Asm_x64.generate lir with
  | Xic.Asm_x64.Unsupported msg ->
      prerr_endline ("xic: the x86_64 backend cannot compile this program yet: " ^ msg);
      prerr_endline "xic: retry with --emit=llvm, which still handles it, and please report the construct above.";
      exit 1

let generate_asm_with_optimization_share lir =
  try Xic.Asm_x64.generate_with_optimization_share lir with
  | Xic.Asm_x64.Unsupported msg ->
      prerr_endline ("xic: the x86_64 backend cannot compile this program yet: " ^ msg);
      prerr_endline "xic: retry with --emit=llvm, which still handles it, and please report the construct above.";
      exit 1

let generate_native_object_with produce_assembled =
  try
    let assembled, byte_emission = timed produce_assembled in
    let object_bytes, object_serialization =
      timed (fun () ->
        if Xic.Build_config.target = "x86_64-windows-gnu" then
          Xic.Object_coff.generate assembled
        else
          Xic.Object_elf64.generate assembled)
    in
    (object_bytes, byte_emission, object_serialization)
  with
  | Xic.Asm_x64.Unsupported msg ->
      prerr_endline ("xic: the x86_64 backend cannot compile this program yet: " ^ msg);
      prerr_endline "xic: retry with --emit=llvm, which still handles it, and please report the construct above.";
      exit 1
  | Xic.Asm_x64_obj.Unsupported msg ->
      prerr_endline ("xic: the direct x86_64 object emitter cannot encode: " ^ msg);
      exit 1

(* --check-lir remains useful after the single-sink move: it validates all
   identifiers, operands and block structure, then exercises the LLVM printer
   without invoking the assembler or linker. *)
let check_lir_report lir =
  (try Xic.Lir.validate lir with
   | Xic.Lir.Malformed msg ->
       Printf.printf "check-lir: INVALID LIR\n  %s\n" msg;
       exit 1);
  (try ignore (Xic.Lir.to_llvm_text lir) with
   | Xic.Lir.Malformed msg ->
       Printf.printf "check-lir: CANNOT PRINT\n  %s\n" msg;
       exit 1);
  print_string "check-lir: OK\n"

let usage =
  String.concat "\n"
    [ "usage: xic <input.xi> [options]";
      "";
      "Options:";
      "  -o, --output <path>  Set the executable or object output path.";
      "  --dump=<kinds>       Print comma-separated outputs: tokens, ast, ir, asm, c.";
      "  --emit <backend>     Code generator: bytes (default), asm, or llvm.";
      "  -c, --compile-only   Emit an object file without linking (bytes backend only).";
      "  --check             Parse and type-check without generating output.";
      "  --check-lir         Validate LIR and verify its LLVM rendering.";
      "  --check-obj         Encode and validate the native object without linking.";
      "  --diagnostics-json  Emit diagnostics as JSON (for editor tooling).";
      "  --stdin             Read source from stdin; input path remains its virtual filename.";
      "  --dmp               Keep generated .ll and inline-C intermediate files.";
      "  -I, --include-dir <path>  Add a native C header search directory.";
      "  -L, --library-dir <path>  Add a native library search directory.";
      "  -l, --library <name>      Link a named native library (like #link).";
      "  --link-file <path>        Link a specific .lib, .a, or object file.";
      "  --size              Link the exe for size (-Oz).";
      "  --speed             Link the exe for speed (-O2).";
      "  --gc=<auto|manual|off>  Select managed-heap collection mode (default: auto).";
      "  --stats             Print compilation timings, memory, and throughput.";
      "  -h, --help          Show this help text.";
      "  -v, --version       Show compiler version." ]

(* Map the Zig target triple (arch-os-abi) baked in at build time to the
   friendly os/arch form shown in --version, e.g. x86_64-windows-gnu ->
   windows/amd64, aarch64-windows-gnu -> windows/arm64. *)
let target_display triple =
  match String.split_on_char '-' triple with
  | arch :: os :: abi :: _ ->
      let arch =
        match arch with
        | "x86_64" -> "amd64"
        | "aarch64" -> "arm64"
        | other -> other
      in
      let platform = if os = "linux" then os ^ "-" ^ abi else os in
      platform ^ "/" ^ arch
  | _ -> triple

let version =
  Printf.sprintf "xi version %s %s %s" Xic.Build_config.xi_version
    (target_display Xic.Build_config.target) Xic.Build_config.preset

exception Cli_error of string

type backend = Native_bytes | Native_asm | Llvm

let () =
  (* Short-lived compiler process: trade heap slack for less major-GC marking
     work. The nursery is sized after the input is loaded, immediately before
     the allocation-heavy compiler phases begin. *)
  Gc.set { (Gc.get ()) with Gc.space_overhead = 200 };
  let native_asm_supported =
    Xic.Build_config.target = "x86_64-windows-gnu"
    || Xic.Build_config.target = "x86_64-linux-musl"
    || Xic.Build_config.target = "x86_64-linux-gnu"
  in
  let input = ref None in
  let output = ref None in
  let dump_tokens = ref false in
  let dump_ast = ref false in
  let dump_ast_expanded = ref false in
  let dump_ir = ref false in
  let dump_asm = ref false in
  let check_lir = ref false in
  let check_obj = ref false in
  (* Native bytes are the production backend. Native assembly and LLVM remain
     independent compiler/toolchain paths and therefore useful differential
     oracles rather than hidden implementation details of the byte emitter. *)
  let backend =
    ref (if native_asm_supported then Native_bytes else Llvm)
  in
  let dump_c = ref false in
  let check = ref false in
  let diagnostics_json = ref false in
  let read_stdin = ref false in
  let keep_ll = ref false in
  let compile_only = ref false in
  let stats = ref false in
  (* None distinguishes an omitted option (which may infer manual mode from an
     actual gc::* use) from an explicit --gc=auto override. *)
  let gc_mode = ref None in
  let include_dirs = ref [] in
  let library_dirs = ref [] in
  let extra_libraries = ref [] in
  let link_files = ref [] in
  (* Default opt level is baked in at build time by the preset (see build.ps1);
     --size / --speed override it per invocation. *)
  let size = ref Xic.Build_config.default_size in
  let select_dump kind =
    match kind with
    | "tokens" -> dump_tokens := true
    | "ast" -> dump_ast := true
    | "ast-expanded" -> dump_ast_expanded := true
    | "ir" -> dump_ir := true
    | "asm" ->
        if not native_asm_supported then
          raise
            (Cli_error
               ("native assembly emission is unavailable for target "
                ^ Xic.Build_config.target));
        dump_asm := true
    | "c" -> dump_c := true
    | "" -> raise (Cli_error "dump list must not be empty")
    | other -> raise (Cli_error ("unknown dump kind: " ^ other))
  in
  let select_dumps value =
    List.iter select_dump (String.split_on_char ',' value)
  in
  let rec parse = function
    | [] -> ()
    | ("-h" | "--help") :: _ -> print_endline usage; exit 0
    | ("-v" | "--version") :: _ -> print_endline version; exit 0
    | arg :: rest
      when String.length arg >= 7 && String.sub arg 0 7 = "--dump=" ->
        select_dumps (String.sub arg 7 (String.length arg - 7));
        parse rest
    (* Structural Lir validation and LLVM-printer check. *)
    | "--check-lir" :: rest -> check_lir := true; parse rest
    | "--check-obj" :: rest -> check_obj := true; parse rest
    | "--emit" :: ("llvm" | "ll") :: rest -> backend := Llvm; parse rest
    | "--emit" :: ("bytes" | "byte" | "native") :: rest ->
        if not native_asm_supported then
          raise
            (Cli_error
               ("native byte emission is unavailable for target "
                ^ Xic.Build_config.target ^ "; use llvm"));
        backend := Native_bytes; parse rest
    | "--emit" :: ("asm" | "s") :: rest ->
        if not native_asm_supported then
          raise
            (Cli_error
               ("native assembly emission is unavailable for target "
                ^ Xic.Build_config.target ^ "; use llvm"));
        backend := Native_asm; parse rest
    | "--emit" :: other :: _ -> raise (Cli_error ("unknown backend: " ^ other))
    | "--emit" :: [] -> raise (Cli_error "expected backend after --emit")
    | "--emit=llvm" :: rest | "--emit=ll" :: rest -> backend := Llvm; parse rest
    | "--emit=bytes" :: rest | "--emit=byte" :: rest
    | "--emit=native" :: rest ->
        if not native_asm_supported then
          raise
            (Cli_error
               ("native byte emission is unavailable for target "
                ^ Xic.Build_config.target ^ "; use llvm"));
        backend := Native_bytes; parse rest
    | "--emit=asm" :: rest | "--emit=s" :: rest ->
        if not native_asm_supported then
          raise
            (Cli_error
               ("native assembly emission is unavailable for target "
                ^ Xic.Build_config.target ^ "; use llvm"));
        backend := Native_asm; parse rest
    | ("-c" | "--compile-only") :: rest ->
        compile_only := true; parse rest
    | "--check" :: rest -> check := true; parse rest
    | "--diagnostics-json" :: rest -> diagnostics_json := true; parse rest
    | "--stdin" :: rest -> read_stdin := true; parse rest
    | "--dmp" :: rest -> keep_ll := true; parse rest
    | "--stats" :: rest -> stats := true; parse rest
    | "--gc" :: mode :: rest ->
        gc_mode := Some
          (match mode with
           | "auto" -> 0
           | "manual" -> 1
           | "off" -> 2
           | _ -> raise (Cli_error ("unknown GC mode: " ^ mode)));
        parse rest
    | "--gc" :: [] -> raise (Cli_error "expected auto, manual, or off after --gc")
    | arg :: rest
      when String.length arg >= 5 && String.sub arg 0 5 = "--gc=" ->
        let mode = String.sub arg 5 (String.length arg - 5) in
        gc_mode := Some
          (match mode with
           | "auto" -> 0
           | "manual" -> 1
           | "off" -> 2
           | _ -> raise (Cli_error ("unknown GC mode: " ^ mode)));
        parse rest
    | "--size" :: rest -> size := true; parse rest
    | "--speed" :: rest -> size := false; parse rest
    | ("-o" | "--output") :: path :: rest ->
        if String.length path > 0 && path.[0] = '-' then
          raise (Cli_error "expected path after output option");
        output := Some path;
        parse rest
    | ("-o" | "--output") :: [] -> raise (Cli_error "expected path after output option")
    | ("-I" | "--include-dir") :: path :: rest ->
        include_dirs := !include_dirs @ [ path ]; parse rest
    | ("-I" | "--include-dir") :: [] ->
        raise (Cli_error "expected path after include-directory option")
    | ("-L" | "--library-dir") :: path :: rest ->
        library_dirs := !library_dirs @ [ path ]; parse rest
    | ("-L" | "--library-dir") :: [] ->
        raise (Cli_error "expected path after library-directory option")
    | ("-l" | "--library") :: name :: rest ->
        extra_libraries := !extra_libraries @ [ name ]; parse rest
    | ("-l" | "--library") :: [] ->
        raise (Cli_error "expected name after library option")
    | "--link-file" :: path :: rest ->
        link_files := !link_files @ [ path ]; parse rest
    | "--link-file" :: [] ->
        raise (Cli_error "expected path after link-file option")
    | arg :: rest when String.length arg > 2 && String.sub arg 0 2 = "-I" ->
        include_dirs := !include_dirs @ [ String.sub arg 2 (String.length arg - 2) ];
        parse rest
    | arg :: rest when String.length arg > 2 && String.sub arg 0 2 = "-L" ->
        library_dirs := !library_dirs @ [ String.sub arg 2 (String.length arg - 2) ];
        parse rest
    | arg :: rest when String.length arg > 2 && String.sub arg 0 2 = "-l" ->
        extra_libraries := !extra_libraries @ [ String.sub arg 2 (String.length arg - 2) ];
        parse rest
    | arg :: rest ->
        if String.length arg > 0 && arg.[0] = '-' then
          raise (Cli_error ("unknown option: " ^ arg))
        else if !input = None then begin
          input := Some arg;
          parse rest
        end else
          raise (Cli_error ("unexpected extra input: " ^ arg))
  in
  (try parse (List.tl (Array.to_list Sys.argv)) with
   | Cli_error msg ->
       prerr_endline ("xic: " ^ msg);
       prerr_endline usage;
       exit 1);
  match !input with
  | None ->
      prerr_endline usage;
      exit 1
  | Some file ->
      let end_to_end_started = now () in
      Xic.Lexer.set_collect_token_widths !stats;
      Xic.Lexer.reset_metrics ();
      let source =
        if !read_stdin then In_channel.input_all stdin
        else In_channel.with_open_bin file In_channel.input_all
      in
      (* Start the serial front end with an empty, wide nursery. Besides avoiding
         collections at the short phase boundaries, changing the size here keeps
         module-initialisation and input-buffer allocations from consuming part
         of the lexer budget. Lowering right-sizes it again below before domains
         begin sharing collection work. *)
      Gc.set { (Gc.get ()) with Gc.minor_heap_size = 1048576 };
      if !dump_ast_expanded then begin
        (* Parser output AFTER source-include expansion: the differential oracle
           for the PowerShell Module_loader port. *)
        let tokens, lex_diags = Xic.Lexer.lex source in
        if lex_diags <> [] then begin
          let diags =
            List.map
              (fun (d : Xic.Lexer.diagnostic) ->
                Xic.Diagnostic.make d.code d.message d.span)
              lex_diags
          in
          prerr_endline (render_diagnostics file source diags);
          exit 1
        end
        else
          match Xic.Parser.parse tokens with
          | Error diags -> prerr_endline (render_diagnostics file source diags); exit 1
          | Ok program -> (
              match Xic.Module_loader.expand_source_includes program file with
              | Error diags -> prerr_endline (render_diagnostics file source diags); exit 1
              | Ok expanded ->
                  print_string (Xic.Ast_dump.dump expanded);
                  print_newline ())
      end
      else begin
        (* Dumps are deliberately emitted in compiler-pipeline order rather than
           option order. A single-kind dump retains its old byte-for-byte stdout
           format, while a comma-separated request can inspect several stages in
           one compiler invocation. *)
        if !dump_tokens then begin
          let tokens, _ = Xic.Lexer.lex source in
          print_string (Xic.Json_out.tokens_to_json tokens);
          print_newline ()
        end;
        if !dump_ast then begin
          (* Raw parser output (pre-include-expansion) as an S-expression: the
             differential oracle for the PowerShell parser port. *)
          let tokens, lex_diags = Xic.Lexer.lex source in
          if lex_diags <> [] then begin
            let diags =
              List.map
                (fun (d : Xic.Lexer.diagnostic) ->
                  Xic.Diagnostic.make d.code d.message d.span)
                lex_diags
            in
            prerr_endline (render_diagnostics file source diags);
            exit 1
          end
          else
            match Xic.Parser.parse tokens with
            | Error diags ->
                prerr_endline (render_diagnostics file source diags);
                exit 1
            | Ok program ->
                print_string (Xic.Ast_dump.dump program);
                print_newline ()
        end;
        let needs_analyzed_output =
          !dump_ir || !dump_asm || !dump_c || !check || !check_lir
          || !check_obj || !compile_only
          || not (!dump_tokens || !dump_ast)
        in
        if not needs_analyzed_output then exit 0;
        if !dump_tokens || !dump_ast then Xic.Lexer.reset_metrics ();
        match analyze_source ~measure_details:!stats file source with
        | Error (diags, _, _) ->
            if !diagnostics_json then begin
              print_endline (Xic.Json_out.diagnostics_to_json diags)
            end
            else prerr_endline (render_diagnostics file source diags);
            exit 1
        | Ok (analyzed, parse_timings, semantic_time) ->
            let parse_duration = parse_time parse_timings in
            let ast_optimization_duration = optimization_time parse_timings in
            let program, sem = analyzed in
            let gc_included =
              List.exists
                (fun (inc : Xic.Ast.include_decl) -> inc.module_ = "gc")
                program.Xic.Ast.includes
            in
            (* gc::* is the manual-control surface. An explicit auto request is
               an override, not an invitation to compile contradictory source. *)
            if !gc_mode = Some 0 && gc_included && sem.Xic.Semantic.uses_gc then begin
              let span = List.hd sem.Xic.Semantic.gc_use_spans in
              let diag =
                Xic.Diagnostic.make "E541"
                  "`gc::*` operations require manual GC; remove `--gc=auto` or pass `--gc=manual`"
                  span
              in
              if !diagnostics_json then
                print_endline (Xic.Json_out.diagnostics_to_json [ diag ])
              else prerr_endline (render_diagnostics file source [ diag ]);
              exit 1
            end;
            let source_functions = List.length program.Xic.Ast.functions in
            (* Inline-C definitions are callable program functions too. Extern
               and intrinsic declarations do not contribute implementations. *)
            let function_count =
              source_functions + List.length program.Xic.Ast.c_functions
            in
            if !check then begin
              if !diagnostics_json then print_endline "[]";
              if !stats then begin
                let source_lines, source_bytes, tokens, tokenization_time =
                  Xic.Lexer.metrics ()
                in
                let mean_token_bytes = Xic.Lexer.mean_token_bytes () in
                let median_token_bytes = Xic.Lexer.median_token_bytes () in
                let total =
                  tokenization_time +. parse_duration
                  +. ast_optimization_duration
                  +. semantic_time
                in
                let rate count seconds =
                  if seconds <= 0. then 0.
                  else float_of_int count /. seconds
                in
                Printf.eprintf
                  "\nStatistics\n----------\nSource lines       %12d\nSource bytes       %12d\nTokens             %12d\nFunctions          %12d\nMean bytes/token   %12.1f\nMedian bytes/token %12.1f\nTokenization       %12.3f ms\nParse & expansion  %12.3f ms\n"
                  source_lines source_bytes tokens function_count mean_token_bytes
                  median_token_bytes
                  (tokenization_time *. 1000.) (parse_duration *. 1000.);
                print_parse_timings parse_timings;
                Printf.eprintf
                  "Optimization       %12.3f ms\n"
                  (ast_optimization_duration *. 1000.);
                print_optimization_timings parse_timings;
                Printf.eprintf
                  "Semantic analysis  %12.3f ms\n----------\nMeasured total     %12.3f ms\nEnd-to-end         %12.3f ms\n\nTokenizer throughput\nTokens/sec          %12.0f\nBytes/sec           %12.0f\n"
                  (semantic_time *. 1000.)
                  (total *. 1000.) ((now () -. end_to_end_started) *. 1000.)
                  (rate tokens tokenization_time)
                  (rate source_bytes tokenization_time);
                print_memory_stats ()
              end
            end
            else begin
              (* With the byte backend, a function's machine code depends on
                 nothing but the function itself, so rendering can start as soon
                 as lowering finishes each one rather than waiting for the whole
                 module. Hand every completed function to a small domain pool and
                 the bulk of byte emission disappears behind lowering. The modes
                 that never ask for an object skip the pool entirely.

                 The source's function count is only an estimate of the module's
                 -- lowering adds closure helpers and adapters -- but it is known
                 before lowering starts, which is when the pool has to exist, and
                 [stream_is_profitable]'s bounds are nowhere near tight enough
                 for the difference to matter. *)
              let assembly_stream =
                if !backend = Native_bytes
                   && not (!check_lir || !dump_ir || !dump_asm || !dump_c)
                   (* Detailed stats use the profiled batch renderer so native
                      planning and emission form non-overlapping wall-time
                      phases. The production path remains streamed. *)
                   && not !stats
                   && Xic.Asm_x64.stream_is_profitable ~functions:source_functions
                then
                  Some
                    (Xic.Asm_x64.start_assembly_stream
                       ~functions:source_functions ())
                else None
              in
              let on_func =
                Option.map
                  (fun stream func -> Xic.Asm_x64.stream_func stream func)
                  assembly_stream
              in
              (* Lowering and byte emission allocate an order of magnitude more
                 than the phases before them and promote a third of it into the
                 Lir structure. A 512k-word nursery is the knee here: it limits
                 promotion-heavy collections without making every participating
                 domain scan the wider nursery used to carry the serial front end
                 across its short phase boundaries. *)
              Gc.set { (Gc.get ()) with Gc.minor_heap_size = 524288 };
              let (lir, libraries, subsystem, inline_c, inline_c_include_dirs),
                  lowering_timings =
                let selected_gc_mode =
                  Option.value !gc_mode
                    ~default:(if gc_included && sem.Xic.Semantic.uses_gc then 1 else 0)
                in
                compile_analyzed ?on_func ~gc_mode:selected_gc_mode analyzed
              in
              let backend_emission_time = ref 0. in
              let native_planning_time = ref 0. in
              let object_serialization_time = ref 0. in
              let optimization_duration () =
                ast_optimization_duration
                +. lowering_timings.native_i64_promotion
                +. lowering_timings.lir_optimization
                +. !native_planning_time
              in
              let lowering_base_time =
                lowering_timings.ir_preparation
                +. lowering_timings.build_metadata
                +. lowering_timings.lir_lowering
                +. lowering_timings.inline_c_emission
              in
              let lowering_time () =
                lowering_base_time +. !backend_emission_time
                +. !object_serialization_time
              in
              let generated_ir = ref None in
              let get_ir () =
                match !generated_ir with
                | Some ir -> ir
                | None ->
                    let ir, elapsed = timed (fun () -> Xic.Lir.to_llvm_text lir) in
                    backend_emission_time := !backend_emission_time +. elapsed;
                    generated_ir := Some ir;
                    ir
              in
              let generated_asm = ref None in
              let get_asm () =
                match !generated_asm with
                | Some asm -> asm
                | None ->
                    let (asm, planning_share), elapsed =
                      if !stats then
                        timed (fun () ->
                          generate_asm_with_optimization_share lir)
                      else
                        let asm, elapsed =
                          timed (fun () -> generate_asm lir)
                        in
                        ((asm, 0.), elapsed)
                    in
                    let planning = elapsed *. planning_share in
                    native_planning_time :=
                      !native_planning_time +. planning;
                    backend_emission_time :=
                      !backend_emission_time +. elapsed -. planning;
                    generated_asm := Some asm;
                    asm
              in
              let generated_object = ref None in
              let get_object () =
                match !generated_object with
                | Some obj -> obj
                | None ->
                    let planning_share = ref 0. in
                    let produce () =
                      match assembly_stream with
                      | Some stream ->
                          (* Most fragments are already rendered; this drains
                             whatever is still in flight and merges them. *)
                          Xic.Asm_x64.finish_assembly_stream stream
                            lir.Xic.Lir.globals
                      | None ->
                          if !stats then begin
                            let assembled, share =
                              Xic.Asm_x64.generate_assembled_with_optimization_share
                                lir
                            in
                            planning_share := share;
                            assembled
                          end
                          else Xic.Asm_x64.generate_assembled lir
                    in
                    let obj, emission, serialization =
                      generate_native_object_with produce
                    in
                    let planning = emission *. !planning_share in
                    native_planning_time :=
                      !native_planning_time +. planning;
                    backend_emission_time :=
                      !backend_emission_time +. emission -. planning;
                    object_serialization_time :=
                      !object_serialization_time +. serialization;
                    generated_object := Some obj;
                    obj
              in
              (* print_newline, not print_string: the reference dumps the IR with
                 println!, so its stdout carries one more newline than the .ll it
                 writes to disk. The file content is the same either way; this
                 only matters because --dump=ir stdout is what the differential
                 oracles compare. *)
              if !check_lir then check_lir_report lir
              else if !check_obj then begin
                ignore (get_object ());
                print_endline "check-obj: OK";
                if !stats then begin
                  let source_lines, source_bytes, tokens, tokenization_time =
                    Xic.Lexer.metrics ()
                  in
                  let mean_token_bytes = Xic.Lexer.mean_token_bytes () in
                  let median_token_bytes = Xic.Lexer.median_token_bytes () in
                  let compiler_time =
                    tokenization_time +. parse_duration
                    +. optimization_duration () +. semantic_time
                    +. lowering_time ()
                  in
                  let rate count seconds =
                    if seconds <= 0. then 0.
                    else float_of_int count /. seconds
                  in
                  Printf.eprintf
                    "\nStatistics\n----------\nSource lines       %12d\nSource bytes       %12d\nTokens             %12d\nFunctions          %12d\nMean bytes/token   %12.1f\nMedian bytes/token %12.1f\nTokenization       %12.3f ms\nParse & expansion  %12.3f ms\nOptimization       %12.3f ms\n"
                    source_lines source_bytes tokens function_count mean_token_bytes
                    median_token_bytes
                    (tokenization_time *. 1000.) (parse_duration *. 1000.)
                    (optimization_duration () *. 1000.);
                  print_optimization_timings
                    ~lir_optimization:lowering_timings.lir_optimization
                    ~native_i64_promotion:
                      lowering_timings.native_i64_promotion
                    ~native_planning:
                      (!native_planning_time, "native byte emission")
                    parse_timings;
                  Printf.eprintf
                    "Semantic analysis  %12.3f ms\nLowering           %12.3f ms\n"
                    (semantic_time *. 1000.) (lowering_base_time *. 1000.);
                  print_timing_child "├─" "IR preparation"
                    lowering_timings.ir_preparation;
                  print_timing_child "├─" "Build metadata"
                    lowering_timings.build_metadata;
                  print_timing_child "├─" "LIR lowering"
                    lowering_timings.lir_lowering;
                  print_lir_lowering_timings lowering_timings;
                  print_timing_child "└─" "Inline-C emission"
                    lowering_timings.inline_c_emission;
                  Printf.eprintf
                    "Native byte emission %10.3f ms\nObject serialization %9.3f ms\n----------\nCompiler core       %12.3f ms\nEnd-to-end          %12.3f ms\n\nTokenizer throughput\nTokens/sec          %12.0f\nBytes/sec           %12.0f\n\nCompiler core throughput\nLines/sec           %12.0f\nTokens/sec          %12.0f\nBytes/sec           %12.0f\n"
                    (!backend_emission_time *. 1000.)
                    (!object_serialization_time *. 1000.)
                    (compiler_time *. 1000.)
                    ((now () -. end_to_end_started) *. 1000.)
                    (rate tokens tokenization_time)
                    (rate source_bytes tokenization_time)
                    (rate source_lines compiler_time)
                    (rate tokens compiler_time)
                    (rate source_bytes compiler_time);
                  print_memory_stats ()
                end
              end
              else if !dump_ir || !dump_asm || !dump_c then begin
                if !dump_ir then (print_string (get_ir ()); print_newline ());
                if !dump_asm then print_string (get_asm ());
                if !dump_c then
                  (match inline_c with
                   | Some source -> print_string source
                   | None -> ())
              end
              else begin
              let paths = Xic.Link.build_paths file !output in
              if !compile_only then begin
                if !backend <> Native_bytes then begin
                  prerr_endline
                    "xic: --compile-only is only supported with --emit=bytes";
                  exit 1
                end;
                if Option.is_some inline_c then begin
                  prerr_endline
                    "xic: --compile-only does not yet support sources with inline C";
                  exit 1
                end;
                Xic.Link.write_bytes
                  (Xic.Link.object_output_path file !output)
                  (get_object ())
              end
              else match Xic.Link.require_toolchain () with
              | Error msg -> prerr_endline msg; exit 1
              | Ok () ->
                  (* Whichever backend is selected, its output is the one file handed
                     to the toolchain; the rest of the link is identical. *)
                  let code_path, code_object =
                    match !backend with
                    | Native_bytes ->
                        (paths.Xic.Link.s_path, Some get_object)
                    | Native_asm ->
                        Xic.Link.write_file paths.Xic.Link.s_path (get_asm ());
                        (paths.Xic.Link.s_path, None)
                    | Llvm ->
                        Xic.Link.write_ir paths.Xic.Link.ll_path (get_ir ());
                        (paths.Xic.Link.ll_path, None)
                  in
                  match Xic.Link.build_native
                          ~libraries:(libraries @ !extra_libraries)
                          ~library_dirs:!library_dirs ~link_files:!link_files
                          ?subsystem ~size:!size ?inline_c
                          ~inline_c_include_dirs:(inline_c_include_dirs @ !include_dirs)
                          ?code_object ~keep_intermediates:!keep_ll ~code_path paths with
                  | Ok link_timings ->
                      if not !keep_ll then Xic.Link.remove_if_exists code_path;
                      if !stats then begin
                        let source_lines, source_bytes, tokens,
                            tokenization_time =
                          Xic.Lexer.metrics ()
                        in
                        let mean_token_bytes = Xic.Lexer.mean_token_bytes () in
                        let median_token_bytes = Xic.Lexer.median_token_bytes () in
                        (* Native byte emission is deferred into [generation],
                           where it overlaps inline-C compilation. Do not add it
                           again when constructing the critical-path total. Text
                           assembly and LLVM rendering still precede generation. *)
                        let serial_backend_emission =
                          match !backend with
                          | Native_bytes -> 0.
                          | Native_asm | Llvm -> !backend_emission_time
                        in
                        let generation_without_internal_optimization =
                          match !backend with
                          | Native_bytes ->
                              max 0.
                                (link_timings.Xic.Link.generation
                                 -. !native_planning_time)
                          | Native_asm | Llvm ->
                              link_timings.Xic.Link.generation
                        in
                        let measured_total =
                          tokenization_time +. parse_duration
                          +. optimization_duration () +. semantic_time
                          +. lowering_base_time +. serial_backend_emission
                          +. generation_without_internal_optimization
                          +. link_timings.Xic.Link.linking
                        in
                        let compiler_time =
                          tokenization_time +. parse_duration
                          +. optimization_duration () +. semantic_time
                          +. lowering_time ()
                        in
                        let end_to_end = now () -. end_to_end_started in
                        let rate count seconds =
                          if seconds <= 0. then 0.
                          else float_of_int count /. seconds
                        in
                        Printf.eprintf
                          "\nStatistics\n----------\nSource lines       %12d\nSource bytes       %12d\nTokens             %12d\nFunctions          %12d\nMean bytes/token   %12.1f\nMedian bytes/token %12.1f\n\nPhase timings\nTokenization       %12.3f ms\nParse & expansion  %12.3f ms\n"
                          source_lines source_bytes tokens function_count
                          mean_token_bytes
                          median_token_bytes
                          (tokenization_time *. 1000.) (parse_duration *. 1000.);
                        print_parse_timings parse_timings;
                        Printf.eprintf
                          "Optimization       %12.3f ms\n"
                          (optimization_duration () *. 1000.);
                        let native_planning =
                          match !backend with
                          | Native_bytes ->
                              Some
                                (!native_planning_time,
                                 "native byte emission")
                          | Native_asm ->
                              Some
                                (!native_planning_time,
                                 "native assembly emission")
                          | Llvm -> None
                        in
                        let external_pipelines =
                          (match !backend with
                           | Llvm ->
                               [ (if !size then "LLVM -Oz pipeline"
                                  else "LLVM -O2 pipeline"),
                                 "LLVM to object" ]
                           | Native_bytes | Native_asm -> [])
                          @
                          (match inline_c with
                           | None -> []
                           | Some _ ->
                               [ (if !size then "Inline-C -Oz pipeline"
                                  else "Inline-C -O2 pipeline"),
                                 "C compilation" ])
                        in
                        print_optimization_timings
                          ?native_planning
                          ~external_pipelines
                          ~lir_optimization:lowering_timings.lir_optimization
                          ~native_i64_promotion:
                            lowering_timings.native_i64_promotion
                          parse_timings;
                        Printf.eprintf
                          "Semantic analysis  %12.3f ms\nLowering           %12.3f ms\n"
                          (semantic_time *. 1000.) (lowering_base_time *. 1000.);
                        print_timing_child "├─" "IR preparation"
                          lowering_timings.ir_preparation;
                        print_timing_child "├─" "Build metadata"
                          lowering_timings.build_metadata;
                        print_timing_child "├─" "LIR lowering"
                          lowering_timings.lir_lowering;
                        print_lir_lowering_timings lowering_timings;
                        print_timing_child "└─" "Inline-C emission"
                          lowering_timings.inline_c_emission;
                        let emission_label, object_label =
                          match !backend with
                          | Native_bytes ->
                              ("Native byte emission", "Object write")
                          | Native_asm ->
                              ("Native assembly emission", "Assembly to object")
                          | Llvm ->
                              ("LLVM IR emission", "LLVM to object")
                        in
                        let emission_overlap =
                          match !backend, inline_c with
                          | Native_bytes, Some _ -> " (overlapped)"
                          | _ -> ""
                        in
                        Printf.eprintf "%-23s %12.3f ms%s\n"
                          emission_label (!backend_emission_time *. 1000.)
                          emission_overlap;
                        (match !backend with
                         | Native_bytes ->
                             Printf.eprintf
                               "%-23s %12.3f ms%s\n"
                               "Object serialization"
                               (!object_serialization_time *. 1000.)
                               emission_overlap
                         | Native_asm | Llvm -> ());
                        Printf.eprintf "%-23s %12.3f ms\n"
                          object_label
                          (link_timings.Xic.Link.object_generation *. 1000.);
                        (match link_timings.Xic.Link.c_compilation with
                         | Some seconds ->
                             Printf.eprintf "C compilation       %12.3f ms\n"
                               (seconds *. 1000.)
                         | None ->
                             Printf.eprintf "C compilation       %12s\n" "n/a");
                        let generation_label =
                          match !backend with
                          | Native_bytes -> "Generation excl opt"
                          | Native_asm | Llvm -> "Generation wall"
                        in
                        Printf.eprintf "%-23s %12.3f ms\n"
                          generation_label
                          (generation_without_internal_optimization *. 1000.);
                        Printf.eprintf
                          "Linking             %12.3f ms\n----------\nCompiler core       %12.3f ms\nMeasured total      %12.3f ms\nEnd-to-end          %12.3f ms\n\nTokenizer throughput\nTokens/sec          %12.0f\nBytes/sec           %12.0f\n\nCompiler core throughput\nLines/sec           %12.0f\nTokens/sec          %12.0f\nBytes/sec           %12.0f\n\nEnd-to-end throughput\nLines/sec           %12.0f\n"
                          (link_timings.Xic.Link.linking *. 1000.)
                          (compiler_time *. 1000.) (measured_total *. 1000.)
                          (end_to_end *. 1000.)
                          (rate tokens tokenization_time)
                          (rate source_bytes tokenization_time)
                          (rate source_lines compiler_time)
                          (rate tokens compiler_time)
                          (rate source_bytes compiler_time)
                          (rate source_lines end_to_end);
                        print_memory_stats ()
                      end
                  | Error msg -> prerr_endline msg; exit 1
              end
            end
      end
