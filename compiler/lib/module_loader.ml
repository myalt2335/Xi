module StringSet = Set.Make (String)

type expansion_timings =
  { enabled : bool
  ; mutable source_loading : float
  ; mutable include_tokenization : float
  ; mutable include_parsing : float
  ; mutable source_annotation : float
  ; mutable namespace_rewriting : float
  ; mutable bookkeeping : float
  }

let empty_expansion_timings enabled =
  { enabled
  ; source_loading = 0.
  ; include_tokenization = 0.
  ; include_parsing = 0.
  ; source_annotation = 0.
  ; namespace_rewriting = 0.
  ; bookkeeping = 0.
  }

let timed_accumulate enabled get set f =
  if not enabled then f ()
  else
    let started = Clock.now () in
    let result = f () in
    set (get () +. (Clock.now () -. started));
    result

let user_modules_dir () =
  match Sys.getenv_opt "LOCALAPPDATA" with
  | Some d -> Some (Filename.concat (Filename.concat d "Xi") "modules")
  | None -> (
      match Sys.getenv_opt "XDG_DATA_HOME" with
      | Some d -> Some (Filename.concat (Filename.concat d "xi") "modules")
      | None -> (
          match Sys.getenv_opt "HOME" with
          | Some h ->
              Some (Filename.concat h (Filename.concat ".local/share" "xi/modules"))
          | None -> None))

let read_file path =
  try
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    Some s
  with Sys_error _ -> None

let annotate_source_origins (program : Ast.program) path source =
  if program.c_preambles = [] && program.c_functions = [] then program
  else begin
    let cursor = ref 0 in
    let line = ref 1 in
    let line_at offset =
      let stop = min offset (String.length source) in
      while !cursor < stop do
        if source.[!cursor] = '\n' then incr line;
        incr cursor
      done;
      !line
    in
    let rec annotate preambles functions preamble_out function_out =
      match preambles, functions with
      | [], [] -> List.rev preamble_out, List.rev function_out
      | (p : Ast.c_preamble_decl) :: rest, [] ->
          let p =
            { p with
              origin = Some path
            ; line = Some (line_at p.body_offset)
            }
          in
          annotate rest functions (p :: preamble_out) function_out
      | (p : Ast.c_preamble_decl) :: preamble_rest,
        ((f : Ast.inline_c_fn_decl) :: _ as functions)
        when p.body_offset <= f.body_offset ->
          let p =
            { p with
              origin = Some path
            ; line = Some (line_at p.body_offset)
            }
          in
          annotate preamble_rest functions (p :: preamble_out) function_out
      | preambles, (f : Ast.inline_c_fn_decl) :: rest ->
          let f =
            { f with
              origin = Some path
            ; line = Some (line_at f.body_offset)
            }
          in
          annotate preambles rest preamble_out (f :: function_out)
    in
    let c_preambles, c_functions =
      annotate program.c_preambles program.c_functions [] []
    in
    { program with c_preambles; c_functions }
  end

let dirname path =
  let dir = Filename.dirname path in
  if dir = "." then Sys.getcwd () else dir

let has_substring s needle =
  let len = String.length s and nlen = String.length needle in
  let rec loop i =
    i + nlen <= len
    && (String.sub s i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let normalize_slashes s =
  String.map (fun ch -> if ch = '\\' then '/' else ch) s

let is_source_include name =
  let n = normalize_slashes name in
  Filename.check_suffix n ".xi"
  || has_substring n "/"
  || (String.length n > 0 && n.[0] = '.')
  || has_substring n ":"

let resolve_source_include base_dir name =
  let n = normalize_slashes name in
  let path =
    if Filename.is_relative n then Filename.concat base_dir n else n
  in
  if Filename.check_suffix path ".xi" then path else path ^ ".xi"

let source_include_namespace name =
  let n = normalize_slashes name in
  let stem =
    let base = Filename.basename n in
    try Filename.remove_extension base with Invalid_argument _ -> base
  in
  let b = Bytes.of_string stem in
  Bytes.iteri
    (fun i ch ->
      let ok =
        (ch >= 'a' && ch <= 'z')
        || (ch >= 'A' && ch <= 'Z')
        || (ch >= '0' && ch <= '9')
        || ch = '_'
      in
      if not ok then Bytes.set b i '_')
    b;
  Bytes.to_string b

let load_source name =
  match user_modules_dir () with
  | Some dir ->
      let path = Filename.concat dir (name ^ ".xi") in
      if Sys.file_exists path then read_file path else Std_embed.find name
  | None -> Std_embed.find name

let load_source_with_path name =
  match user_modules_dir () with
  | Some dir ->
      let path = Filename.concat dir (name ^ ".xi") in
      if Sys.file_exists path then
        Option.map (fun source -> (path, source)) (read_file path)
      else
        Option.map
          (fun source -> ("<embedded:" ^ name ^ ">", source))
          (Std_embed.find name)
  | None ->
      Option.map
        (fun source -> ("<embedded:" ^ name ^ ">", source))
        (Std_embed.find name)

let parsed_module_cache : (string, string * string option * Ast.program) Hashtbl.t =
  Hashtbl.create 16

let cache_parsed_module ?path name source ast =
  Hashtbl.replace parsed_module_cache name (source, path, ast)

let find_cached_module ?path name source =
  match Hashtbl.find_opt parsed_module_cache name with
  | Some (cached_source, cached_path, ast)
    when (cached_source == source || cached_source = source)
         &&
         (match path with
          | None -> true
          | Some path -> cached_path = Some path) ->
      Some ast
  | _ -> None

let load name : Ast.program option =
  match load_source name with
  | None -> None
  | Some src ->
      (match find_cached_module name src with
       | Some ast -> Some ast
       | _ ->
           let tokens, lex_diags = Lexer.lex src in
           if lex_diags <> [] then None
           else
             match Parser.parse tokens with
             | Ok ast ->
                 cache_parsed_module name src ast;
                 Some ast
             | Error _ -> None)

let parse_include_source timings (include_ : Ast.include_decl) path source diagnostics =
  let tokens, lex_diags =
    timed_accumulate
      timings.enabled
      (fun () -> timings.include_tokenization)
      (fun value -> timings.include_tokenization <- value)
      (fun () -> Lexer.lex source)
  in
  if lex_diags <> [] then begin
    diagnostics :=
      Diagnostic.make "E488"
        (Printf.sprintf "source include `%s` failed to tokenize" path)
        include_.span
      :: !diagnostics;
    None
  end
  else
    match
      timed_accumulate
        timings.enabled
        (fun () -> timings.include_parsing)
        (fun value -> timings.include_parsing <- value)
        (fun () -> Parser.parse tokens)
    with
    | Ok ast ->
        Some
          (timed_accumulate
             timings.enabled
             (fun () -> timings.source_annotation)
             (fun value -> timings.source_annotation <- value)
             (fun () ->
               annotate_source_origins ast path source))
    | Error _ ->
        diagnostics :=
          Diagnostic.make "E489"
            (Printf.sprintf "source include `%s` failed to parse" path)
            include_.span
          :: !diagnostics;
        None

let rec namespace_stmt namespace local_functions scopes (stmt : Ast.stmt) =
  match stmt with
  | Ast.VarDecl decl ->
      let init = Option.map (namespace_expr namespace local_functions scopes) decl.init in
      let scopes =
        match scopes with
        | scope :: rest -> StringSet.add decl.name scope :: rest
        | [] -> [ StringSet.singleton decl.name ]
      in
      ({ decl with init } |> fun d -> Ast.VarDecl d), scopes
  | Ast.Assign assign ->
      let value = namespace_expr namespace local_functions scopes assign.value in
      Ast.Assign { assign with value }, scopes
  | Ast.Return ret ->
      Ast.Return { ret with value = Option.map (namespace_expr namespace local_functions scopes) ret.value }, scopes
  | Ast.Break _ | Ast.Continue _ -> stmt, scopes
  | Ast.If if_ ->
      let cond = namespace_expr namespace local_functions scopes if_.cond in
      let then_branch, _ = namespace_stmt namespace local_functions scopes if_.then_branch in
      let else_branch =
        Option.map
          (fun branch -> fst (namespace_stmt namespace local_functions scopes branch))
          if_.else_branch
      in
      Ast.If { if_ with cond; then_branch; else_branch }, scopes
  | Ast.Switch sw ->
      let selector = namespace_expr namespace local_functions scopes sw.selector in
      let cases =
        List.map
          (fun (case : Ast.switch_case) ->
            let value = namespace_expr namespace local_functions scopes case.value in
            let body, _ = namespace_stmt_list namespace local_functions (StringSet.empty :: scopes) case.body in
            { case with value; body })
          sw.cases
      in
      let default =
        Option.map
          (fun (d : Ast.switch_default) ->
            let body, _ = namespace_stmt_list namespace local_functions (StringSet.empty :: scopes) d.body in
            { d with body })
          sw.default
      in
      Ast.Switch { sw with selector; cases; default }, scopes
  | Ast.While while_ ->
      let cond = namespace_expr namespace local_functions scopes while_.cond in
      let body, _ = namespace_stmt namespace local_functions scopes while_.body in
      Ast.While { while_ with cond; body }, scopes
  | Ast.For for_ ->
      let inner_scopes = StringSet.empty :: scopes in
      let init, inner_scopes =
        match for_.init with
        | None -> None, inner_scopes
        | Some (Ast.ForInitVarDecl decl) ->
            let init = Option.map (namespace_expr namespace local_functions inner_scopes) decl.init in
            let inner_scopes =
              match inner_scopes with
              | scope :: rest -> StringSet.add decl.name scope :: rest
              | [] -> [ StringSet.singleton decl.name ]
            in
            Some (Ast.ForInitVarDecl { decl with init }), inner_scopes
        | Some (Ast.ForInitAssign assign) ->
            let value = namespace_expr namespace local_functions inner_scopes assign.value in
            Some (Ast.ForInitAssign { assign with value }), inner_scopes
        | Some (Ast.ForInitExpr expr) ->
            Some (Ast.ForInitExpr (namespace_expr namespace local_functions inner_scopes expr)), inner_scopes
      in
      let cond = Option.map (namespace_expr namespace local_functions inner_scopes) for_.cond in
      let step =
        Option.map
          (function
            | Ast.ForStepAssign assign ->
                Ast.ForStepAssign { assign with value = namespace_expr namespace local_functions inner_scopes assign.value }
            | Ast.ForStepExpr expr -> Ast.ForStepExpr (namespace_expr namespace local_functions inner_scopes expr))
          for_.step
      in
      let body, _ = namespace_stmt namespace local_functions inner_scopes for_.body in
      Ast.For { for_ with init; cond; step; body }, scopes
  | Ast.ForEach for_each ->
      let collection = namespace_expr namespace local_functions scopes for_each.collection in
      let inner_scopes = StringSet.singleton for_each.name :: scopes in
      let body, _ = namespace_stmt namespace local_functions inner_scopes for_each.body in
      Ast.ForEach { for_each with collection; body }, scopes
  | Ast.IoChain io ->
      let items =
        List.map
          (function
            | Ast.IoExpr expr -> Ast.IoExpr (namespace_expr namespace local_functions scopes expr)
            | Ast.Endl _ as endl -> endl)
          io.items
      in
      Ast.IoChain { io with items }, scopes
  | Ast.ExprStmt expr_stmt ->
      Ast.ExprStmt { expr_stmt with expr = namespace_expr namespace local_functions scopes expr_stmt.expr }, scopes
  | Ast.Block block ->
      let statements, _ = namespace_stmt_list namespace local_functions (StringSet.empty :: scopes) block.statements in
      Ast.Block { block with statements }, scopes

and namespace_stmt_list namespace local_functions scopes statements =
  List.fold_left
    (fun (out, scopes) stmt ->
      let stmt, scopes = namespace_stmt namespace local_functions scopes stmt in
      (stmt :: out, scopes))
    ([], scopes) statements
  |> fun (out, scopes) -> (List.rev out, scopes)

and namespace_expr namespace local_functions scopes (expr : Ast.expr) =
  let kind =
    match expr.kind with
    | Ast.Path path ->
        let segments =
          match path.segments with
          | [ name ]
            when StringSet.mem name local_functions
                 && not (List.exists (fun scope -> StringSet.mem name scope) scopes) ->
              [ namespace; name ]
          | segments -> segments
        in
        Ast.Path { path with segments }
    | Ast.Call call ->
        Ast.Call
          { callee = namespace_expr namespace local_functions scopes call.callee
          ; args = List.map (namespace_expr namespace local_functions scopes) call.args
          }
    | Ast.Index index ->
        Ast.Index
          { base = namespace_expr namespace local_functions scopes index.base
          ; index = namespace_expr namespace local_functions scopes index.index
          }
    | Ast.Member member ->
        Ast.Member { member with base = namespace_expr namespace local_functions scopes member.base }
    | Ast.Unary unary ->
        Ast.Unary { unary with rhs = namespace_expr namespace local_functions scopes unary.rhs }
    | Ast.Binary binary ->
        Ast.Binary
          { binary with
            lhs = namespace_expr namespace local_functions scopes binary.lhs
          ; rhs = namespace_expr namespace local_functions scopes binary.rhs
          }
    | Ast.Cast cast ->
        Ast.Cast { cast with value = namespace_expr namespace local_functions scopes cast.value }
    | Ast.Grouping inner ->
        Ast.Grouping (namespace_expr namespace local_functions scopes inner)
    | Ast.Try inner ->
        Ast.Try (namespace_expr namespace local_functions scopes inner)
    | Ast.ArrayLiteral arr ->
        Ast.ArrayLiteral { items = List.map (namespace_expr namespace local_functions scopes) arr.items }
    | Ast.StructLiteral lit ->
        Ast.StructLiteral
          { lit with
            fields =
              List.map
                (fun (field : Ast.struct_literal_field_expr) ->
                  { field with value = namespace_expr namespace local_functions scopes field.value })
                lit.fields
          }
    | Ast.Closure closure ->
        let scope =
          List.fold_left
            (fun acc (param : Ast.param) -> StringSet.add param.name acc)
            StringSet.empty closure.params
        in
        let body, _ = namespace_stmt namespace local_functions (scope :: scopes) closure.body in
        Ast.Closure { closure with body }
    | Ast.Literal _ as lit -> lit
  in
  { expr with kind }

let namespace_program_functions namespace (program : Ast.program) =
  let local_functions =
    List.fold_left
      (fun acc (function_ : Ast.function_decl) ->
        if has_substring function_.name "::" then acc
        else StringSet.add function_.name acc)
      StringSet.empty program.functions
    |> fun names ->
       List.fold_left
         (fun acc (function_ : Ast.inline_c_fn_decl) ->
           if has_substring function_.name "::" then acc
           else StringSet.add function_.name acc)
         names program.c_functions
  in
  if StringSet.is_empty local_functions || namespace = "" then program
  else
    let functions =
      List.map
        (fun (function_ : Ast.function_decl) ->
          if has_substring function_.name "::" then function_
          else
            let initial_scope =
              List.fold_left
                (fun acc (param : Ast.param) -> StringSet.add param.name acc)
                StringSet.empty function_.params
            in
            let body, _ = namespace_stmt namespace local_functions [ initial_scope ] function_.body in
            { function_ with name = namespace ^ "::" ^ function_.name; body })
        program.functions
    in
    let externs =
      List.map
        (fun (fn : Ast.extern_fn_decl) ->
          if has_substring fn.name "::" then fn
          else { fn with name = namespace ^ "::" ^ fn.name })
        program.externs
    in
    let c_functions =
      List.map
        (fun (fn : Ast.inline_c_fn_decl) ->
          if has_substring fn.name "::" then fn
          else { fn with name = namespace ^ "::" ^ fn.name })
        program.c_functions
    in
    { program with functions; externs; c_functions }

let merge_programs (included : Ast.program) (current : Ast.program) : Ast.program =
  { includes = included.includes @ current.includes
  ; links = included.links @ current.links
  ; subsystems = included.subsystems @ current.subsystems
  ; c_preambles = included.c_preambles @ current.c_preambles
  ; c_functions = included.c_functions @ current.c_functions
  ; structs = included.structs @ current.structs
  ; functions = included.functions @ current.functions
  ; externs = included.externs @ current.externs
  ; extern_consts = included.extern_consts @ current.extern_consts
  ; intrinsics = included.intrinsics @ current.intrinsics
  }

let has_source_exports (program : Ast.program) =
  program.functions <> [] || program.structs <> []

let append_include out include_ =
  let current : Ast.program = !out in
  out := { current with includes = current.includes @ [ include_ ] }

let expand_source_includes_with_timings ?(measure = true)
    (program : Ast.program) entry_path =
  let timings = empty_expansion_timings measure in
  let expansion_started = if measure then Clock.now () else 0. in
  let base_dir = dirname entry_path in
  let diagnostics = ref [] in
  let seen = ref StringSet.empty in
  let seen_named = ref StringSet.empty in
  let active_named = ref [] in
  let active = ref [] in
  let rec expand_program (program : Ast.program) base_dir =
    let out : Ast.program =
      { includes = []
      ; links = []
      ; subsystems = []
      ; c_preambles = []
      ; c_functions = []
      ; structs = []
      ; functions = []
      ; externs = []
      ; extern_consts = []
      ; intrinsics = []
      }
    in
    let out = ref out in
    List.iter
      (fun (include_ : Ast.include_decl) ->
        if is_source_include include_.module_ then
          expand_path_include out include_ base_dir
        else
          expand_named_include out include_)
      program.includes;
    let current =
      { program with includes = [] }
    in
    merge_programs !out current
  and expand_path_include out (include_ : Ast.include_decl) base_dir =
    let path = resolve_source_include base_dir include_.module_ in
    let key = try Unix.realpath path with _ -> path in
    if List.mem key !active then
      diagnostics :=
        Diagnostic.make "E486"
          (Printf.sprintf "cyclic source include involving `%s`" path)
          include_.span
        :: !diagnostics
    else if StringSet.mem key !seen then
      ()
    else
      match
        timed_accumulate
          timings.enabled
          (fun () -> timings.source_loading)
          (fun value -> timings.source_loading <- value)
          (fun () -> read_file path)
      with
      | None ->
          diagnostics :=
            Diagnostic.make "E487"
              (Printf.sprintf "failed to read source include `%s`" path)
              include_.span
            :: !diagnostics
      | Some source ->
          (match parse_include_source timings include_ path source diagnostics with
           | None -> ()
           | Some ast ->
               active := key :: !active;
               let expanded = expand_program ast (dirname path) in
               let namespaced =
                 timed_accumulate
                   timings.enabled
                   (fun () -> timings.namespace_rewriting)
                   (fun value -> timings.namespace_rewriting <- value)
                   (fun () ->
                     namespace_program_functions
                       (source_include_namespace include_.module_) expanded)
               in
               active := (match !active with _ :: rest -> rest | [] -> []);
               seen := StringSet.add key !seen;
               out := merge_programs !out namespaced)
  and expand_named_include out (include_ : Ast.include_decl) =
    if StringSet.mem include_.module_ !seen_named then
      append_include out include_
    else if List.mem include_.module_ !active_named then begin
      diagnostics :=
        Diagnostic.make "E486"
          (Printf.sprintf "cyclic module include involving `%s`" include_.module_)
          include_.span
        :: !diagnostics;
      append_include out include_
    end
    else match
      timed_accumulate
        timings.enabled
        (fun () -> timings.source_loading)
        (fun value -> timings.source_loading <- value)
        (fun () -> load_source_with_path include_.module_)
    with
    | None ->
        append_include out include_
    | Some (path, source) ->
        let parsed =
          match find_cached_module ~path include_.module_ source with
          | Some ast -> Some ast
          | None -> parse_include_source timings include_ path source diagnostics
        in
        (match parsed with
         | Some ast when has_source_exports ast ->
             cache_parsed_module ~path include_.module_ source ast;
             active_named := include_.module_ :: !active_named;
             let expanded = expand_program ast (dirname path) in
             active_named :=
               (match !active_named with _ :: rest -> rest | [] -> []);
             seen_named := StringSet.add include_.module_ !seen_named;
             let namespaced =
               timed_accumulate
                 timings.enabled
                 (fun () -> timings.namespace_rewriting)
                 (fun value -> timings.namespace_rewriting <- value)
                 (fun () -> namespace_program_functions include_.module_ expanded)
             in
             let keep_include =
               { include_ with module_ = include_.module_ }
             in
             out := merge_programs !out { namespaced with includes = namespaced.includes @ [ keep_include ] }
         | Some ast ->
             cache_parsed_module ~path include_.module_ source ast;
             if ast.links <> [] || ast.c_preambles <> [] || ast.c_functions <> [] then
               out :=
                 { !out with
                   links = !out.links @ ast.links
                 ; c_preambles = !out.c_preambles @ ast.c_preambles
                 ; c_functions = !out.c_functions @ ast.c_functions
                 };
             append_include out include_
         | None ->
             append_include out include_)
  in
  let expanded = expand_program program base_dir in
  if measure then begin
    let expansion_total = Clock.now () -. expansion_started in
    timings.bookkeeping <-
      max 0.
        (expansion_total
         -. timings.source_loading
         -. timings.include_tokenization
         -. timings.include_parsing
         -. timings.source_annotation
         -. timings.namespace_rewriting)
  end;
  ((if !diagnostics = [] then Ok expanded else Error (List.rev !diagnostics)),
   timings)

let expand_source_includes program entry_path =
  fst (expand_source_includes_with_timings ~measure:false program entry_path)
