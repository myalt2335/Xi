open Ast

let ( let* ) value f = match value with Some value -> f value | None -> None

type assign_or_expr = ParsedAssign of assign_stmt | ParsedExpr of expr
type extern_decl = ExternFn of extern_fn_decl | ExternConst of extern_const_decl
type c_decl = CInlineFn of inline_c_fn_decl * extern_fn_decl | CPreamble of c_preamble_decl

type state =
  { tokens : Token.t array
  ; token_count : int
  ; mutable idx : int
  ; mutable diagnostics : Diagnostic.t list
  }

let eof_token = { Token.kind = Token.Eof; lexeme = ""; span = Span.make 0 0 }

let make_state tokens =
  let arr = if Array.length tokens = 0 then [| eof_token |] else tokens in
  { tokens = arr; token_count = Array.length arr; idx = 0; diagnostics = [] }

let push_diag p code message span =
  p.diagnostics <- Diagnostic.make code message span :: p.diagnostics

let current p = Array.unsafe_get p.tokens p.idx

let kind_at p offset =
  let i = p.idx + offset in
  if i >= 0 && i < p.token_count then
    (Array.unsafe_get p.tokens i).Token.kind
  else Token.Eof

let previous p =
  if p.idx = 0 then None
  else Some (Array.unsafe_get p.tokens (p.idx - 1))

let at p kind = (Array.unsafe_get p.tokens p.idx).Token.kind = kind

let advance p =
  let tok = Array.unsafe_get p.tokens p.idx in
  if tok.Token.kind <> Token.Eof then p.idx <- p.idx + 1;
  tok

let match_kind p kind =
  let tok = Array.unsafe_get p.tokens p.idx in
  if tok.Token.kind = kind then begin
    if kind <> Token.Eof then p.idx <- p.idx + 1;
    true
  end
  else false

let match_identifier_text p text =
  let tok = Array.unsafe_get p.tokens p.idx in
  if tok.Token.kind = Token.Identifier && tok.Token.lexeme = text then begin
    p.idx <- p.idx + 1;
    true
  end
  else false

let error_current p code message = push_diag p code message (current p).Token.span

let consume p kind code message =
  let tok = Array.unsafe_get p.tokens p.idx in
  if tok.Token.kind = kind then begin
    if kind <> Token.Eof then p.idx <- p.idx + 1;
    Some tok
  end
  else begin
    push_diag p code message tok.Token.span;
    None
  end

let previous_span p fallback =
  match previous p with Some tok -> tok.Token.span | None -> fallback

let rec parse_program p =
  let includes = ref [] in
  let links = ref [] in
  let subsystems = ref [] in
  let c_preambles = ref [] in
  let c_functions = ref [] in
  let structs = ref [] in
  let functions = ref [] in
  let externs = ref [] in
  let extern_consts = ref [] in
  let intrinsics = ref [] in
  while not (at p Token.Eof) do
    match (current p).Token.kind with
    | Token.Include ->
      ignore (advance p);
      (match parse_include_decl p with
       | Some include_ -> includes := include_ :: !includes
       | None -> sync_top_level p)
    | Token.LinkDirective ->
      ignore (advance p);
      (match parse_link_decl p with
       | Some link_ -> links := link_ :: !links
       | None -> sync_top_level p)
    | Token.SubsystemDirective ->
      ignore (advance p);
      (match parse_subsystem_decl p with
       | Some sub_ -> subsystems := sub_ :: !subsystems
       | None -> sync_top_level p)
    | Token.KwExtern ->
      ignore (advance p);
      (match parse_extern_decl p ~c_abi:false with
       | Some (ExternFn decl) -> externs := decl :: !externs
       | Some (ExternConst decl) -> extern_consts := decl :: !extern_consts
       | None -> sync_top_level p)
    | Token.KwCextern ->
      ignore (advance p);
      (match parse_extern_decl p ~c_abi:true with
       | Some (ExternFn decl) -> externs := decl :: !externs
       | Some (ExternConst decl) -> extern_consts := decl :: !extern_consts
       | None -> sync_top_level p)
    | Token.Identifier when (current p).Token.lexeme = "c" ->
      ignore (advance p);
      (match parse_c_decl p with
       | Some (CInlineFn (cfn, extern_)) ->
           c_functions := cfn :: !c_functions;
           externs := extern_ :: !externs
       | Some (CPreamble preamble) -> c_preambles := preamble :: !c_preambles
       | None -> sync_top_level p)
    | Token.KwIntrinsic ->
      ignore (advance p);
      (match parse_intrinsic_decl p with
       | Some decl -> intrinsics := decl :: !intrinsics
       | None -> sync_top_level p)
    | Token.KwStruct ->
      ignore (advance p);
      (match parse_struct_decl p ~c_layout:false with
       | Some decl -> structs := decl :: !structs
       | None -> sync_top_level p)
    | Token.KwCstruct ->
      ignore (advance p);
      (match parse_struct_decl p ~c_layout:true with
       | Some decl -> structs := decl :: !structs
       | None -> sync_top_level p)
    | Token.KwF ->
      ignore (advance p);
      (match parse_function_decl p with
       | Some function_ -> functions := function_ :: !functions
       | None -> sync_top_level p)
    | _ ->
      error_current p "E100"
        "expected `#include`, `struct`, `c`, or function declaration keyword (`f`, `fn`, or `function`) at top-level";
      ignore (advance p);
      sync_top_level p
  done;
  { includes = List.rev !includes
  ; links = List.rev !links
  ; subsystems = List.rev !subsystems
  ; c_preambles = List.rev !c_preambles
  ; c_functions = List.rev !c_functions
  ; structs = List.rev !structs
  ; functions = List.rev !functions
  ; externs = List.rev !externs
  ; extern_consts = List.rev !extern_consts
  ; intrinsics = List.rev !intrinsics
  }

and inline_c_symbol name body =
  "xi_inline_c_" ^ Digest.to_hex (Digest.string (name ^ "\000" ^ body)) ^ "_" ^ name

and parse_c_decl p =
  let* c_tok = previous p in
  if match_identifier_text p "preamble" then begin
    let* raw =
      consume p Token.RawCBlock "E161"
        "expected `{ ... }` after `c preamble`"
    in
    Some
      (CPreamble
         ({ body = raw.Token.lexeme
          ; body_offset = raw.Token.span.start + 1
          ; origin = None
          ; line = None
          ; span = Span.merge c_tok.Token.span raw.Token.span
          }
           : c_preamble_decl))
  end
  else if match_kind p Token.KwF then begin
    let* name_tok =
      consume p Token.Identifier "E162" "expected function name after `c f`"
    in
    let* _ = consume p Token.LParen "E163" "expected `(` after inline C function name" in
    let* params = parse_comma_list_until p Token.RParen parse_param in
    let* _ = consume p Token.RParen "E164" "expected `)` after inline C parameters" in
    let* _ =
      consume p Token.Arrow "E165"
        "expected `->` and a return type in inline C function declaration"
    in
    let* return_ty = parse_type_ref p in
    let* raw =
      consume p Token.RawCBlock "E166"
        "expected a `{ ... }` C body after the inline C function signature"
    in
    let symbol = inline_c_symbol name_tok.Token.lexeme raw.Token.lexeme in
    let span = Span.merge c_tok.Token.span raw.Token.span in
    let cfn : inline_c_fn_decl =
      { name = name_tok.Token.lexeme
      ; params
      ; return_ty
      ; symbol
      ; body = raw.Token.lexeme
      ; body_offset = raw.Token.span.start + 1
      ; origin = None
      ; line = None
      ; span
      }
    in
    let extern_ : extern_fn_decl =
      { name = name_tok.Token.lexeme
      ; params
      ; return_ty
      ; symbol
      ; c_abi = true
      ; span
      }
    in
    Some (CInlineFn (cfn, extern_))
  end
  else begin
    error_current p "E185" "expected `preamble` or `f` after `c`";
    None
  end

and parse_intrinsic_decl p =
  let* intrinsic_tok = previous p in
  let* name_tok =
    consume p Token.Identifier "E159" "expected name after `intrinsic`"
  in
  let* semi =
    consume p Token.Semicolon "E160" "expected `;` after intrinsic declaration"
  in
  Some
    ({ name = name_tok.Token.lexeme
     ; span = Span.merge intrinsic_tok.Token.span semi.Token.span
     }
      : intrinsic_decl)

and parse_extern_decl p ~c_abi =
  let* extern_tok = previous p in
  if match_kind p Token.KwF then begin
    let* name_tok =
      consume p Token.Identifier "E150" "expected function name after `extern f`"
    in
    let* _ =
      consume p Token.LParen "E151" "expected `(` after extern function name"
    in
    let* params = parse_comma_list_until p Token.RParen parse_param in
    let* _ =
      consume p Token.RParen "E152" "expected `)` after extern parameters"
    in
    let* _ =
      consume p Token.Arrow "E153"
        "expected `->` and a return type in extern function declaration"
    in
    let* return_ty = parse_type_ref p in
    let* _ =
      consume p Token.Eq "E154" "expected `=` before the backing symbol name"
    in
    let* symbol_tok =
      consume p Token.StringLiteral "E155"
        "expected backing symbol name as a string literal"
    in
    let* semi =
      consume p Token.Semicolon "E156"
        "expected `;` after extern function declaration"
    in
    Some
      (ExternFn
         ({ name = name_tok.Token.lexeme
          ; params
          ; return_ty
          ; symbol = unescape_string_literal symbol_tok.Token.lexeme
          ; c_abi
          ; span = Span.merge extern_tok.Token.span semi.Token.span
          }
           : extern_fn_decl))
  end
  else
    let* ty = parse_type_ref p in
    let* name_tok =
      consume p Token.Identifier "E157"
        "expected name in extern value declaration"
    in
    let* _ =
      consume p Token.Eq "E154" "expected `=` before the backing symbol name"
    in
    let* symbol_tok =
      consume p Token.StringLiteral "E155"
        "expected backing symbol name as a string literal"
    in
    let* semi =
      consume p Token.Semicolon "E158"
        "expected `;` after extern value declaration"
    in
    Some
      (ExternConst
         ({ name = name_tok.Token.lexeme
          ; ty
          ; symbol = unescape_string_literal symbol_tok.Token.lexeme
          ; c_abi
          ; span = Span.merge extern_tok.Token.span semi.Token.span
          }
           : extern_const_decl))

and parse_struct_decl p ~c_layout =
  let* struct_tok = previous p in
  let* name_tok = consume p Token.Identifier "E141" "expected struct name" in
  let* _ = consume p Token.LBrace "E142" "expected `{` after struct name" in
  let* fields = parse_struct_fields p [] in
  let* end_tok =
    consume p Token.RBrace "E145" "expected `}` after struct fields"
  in
  Some
    ({ name = name_tok.Token.lexeme
     ; fields
     ; c_layout
     ; span = Span.merge struct_tok.Token.span end_tok.Token.span
     }
      : struct_decl)

and parse_struct_fields p acc =
  if at p Token.RBrace || at p Token.Eof then Some (List.rev acc)
  else
    let* ty = parse_type_ref p in
    let* field_tok = consume p Token.Identifier "E143" "expected field name" in
    let* semi = consume p Token.Semicolon "E144" "expected `;` after field" in
    let field =
      ({ ty
       ; name = field_tok.Token.lexeme
       ; span = Span.merge field_tok.Token.span semi.Token.span
       }
        : struct_field_decl)
    in
    parse_struct_fields p (field :: acc)

and is_valid_library_name name =
  name <> ""
  && String.for_all
       (fun ch ->
         (ch >= 'a' && ch <= 'z')
         || (ch >= 'A' && ch <= 'Z')
         || (ch >= '0' && ch <= '9')
         || ch = '_' || ch = '-' || ch = '.' || ch = '+')
       name

and parse_link_decl p =
  let* link_tok = previous p in
  let* library_tok =
    consume p Token.StringLiteral "E170"
      "expected a library name as a string literal after `#link`"
  in
  let library = unescape_string_literal library_tok.Token.lexeme in
  let end_span = ref library_tok.Token.span in
  if at p Token.Semicolon then end_span := (advance p).Token.span;
  if not (is_valid_library_name library) then begin
    error_current p "E171"
      "invalid `#link` library name (allowed: letters, digits, `_`, `-`, `.`, `+`)";
    None
  end
  else
    Some
      ({ library; span = Span.merge link_tok.Token.span !end_span }
        : link_decl)

and parse_subsystem_decl p =
  let* sub_tok = previous p in
  let* name_tok =
    consume p Token.StringLiteral "E172"
      "expected a subsystem name as a string literal after `#subsystem` (`\"windows\"` or `\"console\"`)"
  in
  let name = unescape_string_literal name_tok.Token.lexeme in
  let end_span = ref name_tok.Token.span in
  if at p Token.Semicolon then end_span := (advance p).Token.span;
  if name <> "windows" && name <> "console" then begin
    error_current p "E173" "`#subsystem` must be `\"windows\"` or `\"console\"`";
    None
  end
  else Some ({ name; span = Span.merge sub_tok.Token.span !end_span } : subsystem_decl)

and parse_include_decl p =
  let* include_tok = previous p in
  let module_ = Buffer.create 32 in
  let end_span = ref include_tok.Token.span in
  if at p Token.StringLiteral then begin
    let path_tok = advance p in
    Buffer.add_string module_ (unescape_string_literal path_tok.Token.lexeme);
    end_span := path_tok.Token.span
  end
  else
    while is_include_path_part p do
      let tok = advance p in
      end_span := tok.Token.span;
      if tok.Token.kind = Token.Slash && tok.Token.lexeme = "\\" then
        Buffer.add_char module_ '/'
      else Buffer.add_string module_ tok.Token.lexeme
    done;
  let module_text = Buffer.contents module_ in
  if module_text = "" then begin
    error_current p "E101" "expected module name or path after `#include`";
    None
  end
  else
    Some
      ({ module_ = module_text
       ; span = Span.merge include_tok.Token.span !end_span
       }
        : include_decl)

and is_include_path_part p =
  match (current p).Token.kind with
  | Token.Identifier -> (current p).Token.lexeme <> "c"
  | Token.Dot | Token.Slash | Token.Minus | Token.Colon -> true
  | _ -> false

and parse_function_decl p =
  let* f_tok = previous p in
  let* name_tok =
    consume p Token.Identifier "E102"
      "expected function name after function declaration keyword"
  in
  let* _ = consume p Token.LParen "E103" "expected `(` after function name" in
  let* params = parse_comma_list_until p Token.RParen parse_param in
  let* _ = consume p Token.RParen "E104" "expected `)` after parameters" in
  let* body = parse_block_stmt p in
  let end_span = stmt_span body in
  Some
    ({ name = name_tok.Token.lexeme
     ; params
     ; body
     ; span = Span.merge f_tok.Token.span end_span
     }
      : function_decl)

and parse_param p =
  let start_span = (current p).Token.span in
  let binding =
    if match_kind p Token.KwRef then Ref
    else if match_kind p Token.KwLink then Link
    else Value
  in
  let* ty = parse_type_ref p in
  let* name_tok = consume p Token.Identifier "E105" "expected parameter name" in
  Some
    ({ binding
     ; ty
     ; name = name_tok.Token.lexeme
     ; span = Span.merge start_span name_tok.Token.span
     }
      : param)

and parse_block_stmt p =
  let* lbrace = consume p Token.LBrace "E106" "expected `{` to start block" in
  let statements = ref [] in
  while (not (at p Token.RBrace)) && not (at p Token.Eof) do
    match parse_stmt p with
    | Some stmt -> statements := stmt :: !statements
    | None -> sync_stmt p
  done;
  let* rbrace = consume p Token.RBrace "E107" "expected `}` to end block" in
  Some (Block ({ statements = List.rev !statements; span = Span.merge lbrace.Token.span rbrace.Token.span } : block_stmt))

and parse_stmt p =
  let parse_decl () =
    let* decl = parse_var_decl p false in
    let* semicolon =
      consume p Token.Semicolon "E108"
        "expected `;` after variable declaration"
    in
    Some
      (VarDecl
         { (decl : var_decl_stmt) with
           span = Span.merge decl.span semicolon.Token.span
         })
  in
  match (current p).Token.kind with
  | Token.LBrace -> parse_block_stmt p
  | Token.KwIf ->
      ignore (advance p);
      parse_if_stmt p
  | Token.KwSwitch | Token.KwMatch ->
      ignore (advance p);
      parse_switch_stmt p
  | Token.KwWhile ->
      ignore (advance p);
      parse_while_stmt p
  | Token.KwFor ->
      ignore (advance p);
      parse_for_stmt p
  | Token.KwReturn ->
      ignore (advance p);
      parse_return_stmt p
  | Token.KwBreak ->
      ignore (advance p);
      parse_break_stmt p
  | Token.KwContinue ->
      ignore (advance p);
      parse_continue_stmt p
  | Token.Identifier when looks_like_io_chain p -> parse_io_chain_stmt p
  | (Token.KwInt | Token.KwUint | Token.KwDec | Token.KwString | Token.KwBool
    | Token.KwArray) ->
      parse_decl ()
  | Token.KwLink ->
      if looks_like_var_decl p then parse_decl ()
      else parse_explicit_link_assign_stmt p true
  | Token.KwRef | Token.KwF | Token.Identifier ->
      if looks_like_var_decl p then parse_decl ()
      else parse_assign_or_expr_stmt p
  | _ -> parse_assign_or_expr_stmt p

and parse_if_stmt p =
  let* if_tok = previous p in
  let* _ = consume p Token.LParen "E109" "expected `(` after `if`" in
  let* cond = parse_expression p 1 in
  let* _ = consume p Token.RParen "E110" "expected `)` after if condition" in
  let* then_branch = parse_stmt p in
  let* else_branch =
    if match_kind p Token.KwElse then
      if match_kind p Token.KwIf then
        match parse_if_stmt p with Some stmt -> Some (Some stmt) | None -> None
      else
        match parse_stmt p with Some stmt -> Some (Some stmt) | None -> None
    else Some None
  in
  let end_span =
    match else_branch with Some stmt -> stmt_span stmt | None -> stmt_span then_branch
  in
  Some
    (If
       ({ cond
        ; then_branch
        ; else_branch
        ; span = Span.merge if_tok.Token.span end_span
        }
        : if_stmt))

and parse_switch_stmt p =
  let* switch_tok = previous p in
  let* _ =
    consume p Token.LParen "E177" "expected `(` after `switch`/`match`"
  in
  let* selector = parse_expression p 1 in
  let* _ =
    consume p Token.RParen "E178"
      "expected `)` after switch/match selector"
  in
  let* _ =
    consume p Token.LBrace "E179"
      "expected `{` after switch/match selector"
  in
  let cases = ref [] in
  let default = ref None in
  while (not (at p Token.RBrace)) && not (at p Token.Eof) do
    if match_kind p Token.KwCase then begin
      match previous p, parse_expression p 1 with
      | Some case_tok, Some value -> (
          match consume p Token.Colon "E180" "expected `:` after case value" with
          | None -> sync_switch_arm p
          | Some _ ->
              let body = parse_switch_arm_body p in
              let end_span =
                match List.rev body with
                | last :: _ -> stmt_span last
                | [] -> value.span
              in
              cases :=
                ({ value; body; span = Span.merge case_tok.Token.span end_span }
                 : switch_case)
                :: !cases)
      | _ -> sync_switch_arm p
    end
    else if match_kind p Token.KwDefault then begin
      match previous p with
      | None -> sync_switch_arm p
      | Some default_tok -> (
          match consume p Token.Colon "E181" "expected `:` after `default`" with
          | None -> sync_switch_arm p
          | Some _ ->
              let body = parse_switch_arm_body p in
              let end_span =
                match List.rev body with
                | last :: _ -> stmt_span last
                | [] -> default_tok.Token.span
              in
              if !default <> None then
                push_diag p "E182"
                  "switch statement can only have one `default` arm"
                  default_tok.Token.span
              else
                default :=
                  Some
                    ({ body; span = Span.merge default_tok.Token.span end_span }
                     : switch_default))
    end
    else begin
      error_current p "E183" "expected `case`, `default`, or `}` in switch";
      sync_switch_arm p
    end
  done;
  let* rbrace = consume p Token.RBrace "E184" "expected `}` after switch" in
  Some
    (Switch
       ({ selector
        ; cases = List.rev !cases
        ; default = !default
        ; span = Span.merge switch_tok.Token.span rbrace.Token.span
        }
        : switch_stmt))

and parse_switch_arm_body p =
  let statements = ref [] in
  while
    (not (at p Token.KwCase))
    && (not (at p Token.KwDefault))
    && (not (at p Token.RBrace))
    && not (at p Token.Eof)
  do
    match parse_stmt p with
    | Some stmt -> statements := stmt :: !statements
    | None -> sync_switch_arm p
  done;
  List.rev !statements

and parse_while_stmt p =
  let* while_tok = previous p in
  let* _ = consume p Token.LParen "E111" "expected `(` after `while`" in
  let* cond = parse_expression p 1 in
  let* _ =
    consume p Token.RParen "E112" "expected `)` after while condition"
  in
  let* body = parse_stmt p in
  let end_span = stmt_span body in
  Some
    (While
       ({ cond; body; span = Span.merge while_tok.Token.span end_span }
        : while_stmt))

and parse_for_stmt p =
  let* for_tok = previous p in
  if at p Token.LParen then parse_c_for_stmt p for_tok
  else parse_range_for_stmt p for_tok

and parse_c_for_stmt p for_tok =
  let* _ = consume p Token.LParen "E113" "expected `(` after `for`" in
  let* init =
    if at p Token.Semicolon then begin
      ignore (advance p);
      Some None
    end
    else
      let* init =
        if looks_like_var_decl p then
          let* decl = parse_var_decl p false in
          Some (ForInitVarDecl decl)
        else if at p Token.KwLink && not (looks_like_var_decl p) then
          let* stmt = parse_explicit_link_assign_stmt p false in
          match stmt with
          | Assign assign -> Some (ForInitAssign assign)
          | _ ->
              error_current p "E114" "invalid for-loop initializer";
              None
        else
          match parse_assign_or_expr_core p with
          | Some (ParsedAssign assign) -> Some (ForInitAssign assign)
          | Some (ParsedExpr expr) -> Some (ForInitExpr expr)
          | None -> None
      in
      let* _ =
        consume p Token.Semicolon "E115"
          "expected `;` after for-loop initializer"
      in
      Some (Some init)
  in
  let* cond =
    if at p Token.Semicolon then begin
      ignore (advance p);
      Some None
    end
    else
      let* expr = parse_expression p 1 in
      let* _ =
        consume p Token.Semicolon "E116"
          "expected `;` after for-loop condition"
      in
      Some (Some expr)
  in
  let* step =
    if at p Token.RParen then Some None
    else
      match parse_assign_or_expr_core p with
      | Some (ParsedAssign assign) -> Some (Some (ForStepAssign assign))
      | Some (ParsedExpr expr) -> Some (Some (ForStepExpr expr))
      | None -> None
  in
  let* _ =
    consume p Token.RParen "E117" "expected `)` after for-loop header"
  in
  let* body = parse_stmt p in
  let end_span = stmt_span body in
  Some
    (For
       ({ init; cond; step; body; span = Span.merge for_tok.Token.span end_span }
        : for_stmt))

and parse_range_for_stmt p for_tok =
  let* name_tok =
    consume p Token.Identifier "E186" "expected loop variable name after `for`"
  in
  let* _ =
    consume p Token.KwIn "E187" "expected `in` after range loop variable"
  in
  (* Both loop forms start the same way. The head expression decides which one
     this is: `..`/`..<` after it means a range, anything else means the
     expression is a collection to walk. It is parsed with the range-bound
     parser either way, so `for v in xs { ... }` cannot read the loop body as a
     struct literal attached to `xs`. *)
  let* start = parse_range_bound p in
  let inclusive =
    if match_kind p Token.RangeInclusive then Some true
    else if match_kind p Token.RangeExclusive then Some false
    else None
  in
  match inclusive with
  | None ->
      let* body = parse_stmt p in
      Some
        (ForEach
           ({ name = name_tok.Token.lexeme
            ; name_span = name_tok.Token.span
            ; collection = start
            ; body
            ; span = Span.merge for_tok.Token.span (stmt_span body)
            }
             : for_each_stmt))
  | Some _ ->
  let* inclusive =
    match inclusive with
    | Some value -> Some value
    | None ->
        error_current p "E188" "expected `..` or `..<` in range for-loop";
        None
  in
  let* finish = parse_range_bound p in
  let* body = parse_stmt p in
  let loop_path =
    { kind = Path ({ segments = [ name_tok.Token.lexeme ]; span = name_tok.Token.span } : path_expr)
    ; span = name_tok.Token.span
    }
  in
  let loop_ty : type_ref = { kind = Int None; span = name_tok.Token.span } in
  let init =
    ForInitVarDecl
      ({ binding = Value
       ; ty = loop_ty
       ; name = name_tok.Token.lexeme
       ; init = Some start
       ; span = Span.merge name_tok.Token.span start.span
       }
        : var_decl_stmt)
  in
  let cond =
    { kind = Binary ({ op = if inclusive then Le else Lt; lhs = loop_path; rhs = finish } : binary_expr)
    ; span = Span.merge loop_path.span finish.span
    }
  in
  let one = { kind = Literal (LitInt "1"); span = name_tok.Token.span } in
  let step_value =
    { kind = Binary ({ op = Add; lhs = loop_path; rhs = one } : binary_expr)
    ; span = Span.merge loop_path.span one.span
    }
  in
  let step =
    ForStepAssign
      ({ target = { segments = [ name_tok.Token.lexeme ]; span = name_tok.Token.span }
       ; value = step_value
       ; mode = Normal
       ; span = step_value.span
       }
        : assign_stmt)
  in
  let end_span = stmt_span body in
  Some
    (For
       ({ init = Some init
        ; cond = Some cond
        ; step = Some step
        ; body
        ; span = Span.merge for_tok.Token.span end_span
        }
         : for_stmt))

and parse_range_bound p =
  if at p Token.Identifier then begin
    let* path = parse_path_expr p in
    let first = { kind = Path path; span = path.span } in
    let* first = parse_postfix_tail p first in
    parse_expression_tail p 1 first
  end
  else parse_expression p 1

and parse_break_stmt p =
  let* break_tok = previous p in
  let* semicolon = consume p Token.Semicolon "E189" "expected `;` after `break`" in
  Some (Break ({ span = Span.merge break_tok.Token.span semicolon.Token.span } : jump_stmt))

and parse_continue_stmt p =
  let* continue_tok = previous p in
  let* semicolon = consume p Token.Semicolon "E167" "expected `;` after `continue`" in
  Some (Continue ({ span = Span.merge continue_tok.Token.span semicolon.Token.span } : jump_stmt))

and parse_return_stmt p =
  let* ret_tok = previous p in
  if match_kind p Token.Semicolon then
    let span = Span.merge ret_tok.Token.span (previous_span p ret_tok.Token.span) in
    Some (Return ({ value = None; span } : return_stmt))
  else
    let* value = parse_expression p 1 in
    let* semicolon =
      consume p Token.Semicolon "E118" "expected `;` after return value"
    in
    Some
      (Return
         ({ value = Some value
          ; span = Span.merge ret_tok.Token.span semicolon.Token.span
          }
          : return_stmt))

and parse_var_decl p require_semicolon =
  let start = (current p).Token.span in
  let binding =
    if match_kind p Token.KwRef then Ref
    else if match_kind p Token.KwLink then Link
    else Value
  in
  let* ty = parse_type_ref p in
  let* name_tok =
    consume p Token.Identifier "E119" "expected variable name in declaration"
  in
  let end_span = ref name_tok.Token.span in
  let* init =
    if match_kind p Token.Eq then
      let* expr = parse_expression p 1 in
      end_span := expr.span;
      Some (Some expr)
    else Some None
  in
  if require_semicolon then begin
    match
      consume p Token.Semicolon "E120"
        "expected `;` after variable declaration"
    with
    | None -> None
    | Some semicolon ->
        end_span := semicolon.Token.span;
        Some
          ({ binding
           ; ty
           ; name = name_tok.Token.lexeme
           ; init
           ; span = Span.merge start !end_span
           }
            : var_decl_stmt)
  end
  else
    Some
      ({ binding
       ; ty
       ; name = name_tok.Token.lexeme
       ; init
       ; span = Span.merge start !end_span
       }
        : var_decl_stmt)

and parse_explicit_link_assign_stmt p require_semicolon =
  let* link_tok =
    consume p Token.KwLink "E121"
      "expected `link` for explicit link assignment"
  in
  let* target = parse_path_expr p in
  let* _ =
    consume p Token.Eq "E122" "expected `=` in explicit link assignment"
  in
  let* value = parse_expression p 1 in
  let end_span = ref value.span in
  if require_semicolon then begin
    match
      consume p Token.Semicolon "E123"
        "expected `;` after explicit link assignment"
    with
    | None -> None
    | Some semicolon ->
        end_span := semicolon.Token.span;
        Some
          (Assign
             ({ target
              ; value
              ; mode = ExplicitLink
              ; span = Span.merge link_tok.Token.span !end_span
              }
              : assign_stmt))
  end
  else
    Some
      (Assign
         ({ target
          ; value
          ; mode = ExplicitLink
          ; span = Span.merge link_tok.Token.span !end_span
          }
          : assign_stmt))

and parse_io_chain_stmt p =
  let start_tok = current p in
  let* anchor = parse_io_anchor p in
  let* _ =
    consume p Token.ShiftLeft "E124" "expected `<<` after io chain anchor"
  in
  let* items = parse_io_chain_items p [] in
  let* semicolon =
    consume p Token.Semicolon "E125" "expected `;` after io chain"
  in
  Some
    (IoChain
       ({ anchor
        ; items
        ; span = Span.merge start_tok.Token.span semicolon.Token.span
        }
        : io_chain_stmt))

and parse_io_anchor p =
  let* io = consume p Token.Identifier "E126" "expected `io`" in
  if io.Token.lexeme <> "io" then begin
    push_diag p "E126" "io chain must start with `io`" io.Token.span;
    None
  end
  else
    let* _ = consume p Token.Namespace "E127" "expected `::` after `io`" in
    let* target =
      consume p Token.Identifier "E128"
        "expected io chain anchor (`wrt`, `wrtl`, or `wrtr`)"
    in
    match target.Token.lexeme with
    | "wrt" -> Some Wrt
    | "wrtl" -> Some Wrtl
    | "wrtr" -> Some Wrtr
    | _ ->
        push_diag p "E128"
          "io chain anchor must be `io::wrt`, `io::wrtl`, or `io::wrtr`"
          target.Token.span;
        None

and parse_assign_or_expr_stmt p =
  let* parsed = parse_assign_or_expr_core p in
  let* semicolon =
    consume p Token.Semicolon "E129" "expected `;` after statement"
  in
  match parsed with
  | ParsedAssign assign ->
      Some
        (Assign
           { assign with span = Span.merge assign.span semicolon.Token.span })
  | ParsedExpr expr ->
      Some
        (ExprStmt
           ({ expr; span = Span.merge expr.span semicolon.Token.span }
            : expr_stmt))

and compound_assign_op p =
  if match_kind p Token.PlusEq then Some Add
  else if match_kind p Token.MinusEq then Some Sub
  else if match_kind p Token.StarEq then Some Mul
  else if match_kind p Token.SlashEq then Some Div
  else if match_kind p Token.PercentEq then Some Mod
  else None

(* `x op= v` is `x = x op v`, so the target expression is written into the
   result twice. That is invisible for a pure target, but `xs[next()] += 1`
   would call `next` twice — which contradicts Xi's predictable evaluation
   order. Rather than ship that, a call inside an index/member target is
   rejected and the caller hoists it into a variable. Lifting this needs
   lowering-side temporaries, not a different desugaring. *)
and expr_contains_call (e : expr) =
  match e.kind with
  | Call _ -> true
  | Index i -> expr_contains_call i.base || expr_contains_call i.index
  | Member m -> expr_contains_call m.base
  | Unary u -> expr_contains_call u.rhs
  | Binary b -> expr_contains_call b.lhs || expr_contains_call b.rhs
  | Cast c -> expr_contains_call c.value
  | Try inner -> expr_contains_call inner
  | Grouping inner -> expr_contains_call inner
  | ArrayLiteral lit -> List.exists expr_contains_call lit.items
  | _ -> false

and make_binary_expr op lhs rhs span =
  { kind = Binary ({ op; lhs; rhs } : binary_expr); span }

and parse_assign_or_expr_core p =
  let* expr = parse_expression p 1 in
  let compound = compound_assign_op p in
  let plain = compound = None && match_kind p Token.Eq in
  if compound <> None || plain then begin
    let* value = parse_expression p 1 in
    let span = Span.merge expr.span value.span in
    let combined target_read =
      match compound with
      | Some op -> make_binary_expr op target_read value span
      | None -> value
    in
    match expr.kind with
    | Path path ->
        Some
          (ParsedAssign
             ({ target = path; value = combined expr; mode = Normal; span } : assign_stmt))
    | Index indexed ->
        if compound <> None && (expr_contains_call indexed.base || expr_contains_call indexed.index)
        then begin
          push_diag p "E168"
            "compound assignment cannot have a function call in the indexed target; assign the index to a variable first"
            expr.span;
          None
        end
        else Some (ParsedExpr (make_index_set_call indexed.base indexed.index (combined expr) span))
    | Member member ->
        if compound <> None && expr_contains_call member.base then begin
          push_diag p "E168"
            "compound assignment cannot have a function call in the member target; assign the object to a variable first"
            expr.span;
          None
        end
        else Some (ParsedExpr (make_member_set_call member.base member.field (combined expr) span))
    | _ ->
        push_diag p "E130"
          "left side of assignment must be a path, index, or member access"
          expr.span;
        None
  end
  else Some (ParsedExpr expr)

and make_index_set_call base index value span =
  let callee =
    { kind =
        Path
          ({ segments = [ "__index"; "set" ]; span } : path_expr)
    ; span
    }
  in
  { kind = Call ({ callee; args = [ base; index; value ] } : call_expr); span }

and make_member_set_call base field value span =
  let callee =
    { kind =
        Path
          ({ segments = [ "__member"; "set" ]; span } : path_expr)
    ; span
    }
  in
  let field_expr = { kind = Literal (LitStr field); span } in
  { kind = Call ({ callee; args = [ base; field_expr; value ] } : call_expr); span }

and parse_type_ref p =
  let start_span = (current p).Token.span in
  let* kind =
    match (current p).Token.kind with
    | Token.KwInt ->
        ignore (advance p);
        Some (Int None)
    | Token.KwUint ->
        ignore (advance p);
        Some (Uint None)
    | Token.KwDec ->
        ignore (advance p);
        Some (Dec None)
    | Token.KwString ->
        ignore (advance p);
        Some Str
    | Token.KwBool ->
        ignore (advance p);
        Some Bool
    | Token.KwF -> parse_function_type_kind p
    | Token.KwArray ->
        ignore (advance p);
        let* elem_kind =
          if match_kind p Token.Less then
            let* elem = parse_type_atom p in
            let* _ =
              consume p Token.Greater "E139"
                "expected `>` after array element type"
            in
            Some elem
          else Some Str
        in
        Some (Array elem_kind)
    | Token.Identifier ->
        let token = current p in
        if token.Token.lexeme = "result" then begin
          ignore (advance p);
          let* payload =
            if match_kind p Token.Less then
              let* payload = parse_type_ref p in
              let* _ = consume p Token.Greater "E169" "expected `>` in `result<T>`" in
              Some payload.kind
            else begin
              error_current p "E169" "`result` needs a payload type, as in `result<int>`";
              None
            end
          in
          Some (Result payload)
        end
        else if token.Token.lexeme = "map" then begin
          ignore (advance p);
          let* (key_kind, value_kind) =
            if match_kind p Token.Less then
              let* key = parse_type_atom p in
              let* _ =
                consume p Token.Comma "E140" "expected `,` in `map<K, V>`"
              in
              let* value = parse_type_atom p in
              let* _ =
                consume p Token.Greater "E140" "expected `>` in `map<K, V>`"
              in
              if not (is_scalar_map_key key) then begin
                error_current p "E442"
                  "`map<K, V>` keys currently support `int`, `dec`, `bool`, or `string`";
                None
              end
              else Some (key, value)
            else Some (Str, Str)
          in
          Some (Map (key_kind, value_kind))
        end
        else begin
          ignore (advance p);
          match parse_sized_type token.Token.lexeme with
          | Some kind -> Some kind
          | None -> Some (Named token.Token.lexeme)
        end
    | _ ->
        error_current p "E131"
          "expected a type (`int`, `dec`, `string`, `array`, `map`, `array<T>`, `map<K,V>`, `intN`, `decN`)";
        None
  in
  let end_span = previous_span p start_span in
  Some ({ kind; span = Span.merge start_span end_span } : type_ref)

and parse_cast_type p =
  let start_span = (current p).Token.span in
  let* kind = parse_type_atom p in
  let* end_tok = previous p in
  Some ({ kind; span = Span.merge start_span end_tok.Token.span } : type_ref)

and parse_type_atom p =
  match (current p).Token.kind with
  | Token.KwInt ->
      ignore (advance p);
      Some (Int None)
  | Token.KwUint ->
      ignore (advance p);
      Some (Uint None)
  | Token.KwDec ->
      ignore (advance p);
      Some (Dec None)
  | Token.KwString ->
      ignore (advance p);
      Some Str
  | Token.KwBool ->
      ignore (advance p);
      Some Bool
  | Token.KwF -> parse_function_type_kind p
  | Token.Identifier when (current p).Token.lexeme = "result" ->
      ignore (advance p);
      let* payload =
        if match_kind p Token.Less then
          let* payload = parse_type_ref p in
          let* _ = consume p Token.Greater "E169" "expected `>` in `result<T>`" in
          Some payload.kind
        else begin
          error_current p "E169" "`result` needs a payload type, as in `result<int>`";
          None
        end
      in
      Some (Result payload)
  | Token.Identifier ->
      let token = current p in
      ignore (advance p);
      (match parse_sized_type token.Token.lexeme with
      | Some kind -> Some kind
      | None -> Some (Named token.Token.lexeme))
  | _ ->
      error_current p "E139"
        "type parameter must be `int`, `dec`, `bool`, `string`, `intN`, `decN`, or a named struct";
      None

and parse_function_type_kind p =
  let* _ =
    consume p Token.KwF "E174" "expected `fn` in function pointer type"
  in
  let* _ =
    consume p Token.LParen "E174"
      "expected `(` after `fn` in function pointer type"
  in
  let* params =
    parse_comma_list_until p Token.RParen (fun p ->
        let* ty = parse_type_ref p in
        Some ty.kind)
  in
  let* _ =
    consume p Token.RParen "E174"
      "expected `)` after function pointer parameters"
  in
  let* _ =
    consume p Token.Arrow "E174"
      "expected `->` and a return type in function pointer type"
  in
  let* ret = parse_type_ref p in
  Some (Fn (params, ret.kind))

and parse_expression p min_bp =
  let* first = parse_prefix_expr p in
  parse_expression_tail p min_bp first

and parse_expression_tail p min_bp lhs =
  if at p Token.KwAs then
    if 21 < min_bp then Some lhs
    else begin
      ignore (advance p);
      let* ty = parse_cast_type p in
      let span = Span.merge lhs.span ty.span in
      parse_expression_tail p min_bp
        { kind = Cast ({ value = lhs; ty } : cast_expr); span }
    end
  else
    let token_kind = (current p).Token.kind in
    let lbp = binary_left_binding_power token_kind in
    if lbp = 0 || lbp < min_bp then Some lhs
    else begin
      ignore (advance p);
      let op = binary_operator token_kind in
      let rbp = if token_kind = Token.Caret then lbp else lbp + 1 in
      let* rhs = parse_expression p rbp in
      let span = Span.merge lhs.span rhs.span in
      parse_expression_tail p min_bp
        { kind = Binary ({ op; lhs; rhs } : binary_expr); span }
    end

and parse_prefix_expr p =
  if match_kind p Token.Minus then
    let op_span = previous_span p (current p).Token.span in
    let* rhs = parse_expression p 20 in
    Some
      { kind = Unary ({ op = Neg; rhs } : unary_expr)
      ; span = Span.merge op_span rhs.span
      }
  else if match_kind p Token.Not then
    let op_span = previous_span p (current p).Token.span in
    let* rhs = parse_expression p 20 in
    Some
      { kind = Unary ({ op = Not; rhs } : unary_expr)
      ; span = Span.merge op_span rhs.span
      }
  else if match_kind p Token.KwTry then
    let op_span = previous_span p (current p).Token.span in
    let* rhs = parse_expression p 20 in
    Some { kind = Try rhs; span = Span.merge op_span rhs.span }
  else if match_kind p Token.Tilde then
    let op_span = previous_span p (current p).Token.span in
    let* rhs = parse_expression p 20 in
    Some
      { kind = Unary ({ op = BitNot; rhs } : unary_expr)
      ; span = Span.merge op_span rhs.span
      }
  else parse_postfix_expr p

and parse_postfix_expr p =
  let* first = parse_primary_expr p in
  parse_postfix_tail p first

and parse_postfix_tail p (expr : expr) =
  if match_kind p Token.LParen then begin
    let* args =
      parse_comma_list_until p Token.RParen (fun p -> parse_expression p 1)
    in
    let* rparen =
      consume p Token.RParen "E132" "expected `)` after arguments"
    in
    let span = Span.merge expr.span rparen.Token.span in
    parse_postfix_tail p
      { kind = Call ({ callee = expr; args } : call_expr); span }
  end
  else if match_kind p Token.LBracket then begin
    let* index = parse_expression p 1 in
    let* rbracket =
      consume p Token.RBracket "E137" "expected `]` after array index"
    in
    let span = Span.merge expr.span rbracket.Token.span in
    parse_postfix_tail p
      { kind = Index ({ base = expr; index } : index_expr); span }
  end
  else if match_kind p Token.Dot then begin
    let* field_tok =
      consume p Token.Identifier "E146" "expected field name after `.`"
    in
    let span = Span.merge expr.span field_tok.Token.span in
    parse_postfix_tail p
      { kind =
          Member ({ base = expr; field = field_tok.Token.lexeme } : member_expr)
      ; span
      }
  end
  else Some expr

and parse_primary_expr p =
  match (current p).Token.kind with
  | Token.IntLiteral ->
      let tok = advance p in
      Some { kind = Literal (LitInt tok.Token.lexeme); span = tok.Token.span }
  | Token.DecLiteral ->
      let tok = advance p in
      Some { kind = Literal (LitDec tok.Token.lexeme); span = tok.Token.span }
  | Token.StringLiteral ->
      let tok = advance p in
      Some
        { kind = Literal (LitStr (unescape_string_literal tok.Token.lexeme))
        ; span = tok.Token.span
        }
  | Token.KwTrue | Token.KwFalse ->
      let tok = advance p in
      Some
        { kind = Literal (LitBool (tok.Token.kind = Token.KwTrue))
        ; span = tok.Token.span
        }
  | Token.KwF -> parse_closure_expr p
  | Token.KwCstruct ->
      let tok = advance p in
      let* segments, end_span = parse_path_tail p [ "cstruct" ] tok.Token.span in
      let span = Span.merge tok.Token.span end_span in
      Some { kind = Path ({ segments; span } : path_expr); span }
  | Token.Identifier ->
      if kind_at p 1 = Token.LBrace then
        parse_struct_literal_expr p
      else
        let* path = parse_path_expr p in
        Some { kind = Path path; span = path.span }
  | Token.LBracket ->
      let lbracket = advance p in
      let* items =
        parse_comma_list_until p Token.RBracket (fun p -> parse_expression p 1)
      in
      let* rbracket =
        consume p Token.RBracket "E138" "expected `]` after array literal"
      in
      Some
        { kind = ArrayLiteral ({ items } : array_literal_expr)
        ; span = Span.merge lbracket.Token.span rbracket.Token.span
        }
  | Token.LParen ->
      let lparen = advance p in
      let* inner = parse_expression p 1 in
      let* rparen =
        consume p Token.RParen "E133" "expected `)` after expression"
      in
      Some
        { kind = Grouping inner
        ; span = Span.merge lparen.Token.span rparen.Token.span
        }
  | _ ->
      error_current p "E134" "expected expression";
      None

and parse_closure_expr p =
  let* start = consume p Token.KwF "E190" "expected `fn`" in
  let* _ = consume p Token.LParen "E191" "expected `(` after `fn`" in
  let* params = parse_comma_list_until p Token.RParen parse_param in
  let* _ =
    consume p Token.RParen "E192" "expected `)` after closure parameters"
  in
  let* _ =
    consume p Token.Arrow "E193"
      "expected `->` and a return type after closure parameters"
  in
  let* return_ty = parse_type_ref p in
  let* body = parse_block_stmt p in
  let end_span = stmt_span body in
  Some
    { kind = Closure ({ params; return_ty; body } : closure_expr)
    ; span = Span.merge start.Token.span end_span
    }

and parse_struct_literal_expr p =
  let* name_tok =
    consume p Token.Identifier "E147"
      "expected struct name in struct literal"
  in
  let* _ =
    consume p Token.LBrace "E148" "expected `{` in struct literal"
  in
  let* fields =
    if at p Token.RBrace then Some [] else parse_struct_literal_fields p []
  in
  let* rbrace =
    consume p Token.RBrace "E176"
      "expected `}` after struct literal fields"
  in
  Some
    { kind =
        StructLiteral
          ({ name = name_tok.Token.lexeme; fields } : struct_literal_expr)
    ; span = Span.merge name_tok.Token.span rbrace.Token.span
    }

and parse_struct_literal_fields p acc =
  let* field_tok =
    consume p Token.Identifier "E149" "expected field name in struct literal"
  in
  let* _ =
    consume p Token.Colon "E175"
      "expected `:` after struct literal field name"
  in
  let* value = parse_expression p 1 in
  let field =
    ({ name = field_tok.Token.lexeme
     ; value
     ; span = Span.merge field_tok.Token.span value.span
     }
      : struct_literal_field_expr)
  in
  let acc = field :: acc in
  if not (match_kind p Token.Comma) then Some (List.rev acc)
  else if at p Token.RBrace then Some (List.rev acc)
  else parse_struct_literal_fields p acc

and parse_path_expr p =
  let* first = consume p Token.Identifier "E135" "expected identifier" in
  let* (segments, end_span) =
    parse_path_tail p [ first.Token.lexeme ] first.Token.span
  in
  Some
    ({ segments
     ; span = Span.merge first.Token.span end_span
     }
      : path_expr)

and parse_path_tail p acc end_span =
  if match_kind p Token.Namespace then
    let* ident =
      consume p Token.Identifier "E136" "expected identifier after `::`"
    in
    parse_path_tail p (ident.Token.lexeme :: acc) ident.Token.span
  else Some (List.rev acc, end_span)

and looks_like_io_chain p =
  if p.idx + 3 >= p.token_count then false
  else
    let a = Array.unsafe_get p.tokens p.idx in
    let c = Array.unsafe_get p.tokens (p.idx + 2) in
    a.Token.kind = Token.Identifier
    && (Array.unsafe_get p.tokens (p.idx + 1)).Token.kind = Token.Namespace
    && c.Token.kind = Token.Identifier
    && (Array.unsafe_get p.tokens (p.idx + 3)).Token.kind = Token.ShiftLeft
    && a.Token.lexeme = "io"
    && (c.Token.lexeme = "wrt"
        || c.Token.lexeme = "wrtl"
        || c.Token.lexeme = "wrtr")

and looks_like_var_decl p =
  let i = ref 0 in
  (match kind_at p !i with
  | Token.KwRef | Token.KwLink -> incr i
  | _ -> ());
  match type_ref_lookahead_len p !i with
  | None -> false
  | Some ty_len -> kind_at p (!i + ty_len) = Token.Identifier

and type_ref_lookahead_len p offset =
  let token =
    if p.idx + offset < p.token_count then
      Array.unsafe_get p.tokens (p.idx + offset)
    else eof_token
  in
  match token.Token.kind with
      | Token.KwInt | Token.KwUint | Token.KwDec | Token.KwString | Token.KwBool
        ->
          Some 1
      | Token.KwF -> function_type_lookahead_len p offset
      | Token.KwArray ->
          if kind_at p (offset + 1) <> Token.Less then Some 1
          else
            let* elem_len = type_ref_lookahead_len p (offset + 2) in
            if kind_at p (offset + 2 + elem_len) = Token.Greater
            then Some (3 + elem_len)
            else None
      | Token.Identifier when token.Token.lexeme = "result" ->
          if kind_at p (offset + 1) <> Token.Less then None
          else
            let* payload_len = type_ref_lookahead_len p (offset + 2) in
            if kind_at p (offset + 2 + payload_len) = Token.Greater
            then Some (3 + payload_len)
            else None
      | Token.Identifier when token.Token.lexeme = "map" ->
          if kind_at p (offset + 1) <> Token.Less then Some 1
          else
            let* key_len = type_ref_lookahead_len p (offset + 2) in
            if kind_at p (offset + 2 + key_len) <> Token.Comma
            then None
            else
              let value_offset = offset + 3 + key_len in
              let* value_len = type_ref_lookahead_len p value_offset in
              if kind_at p (value_offset + value_len) = Token.Greater
              then Some (value_offset + value_len + 1 - offset)
              else None
      | Token.Identifier -> Some 1
      | _ -> None

and function_type_lookahead_len p offset =
  if kind_at p (offset + 1) <> Token.LParen then None
  else
    let i = ref (offset + 2) in
    if kind_at p !i <> Token.RParen then begin
      let keep_going = ref true in
      while !keep_going do
        match type_ref_lookahead_len p !i with
        | None -> keep_going := false
        | Some len ->
            i := !i + len;
            if kind_at p !i = Token.Comma then i := !i + 1
            else keep_going := false
      done
    end;
    if kind_at p !i <> Token.RParen then
      None
    else begin
      i := !i + 1;
      if kind_at p !i <> Token.Arrow then
        None
      else begin
        i := !i + 1;
        let* ret_len = type_ref_lookahead_len p !i in
        Some (!i + ret_len - offset)
      end
    end

and sync_top_level p =
  while
    (not (at p Token.Eof))
    && (not (at p Token.Include))
    && (not (at p Token.KwStruct))
    && not (at p Token.KwF)
  do
    ignore (advance p)
  done

and sync_stmt p =
  let keep_going = ref true in
  while !keep_going && not (at p Token.Eof) do
    if match_kind p Token.Semicolon then keep_going := false
    else if at p Token.RBrace then keep_going := false
    else ignore (advance p)
  done

and sync_switch_arm p =
  let keep_going = ref true in
  while
    !keep_going
    && (not (at p Token.Eof))
    && (not (at p Token.RBrace))
    && (not (at p Token.KwCase))
    && not (at p Token.KwDefault)
  do
    if match_kind p Token.Semicolon then keep_going := false
    else ignore (advance p)
  done

and stmt_span = function
  | VarDecl v -> v.span
  | Break b | Continue b -> b.span
  | ForEach f -> f.span
  | Assign a -> a.span
  | Return r -> r.span
  | If i -> i.span
  | Switch s -> s.span
  | While w -> w.span
  | For f -> f.span
  | IoChain io -> io.span
  | ExprStmt e -> e.span
  | Block b -> b.span

and is_io_endl_expr expr =
  match expr.kind with
  | Path path -> path.segments = [ "io"; "endl" ]
  | _ -> false

and parse_io_chain_items p acc =
  let* expr = parse_expression p 1 in
  let item = if is_io_endl_expr expr then Endl expr.span else IoExpr expr in
  if match_kind p Token.ShiftLeft then parse_io_chain_items p (item :: acc)
  else Some (List.rev (item :: acc))

and parse_comma_list_until :
    'a. state -> Token.kind -> (state -> 'a option) -> 'a list option =
 fun p terminator parse_item ->
  if at p terminator then Some []
  else
    let rec loop acc =
      let* item = parse_item p in
      if match_kind p Token.Comma then loop (item :: acc)
      else Some (List.rev (item :: acc))
    in
    loop []

and binary_left_binding_power = function
  | Token.OrOr -> 2
  | Token.AndAnd -> 4
  | Token.Pipe -> 6
  | Token.CaretCaret -> 8
  | Token.Amp -> 10
  | Token.EqEq | Token.NotEq -> 12
  | Token.Less | Token.LessEq | Token.Greater | Token.GreaterEq -> 14
  | Token.Plus | Token.Minus -> 16
  | Token.Star | Token.Slash | Token.Percent -> 18
  | Token.Caret -> 20
  | _ -> 0

and binary_operator = function
  | Token.OrOr -> Or
  | Token.AndAnd -> And
  | Token.Pipe -> BitOr
  | Token.CaretCaret -> BitXor
  | Token.Amp -> BitAnd
  | Token.EqEq -> Eq
  | Token.NotEq -> Ne
  | Token.Less -> Lt
  | Token.LessEq -> Le
  | Token.Greater -> Gt
  | Token.GreaterEq -> Ge
  | Token.Plus -> Add
  | Token.Minus -> Sub
  | Token.Star -> Mul
  | Token.Slash -> Div
  | Token.Percent -> Mod
  | Token.Caret -> Pow
  | _ -> invalid_arg "binary_operator"

and is_scalar_map_key = function
  | Int _ | Uint _ | Dec _ | Bool | Str -> true
  | _ -> false

and parse_sized_type value =
  let parse_prefixed prefix ctor =
    let plen = String.length prefix in
    let rec has_prefix i =
      i = plen || (value.[i] = prefix.[i] && has_prefix (i + 1))
    in
    if String.length value > plen && has_prefix 0 then
      let rest = String.sub value plen (String.length value - plen) in
      if
        rest <> ""
        && string_for_all (fun c -> c >= '0' && c <= '9') rest
      then
        try Some (ctor (Some (int_of_string rest))) with Failure _ -> None
      else None
    else None
  in
  match parse_prefixed "uint" (fun x -> Uint x) with
  | Some _ as v -> v
  | None -> (
      match parse_prefixed "int" (fun x -> Int x) with
      | Some _ as v -> v
      | None -> parse_prefixed "dec" (fun x -> Dec x))

and unescape_string_literal raw =
  let len = String.length raw in
  if len < 2 then raw
  else if not (String.contains_from raw 1 '\\') then
    String.sub raw 1 (len - 2)
  else
    let b = Buffer.create len in
    let i = ref 1 in
    while !i < len - 1 do
      let ch = raw.[!i] in
      if ch <> '\\' then Buffer.add_char b ch
      else begin
        incr i;
        if !i < len - 1 then
          match raw.[!i] with
          | 'n' -> Buffer.add_char b '\n'
          | 'r' -> Buffer.add_char b '\r'
          | 't' -> Buffer.add_char b '\t'
          | '"' -> Buffer.add_char b '"'
          | '\\' -> Buffer.add_char b '\\'
          | other -> Buffer.add_char b other
      end;
      incr i
    done;
    Buffer.contents b

and string_for_all pred s =
  let rec loop i =
    if i = String.length s then true
    else if pred s.[i] then loop (i + 1)
    else false
  in
  loop 0

let parse tokens =
  let parser = make_state tokens in
  let program = parse_program parser in
  match List.rev parser.diagnostics with
  | [] -> Ok program
  | diagnostics -> Error diagnostics
