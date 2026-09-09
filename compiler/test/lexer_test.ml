let fail format = Printf.ksprintf failwith format

let diagnostics source =
  let _, diagnostics = Xic.Lexer.lex source in
  diagnostics

let tokens source =
  let tokens, diagnostics = Xic.Lexer.lex source in
  if diagnostics <> [] then fail "unexpected lexer diagnostic for %S" source;
  Array.to_list tokens

let has_kind kind tokens =
  List.exists (fun (token : Xic.Token.t) -> token.kind = kind) tokens

let expect_one_e000 source =
  match diagnostics source with
  | [ diagnostic ] ->
      if diagnostic.code <> "E000" then
        fail "expected E000 for %S, got %s" source diagnostic.code;
      if diagnostic.span.start <> 0
         || diagnostic.span.end_ <> String.length source
      then fail "E000 did not cover malformed token %S" source
  | diagnostics ->
      fail "expected one diagnostic for %S, got %d" source
        (List.length diagnostics)

let () =
  Xic.Lexer.set_collect_token_widths true;
  Xic.Lexer.reset_metrics ();
  ignore (tokens "a longer zz");
  if Xic.Lexer.mean_token_bytes () <> 3. then
    fail "expected mean token width of 3 bytes";
  if Xic.Lexer.median_token_bytes () <> 2. then
    fail "expected median token width of 2 bytes";
  Xic.Lexer.set_collect_token_widths false;

  List.iter
    (fun source ->
      let source_tokens, source_diagnostics = Xic.Lexer.lex source in
      if source_diagnostics = [] then fail "expected directive suffix rejection";
      if has_kind Xic.Token.Include (Array.to_list source_tokens)
         || has_kind Xic.Token.LinkDirective (Array.to_list source_tokens)
         || has_kind Xic.Token.SubsystemDirective (Array.to_list source_tokens)
      then fail "directive prefix was accepted in %S" source;
      expect_one_e000 source)
    [ "#includeWhatever"; "#linkage"; "#subsystemFoo" ];

  expect_one_e000 "\"unterminated";
  List.iter expect_one_e000 [ "1_"; "1__2"; "0x_FF"; "0xFF__"; "12._" ];

  ignore (tokens "1_000 0xFF_FF 0b1010_0101 0o7_7 12.34_56");

  let valid_c = tokens "c f value(int32 n) -> int32 { return n; }" in
  if not (has_kind Xic.Token.RawCBlock valid_c) then
    fail "valid inline C body was not captured";

  let malformed_c = tokens "c f broken ; f main() { return 0; }" in
  if has_kind Xic.Token.RawCBlock malformed_c then
    fail "malformed c f state leaked into a later Xi block";

  let preserved = tokens "c preamble { /* C comment */ int x; }" in
  let raw =
    List.find (fun (token : Xic.Token.t) -> token.kind = Xic.Token.RawCBlock)
      preserved
  in
  if not (String.contains raw.lexeme '*') then
    fail "C block comment was stripped from raw C";

  let _, unterminated_c =
    Xic.Lexer.lex "c preamble { /* unterminated\n}"
  in
  if List.exists
       (fun (diagnostic : Xic.Lexer.diagnostic) -> diagnostic.code = "E001")
       unterminated_c
  then fail "Xi comment diagnostics leaked into raw C"
