let fail format = Printf.ksprintf failwith format

let diagnostic_codes source =
  let tokens, lex_diagnostics = Xic.Lexer.lex source in
  match lex_diagnostics with
  | _ :: _ -> List.map (fun (d : Xic.Lexer.diagnostic) -> d.code) lex_diagnostics
  | [] ->
      (match Xic.Parser.parse tokens with
       | Error diagnostics ->
           List.map (fun (d : Xic.Diagnostic.t) -> d.code) diagnostics
       | Ok program ->
           let result = Xic.Semantic.analyze program in
           List.map (fun (d : Xic.Diagnostic.t) -> d.code) result.diagnostics)

let expect_code expected source =
  let actual = diagnostic_codes source in
  if not (List.mem expected actual) then
    fail "expected %s, got [%s] for:\n%s"
      expected (String.concat ", " actual) source

let () =
  expect_code "E442"
    "struct Key { int value; } f main(map<Key, int> values) { }";
  expect_code "E499"
    "cextern int external_value = \"external_value\"; f main() { }";
  expect_code "E407"
    "f main() { array<int> xs = [1]; int value = xs[\"bad\"]; }";
  expect_code "E408"
    "f main() { map<string, int> values = map::new(); int value = values[1]; }";
  expect_code "E410"
    "f main() { array<string> xs = [\"x\"]; string value = arr::get_unchecked(xs, 0); }";
  expect_code "E443"
    "f main() { map<string, int> values = map::new(); values[1] = 2; }";
  expect_code "E456" "f main() { int value = str::len(1); }";
  expect_code "E557" "f main() { int value = str::char_code_at(1, 0); }";
  expect_code "E558" "f main() { string value = str::from_char_code(\"x\"); }";
  expect_code "E457" "f main() { int value = str::does_not_exist(); }"
