let escape s =
  let b = Buffer.create (String.length s + 2) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | '\b' -> Buffer.add_string b "\\b"
      | '\012' -> Buffer.add_string b "\\f"
      | c when Char.code c < 0x20 ->
          Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let tokens_to_json (tokens : Token.t array) : string =
  if Array.length tokens = 0 then "[]"
  else begin
      let b = Buffer.create 4096 in
      Buffer.add_string b "[\n";
      Array.iteri
        (fun idx (t : Token.t) ->
          if idx > 0 then Buffer.add_string b ",\n";
          Buffer.add_string b "  {\n";
          Buffer.add_string b
            (Printf.sprintf "    \"kind\": \"%s\",\n"
               (Token.kind_to_string t.kind));
          Buffer.add_string b
            (Printf.sprintf "    \"lexeme\": \"%s\",\n" (escape t.lexeme));
          Buffer.add_string b "    \"span\": {\n";
          Buffer.add_string b
            (Printf.sprintf "      \"start\": %d,\n" t.span.start);
          Buffer.add_string b
            (Printf.sprintf "      \"end\": %d\n" t.span.end_);
          Buffer.add_string b "    }\n";
          Buffer.add_string b "  }")
        tokens;
      Buffer.add_string b "\n]";
      Buffer.contents b
  end

let diagnostics_to_json (diagnostics : Diagnostic.t list) : string =
  match diagnostics with
  | [] -> "[]"
  | _ ->
      let b = Buffer.create 1024 in
      Buffer.add_string b "[\n";
      List.iteri
        (fun idx (d : Diagnostic.t) ->
          if idx > 0 then Buffer.add_string b ",\n";
          Buffer.add_string b "  {\n";
          Buffer.add_string b
            (Printf.sprintf "    \"code\": \"%s\",\n" (escape d.code));
          Buffer.add_string b
            (Printf.sprintf "    \"message\": \"%s\",\n" (escape d.message));
          Buffer.add_string b "    \"span\": {\n";
          Buffer.add_string b
            (Printf.sprintf "      \"start\": %d,\n" d.span.start);
          Buffer.add_string b
            (Printf.sprintf "      \"end\": %d\n" d.span.end_);
          Buffer.add_string b "    }\n";
          Buffer.add_string b "  }")
        diagnostics;
      Buffer.add_string b "\n]";
      Buffer.contents b
