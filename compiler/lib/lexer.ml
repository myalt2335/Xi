type diagnostic = { code : string; message : string; span : Span.t }

let measured_source_lines = ref 0
let measured_source_bytes = ref 0
let measured_tokens = ref 0
let measured_tokenization_seconds = ref 0.
let collect_token_widths = ref false
let measured_token_widths = ref (Array.make 1024 0)
let measured_token_width_count = ref 0

let set_collect_token_widths enabled = collect_token_widths := enabled

let reset_metrics () =
  measured_source_lines := 0;
  measured_source_bytes := 0;
  measured_tokens := 0;
  measured_tokenization_seconds := 0.;
  measured_token_width_count := 0

let metrics () =
  (!measured_source_lines, !measured_source_bytes, !measured_tokens,
   !measured_tokenization_seconds)

let record_token_width width =
  if !measured_token_width_count >= Array.length !measured_token_widths then begin
    let bigger = Array.make (2 * Array.length !measured_token_widths) 0 in
    Array.blit !measured_token_widths 0 bigger 0 !measured_token_width_count;
    measured_token_widths := bigger
  end;
  Array.unsafe_set !measured_token_widths !measured_token_width_count width;
  incr measured_token_width_count

let mean_token_bytes () =
  let count = !measured_token_width_count in
  if count = 0 then 0.
  else begin
    let total = ref 0. in
    for index = 0 to count - 1 do
      total := !total +. float_of_int (Array.unsafe_get !measured_token_widths index)
    done;
    !total /. float_of_int count
  end

let median_token_bytes () =
  let count = !measured_token_width_count in
  if count = 0 then 0.
  else begin
    let widths = Array.sub !measured_token_widths 0 count in
    Array.sort Int.compare widths;
    if count mod 2 = 1 then float_of_int widths.(count / 2)
    else
      let upper = count / 2 in
      float_of_int (widths.(upper - 1) + widths.(upper)) /. 2.
  end

let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_ident_start c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_'
let is_ident c = is_ident_start c || is_digit c
let is_ws c = c = ' ' || c = '\t' || c = '\r' || c = '\n' || c = '\012'
let is_bin c = c = '0' || c = '1'
let is_oct c = c >= '0' && c <= '7'

let strip_underscores s =
  if not (String.contains s '_') then s
  else begin
    let b = Buffer.create (String.length s) in
    String.iter (fun c -> if c <> '_' then Buffer.add_char b c) s;
    Buffer.contents b
  end

let digit_value c =
  if is_digit c then Char.code c - Char.code '0'
  else if c >= 'a' && c <= 'f' then Char.code c - Char.code 'a' + 10
  else Char.code c - Char.code 'A' + 10

let radix_to_decimal body radix =
  let acc = ref (Array.make 1 0) in
  let len = ref 1 in
  let push d =
    if !len >= Array.length !acc then begin
      let na = Array.make (2 * Array.length !acc) 0 in
      Array.blit !acc 0 na 0 !len;
      acc := na
    end;
    !acc.(!len) <- d;
    incr len
  in
  String.iter
    (fun ch ->
      if ch <> '_' then begin
        let carry = ref (digit_value ch) in
        for k = 0 to !len - 1 do
          let v = (!acc.(k) * radix) + !carry in
          !acc.(k) <- v mod 10;
          carry := v / 10
        done;
        while !carry > 0 do
          push (!carry mod 10);
          carry := !carry / 10
        done
      end)
    body;
  while !len > 1 && !acc.(!len - 1) = 0 do
    decr len
  done;
  let buf = Buffer.create !len in
  for k = !len - 1 downto 0 do
    Buffer.add_char buf (Char.chr (Char.code '0' + !acc.(k)))
  done;
  Buffer.contents buf

let keyword = function
  | "extern" -> Some Token.KwExtern
  | "cextern" -> Some Token.KwCextern
  | "cstruct" -> Some Token.KwCstruct
  | "intrinsic" -> Some Token.KwIntrinsic
  | "f" | "fn" | "func" | "function" -> Some Token.KwF
  | "if" -> Some Token.KwIf
  | "else" -> Some Token.KwElse
  | "switch" -> Some Token.KwSwitch
  | "match" -> Some Token.KwMatch
  | "case" -> Some Token.KwCase
  | "default" -> Some Token.KwDefault
  | "while" -> Some Token.KwWhile
  | "for" -> Some Token.KwFor
  | "break" -> Some Token.KwBreak
  | "continue" -> Some Token.KwContinue
  | "try" -> Some Token.KwTry
  | "in" -> Some Token.KwIn
  | "return" -> Some Token.KwReturn
  | "int" -> Some Token.KwInt
  | "uint" -> Some Token.KwUint
  | "dec" -> Some Token.KwDec
  | "string" -> Some Token.KwString
  | "bool" -> Some Token.KwBool
  | "true" -> Some Token.KwTrue
  | "false" -> Some Token.KwFalse
  | "array" -> Some Token.KwArray
  | "ref" -> Some Token.KwRef
  | "link" -> Some Token.KwLink
  | "struct" -> Some Token.KwStruct
  | "as" -> Some Token.KwAs
  | _ -> None

type c_lex_state =
  | CNormal
  | CAfterKeyword
  | CExpectBlock
  | CFunctionExpectName
  | CFunctionExpectParams
  | CFunctionParams of int
  | CFunctionExpectArrow
  | CFunctionExpectReturn
  | CFunctionReady

let scan_raw_c_block source start =
  let n = String.length source in
  let i = ref (start + 1) in
  let depth = ref 1 in
  let mode = ref `Normal in
  let escaped = ref false in
  while !i < n && !depth > 0 do
    let c = source.[!i] in
    let next ch = !i + 1 < n && source.[!i + 1] = ch in
    (match !mode with
     | `String ->
         if !escaped then escaped := false
         else if c = '\\' then escaped := true
         else if c = '"' then mode := `Normal
     | `Char ->
         if !escaped then escaped := false
         else if c = '\\' then escaped := true
         else if c = '\'' then mode := `Normal
     | `LineComment -> if c = '\n' then mode := `Normal
     | `BlockComment ->
         if c = '*' && next '/' then begin mode := `Normal; incr i end
     | `Normal ->
         if c = '"' then mode := `String
         else if c = '\'' then mode := `Char
         else if c = '/' && next '/' then begin mode := `LineComment; incr i end
         else if c = '/' && next '*' then begin mode := `BlockComment; incr i end
         else if c = '{' then incr depth
         else if c = '}' then decr depth);
    incr i
  done;
  if !depth = 0 then
    let close = !i - 1 in
    Some (String.sub source (start + 1) (close - start - 1), !i)
  else None

let count_source_lines source =
  let n = String.length source in
  if n = 0 then 0
  else begin
    let newlines = ref 0 in
    let i = ref 0 in
    (try
       while true do
         let j = String.index_from source !i '\n' in
         incr newlines;
         i := j + 1
       done
     with Not_found -> ());
    !newlines + if source.[n - 1] = '\n' then 0 else 1
  end

let one_char_lexemes = Array.init 256 (fun code -> String.make 1 (Char.chr code))

type token_match =
  | Matched of Token.kind * string * int
  | Malformed of string * int
  | NoMatch

let match_directive source cleaned n i text kind =
  let length = String.length text in
  let end_ = i + length in
  if end_ <= n && String.sub cleaned i length = text then
    if end_ = n || not (is_ident cleaned.[end_]) then
      Matched (kind, String.sub source i length, end_)
    else begin
      let suffix_end = ref (end_ + 1) in
      while !suffix_end < n && is_ident cleaned.[!suffix_end] do
        incr suffix_end
      done;
      Malformed ("invalid preprocessor directive", !suffix_end)
    end
  else NoMatch

let scan_digits source n digit start =
  let j = ref start in
  let valid = ref true in
  while !j < n && (digit source.[!j] || source.[!j] = '_') do
    if source.[!j] = '_'
       && (!j = start
           || not (digit source.[!j - 1])
           || !j + 1 >= n
           || not (digit source.[!j + 1]))
    then valid := false;
    incr j
  done;
  (!j, !valid)

let malformed_number_end source n start =
  let j = ref start in
  while !j < n && is_ident source.[!j] do incr j done;
  !j

let match_token source cleaned n i =
  let has k = i + k < n in
  let at k = cleaned.[i + k] in
  let sub a b = String.sub source a (b - a) in
  let c = cleaned.[i] in
  let multi_char () =
    if not (has 1) then None
    else
      let d = at 1 in
      if c = '.' && d = '.' then
        (if has 2 && at 2 = '<' then Some (Token.RangeExclusive, "..<")
         else Some (Token.RangeInclusive, ".."))
      else
        match c, d with
        | '=', '=' -> Some (Token.EqEq, "==")
        | '+', '=' -> Some (Token.PlusEq, "+=")
        | '-', '=' -> Some (Token.MinusEq, "-=")
        | '*', '=' -> Some (Token.StarEq, "*=")
        | '/', '=' -> Some (Token.SlashEq, "/=")
        | '%', '=' -> Some (Token.PercentEq, "%=")
        | '!', '=' -> Some (Token.NotEq, "!=")
        | '<', '=' -> Some (Token.LessEq, "<=")
        | '>', '=' -> Some (Token.GreaterEq, ">=")
        | '&', '&' -> Some (Token.AndAnd, "&&")
        | '|', '|' -> Some (Token.OrOr, "||")
        | '<', '<' -> Some (Token.ShiftLeft, "<<")
        | '^', '^' -> Some (Token.CaretCaret, "^^")
        | ':', ':' -> Some (Token.Namespace, "::")
        | '-', '>' -> Some (Token.Arrow, "->")
        | _ -> None
  in
  let one_char () =
    match c with
    | '=' -> Some Token.Eq
    | '+' -> Some Token.Plus
    | '-' -> Some Token.Minus
    | '*' -> Some Token.Star
    | '/' | '\\' -> Some Token.Slash
    | '%' -> Some Token.Percent
    | '^' -> Some Token.Caret
    | '&' -> Some Token.Amp
    | '|' -> Some Token.Pipe
    | '~' -> Some Token.Tilde
    | '<' -> Some Token.Less
    | '>' -> Some Token.Greater
    | '!' -> Some Token.Not
    | '(' -> Some Token.LParen
    | ')' -> Some Token.RParen
    | '{' -> Some Token.LBrace
    | '}' -> Some Token.RBrace
    | '[' -> Some Token.LBracket
    | ']' -> Some Token.RBracket
    | ',' -> Some Token.Comma
    | ':' -> Some Token.Colon
    | ';' -> Some Token.Semicolon
    | '.' -> Some Token.Dot
    | _ -> None
  in
  if c = '#' then
    if i + 8 <= n && String.sub cleaned i 8 = "#include" then
      match_directive source cleaned n i "#include" Token.Include
    else if i + 10 <= n && String.sub cleaned i 10 = "#subsystem" then
      match_directive source cleaned n i "#subsystem" Token.SubsystemDirective
    else if i + 5 <= n && String.sub cleaned i 5 = "#link" then
      match_directive source cleaned n i "#link" Token.LinkDirective
    else NoMatch
  else if c = '"' then begin
    let j = ref (i + 1) in
    let closed = ref false in
    while (not !closed) && !j < n do
      if cleaned.[!j] = '\\' then j := !j + 2
      else if cleaned.[!j] = '"' then begin
        closed := true;
        incr j
      end
      else incr j
    done;
    if !closed then Matched (Token.StringLiteral, sub i !j, !j)
    else Malformed ("unterminated string literal", n)
  end
  else if is_digit c then begin
    let radix digit base =
      let body = i + 2 in
      if body >= n || not (digit cleaned.[body]) then
        let message =
          if body < n && cleaned.[body] = '_' then "invalid numeric separator"
          else "invalid numeric literal"
        in
        Malformed (message, malformed_number_end cleaned n body)
      else
        let end_, valid = scan_digits cleaned n digit body in
        if not valid || (end_ < n && is_ident cleaned.[end_]) then
          Malformed
            ("invalid numeric separator", malformed_number_end cleaned n end_)
        else Matched (Token.IntLiteral, radix_to_decimal (sub body end_) base, end_)
    in
    if c = '0' && has 1 && (at 1 = 'x' || at 1 = 'X') then radix is_hex 16
    else if c = '0' && has 1 && (at 1 = 'b' || at 1 = 'B') then radix is_bin 2
    else if c = '0' && has 1 && (at 1 = 'o' || at 1 = 'O') then radix is_oct 8
    else begin
      let j, integer_valid = scan_digits cleaned n is_digit i in
      if not integer_valid then
        Malformed
          ("invalid numeric separator", malformed_number_end cleaned n j)
      else if j + 1 < n && cleaned.[j] = '.' && is_digit cleaned.[j + 1] then begin
        let k, fraction_valid = scan_digits cleaned n is_digit (j + 1) in
        if fraction_valid then
          Matched (Token.DecLiteral, strip_underscores (sub i k), k)
        else
          Malformed
            ("invalid numeric separator", malformed_number_end cleaned n k)
      end
      else if j + 1 < n && cleaned.[j] = '.' && cleaned.[j + 1] = '_' then
        Malformed
          ("invalid numeric separator", malformed_number_end cleaned n (j + 1))
      else Matched (Token.IntLiteral, strip_underscores (sub i j), j)
    end
  end
  else if is_ident_start c then begin
    let j = ref (i + 1) in
    while !j < n && is_ident cleaned.[!j] do incr j done;
    let text = sub i !j in
    let kind = match keyword text with Some k -> k | None -> Token.Identifier in
    Matched (kind, text, !j)
  end
  else
  match multi_char () with
    | Some (k, lexeme) -> Matched (k, lexeme, i + String.length lexeme)
    | None -> (
        match one_char () with
        | Some k ->
            Matched
              (k, Array.unsafe_get one_char_lexemes (Char.code source.[i]), i + 1)
        | None -> NoMatch)

let eof_token = { Token.kind = Token.Eof; lexeme = ""; span = Span.make 0 0 }

let lex source =
  let started = Clock.now () in
  measured_source_bytes := !measured_source_bytes + String.length source;
  let cleaned = source in
  let diags = [] in
  let source_lines = count_source_lines source in
  measured_source_lines := !measured_source_lines + source_lines;
  let n = String.length cleaned in
  let tokens = ref (Array.make (max 64 (n / 3)) eof_token) in
  let count = ref 0 in
  let push token =
    if !count >= Array.length !tokens then begin
      let bigger = Array.make (2 * Array.length !tokens) eof_token in
      Array.blit !tokens 0 bigger 0 !count;
      tokens := bigger
    end;
    Array.unsafe_set !tokens !count token;
    incr count
  in
  let diags = ref (List.rev diags) in
  let i = ref 0 in
  let c_state = ref CNormal in
  while !i < n do
    let c = String.unsafe_get cleaned !i in
    if is_ws c then incr i
    else if !i + 1 < n && c = '/' && cleaned.[!i + 1] = '/' then begin
      i := !i + 2;
      while !i < n && cleaned.[!i] <> '\n' do incr i done
    end
    else if !i + 1 < n && c = '/' && cleaned.[!i + 1] = '*' then begin
      let start = !i in
      i := !i + 2;
      let depth = ref 1 in
      while !i < n && !depth > 0 do
        if !i + 1 < n && cleaned.[!i] = '/' && cleaned.[!i + 1] = '*' then begin
          i := !i + 2;
          incr depth
        end
        else if !i + 1 < n && cleaned.[!i] = '*' && cleaned.[!i + 1] = '/'
        then begin
          i := !i + 2;
          decr depth
        end
        else incr i
      done;
      if !depth > 0 then
        diags :=
          { code = "E001"; message = "unterminated block comment";
            span = Span.make start n }
          :: !diags
    end
    else if c = '{' && (!c_state = CExpectBlock || !c_state = CFunctionReady) then begin
      match scan_raw_c_block source !i with
      | Some (body, end_) ->
          push { Token.kind = Token.RawCBlock; lexeme = body; span = Span.make !i end_ };
          c_state := CNormal;
          i := end_
      | None ->
          diags :=
            { code = "E002"; message = "unterminated inline C block";
              span = Span.make !i n }
            :: !diags;
          i := n
    end
    else
      match match_token source cleaned n !i with
      | Matched (kind, lexeme, end_) ->
          push { Token.kind; lexeme; span = Span.make !i end_ };
          c_state :=
            (match !c_state, kind with
             | CNormal, Token.Identifier when lexeme = "c" -> CAfterKeyword
             | CAfterKeyword, Token.Identifier when lexeme = "preamble" -> CExpectBlock
             | CAfterKeyword, Token.KwF -> CFunctionExpectName
             | CFunctionExpectName, Token.Identifier -> CFunctionExpectParams
             | CFunctionExpectParams, Token.LParen -> CFunctionParams 1
             | CFunctionParams depth, Token.LParen -> CFunctionParams (depth + 1)
             | CFunctionParams 1, Token.RParen -> CFunctionExpectArrow
             | CFunctionParams depth, Token.RParen -> CFunctionParams (depth - 1)
             | CFunctionParams depth,
               (Token.Identifier | Token.KwF | Token.KwInt | Token.KwUint
               | Token.KwDec | Token.KwString | Token.KwBool | Token.Comma
               | Token.Less | Token.Greater | Token.Arrow) ->
                 CFunctionParams depth
             | CFunctionExpectArrow, Token.Arrow -> CFunctionExpectReturn
             | CFunctionExpectReturn,
               (Token.Identifier | Token.KwF | Token.KwInt | Token.KwUint
               | Token.KwDec | Token.KwString | Token.KwBool) ->
                 CFunctionReady
             | _ -> CNormal);
          i := end_
      | Malformed (message, end_) ->
          diags :=
            { code = "E000"; message; span = Span.make !i end_ }
            :: !diags;
          c_state := CNormal;
          i := max (!i + 1) end_
      | NoMatch ->
          diags :=
            { code = "E000"; message = "invalid token";
              span = Span.make !i (!i + 1) }
            :: !diags;
          c_state := CNormal;
          incr i
  done;
  measured_tokens := !measured_tokens + !count;
  push
    { Token.kind = Token.Eof; lexeme = "";
      span = Span.make (String.length source) (String.length source) };
  let tokens = Array.sub !tokens 0 !count in
  measured_tokenization_seconds :=
    !measured_tokenization_seconds +. (Clock.now () -. started);
  if !collect_token_widths then
    for token_index = 0 to !count - 2 do
      let token = Array.unsafe_get tokens token_index in
      record_token_width (token.Token.span.end_ - token.Token.span.start)
    done;
  (tokens, List.rev !diags)
