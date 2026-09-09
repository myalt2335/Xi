type t = { code : string; message : string; span : Span.t }

let make code message span = { code; message; span }

type line_col = { line : int; col : int }

let offset_to_line_col source offset =
  let limit = min offset (String.length source) in
  let line = ref 1 in
  let col = ref 1 in
  let idx = ref 0 in
  while !idx < limit do
    let ch = source.[!idx] in
    if ch = '\n' then begin
      incr line;
      col := 1
    end
    else incr col;
    incr idx
  done;
  { line = !line; col = !col }

let format filename source d =
  let lc = offset_to_line_col source d.span.start in
  Printf.sprintf "error[%s] %s:%d:%d: %s" d.code filename lc.line lc.col
    d.message
