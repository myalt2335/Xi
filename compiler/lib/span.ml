type t = { start : int; end_ : int }

let make start end_ = { start; end_ }

let merge a b = { start = a.start; end_ = b.end_ }
