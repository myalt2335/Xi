type big = { neg : bool; mag : int array }

let base = 10_000
let limb_digits = 4

let mag_norm a =
  let n = ref (Array.length a) in
  while !n > 0 && a.(!n - 1) = 0 do
    decr n
  done;
  if !n = Array.length a then a else Array.sub a 0 !n

let mag_is_zero a = Array.length a = 0

let mag_cmp a b =
  let la = Array.length a and lb = Array.length b in
  if la <> lb then compare la lb
  else begin
    let result = ref 0 and i = ref (la - 1) in
    while !result = 0 && !i >= 0 do
      result := compare a.(!i) b.(!i);
      decr i
    done;
    !result
  end

let mag_add a b =
  let la = Array.length a and lb = Array.length b in
  let n = max la lb + 1 in
  let out = Array.make n 0 in
  let carry = ref 0 in
  for i = 0 to n - 1 do
    let sum = !carry + (if i < la then a.(i) else 0) + if i < lb then b.(i) else 0 in
    out.(i) <- sum mod base;
    carry := sum / base
  done;
  mag_norm out

let mag_sub a b =
  let la = Array.length a and lb = Array.length b in
  let out = Array.make la 0 in
  let borrow = ref 0 in
  for i = 0 to la - 1 do
    let d = a.(i) - !borrow - if i < lb then b.(i) else 0 in
    if d < 0 then begin
      out.(i) <- d + base;
      borrow := 1
    end
    else begin
      out.(i) <- d;
      borrow := 0
    end
  done;
  mag_norm out

let mag_mul a b =
  let la = Array.length a and lb = Array.length b in
  if la = 0 || lb = 0 then [||]
  else begin
    let out = Array.make (la + lb) 0 in
    for i = 0 to la - 1 do
      let carry = ref 0 in
      for j = 0 to lb - 1 do
        let cur = out.(i + j) + (a.(i) * b.(j)) + !carry in
        out.(i + j) <- cur mod base;
        carry := cur / base
      done;
      let k = ref (i + lb) in
      while !carry <> 0 do
        let cur = out.(!k) + !carry in
        out.(!k) <- cur mod base;
        carry := cur / base;
        incr k
      done
    done;
    mag_norm out
  end

let mag_of_digits digits =
  let n = String.length digits in
  let limbs = (n + limb_digits - 1) / limb_digits in
  let out = Array.make limbs 0 in
  let pos = ref n in
  for i = 0 to limbs - 1 do
    let lo = max 0 (!pos - limb_digits) in
    out.(i) <- int_of_string (String.sub digits lo (!pos - lo));
    pos := lo
  done;
  mag_norm out

let mag_to_string a =
  let n = Array.length a in
  if n = 0 then "0"
  else begin
    let buf = Buffer.create (n * limb_digits) in
    Buffer.add_string buf (string_of_int a.(n - 1));
    for i = n - 2 downto 0 do
      Buffer.add_string buf (Printf.sprintf "%04d" a.(i))
    done;
    Buffer.contents buf
  end

let big_zero = { neg = false; mag = [||] }

let big_make neg mag = if mag_is_zero mag then big_zero else { neg; mag }

let big_neg x = big_make (not x.neg) x.mag

let big_add x y =
  if x.neg = y.neg then big_make x.neg (mag_add x.mag y.mag)
  else
    let c = mag_cmp x.mag y.mag in
    if c = 0 then big_zero
    else if c > 0 then big_make x.neg (mag_sub x.mag y.mag)
    else big_make y.neg (mag_sub y.mag x.mag)

let big_sub x y = big_add x (big_neg y)
let big_mul x y = big_make (x.neg <> y.neg) (mag_mul x.mag y.mag)
let big_to_string x = (if x.neg then "-" else "") ^ mag_to_string x.mag

let big_of_literal s =
  let n = String.length s in
  let neg, start = if n = 0 then (false, 0) else match s.[0] with
    | '-' -> (true, 1)
    | '+' -> (false, 1)
    | _ -> (false, 0)
  in
  if start >= n then None
  else begin
    let digits = String.sub s start (n - start) in
    if String.for_all (fun c -> c >= '0' && c <= '9') digits then
      Some (big_make neg (mag_of_digits digits))
    else None
  end

let i128_max_mag = mag_of_digits "170141183460469231731687303715884105727"
let i128_min_mag = mag_of_digits "170141183460469231731687303715884105728"

let in_i128 x =
  mag_cmp x.mag (if x.neg then i128_min_mag else i128_max_mag) <= 0

let literal_fits_i128 v =
  match big_of_literal v with Some x -> in_i128 x | None -> false

let ( let* ) o f = match o with None -> None | Some x -> f x

(* Exactly `big_to_string (big_of_literal text) = text && in_i128`, decided
   without building the bignum for the overwhelmingly common short case. *)
let literal_is_canonical_i128 text =
  let n = String.length text in
  let start = if n > 0 && text.[0] = '-' then 1 else 0 in
  let digits = n - start in
  if digits = 0 then false
  else if text.[start] = '0' then digits = 1 && start = 0
  else if not (String.for_all (fun c -> c >= '0' && c <= '9')
                 (String.sub text start digits)) then false
  else if digits <= 38 then true
  else
    match big_of_literal text with
    | Some v -> in_i128 v
    | None -> false

(* Sharing-preserving maps: constant folding leaves almost every node alone, so
   return the original list/option/node whenever nothing underneath changed
   rather than reallocating the entire tree. *)
let rec map_shared f xs =
  match xs with
  | [] -> xs
  | x :: rest ->
      let x' = f x in
      let rest' = map_shared f rest in
      if x' == x && rest' == rest then xs else x' :: rest'

let opt_shared f o =
  match o with
  | None -> o
  | Some x ->
      let x' = f x in
      if x' == x then o else Some x'

(* Fold only while the value fits in a signed 128-bit integer.

   The bound is not about what we can represent -- `big` is unbounded -- it is
   about matching the reference, which folds in `i128` with `checked_*` and gives
   up on overflow. Widening it here would make us fold expressions the reference
   leaves alone, and a folded literal takes a different path through codegen than
   an unfolded one, so the two compilers would emit different IR. Checking the
   range once per level reproduces `checked_*` exactly: it returns None precisely
   when the true result leaves the range. *)
(* Bottom-up: rewrite the children, then try to collapse the node itself. The
   span is carried over from the original expression so diagnostics still point
   at the source the value came from. Carry the already-evaluated integer
   alongside the folded node. This avoids reparsing a freshly-rendered literal
   at every parent of a constant expression. *)
(* A linear transfer loop has the form:

     while (cells[p] != 0) { cells[p] = cells[p] - 1; ...; p = p + k; }

   with a net pointer movement of zero.  When the source is positive, the
   loop is ordinary integer arithmetic: every destination receives [source *
   delta] and the source becomes zero.  Do not apply this to a negative source:
   the original loop does not terminate in that case.  This is deliberately a
   recurrence optimization, not a Brainfuck special case: [cells] and [p] are
   merely local path names, so it applies to ordinary array processing too. *)
let linear_lit span n = { Ast.kind = Ast.Literal (Ast.LitInt (string_of_int n)); span }
let linear_path span name = { Ast.kind = Ast.Path { segments = [ name ]; span }; span }
let linear_bin span op lhs rhs = { Ast.kind = Ast.Binary { op; lhs; rhs }; span }

let linear_int = function
  | { Ast.kind = Ast.Literal (Ast.LitInt text); _ } ->
      (try Some (int_of_string text) with Failure _ -> None)
  | _ -> None

let linear_local = function
  | { Ast.kind = Ast.Path { segments = [ name ]; _ }; _ } -> Some name
  | _ -> None

let linear_index = function
  | { Ast.kind = Ast.Index { base; index }; _ } ->
      Option.bind (linear_local base) (fun array ->
        Option.map (fun index_name -> array, index_name) (linear_local index))
  | _ -> None

let linear_set_call = function
  | Ast.ExprStmt { expr = { kind = Ast.Call { callee = { kind = Ast.Path { segments = [ "__index"; "set" ]; _ }; _ }; args = [ arr; idx; value ] }; _ }; _ } ->
      Some (arr, idx, value)
  | _ -> None

let linear_pointer_step ptr = function
  | Ast.Assign { target = { segments = [ name ]; _ }; mode = Ast.Normal; value = { kind = Ast.Binary { op; lhs; rhs }; _ }; _ }
    when name = ptr ->
      (match linear_local lhs, linear_int rhs with
       | Some same, Some delta when same = ptr && (op = Ast.Add || op = Ast.Sub) ->
           Some (if op = Ast.Add then delta else -delta)
       | _ -> None)
  | _ -> None

let linear_cell_delta array ptr = function
  | stmt ->
      (match linear_set_call stmt with
       | Some (arr, idx, { kind = Ast.Binary { op; lhs; rhs }; _ }) ->
           (match linear_local arr, linear_local idx, linear_index lhs, linear_int rhs with
            | Some a, Some i, Some (a', i'), Some delta
              when a = array && i = ptr && a' = array && i' = ptr
                   && (op = Ast.Add || op = Ast.Sub) ->
                Some (if op = Ast.Add then delta else -delta)
            | _ -> None)
       | Some _ | None -> None)

let optimize_linear_transfer (loop : Ast.while_stmt) =
  match loop.cond.kind, loop.body with
  | Ast.Binary { op = Ast.Ne; lhs; rhs }, Ast.Block { statements; _ } ->
      (match linear_index lhs, linear_int rhs with
       | Some (array, ptr), Some 0 ->
           let offset = ref 0 in
           let deltas = Hashtbl.create 8 in
           let valid = ref true in
           List.iter
             (fun stmt ->
               match linear_pointer_step ptr stmt, linear_cell_delta array ptr stmt with
               | Some step, _ -> offset := !offset + step
               | None, Some delta ->
                   let prior = Option.value (Hashtbl.find_opt deltas !offset) ~default:0 in
                   Hashtbl.replace deltas !offset (prior + delta)
               | None, None -> valid := false)
             statements;
           if not !valid || !offset <> 0 || Hashtbl.find_opt deltas 0 <> Some (-1)
           then None
           else begin
             let span = loop.span in
             let at offset =
               let p = linear_path span ptr in
               let index = if offset = 0 then p else linear_bin span Ast.Add p (linear_lit span offset) in
               { Ast.kind = Ast.Index { base = linear_path span array; index }; span }
             in
             let set offset value =
               Ast.ExprStmt
                 { expr =
                     { Ast.kind = Ast.Call
                         { callee = { Ast.kind = Ast.Path { segments = [ "__index"; "set" ]; span }; span }
                         ; args = [ linear_path span array; (match (at offset).kind with Ast.Index x -> x.index | _ -> assert false); value ]
                         }
                     ; span
                     }
                 ; span
                 }
             in
             let updates =
               Hashtbl.fold
                 (fun offset delta acc ->
                   if offset = 0 || delta = 0 then acc
                   else
                     let source = at 0 in
                     let scaled = linear_bin span Ast.Mul source (linear_lit span delta) in
                     set offset (linear_bin span Ast.Add (at offset) scaled) :: acc)
                 deltas []
             in
             let clear = set 0 (linear_lit span 0) in
             let positive = linear_bin span Ast.Gt lhs (linear_lit span 0) in
             Some
               (Ast.If
                  { cond = positive
                  ; then_branch = Ast.Block { statements = List.rev (clear :: updates); span }
                  ; else_branch = Some (Ast.While loop)
                  ; span
                  })
           end
       | _ -> None)
  | _ -> None

let rec fold_expr_value (expr : Ast.expr) : Ast.expr * string option =
  (* Fold sites parse the carried canonical text back into a `big`; this only
     runs when an actual constant operation folds, which is rare. *)
  let bounded value = if in_i128 value then Some (big_to_string value) else None in
  let parsed text = Option.get (big_of_literal text) in
  match expr.kind with
  | Literal (LitInt text) ->
      if literal_is_canonical_i128 text then expr, Some text
      else
        (match big_of_literal text with
         | Some value when in_i128 value ->
             let rendered = big_to_string value in
             { expr with kind = Ast.Literal (Ast.LitInt rendered) },
             Some rendered
         | _ -> expr, None)
  | Literal _ | Path _ -> expr, None
  | _ ->
      let kind, value =
        match expr.kind with
        | Literal _ | Path _ -> expr.kind, None
        | Grouping inner ->
            let inner', value = fold_expr_value inner in
            (if inner' == inner then expr.kind else Ast.Grouping inner'), value
        | Try inner ->
            let inner' = fold_expr inner in
            (if inner' == inner then expr.kind else Ast.Try inner'), None
        | Unary u ->
            let rhs, rhs_value = fold_expr_value u.rhs in
            let value =
              match u.op, rhs_value with
              | Neg, Some value -> bounded (big_neg (parsed value))
              | _ -> None
            in
            (if rhs == u.rhs then expr.kind else Ast.Unary { u with rhs }), value
        | Binary b ->
            let lhs, lhs_value = fold_expr_value b.lhs in
            let rhs, rhs_value = fold_expr_value b.rhs in
            let value =
              match b.op, lhs_value, rhs_value with
              | Add, Some lhs, Some rhs -> bounded (big_add (parsed lhs) (parsed rhs))
              | Sub, Some lhs, Some rhs -> bounded (big_sub (parsed lhs) (parsed rhs))
              | Mul, Some lhs, Some rhs -> bounded (big_mul (parsed lhs) (parsed rhs))
              | _ -> None
            in
            (if lhs == b.lhs && rhs == b.rhs then expr.kind
             else Ast.Binary { b with lhs; rhs }),
            value
        | Call c ->
            let callee = fold_expr c.callee in
            let args = map_shared fold_expr c.args in
            (if callee == c.callee && args == c.args then expr.kind
             else Ast.Call { callee; args }),
            None
        | Index i ->
            let base = fold_expr i.base in
            let index = fold_expr i.index in
            (if base == i.base && index == i.index then expr.kind
             else Ast.Index { base; index }),
            None
        | Member m ->
            let base = fold_expr m.base in
            (if base == m.base then expr.kind else Ast.Member { m with base }), None
        | Cast c ->
            let value = fold_expr c.value in
            (if value == c.value then expr.kind else Ast.Cast { c with value }), None
        | ArrayLiteral a ->
            let items = map_shared fold_expr a.items in
            (if items == a.items then expr.kind else Ast.ArrayLiteral { items }), None
        | StructLiteral s ->
            let fields =
              map_shared
                (fun (f : Ast.struct_literal_field_expr) ->
                  let value = fold_expr f.value in
                  if value == f.value then f else { f with value })
                s.fields
            in
            (if fields == s.fields then expr.kind
             else Ast.StructLiteral { s with fields }),
            None
        | Closure c ->
            let body = fold_stmt c.body in
            (if body == c.body then expr.kind else Ast.Closure { c with body }), None
      in
      match value with
      | Some rendered ->
          { expr with kind = Ast.Literal (Ast.LitInt rendered) }, value
      | None ->
          (if kind == expr.kind then expr else { expr with kind }), None

and fold_expr expr = fst (fold_expr_value expr)

and fold_stmt (stmt : Ast.stmt) : Ast.stmt =
  match stmt with
  | VarDecl s ->
      let init = opt_shared fold_expr s.init in
      if init == s.init then stmt else Ast.VarDecl { s with init }
  | Assign s ->
      let value = fold_expr s.value in
      if value == s.value then stmt else Ast.Assign { s with value }
  | Return s ->
      let value = opt_shared fold_expr s.value in
      if value == s.value then stmt else Ast.Return { s with value }
  | Break _ | Continue _ -> stmt
  | If s ->
      let cond = fold_expr s.cond in
      let then_branch = fold_stmt s.then_branch in
      let else_branch = opt_shared fold_stmt s.else_branch in
      if cond == s.cond && then_branch == s.then_branch && else_branch == s.else_branch
      then stmt
      else Ast.If { s with cond; then_branch; else_branch }
  | Switch s ->
      let selector = fold_expr s.selector in
      let cases =
        map_shared
          (fun (c : Ast.switch_case) ->
            let value = fold_expr c.value in
            let body = map_shared fold_stmt c.body in
            if value == c.value && body == c.body then c else { c with value; body })
          s.cases
      in
      let default =
        opt_shared
          (fun (d : Ast.switch_default) ->
            let body = map_shared fold_stmt d.body in
            if body == d.body then d else { d with body })
          s.default
      in
      if selector == s.selector && cases == s.cases && default == s.default then stmt
      else Ast.Switch { s with selector; cases; default }
  | While s ->
      let cond = fold_expr s.cond in
      let body = fold_stmt s.body in
      let loop = { s with cond; body } in
      (match optimize_linear_transfer loop with
       | Some optimized -> optimized
       | None -> if cond == s.cond && body == s.body then stmt else Ast.While loop)
  | ForEach s ->
      let collection = fold_expr s.collection in
      let body = fold_stmt s.body in
      if collection == s.collection && body == s.body then stmt
      else Ast.ForEach { s with collection; body }
  | For s ->
      let init =
        opt_shared
          (fun i ->
            match i with
            | Ast.ForInitVarDecl v ->
                let init = opt_shared fold_expr v.init in
                if init == v.init then i else Ast.ForInitVarDecl { v with init }
            | Ast.ForInitAssign a ->
                let value = fold_expr a.value in
                if value == a.value then i else Ast.ForInitAssign { a with value }
            | Ast.ForInitExpr e ->
                let e' = fold_expr e in
                if e' == e then i else Ast.ForInitExpr e')
          s.init
      in
      let step =
        opt_shared
          (fun st ->
            match st with
            | Ast.ForStepAssign a ->
                let value = fold_expr a.value in
                if value == a.value then st else Ast.ForStepAssign { a with value }
            | Ast.ForStepExpr e ->
                let e' = fold_expr e in
                if e' == e then st else Ast.ForStepExpr e')
          s.step
      in
      let cond = opt_shared fold_expr s.cond in
      let body = fold_stmt s.body in
      if init == s.init && cond == s.cond && step == s.step && body == s.body
      then stmt
      else Ast.For { s with init; cond; step; body }
  | IoChain s ->
      let items =
        map_shared
          (fun item ->
            match item with
            | Ast.IoExpr e ->
                let e' = fold_expr e in
                if e' == e then item else Ast.IoExpr e'
            | Ast.Endl _ -> item)
          s.items
      in
      if items == s.items then stmt else Ast.IoChain { s with items }
  | ExprStmt s ->
      let expr = fold_expr s.expr in
      if expr == s.expr then stmt else Ast.ExprStmt { s with expr }
  | Block s ->
      let statements = map_shared fold_stmt s.statements in
      if statements == s.statements then stmt else Ast.Block { s with statements }

let fold_program (program : Ast.program) : Ast.program =
  let functions =
    map_shared
      (fun (f : Ast.function_decl) ->
        let body = fold_stmt f.body in
        if body == f.body then f else { f with body })
      program.functions
  in
  if functions == program.functions then program else { program with functions }
