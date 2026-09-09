(* Ast_dump -- a shallow, line-oriented S-expression rendering of the AST, used
   only by `--dump=ast` as a differential oracle for the PowerShell port
   (Xi-ps). It is NOT part of the normal compile pipeline.

   The format is deliberately dumb: one node per line, 2-space indent per depth,
   payload scalars inline. There is no generic pretty-printer -- both this and
   the PowerShell AstDump.ps1 hand-walk the tree emitting identical lines, so
   byte-exactness reduces to "emit the same lines in the same order". Keep the
   two in lockstep. *)

let dump (p : Ast.program) : string =
  let b = Buffer.create 4096 in
  let emit depth text =
    for _ = 1 to depth do Buffer.add_string b "  " done;
    Buffer.add_string b text;
    Buffer.add_char b '\n'
  in
  let span (s : Span.t) = Printf.sprintf "[%d,%d)" s.start s.end_ in
  let q s = "\"" ^ Json_out.escape s ^ "\"" in
  let rec render_type = function
    | Ast.Int None -> "int"
    | Ast.Int (Some n) -> Printf.sprintf "int:%d" n
    | Ast.Uint None -> "uint"
    | Ast.Uint (Some n) -> Printf.sprintf "uint:%d" n
    | Ast.Dec None -> "dec"
    | Ast.Dec (Some n) -> Printf.sprintf "dec:%d" n
    | Ast.Str -> "str"
    | Ast.Bool -> "bool"
    | Ast.Array t -> "array<" ^ render_type t ^ ">"
    | Ast.Map (k, v) -> "map<" ^ render_type k ^ "," ^ render_type v ^ ">"
    | Ast.Fn (args, ret) ->
        "fn<" ^ String.concat "," (List.map render_type args) ^ "->"
        ^ render_type ret ^ ">"
    | Ast.Result t -> "result<" ^ render_type t ^ ">"
    | Ast.Named s -> "named:" ^ s
  in
  let ty (t : Ast.type_ref) = render_type t.kind in
  let binding = function
    | Ast.Value -> "val" | Ast.Ref -> "ref" | Ast.Link -> "link"
  in
  let assign_mode = function Ast.Normal -> "normal" | Ast.ExplicitLink -> "link" in
  let io_anchor = function
    | Ast.Wrt -> "wrt"
    | Ast.Wrtl -> "wrtl"
    | Ast.Wrtr -> "wrtr"
  in
  let unary_op = function
    | Ast.Neg -> "neg" | Ast.Not -> "not" | Ast.BitNot -> "bitnot"
  in
  let binary_op = function
    | Ast.Add -> "add" | Ast.Sub -> "sub" | Ast.Mul -> "mul" | Ast.Div -> "div"
    | Ast.Mod -> "mod" | Ast.Pow -> "pow" | Ast.Eq -> "eq" | Ast.Ne -> "ne"
    | Ast.Lt -> "lt" | Ast.Le -> "le" | Ast.Gt -> "gt" | Ast.Ge -> "ge"
    | Ast.And -> "and" | Ast.Or -> "or" | Ast.BitAnd -> "bitand"
    | Ast.BitOr -> "bitor" | Ast.BitXor -> "bitxor"
  in
  let param depth (pm : Ast.param) =
    emit depth
      (Printf.sprintf "param %s %s %s %s" (binding pm.binding) (q pm.name)
         (ty pm.ty) (span pm.span))
  in
  let rec expr depth (e : Ast.expr) =
    let sp = span e.span in
    match e.kind with
    | Ast.Literal (LitInt s) -> emit depth (Printf.sprintf "lit-int %s %s" (q s) sp)
    | Ast.Literal (LitDec s) -> emit depth (Printf.sprintf "lit-dec %s %s" (q s) sp)
    | Ast.Literal (LitStr s) -> emit depth (Printf.sprintf "lit-str %s %s" (q s) sp)
    | Ast.Literal (LitBool bl) -> emit depth (Printf.sprintf "lit-bool %b %s" bl sp)
    | Ast.ArrayLiteral a ->
        emit depth (Printf.sprintf "array-lit %s" sp);
        List.iter (fun it -> expr (depth + 1) it) a.items
    | Ast.StructLiteral s ->
        emit depth (Printf.sprintf "struct-lit %s %s" (q s.name) sp);
        List.iter
          (fun (f : Ast.struct_literal_field_expr) ->
            emit (depth + 1) (Printf.sprintf "field %s %s" (q f.name) (span f.span));
            expr (depth + 2) f.value)
          s.fields
    | Ast.Closure c ->
        emit depth (Printf.sprintf "closure ret=%s %s" (ty c.return_ty) sp);
        emit (depth + 1) "params";
        List.iter (fun pm -> param (depth + 2) pm) c.params;
        emit (depth + 1) "body";
        stmt (depth + 2) c.body
    | Ast.Path pth ->
        emit depth
          (Printf.sprintf "path %s %s" (String.concat "::" pth.segments) sp)
    | Ast.Call c ->
        emit depth (Printf.sprintf "call %s" sp);
        emit (depth + 1) "callee";
        expr (depth + 2) c.callee;
        emit (depth + 1) "args";
        List.iter (fun a -> expr (depth + 2) a) c.args
    | Ast.Index ix ->
        emit depth (Printf.sprintf "index %s" sp);
        emit (depth + 1) "base";
        expr (depth + 2) ix.base;
        emit (depth + 1) "index";
        expr (depth + 2) ix.index
    | Ast.Member m ->
        emit depth (Printf.sprintf "member %s %s" (q m.field) sp);
        expr (depth + 1) m.base
    | Ast.Unary u ->
        emit depth (Printf.sprintf "unary %s %s" (unary_op u.op) sp);
        expr (depth + 1) u.rhs
    | Ast.Binary bn ->
        emit depth (Printf.sprintf "binary %s %s" (binary_op bn.op) sp);
        expr (depth + 1) bn.lhs;
        expr (depth + 1) bn.rhs
    | Ast.Cast c ->
        emit depth (Printf.sprintf "cast %s %s" (ty c.ty) sp);
        expr (depth + 1) c.value
    | Ast.Grouping g ->
        emit depth (Printf.sprintf "grouping %s" sp);
        expr (depth + 1) g
    | Ast.Try inner ->
        emit depth (Printf.sprintf "try %s" sp);
        expr (depth + 1) inner
  and stmt depth (s : Ast.stmt) =
    match s with
    | Ast.VarDecl v ->
        emit depth
          (Printf.sprintf "vardecl %s %s %s %s" (binding v.binding) (q v.name)
             (ty v.ty) (span v.span));
        (match v.init with
         | Some e -> emit (depth + 1) "init"; expr (depth + 2) e
         | None -> ())
    | Ast.Assign a ->
        emit depth (Printf.sprintf "assign %s %s" (assign_mode a.mode) (span a.span));
        emit (depth + 1)
          (Printf.sprintf "target %s %s"
             (String.concat "::" a.target.segments) (span a.target.span));
        emit (depth + 1) "value";
        expr (depth + 2) a.value
    | Ast.Return r ->
        emit depth (Printf.sprintf "return %s" (span r.span));
        (match r.value with Some e -> expr (depth + 1) e | None -> ())
    | Ast.Break b -> emit depth (Printf.sprintf "break %s" (span b.span))
    | Ast.Continue c -> emit depth (Printf.sprintf "continue %s" (span c.span))
    | Ast.If i ->
        emit depth (Printf.sprintf "if %s" (span i.span));
        emit (depth + 1) "cond";
        expr (depth + 2) i.cond;
        emit (depth + 1) "then";
        stmt (depth + 2) i.then_branch;
        (match i.else_branch with
         | Some e -> emit (depth + 1) "else"; stmt (depth + 2) e
         | None -> ())
    | Ast.Switch sw ->
        emit depth (Printf.sprintf "switch %s" (span sw.span));
        emit (depth + 1) "selector";
        expr (depth + 2) sw.selector;
        List.iter
          (fun (c : Ast.switch_case) ->
            emit (depth + 1) (Printf.sprintf "case %s" (span c.span));
            emit (depth + 2) "value";
            expr (depth + 3) c.value;
            emit (depth + 2) "body";
            List.iter (fun st -> stmt (depth + 3) st) c.body)
          sw.cases;
        (match sw.default with
         | Some d ->
             emit (depth + 1) (Printf.sprintf "default %s" (span d.span));
             List.iter (fun st -> stmt (depth + 2) st) d.body
         | None -> ())
    | Ast.While w ->
        emit depth (Printf.sprintf "while %s" (span w.span));
        emit (depth + 1) "cond";
        expr (depth + 2) w.cond;
        emit (depth + 1) "body";
        stmt (depth + 2) w.body
    | Ast.ForEach f ->
        emit depth (Printf.sprintf "foreach %s %s" (q f.name) (span f.span));
        emit (depth + 1) "collection";
        expr (depth + 2) f.collection;
        emit (depth + 1) "body";
        stmt (depth + 2) f.body
    | Ast.For f ->
        emit depth (Printf.sprintf "for %s" (span f.span));
        (match f.init with
         | Some (ForInitVarDecl v) -> emit (depth + 1) "init"; stmt (depth + 2) (Ast.VarDecl v)
         | Some (ForInitAssign a) -> emit (depth + 1) "init"; stmt (depth + 2) (Ast.Assign a)
         | Some (ForInitExpr e) -> emit (depth + 1) "init"; expr (depth + 2) e
         | None -> ());
        (match f.cond with
         | Some e -> emit (depth + 1) "cond"; expr (depth + 2) e
         | None -> ());
        (match f.step with
         | Some (ForStepAssign a) -> emit (depth + 1) "step"; stmt (depth + 2) (Ast.Assign a)
         | Some (ForStepExpr e) -> emit (depth + 1) "step"; expr (depth + 2) e
         | None -> ());
        emit (depth + 1) "body";
        stmt (depth + 2) f.body
    | Ast.IoChain io ->
        emit depth
          (Printf.sprintf "iochain %s %s" (io_anchor io.anchor) (span io.span));
        List.iter
          (fun it ->
            match it with
            | Ast.IoExpr e -> emit (depth + 1) "io-expr"; expr (depth + 2) e
            | Ast.Endl sp -> emit (depth + 1) (Printf.sprintf "io-endl %s" (span sp)))
          io.items
    | Ast.ExprStmt e ->
        emit depth (Printf.sprintf "exprstmt %s" (span e.span));
        expr (depth + 1) e.expr
    | Ast.Block bl ->
        emit depth (Printf.sprintf "block %s" (span bl.span));
        List.iter (fun st -> stmt (depth + 1) st) bl.statements
  in
  emit 0 "program";
  emit 1 "includes";
  List.iter
    (fun (i : Ast.include_decl) ->
      emit 2 (Printf.sprintf "include %s %s" (q i.module_) (span i.span)))
    p.includes;
  emit 1 "links";
  List.iter
    (fun (l : Ast.link_decl) ->
      emit 2 (Printf.sprintf "link %s %s" (q l.library) (span l.span)))
    p.links;
  emit 1 "subsystems";
  List.iter
    (fun (s : Ast.subsystem_decl) ->
      emit 2 (Printf.sprintf "subsystem %s %s" (q s.name) (span s.span)))
    p.subsystems;
  if p.c_preambles <> [] || p.c_functions <> [] then begin
    emit 1 "inline_c";
    List.iter
      (fun (c : Ast.c_preamble_decl) ->
        emit 2 (Printf.sprintf "c-preamble bytes=%d %s" (String.length c.body) (span c.span)))
      p.c_preambles;
    List.iter
      (fun (c : Ast.inline_c_fn_decl) ->
        emit 2
          (Printf.sprintf "c-function %s symbol=%s ret=%s bytes=%d %s"
             (q c.name) (q c.symbol) (ty c.return_ty) (String.length c.body)
             (span c.span));
        emit 3 "params";
        List.iter (fun pm -> param 4 pm) c.params)
      p.c_functions
  end;
  emit 1 "structs";
  List.iter
    (fun (s : Ast.struct_decl) ->
      emit 2
        (Printf.sprintf "struct %s clayout=%b %s" (q s.name) s.c_layout
           (span s.span));
      List.iter
        (fun (fd : Ast.struct_field_decl) ->
          emit 3
            (Printf.sprintf "field %s %s %s" (q fd.name) (ty fd.ty)
               (span fd.span)))
        s.fields)
    p.structs;
  emit 1 "functions";
  List.iter
    (fun (fn : Ast.function_decl) ->
      emit 2 (Printf.sprintf "function %s %s" (q fn.name) (span fn.span));
      emit 3 "params";
      List.iter (fun pm -> param 4 pm) fn.params;
      emit 3 "body";
      stmt 4 fn.body)
    p.functions;
  emit 1 "externs";
  List.iter
    (fun (e : Ast.extern_fn_decl) ->
      emit 2
        (Printf.sprintf "extern %s symbol=%s cabi=%b ret=%s %s" (q e.name)
           (q e.symbol) e.c_abi (ty e.return_ty) (span e.span));
      emit 3 "params";
      List.iter (fun pm -> param 4 pm) e.params)
    p.externs;
  emit 1 "extern_consts";
  List.iter
    (fun (e : Ast.extern_const_decl) ->
      emit 2
        (Printf.sprintf "extern_const %s symbol=%s cabi=%b %s %s" (q e.name)
           (q e.symbol) e.c_abi (ty e.ty) (span e.span)))
    p.extern_consts;
  emit 1 "intrinsics";
  List.iter
    (fun (i : Ast.intrinsic_decl) ->
      emit 2 (Printf.sprintf "intrinsic %s %s" (q i.name) (span i.span)))
    p.intrinsics;
  Buffer.contents b
