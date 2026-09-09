open Ast

module StringSet = Set.Make (String)

type sem_type =
  | Int
  | IntN of int
  | Uint
  | UintN of int
  | Dec
  | DecN of int
  | Bool
  | Str
  | Cptr
  | ArrayInt of int option
  | ArrayUint of int option
  | ArrayDec
  | ArrayBool
  | ArrayStr
  | ArrayStruct of int
  | ArrayFnPtr of int
  | ArrayUnknown
  | MapInt
  | MapDec
  | MapBool
  | MapStr
  | MapStruct of int
  | MapFnPtr of int
  | MapKeyed of map_key_type * map_value_type
  | MapUnknown
  | Struct of int
  | CStruct of int
  | FnPtr of int
  | Result of sem_type
  | ResultUnknown
  | Void
  | NumUnknown
  | Error

and map_key_type = KeyInt | KeyDec | KeyBool

and map_value_type =
  | ValueInt
  | ValueDec
  | ValueBool
  | ValueStr
  | ValueStruct of int
  | ValueFnPtr of int
  | ValueUnknown

type param_sig = { ps_name : string; ps_ty : sem_type; ps_binding : binding_kind }

type function_sig =
  { fs_name : string
  ; fs_params : param_sig list
  ; fs_return_ty : sem_type
  ; fs_span : Span.t
  }

type fn_ptr_sig = { fps_params : sem_type list; fps_return_ty : sem_type }
type struct_field_sig = { sfs_name : string; sfs_ty : sem_type }

type struct_sig =
  { ss_id : int
  ; ss_name : string
  ; ss_fields : struct_field_sig list
  ; ss_span : Span.t
  }

type cstruct_field_sig = { cfs_name : string; cfs_ty : sem_type; cfs_offset : int }

type cstruct_sig =
  { cs_id : int
  ; cs_name : string
  ; cs_fields : cstruct_field_sig list
  ; cs_size : int
  ; cs_align : int
  }

type module_fn_sig =
  { mfs_module : string
  ; mfs_name : string
  ; mfs_params : param_sig list
  ; mfs_return_ty : sem_type
  ; mfs_symbol : string
  }

type module_const_sig =
  { mcs_module : string
  ; mcs_name : string
  ; mcs_ty : sem_type
  ; mcs_symbol : string
  }

type module_table =
  { mt_fns : module_fn_sig list
  ; mt_consts : module_const_sig list
  ; mt_intrinsics : (string * string) list
  }

type semantic_result =
  { function_sigs : (string * function_sig) list
  ; fn_ptr_sigs : fn_ptr_sig list
  ; struct_sigs_by_name : (string * struct_sig) list
  ; struct_sigs_by_id : (int * struct_sig) list
  ; cstruct_sigs_by_name : (string * cstruct_sig) list
  ; cstruct_sigs_by_id : (int * cstruct_sig) list
  ; modules : module_table
  ; diagnostics : Diagnostic.t list
  }

let empty_modules = { mt_fns = []; mt_consts = []; mt_intrinsics = [] }

let empty_semantic_result =
  { function_sigs = []
  ; fn_ptr_sigs = []
  ; struct_sigs_by_name = []
  ; struct_sigs_by_id = []
  ; cstruct_sigs_by_name = []
  ; cstruct_sigs_by_id = []
  ; modules = empty_modules
  ; diagnostics = []
  }

type code_value =
  | Num of string
  | NativeInt of string * int * bool
  | NativeFloat of string * int
  | BoolV of string
  | StrV of string
  | Arr of string
  | MapV of string
  | StructV of string
  | FnPtrV of string
  | CptrV of string
  | CStructV of string

type array_runtime_kind = ArrayNum | ArrayBoolKind | ArrayStructKind | ArrayFnPtrKind | ArrayStrKind
type map_runtime_kind = MapNum | MapBoolKind | MapStructKind | MapFnPtrKind | MapStrKind
type map_key_runtime_kind = MapKeyStr | MapKeyNum | MapKeyBool

type adaptive_local_state =
  { native_slot : string
  ; boxed_slot : string
  ; boxed_flag_slot : string
  }

type local_binding =
  { mutable slot : string
  ; binding : binding_kind
  ; ty : sem_type
  ; adaptive_state : adaptive_local_state option
  }
type bounds_proof = { arr_name : string; index_name : string }
type closure_capture = { capture_name : string; capture_ty : sem_type }

let assoc_opt key xs =
  List.find_map (fun (k, v) -> if k = key then Some v else None) xs

let fixed_numeric_bit_width = function
  | IntN width | UintN width | DecN width -> Some width
  | _ -> None

let is_unsigned_int = function Uint | UintN _ -> true | _ -> false
let llvm_int_ty = function
  | 1 -> "i1"
  | 8 -> "i8"
  | 16 -> "i16"
  | 32 -> "i32"
  | 64 -> "i64"
  | width -> "i" ^ string_of_int width

let is_valid_int_width width =
  width >= 8 && width mod 8 = 0 && width <= 65535

let is_native_float_width = function 16 | 32 | 64 -> true | _ -> false

let llvm_float_ty = function
  | 16 -> "half"
  | 32 -> "float"
  | _ -> "double"

let llvm_float_const width v =
  let d = float_of_string v in
  let bits =
    if width = 32 then Int64.bits_of_float (Int32.float_of_bits (Int32.bits_of_float d))
    else Int64.bits_of_float d
  in
  Printf.sprintf "0x%016LX" bits

let native_width_of = function IntN w | UintN w -> Some w | _ -> None
let native_signed_of = function UintN _ -> false | _ -> true
let native_float_width_of = function DecN w when is_native_float_width w -> Some w | _ -> None
let software_float_width_of = function DecN w when not (is_native_float_width w) -> Some w | _ -> None

let native_align width =
  let bytes = max 1 (width / 8) in
  let rec loop a = if a * 2 <= bytes then loop (a * 2) else a in
  loop 1

let sanitize_ident name =
  let valid ch =
    (ch >= 'a' && ch <= 'z')
    || (ch >= 'A' && ch <= 'Z')
    || (ch >= '0' && ch <= '9')
    || ch = '_'
  in
  if String.for_all valid name then name
  else
    let b = Bytes.of_string name in
    Bytes.iteri (fun i ch -> if not (valid ch) then Bytes.set b i '_') b;
    Bytes.to_string b

let ir_return_ty = function
  | Void -> "void"
  | IntN w | UintN w -> llvm_int_ty w
  | DecN w when is_native_float_width w -> llvm_float_ty w
  | Bool -> "i1"
  | _ -> "ptr"

let extern_abi_ty = function
  | Void -> "void"
  | Bool -> "i1"
  | IntN w | UintN w -> llvm_int_ty w
  | DecN w when is_native_float_width w -> llvm_float_ty w
  | _ -> "ptr"

let c_abi_ext = function
  | IntN w when w < 32 -> Lir.Sign_ext
  | UintN w when w < 32 -> Lir.Zero_ext
  | Bool -> Lir.Zero_ext
  | _ -> Lir.No_ext

let rec xi_type_name sem = function
  | Result payload -> Printf.sprintf "result<%s>" (xi_type_name sem payload)
  | ResultUnknown -> "result<unknown>"
  | Int | IntN _ -> "int"
  | Uint | UintN _ -> "uint"
  | Dec | DecN _ -> "dec"
  | Bool -> "bool"
  | Str -> "string"
  | Cptr -> "cptr"
  | ArrayInt None -> "array<int>"
  | ArrayInt (Some w) -> Printf.sprintf "array<int%d>" w
  | ArrayUint None -> "array<uint>"
  | ArrayUint (Some w) -> Printf.sprintf "array<uint%d>" w
  | ArrayDec -> "array<dec>"
  | ArrayBool -> "array<bool>"
  | ArrayStr -> "array<string>"
  | ArrayStruct id ->
      Printf.sprintf "array<%s>"
        (match assoc_opt id sem.struct_sigs_by_id with Some s -> s.ss_name | None -> "struct")
  | ArrayFnPtr _ -> "array<fn>"
  | ArrayUnknown -> "array<unknown>"
  | MapInt -> "map<string, int>"
  | MapDec -> "map<string, dec>"
  | MapBool -> "map<string, bool>"
  | MapStr -> "map<string, string>"
  | MapStruct id ->
      Printf.sprintf "map<string, %s>"
        (match assoc_opt id sem.struct_sigs_by_id with Some s -> s.ss_name | None -> "struct")
  | MapFnPtr _ -> "map<string, fn>"
  | MapKeyed (key, value) ->
      let key_name = match key with KeyInt -> "int" | KeyDec -> "dec" | KeyBool -> "bool" in
      let value_name =
        match value with
        | ValueInt -> "int"
        | ValueDec -> "dec"
        | ValueBool -> "bool"
        | ValueStr -> "string"
        | ValueStruct id ->
            (match assoc_opt id sem.struct_sigs_by_id with Some s -> s.ss_name | None -> "struct")
        | ValueFnPtr _ -> "fn"
        | ValueUnknown -> "unknown"
      in
      Printf.sprintf "map<%s, %s>" key_name value_name
  | MapUnknown -> "map<string, unknown>"
  | Struct id -> (match assoc_opt id sem.struct_sigs_by_id with Some s -> s.ss_name | None -> "struct")
  | CStruct id -> (match assoc_opt id sem.cstruct_sigs_by_id with Some s -> s.cs_name | None -> "cstruct")
  | FnPtr _ -> "fn"
  | Void -> "void"
  | NumUnknown -> "number"
  | Error -> "error"

let rec sem_type_from_typekind (kind : Ast.type_kind) sem =
  match kind with
  | Ast.Int None -> Int
  | Ast.Int (Some w) when is_valid_int_width w -> IntN w
  | Ast.Int (Some _) -> Int
  | Ast.Uint None -> Uint
  | Ast.Uint (Some w) when is_valid_int_width w -> UintN w
  | Ast.Uint (Some _) -> Uint
  | Ast.Dec None -> Dec
  | Ast.Dec (Some w) when is_valid_int_width w -> DecN w
  | Ast.Dec (Some _) -> Dec
  | Ast.Str -> Str
  | Ast.Bool -> Bool
  | Ast.Named "cptr" -> Cptr
  | Ast.Named "void" -> Void
  | Ast.Result payload -> Result (sem_type_from_typekind payload sem)
  | Ast.Array elem ->
      (match elem with
       | Ast.Int None -> ArrayInt None
       | Ast.Int (Some w) when is_valid_int_width w -> ArrayInt (Some w)
       | Ast.Int (Some _) -> ArrayInt None
       | Ast.Uint None -> ArrayUint None
       | Ast.Uint (Some w) when is_valid_int_width w -> ArrayUint (Some w)
       | Ast.Uint (Some _) -> ArrayUint None
       | Ast.Dec _ -> ArrayDec
       | Ast.Bool -> ArrayBool
       | Ast.Str -> ArrayStr
       | Ast.Fn (params, ret) ->
           (match fn_ptr_type_from_typekind params ret sem with
            | FnPtr id -> ArrayFnPtr id
            | _ -> ArrayUnknown)
       | Ast.Named name ->
           (match assoc_opt name sem.struct_sigs_by_name with
            | Some s -> ArrayStruct s.ss_id
            | None -> Error)
       | Ast.Array _ | Ast.Map _ | Ast.Result _ -> ArrayUnknown)
  | Ast.Map (key, value) ->
      if key = Ast.Str then
        match value with
        | Ast.Int _ | Ast.Uint _ -> MapInt
        | Ast.Dec _ -> MapDec
        | Ast.Bool -> MapBool
        | Ast.Str -> MapStr
        | Ast.Fn (params, ret) ->
            (match fn_ptr_type_from_typekind params ret sem with FnPtr id -> MapFnPtr id | _ -> MapUnknown)
        | Ast.Named name ->
            (match assoc_opt name sem.struct_sigs_by_name with
             | Some s -> MapStruct s.ss_id
             | None -> Error)
        | Ast.Array _ | Ast.Map _ | Ast.Result _ -> MapUnknown
      else
        let key_ty =
          match key with
          | Ast.Int _ | Ast.Uint _ -> Some KeyInt
          | Ast.Dec _ -> Some KeyDec
          | Ast.Bool -> Some KeyBool
          | _ -> None
        in
        (match key_ty with
         | None -> Error
         | Some key_ty ->
             let value_ty = map_value_kind_from_typekind value sem in
             if value_ty = ValueUnknown then MapUnknown else MapKeyed (key_ty, value_ty))
  | Ast.Fn (params, ret) -> fn_ptr_type_from_typekind params ret sem
  | Ast.Named name ->
      (match assoc_opt name sem.cstruct_sigs_by_name with
       | Some s -> CStruct s.cs_id
       | None ->
           (match assoc_opt name sem.struct_sigs_by_name with
            | Some s -> Struct s.ss_id
            | None -> Error))

and sem_type_from_typeref (ty : Ast.type_ref) sem = sem_type_from_typekind ty.kind sem

and fn_ptr_type_from_typekind (params : Ast.type_kind list) (ret : Ast.type_kind) sem =
  let wanted =
    { fps_params = List.map (fun kind -> sem_type_from_typekind kind sem) params
    ; fps_return_ty = sem_type_from_typekind ret sem
    }
  in
  let rec find idx = function
    | [] -> Error
    | sig_ :: rest -> if sig_ = wanted then FnPtr idx else find (idx + 1) rest
  in
  find 0 sem.fn_ptr_sigs

and map_value_kind_from_typekind (value : Ast.type_kind) sem =
  match value with
  | Ast.Int _ | Ast.Uint _ -> ValueInt
  | Ast.Dec _ -> ValueDec
  | Ast.Bool -> ValueBool
  | Ast.Str -> ValueStr
  | Ast.Fn (params, ret) ->
      (match fn_ptr_type_from_typekind params ret sem with FnPtr id -> ValueFnPtr id | _ -> ValueUnknown)
  | Ast.Named name ->
      (match assoc_opt name sem.struct_sigs_by_name with
       | Some s -> ValueStruct s.ss_id
       | None -> ValueUnknown)
  | Ast.Array _ | Ast.Map _ | Ast.Result _ -> ValueUnknown

let is_array_sem_type = function
  | ArrayInt _ | ArrayUint _ | ArrayDec | ArrayBool | ArrayStr | ArrayStruct _ | ArrayFnPtr _
  | ArrayUnknown -> true
  | _ -> false

let is_map_sem_type = function
  | MapInt | MapDec | MapBool | MapStr | MapStruct _ | MapFnPtr _ | MapKeyed _ | MapUnknown -> true
  | _ -> false

let array_element_sem_type = function
  | ArrayInt w -> Some (match w with Some w -> IntN w | None -> Int)
  | ArrayUint w -> Some (match w with Some w -> UintN w | None -> Uint)
  | ArrayDec -> Some Dec
  | ArrayBool -> Some Bool
  | ArrayStr -> Some Str
  | ArrayStruct id -> Some (Struct id)
  | ArrayFnPtr id -> Some (FnPtr id)
  | _ -> None

let array_sem_type_from_element = function
  | Int -> ArrayInt None
  | IntN w -> ArrayInt (Some w)
  | Uint -> ArrayUint None
  | UintN w -> ArrayUint (Some w)
  | Dec | DecN _ -> ArrayDec
  | Bool -> ArrayBool
  | Str -> ArrayStr
  | Struct id -> ArrayStruct id
  | FnPtr id -> ArrayFnPtr id
  | _ -> ArrayUnknown

let map_value_sem_type = function
  | MapInt -> Some Int
  | MapDec -> Some Dec
  | MapBool -> Some Bool
  | MapStr -> Some Str
  | MapStruct id -> Some (Struct id)
  | MapFnPtr id -> Some (FnPtr id)
  | MapKeyed (_, value) ->
      (match value with
       | ValueInt -> Some Int
       | ValueDec -> Some Dec
       | ValueBool -> Some Bool
       | ValueStr -> Some Str
       | ValueStruct id -> Some (Struct id)
       | ValueFnPtr id -> Some (FnPtr id)
       | ValueUnknown -> None)
  | _ -> None

let map_keys_array_sem_type = function
  | MapKeyed (KeyInt, _) -> ArrayInt None
  | MapKeyed (KeyDec, _) -> ArrayDec
  | MapKeyed (KeyBool, _) -> ArrayBool
  | _ -> ArrayStr

let unify_numeric_sem_type a b =
  match a, b with
  | Dec, _ | _, Dec -> Dec
  | DecN wa, DecN wb -> DecN (max wa wb)
  | DecN w, (Int | IntN _ | Uint | UintN _ | NumUnknown)
  | (Int | IntN _ | Uint | UintN _ | NumUnknown), DecN w -> DecN w
  | IntN wa, IntN wb -> IntN (max wa wb)
  | UintN wa, UintN wb -> UintN (max wa wb)
  | IntN w, NumUnknown | NumUnknown, IntN w -> IntN w
  | UintN w, NumUnknown | NumUnknown, UintN w -> UintN w
  | IntN _, Int | Int, IntN _ | Int, Int | Int, NumUnknown | NumUnknown, Int -> Int
  | UintN _, Uint | Uint, UintN _ | Uint, Uint | Uint, NumUnknown | NumUnknown, Uint -> Uint
  | Int, (Uint | UintN _) | (Uint | UintN _), Int -> Int
  | NumUnknown, NumUnknown -> NumUnknown
  | _ -> Error

let unify_array_literal_elem_sem_type a b =
  match a, b with
  | Str, Str -> Some Str
  | Bool, Bool -> Some Bool
  | Struct a_id, Struct b_id when a_id = b_id -> Some (Struct a_id)
  | FnPtr a_id, FnPtr b_id when a_id = b_id -> Some (FnPtr a_id)
  | _ ->
      let unified = unify_numeric_sem_type a b in
      if unified = Error then None else Some unified

let parse_i64 v =
  let n = String.length v in
  let start = if n > 0 && (v.[0] = '-' || v.[0] = '+') then 1 else 0 in
  let digits_only =
    n > start
    && (let ok = ref true in
        String.iteri (fun i c -> if i >= start && not (c >= '0' && c <= '9') then ok := false) v;
        !ok)
  in
  if digits_only then Int64.of_string_opt v else None

let parse_u64_as_i64 v =
  let n = String.length v in
  let digits_only =
    n > 0 && (let ok = ref true in
              String.iter (fun c -> if not (c >= '0' && c <= '9') then ok := false) v;
              !ok)
  in
  if digits_only then
    Int64.of_string_opt ("0u" ^ v)
  else None

let rec collect_strings_stmt (stmt : Ast.stmt) out =
  match stmt with
  | VarDecl v -> Option.iter (fun e -> collect_strings_expr e out) v.init
  | Assign a -> collect_strings_expr a.value out
  | Return r -> Option.iter (fun e -> collect_strings_expr e out) r.value
  | Break _ | Continue _ -> ()
  | If i ->
      collect_strings_expr i.cond out;
      collect_strings_stmt i.then_branch out;
      Option.iter (fun s -> collect_strings_stmt s out) i.else_branch
  | Switch s ->
      collect_strings_expr s.selector out;
      List.iter
        (fun (case : Ast.switch_case) ->
          collect_strings_expr case.value out;
          List.iter (fun st -> collect_strings_stmt st out) case.body)
        s.cases;
      Option.iter (fun (d : Ast.switch_default) -> List.iter (fun st -> collect_strings_stmt st out) d.body) s.default
  | While w ->
      collect_strings_expr w.cond out;
      collect_strings_stmt w.body out
  | ForEach f ->
      collect_strings_expr f.collection out;
      collect_strings_stmt f.body out
  | For f ->
      Option.iter
        (function
          | ForInitVarDecl v -> Option.iter (fun e -> collect_strings_expr e out) v.init
          | ForInitAssign a -> collect_strings_expr a.value out
          | ForInitExpr e -> collect_strings_expr e out)
        f.init;
      Option.iter (fun e -> collect_strings_expr e out) f.cond;
      Option.iter
        (function
          | ForStepAssign a -> collect_strings_expr a.value out
          | ForStepExpr e -> collect_strings_expr e out)
        f.step;
      collect_strings_stmt f.body out
  | IoChain io ->
      List.iter (function IoExpr e -> collect_strings_expr e out | Endl _ -> ()) io.items
  | ExprStmt e -> collect_strings_expr e.expr out
  | Block b -> List.iter (fun st -> collect_strings_stmt st out) b.statements

and collect_strings_expr (expr : Ast.expr) out =
  match expr.kind with
  | Literal (LitStr s) -> out := s :: !out
  | Literal (LitInt v) ->
      if parse_i64 v = None then out := v :: !out
  | Literal (LitDec v) -> out := v :: !out
  | Literal (LitBool _) | Path _ -> ()
  | ArrayLiteral arr -> List.iter (fun item -> collect_strings_expr item out) arr.items
  | StructLiteral s ->
      List.iter
        (fun field ->
          out := field.name :: !out;
          collect_strings_expr field.value out)
        s.fields
  | Closure c -> collect_strings_stmt c.body out
  | Call c ->
      collect_strings_expr c.callee out;
      List.iter (fun arg -> collect_strings_expr arg out) c.args
  | Index i ->
      collect_strings_expr i.base out;
      collect_strings_expr i.index out
  | Member m ->
      out := m.field :: !out;
      collect_strings_expr m.base out
  | Unary u -> collect_strings_expr u.rhs out
  | Binary b ->
      collect_strings_expr b.lhs out;
      collect_strings_expr b.rhs out
  | Cast c -> collect_strings_expr c.value out
  | Grouping g | Try g -> collect_strings_expr g out

let collect_string_literals (program : Ast.program) =
  let strings = ref [] in
  List.iter (fun (f : Ast.function_decl) -> collect_strings_stmt f.body strings) program.functions;
  let ordered = List.rev !strings in
  let table = Hashtbl.create 32 in
  let max_id = ref (-1) in
  List.iteri
    (fun idx s ->
      if not (Hashtbl.mem table s) then begin
        Hashtbl.add table s ("@.str." ^ string_of_int idx);
        max_id := idx
      end)
    ordered;
  table, !max_id + 1

let rec expr_as_local_name expr =
  match expr.kind with
  | Path path -> (match path.segments with [ name ] -> Some name | _ -> None)
  | Grouping inner -> expr_as_local_name inner
  | _ -> None

let path_segments expr = match expr.kind with Path path -> Some path.segments | _ -> None
let path_is expr segments = match path_segments expr with Some s -> s = segments | None -> false

let expr_is_name expr name =
  match expr.kind with Path { segments = [ found ]; _ } -> found = name | _ -> false

let rec expr_is_one expr =
  match expr.kind with
  | Literal (LitInt "1") -> true
  | Grouping inner -> expr_is_one inner
  | _ -> false

let rec expr_is_pure_native_index expr =
  match expr.kind with
  | Literal (LitInt _) -> true
  | Path { segments = [ _ ]; _ } -> true
  | Grouping inner -> expr_is_pure_native_index inner
  | Binary { op = (Add | Sub | Mul | Div | Mod); lhs; rhs } ->
      expr_is_pure_native_index lhs && expr_is_pure_native_index rhs
  | _ -> false

let rec expr_same_pure_index a b =
  match a.kind, b.kind with
  | Literal (LitInt x), Literal (LitInt y) -> x = y
  | Path x, Path y -> x.segments = y.segments
  | Grouping x, Grouping y -> expr_same_pure_index x y
  | Grouping x, _ -> expr_same_pure_index x b
  | _, Grouping y -> expr_same_pure_index a y
  | Binary x, Binary y ->
      x.op = y.op
      && expr_same_pure_index x.lhs y.lhs
      && expr_same_pure_index x.rhs y.rhs
  | _ -> false

let rec expr_is_scalar_scan_pure expr =
  match expr.kind with
  | Literal _ | Path _ -> true
  | Grouping inner -> expr_is_scalar_scan_pure inner
  | Unary x -> expr_is_scalar_scan_pure x.rhs
  | Binary x ->
      expr_is_scalar_scan_pure x.lhs && expr_is_scalar_scan_pure x.rhs
  | _ -> false

let rec stmt_is_readonly_scan_body array_name = function
  | Assign x ->
      x.mode = Normal
      && x.target.segments <> [ array_name ]
      && expr_is_scalar_scan_pure x.value
  | If x ->
      expr_is_scalar_scan_pure x.cond
      && stmt_is_readonly_scan_body array_name x.then_branch
      && Option.fold ~none:true
           ~some:(stmt_is_readonly_scan_body array_name) x.else_branch
  | Block x -> List.for_all (stmt_is_readonly_scan_body array_name) x.statements
  | Break _ | Continue _ -> true
  | VarDecl _ | Return _ | Switch _ | While _ | ForEach _ | For _
  | IoChain _ | ExprStmt _ -> false

let packed_scan_loop_parts (loop : Ast.while_stmt) =
  let indexed_nonzero lhs rhs =
    match lhs.kind, rhs.kind with
    | Index x, Literal (LitInt "0") -> Some x
    | _ -> None
  in
  match loop.cond.kind with
  | Binary { op = Ne; lhs; rhs } ->
      let candidate =
        match indexed_nonzero lhs rhs with
        | None -> indexed_nonzero rhs lhs
        | some -> some
      in
      Option.bind candidate (fun (index : Ast.index_expr) ->
           match expr_as_local_name index.base with
           | Some array_name
             when expr_is_pure_native_index index.index
                  && stmt_is_readonly_scan_body array_name loop.body ->
               Some (index.base, index.index)
           | _ -> None)
  | _ -> None

let assignment_is_add_one name (assign : Ast.assign_stmt) =
  assign.target.segments = [ name ]
  && match assign.value.kind with
     | Binary b ->
         b.op = Add
         && ((expr_is_name b.lhs name && expr_is_one b.rhs)
             || (expr_is_one b.lhs && expr_is_name b.rhs name))
     | _ -> false

let rec assignments_to_name name (stmt : Ast.stmt) =
  let plus (a, b) (c, d) = (a + c, b + d) in
  let list xs =
    List.fold_left
      (fun acc s -> plus acc (assignments_to_name name s)) (0, 0) xs
  in
  let assign a =
    if a.target.segments <> [ name ] then (0, 0)
    else (1, if assignment_is_add_one name a then 1 else 0)
  in
  match stmt with
  | Assign a -> assign a
  | If i ->
      plus (assignments_to_name name i.then_branch)
        (Option.fold ~none:(0, 0) ~some:(assignments_to_name name) i.else_branch)
  | Switch s ->
      let cases =
        List.fold_left
          (fun acc (case : Ast.switch_case) -> plus acc (list case.body))
          (0, 0) s.cases
      in
      plus cases
        (Option.fold ~none:(0, 0)
           ~some:(fun (d : Ast.switch_default) -> list d.body) s.default)
  | While w -> assignments_to_name name w.body
  | ForEach f -> assignments_to_name name f.body
  | For f ->
      let init = match f.init with Some (ForInitAssign a) -> assign a | _ -> (0, 0) in
      let step = match f.step with Some (ForStepAssign a) -> assign a | _ -> (0, 0) in
      plus init (plus (assignments_to_name name f.body) step)
  | Block b -> list b.statements
  | VarDecl _ | Return _ | Break _ | Continue _ | IoChain _ | ExprStmt _ -> (0, 0)

let lt_condition_name expr =
  match expr.kind with
  | Binary { op = Lt; lhs = { kind = Path { segments = [ name ]; _ }; _ }; _ } -> Some name
  | _ -> None

type local_env =
  { mutable bindings : (string, local_binding) Hashtbl.t
  ; mutable undo : (string * local_binding option) list list
  }

type generator =
  { program : program
  ; sem : semantic_result
  ; gc_mode : int
  ; function_sigs : (string, function_sig) Hashtbl.t
  ; module_fns : (string * string, module_fn_sig) Hashtbl.t
  ; module_consts : (string * string, module_const_sig) Hashtbl.t
  ; mutable temp_counter : int
  ; mutable label_counter : int
  ; local_scopes : local_env
  ; link_groups : (string, int) Hashtbl.t
  ; group_members : (int, StringSet.t) Hashtbl.t
  ; mutable next_group_id : int
  ; mutable fn_return_ty : sem_type
  ; mutable fn_is_entry_main : bool
  ; string_literals : (string, string) Hashtbl.t
  ; mutable next_string_id : int
  ; fast_i64_int_functions : StringSet.t
  ; mutable fn_uses_fast_i64_int : bool
  ; mutable fn_fast_result_out : string option
  ; mutable fn_i64_overflow_label : string option
  ; mutable proven_i64_add_one : StringSet.t
  ; mutable effect_i64_locals : StringSet.t
  ; mutable effect_i64_bounds : (string, int64 * int64) Hashtbl.t
  ; mutable adaptive_state_locals : StringSet.t
  ; mutable inline_guarded_packed_ops : bool
  ; mutable bounds_proofs : bounds_proof list
  ; mutable discarded_call : Ast.call_expr option
  ; mutable loop_targets : (string * string) list
  ; mutable closure_counter : int
  ; fn_adapter_names : (string, string) Hashtbl.t
  ; mutable lir_label : string
  ; mutable lir_instrs : Lir.instr list
  ; mutable lir_blocks : Lir.block list
  ; mutable lir_entry_allocas : Lir.instr list
  ; mutable lir_funcs : Lir.func list
  ; mutable lir_generated_funcs : Lir.func list
  ; mutable lir_decls : Lir.decl list
  ; mutable lir_function_analysis_time : float
  ; mutable lir_optimization_time : float
  ; mutable lir_sink : (Lir.func -> unit) option
  }

let function_sig_opt g name = Hashtbl.find_opt g.function_sigs name
let has_function g name = Hashtbl.mem g.function_sigs name
let module_fn_opt g module_ name = Hashtbl.find_opt g.module_fns (module_, name)
let module_const_opt g module_ name =
  Hashtbl.find_opt g.module_consts (module_, name)

let strip_at s =
  if String.length s > 0 && s.[0] = '@' then String.sub s 1 (String.length s - 1) else s

let lir_ty = Lir.ty_of_fragment
let lir_val = Lir.value_of_fragment
let lir_ptr = Lir.value_of_fragment Lir.Ptr
let lir_add g instr = g.lir_instrs <- instr :: g.lir_instrs

let lir_close g term =
  if g.lir_label <> "" then begin
    g.lir_blocks <- { Lir.label = g.lir_label; instrs = List.rev g.lir_instrs; term } :: g.lir_blocks;
    g.lir_instrs <- [];
    g.lir_label <- ""
  end
  else if g.lir_instrs <> [] then
    failwith "Ir: instructions emitted after a terminator with no label between"

let lir_open g label =
  if g.lir_label <> "" then lir_close g (Lir.Br label);
  g.lir_label <- label

let emit_label g label =
  lir_open g label

let emit_entry_alloca_of g slot ty_name align =
  g.lir_entry_allocas <-
    Lir.Alloca { dst = slot; ty = lir_ty ty_name; align } :: g.lir_entry_allocas

let emit_entry_alloca g slot = emit_entry_alloca_of g slot "ptr" 8
let emit_entry_alloca_bool g slot = emit_entry_alloca_of g slot "i1" 1
let emit_entry_alloca_native g slot width = emit_entry_alloca_of g slot (llvm_int_ty width) (native_align width)
let emit_entry_alloca_float g slot width = emit_entry_alloca_of g slot (llvm_float_ty width) (max 1 (width / 8))

let emit_store g ty_name src dst align =
  let ty = lir_ty ty_name in
  lir_add g (Lir.Store { ty; src = lir_val ty src; dst = lir_ptr dst; align })

let emit_load g dst ty_name src align =
  lir_add g (Lir.Load { dst = dst; ty = lir_ty ty_name; src = lir_ptr src; align })

let emit_bin g dst op ty_name lhs rhs =
  let ty = lir_ty ty_name in
  match Lir.binop_of_string_opt op, Lir.fbinop_of_string_opt op with
  | Some op, _ -> lir_add g (Lir.Bin { dst = dst; op; ty; lhs = lir_val ty lhs; rhs = lir_val ty rhs })
  | _, Some op -> lir_add g (Lir.Fbin { dst = dst; op; ty; lhs = lir_val ty lhs; rhs = lir_val ty rhs })
  | None, None -> failwith (Printf.sprintf "Ir: unknown binary op %S" op)

let emit_icmp g dst pred ty_name lhs rhs =
  let ty = lir_ty ty_name in
  lir_add g
    (Lir.Icmp
       { dst = dst; pred = Lir.icmp_pred_of_string pred; ty
       ; lhs = lir_val ty lhs; rhs = lir_val ty rhs })

let emit_fcmp g dst pred ty_name lhs rhs =
  let ty = lir_ty ty_name in
  lir_add g
    (Lir.Fcmp
       { dst = dst; pred = Lir.fcmp_pred_of_string pred; ty
       ; lhs = lir_val ty lhs; rhs = lir_val ty rhs })

let emit_conv g dst op from_ty_name src to_ty_name =
  let from_ty = lir_ty from_ty_name in
  lir_add g
    (Lir.Conv
       { dst = dst; op = Lir.conv_of_string op; from_ty
       ; src = lir_val from_ty src; to_ty = lir_ty to_ty_name })

let emit_gep_global g dst len global_text =
  ignore len;
  let sym =
    if String.length global_text > 0 && global_text.[0] = '@' then
      String.sub global_text 1 (String.length global_text - 1)
    else global_text
  in
  lir_add g (Lir.Gep_global { dst = dst; sym })

let emit_gep_byte g dst base index =
  lir_add g
    (Lir.Gep_byte
       { dst = dst; base = lir_ptr base; index = (Lir.I 64, lir_val (Lir.I 64) index) })

let emit_ret g ty_name value =
  let ty = lir_ty ty_name in
  lir_close g (Lir.Ret (Some (ty, lir_val ty value)))

let emit_ret_void g =
  lir_close g (Lir.Ret None)

let emit_br g label =
  lir_close g (Lir.Br label)

let emit_br_cond g cond then_label else_label =
  lir_close g (Lir.Br_cond { cond = lir_val (Lir.I 1) cond; then_label; else_label })

let emit_switch g ty_name selector default_label cases =
  let ty = lir_ty ty_name in
  lir_close g (Lir.Switch { ty; selector = lir_val ty selector; default_label; cases })

let new_temp g =
  let name = "%t" ^ string_of_int g.temp_counter in
  g.temp_counter <- g.temp_counter + 1;
  name

let new_temp_named g base =
  let name = "%" ^ base ^ "." ^ string_of_int g.temp_counter in
  g.temp_counter <- g.temp_counter + 1;
  name

let new_label g base =
  let name = base ^ "." ^ string_of_int g.label_counter in
  g.label_counter <- g.label_counter + 1;
  name

let push_scope g =
  let env = g.local_scopes in
  env.undo <- [] :: env.undo

let pop_scope g =
  let env = g.local_scopes in
  match env.undo with
  | [] -> ()
  | frame :: rest ->
      List.iter
        (fun (name, prev) ->
          (match Hashtbl.find_opt g.link_groups name with
           | None -> ()
           | Some gid ->
               Hashtbl.remove g.link_groups name;
               let members =
                 Hashtbl.find_opt g.group_members gid
                 |> Option.value ~default:StringSet.empty
                 |> StringSet.remove name
               in
               if StringSet.is_empty members then Hashtbl.remove g.group_members gid
               else Hashtbl.replace g.group_members gid members);
          match prev with
          | Some binding -> Hashtbl.replace env.bindings name binding
          | None -> Hashtbl.remove env.bindings name)
        frame;
      env.undo <- rest

let declare_local ?adaptive_state g name slot binding ty =
  let env = g.local_scopes in
  match env.undo with
  | frame :: rest ->
      let prev = Hashtbl.find_opt env.bindings name in
      Hashtbl.replace env.bindings name { slot; binding; ty; adaptive_state };
      env.undo <- ((name, prev) :: frame) :: rest;
      if binding = Link && not (Hashtbl.mem g.link_groups name) then begin
        let gid = g.next_group_id in
        g.next_group_id <- g.next_group_id + 1;
        Hashtbl.add g.link_groups name gid;
        Hashtbl.add g.group_members gid (StringSet.singleton name)
      end
  | [] -> ()

let lookup_local g name = Hashtbl.find_opt g.local_scopes.bindings name

let lookup_local_slot g name = Option.map (fun b -> b.slot) (lookup_local g name)
let lookup_local_binding g name = Option.map (fun b -> b.binding) (lookup_local g name)
let lookup_local_ty g name = Option.map (fun b -> b.ty) (lookup_local g name)
let lookup_adaptive_state g name = Option.bind (lookup_local g name) (fun b -> b.adaptive_state)

let update_local_slot g name new_slot =
  match Hashtbl.find_opt g.local_scopes.bindings name with
  | Some binding ->
      binding.slot <- new_slot;
      true
  | None -> false

let ensure_link_group g name =
  match Hashtbl.find_opt g.link_groups name with
  | Some gid -> gid
  | None ->
      let gid = g.next_group_id in
      g.next_group_id <- g.next_group_id + 1;
      Hashtbl.add g.link_groups name gid;
      Hashtbl.add g.group_members gid (StringSet.singleton name);
      gid

let merge_link_groups g a b =
  let ga = ensure_link_group g a in
  let gb = ensure_link_group g b in
  if ga <> gb then begin
    let keep, drop = if ga < gb then ga, gb else gb, ga in
    let drop_members =
      Hashtbl.find_opt g.group_members drop |> Option.value ~default:StringSet.empty
    in
    let keep_members =
      Hashtbl.find_opt g.group_members keep |> Option.value ~default:StringSet.empty
    in
    let merged = StringSet.union keep_members drop_members in
    Hashtbl.remove g.group_members drop;
    Hashtbl.replace g.group_members keep merged;
    StringSet.iter (fun member -> Hashtbl.replace g.link_groups member keep) drop_members
  end

let retarget_link_group_to_slot g name new_slot =
  match Hashtbl.find_opt g.link_groups name with
  | Some gid ->
      let members = Hashtbl.find_opt g.group_members gid |> Option.value ~default:StringSet.empty in
      StringSet.iter (fun member -> ignore (update_local_slot g member new_slot)) members
  | None -> ignore (update_local_slot g name new_slot)

let lir_callee text =
  if String.length text > 0 && text.[0] = '@' then
    Lir.Direct (String.sub text 1 (String.length text - 1))
  else Lir.Indirect (lir_ptr text)

let lir_call ?(ret_ext = Lir.No_ext) ?arg_exts g dst ret_ty callee args =
  let arg_exts =
    match arg_exts with
    | Some exts -> exts
    | None -> Lir.no_exts (List.length args)
  in
  lir_add g
    (Lir.Call
       { dst = dst
       ; ret = lir_ty ret_ty
       ; ret_ext
       ; callee = lir_callee callee
       ; args
       ; arg_exts
       })

let emit_call_typed ?(ret_ext = Lir.No_ext) ?arg_exts g ret_ty callee args =
  let tmp = new_temp g in
  lir_call ~ret_ext ?arg_exts g (Some tmp) ret_ty callee args;
  tmp

let emit_call_void ?arg_exts g callee args =
  lir_call ?arg_exts g None "void" callee args

let lir_reset_function g =
  g.lir_label <- "";
  g.lir_instrs <- [];
  g.lir_blocks <- [];
  g.lir_entry_allocas <- []

let lir_finish_function ?(generated = false) ?(ret_ext = Lir.No_ext)
    ?param_exts g name ret params =
  if g.lir_label <> "" then
    failwith (Printf.sprintf "Ir: function %s ends without a terminator" name);
  let blocks =
    match List.rev g.lir_blocks with
    | entry :: rest -> { entry with Lir.instrs = List.rev g.lir_entry_allocas @ entry.Lir.instrs } :: rest
    | [] -> []
  in
  let param_exts =
    match param_exts with
    | Some exts -> exts
    | None -> Lir.no_exts (List.length params)
  in
  let optimization_started = Clock.now () in
  let func =
    Lir.optimize_func { Lir.name; ret; ret_ext; params; param_exts; blocks }
  in
  g.lir_optimization_time <-
    g.lir_optimization_time +. (Clock.now () -. optimization_started);
  if generated then g.lir_generated_funcs <- func :: g.lir_generated_funcs
  else begin
    g.lir_funcs <- func :: g.lir_funcs;
    match g.lir_sink with Some sink -> sink func | None -> ()
  end;
  lir_reset_function g

let emit_call_ptr g callee args = emit_call_typed g "ptr" callee args
let emit_call_i1 g callee args = emit_call_typed g "i1" callee args
let emit_call_i64 g callee args = emit_call_typed g "i64" callee args

let arg_ptr value = (Lir.Ptr, lir_ptr value)
let arg_i1 value = (Lir.I 1, lir_val (Lir.I 1) value)
let arg_i32 value = (Lir.I 32, lir_val (Lir.I 32) value)
let arg_i64 value = (Lir.I 64, lir_val (Lir.I 64) value)
let arg_double value = (Lir.F 64, lir_val (Lir.F 64) value)
let arg_int width value = (Lir.I width, lir_val (Lir.I width) value)
let arg_float width value = (Lir.F width, lir_val (Lir.F width) value)

let emit_num_from_i64 g value =
  emit_call_ptr g "@xi_num_from_i64" [ arg_i64 (string_of_int value) ]

let emit_num_from_i64_64 g value =
  emit_call_ptr g "@xi_num_from_i64" [ arg_i64 (Int64.to_string value) ]

let emit_array_header_load g arr offset ty align stem =
  let addr = new_temp_named g (stem ^ ".addr") in
  emit_gep_byte g addr arr (string_of_int offset);
  let value = new_temp_named g stem in
  emit_load g value ty addr align;
  value

let emit_bool_and g lhs rhs =
  let out = new_temp g in
  emit_bin g out "and" "i1" lhs rhs;
  out

let emit_guarded_packed_array_prefix g arr idx fallback packed =
  let nonnull = new_temp g in
  emit_icmp g nonnull "ne" "ptr" arr "null";
  let header = new_label g "arr.packed.header" in
  emit_br_cond g nonnull header fallback;
  emit_label g header;
  let nonnegative = new_temp g in
  emit_icmp g nonnegative "sge" "i64" idx "0";
  let bounds = new_label g "arr.packed.bounds" in
  emit_br_cond g nonnegative bounds fallback;
  emit_label g bounds;
  let len = emit_array_header_load g arr 0 "i64" 8 "arr.len" in
  let in_bounds = new_temp g in
  emit_icmp g in_bounds "ult" "i64" idx len;
  let representation = new_label g "arr.packed.representation" in
  emit_br_cond g in_bounds representation fallback;
  emit_label g representation;
  let data = emit_array_header_load g arr 24 "ptr" 8 "arr.packed.data" in
  let has_data = new_temp g in
  emit_icmp g has_data "ne" "ptr" data "null";
  let signed_label = new_label g "arr.packed.signed" in
  emit_br_cond g has_data signed_label fallback;
  emit_label g signed_label;
  let unsigned = emit_array_header_load g arr 34 "i8" 1 "arr.packed.unsigned" in
  let is_signed = new_temp g in
  emit_icmp g is_signed "eq" "i8" unsigned "0";
  emit_br_cond g is_signed packed fallback;
  data

let packed_widths = [ (8, 1); (16, 2); (32, 4); (64, 8) ]

let emit_packed_element_addr g data idx bytes =
  let offset = new_temp g in
  emit_bin g offset "mul" "i64" idx (string_of_int bytes);
  let addr = new_temp_named g "arr.packed.element" in
  emit_gep_byte g addr data offset;
  addr

let emit_packed_signed_load g addr width =
  let raw = new_temp g in
  emit_load g raw (llvm_int_ty width) addr 1;
  if width = 64 then raw
  else begin
    let extended = new_temp g in
    emit_conv g extended "sext" (llvm_int_ty width) raw "i64";
    extended
  end

let emit_guarded_packed_cmp g arr idx rhs pred fallback_callee =
  let result_slot = new_temp_named g "arr.packed.cmp.result.addr" in
  emit_entry_alloca_bool g result_slot;
  let fallback = new_label g "arr.packed.cmp.fallback" in
  let packed = new_label g "arr.packed.cmp.width" in
  let done_label = new_label g "arr.packed.cmp.done" in
  let data = emit_guarded_packed_array_prefix g arr idx fallback packed in
  emit_label g packed;
  let bits = emit_array_header_load g arr 32 "i16" 2 "arr.packed.bits" in
  let width_labels = List.map (fun (width, _) -> width, new_label g "arr.packed.cmp") packed_widths in
  emit_switch g "i16" bits fallback
    (List.map (fun (width, label) -> string_of_int width, label) width_labels);
  List.iter2
    (fun (width, bytes) (_, label) ->
      emit_label g label;
      let addr = emit_packed_element_addr g data idx bytes in
      let lhs = emit_packed_signed_load g addr width in
      let compared = new_temp g in
      emit_icmp g compared pred "i64" lhs rhs;
      emit_store g "i1" compared result_slot 1;
      emit_br g done_label)
    packed_widths width_labels;
  emit_label g fallback;
  let slow = emit_call_i1 g fallback_callee [ arg_ptr arr; arg_i64 idx; arg_i64 rhs ] in
  emit_store g "i1" slow result_slot 1;
  emit_br g done_label;
  emit_label g done_label;
  let result = new_temp g in
  emit_load g result "i1" result_slot 1;
  result

let emit_guarded_packed_read_i64 g arr idx =
  let result_slot = new_temp_named g "arr.packed.read.result.addr" in
  emit_entry_alloca_native g result_slot 64;
  let fallback = new_label g "arr.packed.read.fallback" in
  let packed = new_label g "arr.packed.read.width" in
  let done_label = new_label g "arr.packed.read.done" in
  let data = emit_guarded_packed_array_prefix g arr idx fallback packed in
  emit_label g packed;
  let bits = emit_array_header_load g arr 32 "i16" 2 "arr.packed.bits" in
  let width_labels = List.map (fun (width, _) -> width, new_label g "arr.packed.read") packed_widths in
  emit_switch g "i16" bits fallback
    (List.map (fun (width, label) -> string_of_int width, label) width_labels);
  List.iter2
    (fun (width, bytes) (_, label) ->
      emit_label g label;
      let addr = emit_packed_element_addr g data idx bytes in
      let value = emit_packed_signed_load g addr width in
      emit_store g "i64" value result_slot 8;
      emit_br g done_label)
    packed_widths width_labels;
  emit_label g fallback;
  let slow = emit_call_i64 g "@xi_arr_get_num_i64_at_i64"
    [ arg_ptr arr; arg_i64 idx ] in
  emit_store g "i64" slow result_slot 8;
  emit_br g done_label;
  emit_label g done_label;
  let result = new_temp g in
  emit_load g result "i64" result_slot 8;
  result

let emit_guarded_packed_update g arr idx rhs op fallback_callee =
  let fallback = new_label g "arr.packed.update.fallback" in
  let packed = new_label g "arr.packed.update.width" in
  let done_label = new_label g "arr.packed.update.done" in
  let data = emit_guarded_packed_array_prefix g arr idx fallback packed in
  emit_label g packed;
  let bits = emit_array_header_load g arr 32 "i16" 2 "arr.packed.bits" in
  let width_labels = List.map (fun (width, _) -> width, new_label g "arr.packed.update") packed_widths in
  emit_switch g "i16" bits fallback
    (List.map (fun (width, label) -> string_of_int width, label) width_labels);
  List.iter2
    (fun (width, bytes) (_, label) ->
      emit_label g label;
      let addr = emit_packed_element_addr g data idx bytes in
      let old = emit_packed_signed_load g addr width in
      let native = new_temp g in
      let overflow_slot = new_temp_named g "arr.packed.update.overflow.addr" in
      emit_entry_alloca_bool g overflow_slot;
      lir_add g
        (Lir.Checked_arith
           { dst = native
           ; overflow = lir_ptr overflow_slot
           ; op
           ; lhs = lir_val (Lir.I 64) old
           ; rhs = lir_val (Lir.I 64) rhs
           });
      let overflow = new_temp g in
      emit_load g overflow "i1" overflow_slot 1;
      let arithmetic_ok = new_label g "arr.packed.update.arithmetic.ok" in
      emit_br_cond g overflow fallback arithmetic_ok;
      emit_label g arithmetic_ok;
      let fits =
        if width = 64 then "true"
        else begin
          let lo = Int64.neg (Int64.shift_left 1L (width - 1)) in
          let hi = Int64.pred (Int64.shift_left 1L (width - 1)) in
          let above = new_temp g in
          let below = new_temp g in
          emit_icmp g above "sge" "i64" native (Int64.to_string lo);
          emit_icmp g below "sle" "i64" native (Int64.to_string hi);
          emit_bool_and g above below
        end
      in
      let store_label = new_label g "arr.packed.update.store" in
      emit_br_cond g fits store_label fallback;
      emit_label g store_label;
      let stored =
        if width = 64 then native
        else begin
          let truncated = new_temp g in
          emit_conv g truncated "trunc" "i64" native (llvm_int_ty width);
          truncated
        end
      in
      emit_store g (llvm_int_ty width) stored addr 1;
      emit_br g done_label)
    packed_widths width_labels;
  emit_label g fallback;
  emit_call_void g fallback_callee [ arg_ptr arr; arg_i64 idx; arg_i64 rhs ];
  emit_br g done_label;
  emit_label g done_label

let emit_checked_i64_arith g op lhs rhs overflow_label =
  let result = new_temp g in
  let overflow_slot = new_temp_named g "arith.overflow.addr" in
  emit_entry_alloca_bool g overflow_slot;
  lir_add g
    (Lir.Checked_arith
       { dst = result
       ; overflow = lir_ptr overflow_slot
       ; op
       ; lhs = lir_val (Lir.I 64) lhs
       ; rhs = lir_val (Lir.I 64) rhs
       });
  let overflow = new_temp g in
  emit_load g overflow "i1" overflow_slot 1;
  let success_label = new_label g "checked.i64.ok" in
  emit_br_cond g overflow overflow_label success_label;
  emit_label g success_label;
  result

let emit_checked_i64_binary g runtime_fn lhs rhs overflow_label =
  let op = if runtime_fn = "@xi_i64_add_checked" then Lir.Add else Lir.Sub in
  emit_checked_i64_arith g op lhs rhs overflow_label

let emit_checked_i64_neg g value overflow_label =
  let overflow = new_temp g in
  emit_icmp g overflow "eq" "i64" value "-9223372036854775808";
  let success_label = new_label g "checked.neg.ok" in
  emit_br_cond g overflow overflow_label success_label;
  emit_label g success_label;
  let result = new_temp g in
  emit_bin g result "sub" "i64" "0" value;
  result

let emit_checked_i64_mul g lhs rhs overflow_label =
  emit_checked_i64_arith g Lir.Mul lhs rhs overflow_label

let emit_safe_i64_mod g lhs rhs =
  let result_slot = new_temp_named g "mod.result.addr" in
  emit_entry_alloca_native g result_slot 64;
  let is_zero = new_temp g in
  let is_minus_one = new_temp g in
  emit_icmp g is_zero "eq" "i64" rhs "0";
  emit_icmp g is_minus_one "eq" "i64" rhs "-1";
  let is_special = new_temp g in
  emit_bin g is_special "or" "i1" is_zero is_minus_one;
  let special_label = new_label g "mod.special" in
  let normal_label = new_label g "mod.normal" in
  let done_label = new_label g "mod.done" in
  emit_br_cond g is_special special_label normal_label;
  emit_label g special_label;
  emit_store g "i64" "0" result_slot 8;
  emit_br g done_label;
  emit_label g normal_label;
  let remainder = new_temp g in
  emit_bin g remainder "srem" "i64" lhs rhs;
  emit_store g "i64" remainder result_slot 8;
  emit_br g done_label;
  emit_label g done_label;
  let result = new_temp g in
  emit_load g result "i64" result_slot 8;
  result

let emit_checked_i64_pow g base exponent overflow_label =
  let result_slot = new_temp_named g "pow.result.addr" in
  let base_slot = new_temp_named g "pow.base.addr" in
  let exponent_slot = new_temp_named g "pow.exponent.addr" in
  emit_entry_alloca_native g result_slot 64;
  emit_entry_alloca_native g base_slot 64;
  emit_entry_alloca_native g exponent_slot 64;
  emit_store g "i64" "1" result_slot 8;
  emit_store g "i64" base base_slot 8;
  emit_store g "i64" exponent exponent_slot 8;
  let negative = new_temp g in
  emit_icmp g negative "slt" "i64" exponent "0";
  let loop_label = new_label g "pow.loop" in
  emit_br_cond g negative overflow_label loop_label;

  emit_label g loop_label;
  let current_exponent = new_temp g in
  emit_load g current_exponent "i64" exponent_slot 8;
  let finished = new_temp g in
  emit_icmp g finished "eq" "i64" current_exponent "0";
  let done_label = new_label g "pow.done" in
  let body_label = new_label g "pow.body" in
  emit_br_cond g finished done_label body_label;

  emit_label g body_label;
  let low_bit = new_temp g in
  emit_bin g low_bit "and" "i64" current_exponent "1";
  let multiply_result = new_temp g in
  emit_icmp g multiply_result "ne" "i64" low_bit "0";
  let multiply_label = new_label g "pow.multiply" in
  let shift_label = new_label g "pow.shift" in
  emit_br_cond g multiply_result multiply_label shift_label;

  emit_label g multiply_label;
  let current_result = new_temp g in
  let current_base = new_temp g in
  emit_load g current_result "i64" result_slot 8;
  emit_load g current_base "i64" base_slot 8;
  let product = emit_checked_i64_mul g current_result current_base overflow_label in
  emit_store g "i64" product result_slot 8;
  emit_br g shift_label;

  emit_label g shift_label;
  let exponent_before_shift = new_temp g in
  emit_load g exponent_before_shift "i64" exponent_slot 8;
  let shifted = new_temp g in
  emit_bin g shifted "lshr" "i64" exponent_before_shift "1";
  emit_store g "i64" shifted exponent_slot 8;
  let no_more_bits = new_temp g in
  emit_icmp g no_more_bits "eq" "i64" shifted "0";
  let square_label = new_label g "pow.square" in
  emit_br_cond g no_more_bits done_label square_label;

  emit_label g square_label;
  let base_before_square = new_temp g in
  emit_load g base_before_square "i64" base_slot 8;
  let squared = emit_checked_i64_mul g base_before_square base_before_square overflow_label in
  emit_store g "i64" squared base_slot 8;
  emit_br g loop_label;

  emit_label g done_label;
  let result = new_temp g in
  emit_load g result "i64" result_slot 8;
  result

let intern_string_literal g value =
  match Hashtbl.find_opt g.string_literals value with
  | Some name -> name
  | None ->
      let name = "@.str." ^ string_of_int g.next_string_id in
      g.next_string_id <- g.next_string_id + 1;
      Hashtbl.add g.string_literals value name;
      name

let emit_cstr_ptr g value =
  let global_name = intern_string_literal g value in
  let len = String.length value + 1 in
  let tmp = new_temp g in
  emit_gep_global g tmp len global_name;
  tmp

let native_to_i64 g v width signed =
  if width = 64 then v
  else
    let tmp = new_temp g in
    let op = if width < 64 then if signed then "sext" else "zext" else "trunc" in
    emit_conv g tmp op (llvm_int_ty width) v "i64";
    tmp

let i64_to_native g as_i64 width signed =
  if width = 64 then as_i64
  else
    let tmp = new_temp g in
    let op = if width < 64 then "trunc" else if signed then "sext" else "zext" in
    emit_conv g tmp op "i64" as_i64 (llvm_int_ty width);
    tmp

let coerce_native_width g v from_ to_ signed =
  if from_ = to_ then v
  else
    let tmp = new_temp g in
    let op = if to_ < from_ then "trunc" else if signed then "sext" else "zext" in
    emit_conv g tmp op (llvm_int_ty from_) v (llvm_int_ty to_);
    tmp

let native_float_to_double g v width =
  if width = 64 then v
  else
    let tmp = new_temp g in
    emit_conv g tmp "fpext" (llvm_float_ty width) v "double";
    tmp

let double_to_native_float g v width =
  if width = 64 then v
  else
    let tmp = new_temp g in
    emit_conv g tmp "fptrunc" "double" v (llvm_float_ty width);
    tmp

let coerce_native_float_width g v from_ to_ =
  if from_ = to_ then v
  else
    let tmp = new_temp g in
    let op = if to_ < from_ then "fptrunc" else "fpext" in
    emit_conv g tmp op (llvm_float_ty from_) v (llvm_float_ty to_);
    tmp

let box_native_int g v width signed =
  if signed && width <= 64 then
    let as_i64 = native_to_i64 g v width true in
    emit_call_ptr g "@xi_num_from_i64" [ arg_i64 as_i64 ]
  else
    let slot = new_temp_named g "widebox.addr" in
    emit_entry_alloca_native g slot width;
    emit_store g (llvm_int_ty width) v slot (native_align width);
    emit_call_ptr g "@xi_num_from_wide"
      [ arg_ptr slot
      ; arg_i64 (string_of_int (width / 8))
      ; arg_i1 (if signed then "1" else "0")
      ]

let box_native_float g v width =
  let as_double = native_float_to_double g v width in
  emit_call_ptr g "@xi_num_from_f64" [ arg_double as_double ]

let unbox_native_int g boxed width signed =
  if width <= 64 then
    let as_i64 = emit_call_i64 g "@xi_num_as_i64" [ arg_ptr boxed ] in
    i64_to_native g as_i64 width signed
  else
    let slot = new_temp_named g "wideunbox.addr" in
    emit_entry_alloca_native g slot width;
    emit_call_void g "@xi_num_to_wide"
      [ arg_ptr boxed; arg_ptr slot; arg_i64 (string_of_int (width / 8)) ];
    let tmp = new_temp g in
    emit_load g tmp (llvm_int_ty width) slot (native_align width);
    tmp

let emit_wide_print g v width signed =
  let slot = new_temp_named g "wideprint.addr" in
  emit_entry_alloca_native g slot width;
  emit_store g (llvm_int_ty width) v slot (native_align width);
  emit_call_void g "@xi_io_write_wide"
    [ arg_ptr slot
    ; arg_i64 (string_of_int (width / 8))
    ; arg_i1 (if signed then "1" else "0")
    ]

let ensure_num g = function
  | Num v -> v
  | NativeInt (v, width, signed) -> box_native_int g v width signed
  | NativeFloat (v, width) -> box_native_float g v width
  | BoolV b ->
      let i64 = new_temp g in
      emit_conv g i64 "zext" "i1" b "i64";
      emit_call_ptr g "@xi_num_from_i64" [ arg_i64 i64 ]
  | CptrV v | CStructV v ->
      let tmp = new_temp g in
      emit_conv g tmp "ptrtoint" "ptr" v "i64";
      emit_call_ptr g "@xi_num_from_i64" [ arg_i64 tmp ]
  | StrV _ | Arr _ | MapV _ | StructV _ | FnPtrV _ -> emit_num_from_i64 g 0

let ensure_str = function StrV v -> v | _ -> "null"
let ensure_cptr = function CptrV v -> v | CStructV v -> v | _ -> "null"
let ensure_cstruct = function CStructV v -> v | CptrV v -> v | _ -> "null"
let ensure_arr = function Arr v -> v | _ -> "null"
let ensure_map = function MapV v -> v | _ -> "null"
let ensure_struct = function StructV v -> v | _ -> "null"
let ensure_fn_ptr = function FnPtrV v -> v | _ -> "null"

let ensure_bool g = function
  | BoolV v -> v
  | NativeInt (v, width, _) ->
      let tmp = new_temp g in
      emit_icmp g tmp "ne" (llvm_int_ty width) v "0";
      tmp
  | NativeFloat (v, width) ->
      let tmp = new_temp g in
      emit_fcmp g tmp "une" (llvm_float_ty width) v "0.0";
      tmp
  | Num v -> emit_call_i1 g "@xi_num_truthy" [ arg_ptr v ]
  | StrV v | Arr v | MapV v | StructV v | FnPtrV v | CptrV v | CStructV v ->
      let tmp = new_temp g in
      emit_icmp g tmp "ne" "ptr" v "null";
      tmp

let rec default_value_for_type g ty =
  match ty with
  | Str -> "null"
  | Cptr | CStruct _ -> "null"
  | Bool -> emit_num_from_i64 g 0
  | ty when is_array_sem_type ty || is_map_sem_type ty -> "null"
  | Struct id -> emit_default_struct_value g id
  | Result _ | ResultUnknown ->
      let payload = match ty with Result payload -> payload | _ -> Int in
      let out = emit_call_ptr g "@xi_struct_new" [] in
      emit_struct_set g out (emit_cstr_ptr g "ok") (ensure_value_for_type g (BoolV "false") Bool) Bool;
      emit_struct_set g out (emit_cstr_ptr g "value") (default_value_for_type g payload) payload;
      emit_struct_set g out (emit_cstr_ptr g "error") (emit_cstr_ptr g "") Str;
      out
  | FnPtr _ -> "null"
  | DecN w when software_float_width_of (DecN w) <> None ->
      emit_call_ptr g "@xi_softfloat_zero" [ arg_i32 (string_of_int w) ]
  | _ -> emit_num_from_i64 g 0

and ensure_value_for_type g value ty =
  match ty with
  | Bool ->
      let b = ensure_bool g value in
      let i64 = new_temp g in
      emit_conv g i64 "zext" "i1" b "i64";
      emit_call_ptr g "@xi_num_from_i64" [ arg_i64 i64 ]
  | Str -> ensure_str value
  | Cptr -> ensure_cptr value
  | CStruct _ -> ensure_cstruct value
  | ty when is_array_sem_type ty -> ensure_arr value
  | ty when is_map_sem_type ty -> ensure_map value
  | Struct _ | Result _ | ResultUnknown -> ensure_struct value
  | FnPtr _ -> ensure_fn_ptr value
  | Uint ->
      let v = ensure_num g value in
      emit_call_ptr g "@xi_num_clamp_nonneg" [ arg_ptr v ]
  | DecN w when software_float_width_of (DecN w) <> None ->
      let v = ensure_num g value in
      emit_call_ptr g "@xi_softfloat_coerce" [ arg_ptr v; arg_i32 (string_of_int w) ]
  | _ -> ensure_num g value

and emit_default_struct_value g struct_id =
  let out = emit_call_ptr g "@xi_struct_new" [] in
  (match assoc_opt struct_id g.sem.struct_sigs_by_id with
   | None -> ()
   | Some sig_ ->
       List.iter
         (fun field ->
           let key = emit_cstr_ptr g field.sfs_name in
           let value = default_value_for_type g field.sfs_ty in
           emit_struct_set g out key value field.sfs_ty)
         sig_.ss_fields);
  out

and emit_struct_set g base key value ty =
  match ty with
  | Str -> emit_call_void g "@xi_struct_set_str" [ (arg_ptr base); (arg_ptr key); (arg_ptr value) ]
  | _ -> emit_call_void g "@xi_struct_set" [ (arg_ptr base); (arg_ptr key); (arg_ptr value) ]

let rec sem_type_of_expr g expr =
  match expr.kind with
  | Literal (LitInt _) -> Int
  | Literal (LitDec _) -> Dec
  | Literal (LitStr _) -> Str
  | Literal (LitBool _) -> Bool
  | ArrayLiteral arr ->
      let elem =
        List.fold_left
          (fun acc item ->
            match acc with
            | None -> Some (sem_type_of_expr g item)
            | Some prev -> unify_array_literal_elem_sem_type prev (sem_type_of_expr g item))
          None arr.items
      in
      (match elem with Some ty -> array_sem_type_from_element ty | None -> ArrayUnknown)
  | StructLiteral lit ->
      (match assoc_opt lit.name g.sem.struct_sigs_by_name with Some s -> Struct s.ss_id | None -> Error)
  | Closure closure ->
      let wanted =
        { fps_params = List.map (fun (p : Ast.param) -> sem_type_from_typeref p.ty g.sem) closure.params
        ; fps_return_ty = sem_type_from_typeref closure.return_ty g.sem
        }
      in
      let rec find idx = function [] -> Error | x :: xs -> if x = wanted then FnPtr idx else find (idx + 1) xs in
      find 0 g.sem.fn_ptr_sigs
  | Path path ->
      (match path.segments with
       | [ name ] ->
           (match lookup_local_ty g name with
            | Some ty -> ty
            | None ->
                (match function_sig_opt g name with
                 | Some sig_ ->
                     let wanted = { fps_params = List.map (fun p -> p.ps_ty) sig_.fs_params; fps_return_ty = sig_.fs_return_ty } in
                     let rec find idx = function [] -> FnPtr 0 | x :: xs -> if x = wanted then FnPtr idx else find (idx + 1) xs in
                     find 0 g.sem.fn_ptr_sigs
                 | None -> Int))
       | [ module_; name ] ->
           (match module_const_opt g module_ name with
            | Some c -> c.mcs_ty
            | None -> (match function_sig_opt g (module_ ^ "::" ^ name) with Some s -> s.fs_return_ty | None -> Int))
       | _ -> Int)
  | Call call when indirect_call_callee_type g call.callee <> None ->
      (match indirect_call_callee_type g call.callee with
       | Some (FnPtr sig_id) ->
           (match List.nth_opt g.sem.fn_ptr_sigs sig_id with
            | Some sig_ -> sig_.fps_return_ty
            | None -> Error)
       | _ -> Error)
  | Call call ->
      (match path_segments call.callee with
       | Some [ "type" ] -> Str
       | Some [ "size" ] -> Int
       | Some [ "cast" ] ->
           (match call.args with
            | value :: width :: _ ->
                (match cast_width_literal width with
                 | Some w -> (match cast_result_type (sem_type_of_expr g value) w with Some ty -> ty | None -> Int)
                 | None -> Int)
            | _ -> Int)
       | Some [ "copy" ] when not (has_function g "copy") ->
           (match call.args with arg :: _ -> sem_type_of_expr g arg | [] -> Error)
       | Some [ "res"; "ok" ] ->
           (match call.args with [ v ] -> Result (sem_type_of_expr g v) | _ -> ResultUnknown)
       | Some [ "res"; "err" ] -> ResultUnknown
       | Some [ "res"; "is_ok" ] | Some [ "res"; "is_err" ] -> Bool
       | Some [ "res"; "error" ] -> Str
       | Some [ "res"; "value" ] ->
           (match call.args with
            | [ r ] -> (match sem_type_of_expr g r with Result payload -> payload | _ -> NumUnknown)
            | _ -> NumUnknown)
       | Some [ "res"; "or" ] ->
           (match call.args with
            | [ r; fallback ] ->
                (match sem_type_of_expr g r with
                 | Result payload when payload <> NumUnknown -> payload
                 | _ -> sem_type_of_expr g fallback)
            | _ -> NumUnknown)
       | Some [ "gc"; "collect" ] -> Void
       | Some [ "gc"; ("collections" | "live") ] -> Int
       | Some [ "arr"; "new" ] -> ArrayUnknown
       | Some [ "arr"; ("get" | "get_unchecked" | "pop" | "remove") ] ->
           (match call.args with
            | arr :: _ ->
                (match array_element_sem_type (sem_type_of_expr g arr) with
                 | Some ty -> ty
                 | None -> NumUnknown)
            | [] -> NumUnknown)
       | Some [ "arr"; ("slice" | "slice_from") ] ->
           (match call.args with arr :: _ -> sem_type_of_expr g arr | [] -> ArrayUnknown)
       | Some [ "arr"; ("keys" | "values") ] -> ArrayUnknown
       | Some [ "arr"; "len" ] -> IntN 64
       | Some [ "map"; "new" ] -> MapUnknown
       | Some [ "map"; ("get" | "get_or") ] ->
           (match call.args with
            | map :: _ ->
                (match map_value_sem_type (sem_type_of_expr g map) with Some ty -> ty | None -> Str)
            | [] -> Str)
       | Some [ "map"; "keys" ] ->
           (match call.args with
            | map :: _ -> map_keys_array_sem_type (sem_type_of_expr g map)
            | [] -> ArrayUnknown)
       | Some [ "map"; "values" ] ->
           (match call.args with
            | map :: _ ->
                array_sem_type_from_element
                  (match map_value_sem_type (sem_type_of_expr g map) with Some ty -> ty | None -> Str)
            | [] -> ArrayUnknown)
       | Some [ "map"; "len" | "has" ] -> Int
       | Some [ "str"; ("len" | "char_code_at") ] -> IntN 64
       | Some [ "str"; "from_char_code" ] -> Str
       | Some [ "cptr"; op ] -> cptr_call_sem_type op
       | Some [ "cstruct"; op ] -> cstruct_call_sem_type g op call.args
       | Some [ "cfn"; _ ] -> Cptr
       | Some [ module_; name ] ->
           (match module_fn_opt g module_ name with Some s -> s.mfs_return_ty | None -> Int)
       | Some [ name ] ->
           (match function_sig_opt g name with
            | Some s -> s.fs_return_ty
            | None ->
                (match module_fn_opt g "" name with
                 | Some s -> s.mfs_return_ty
                 | None -> Int))
       | _ -> Int)
  | Index idx ->
      let base_ty = sem_type_of_expr g idx.base in
      (match array_element_sem_type base_ty with Some ty -> ty | None -> (match map_value_sem_type base_ty with Some ty -> ty | None -> Int))
  | Member member ->
      (match sem_type_of_expr g member.base with
       | Struct id ->
           (match assoc_opt id g.sem.struct_sigs_by_id with
            | Some s ->
                (match List.find_opt (fun field -> field.sfs_name = member.field) s.ss_fields with
                 | Some field -> field.sfs_ty
                 | None -> Error)
            | None -> Error)
       | CStruct id ->
           (match cstruct_field_info g id member.field with Some (_, ty) -> ty | None -> Error)
       | _ -> Error)
  | Unary unary ->
      (match unary.op with Not -> Bool | _ -> sem_type_of_expr g unary.rhs)
  | Binary binary ->
      (match binary.op with
       | Eq | Ne | Lt | Le | Gt | Ge | And | Or -> Bool
       | Add when sem_type_of_expr g binary.lhs = Str || sem_type_of_expr g binary.rhs = Str -> Str
       | Add | Sub | Mul | Div | Mod | Pow | BitAnd | BitOr | BitXor ->
           unify_numeric_sem_type (sem_type_of_expr g binary.lhs) (sem_type_of_expr g binary.rhs))
  | Cast cast -> sem_type_from_typeref cast.ty g.sem
  | Try inner ->
      (match sem_type_of_expr g inner with Result payload -> payload | _ -> NumUnknown)
  | Grouping inner -> sem_type_of_expr g inner

(* A call whose callee is a *value* of function-pointer type rather than a
   function name -- `fp(x)` where `fp` is a parameter or local. Such a value is a
   closure object, so the call has to go through its code/env pair; emitting
   `call @fp` names a symbol that does not exist and fails at link time. Returns
   the callee's `FnPtr` type, or `None` for an ordinary call. *)
and indirect_call_callee_type g (callee : Ast.expr) =
  match callee.kind with
  | Path path ->
      (match path.segments with
       | [ name ] -> (match lookup_local_ty g name with Some (FnPtr _ as ty) -> Some ty | _ -> None)
       | _ -> None)
  | _ -> (match sem_type_of_expr g callee with FnPtr _ as ty -> Some ty | _ -> None)

and cast_width_literal expr =
  match expr.kind with
  | Literal (LitInt s) -> int_of_string_opt s
  | Grouping inner -> cast_width_literal inner
  | _ -> None

and cptr_call_sem_type = function
  | "alloc" | "null" | "offset" | "read_ptr" | "from_addr" -> Cptr
  | "is_null" -> Bool
  | "addr" -> UintN 64
  | "read_i8" -> IntN 8 | "read_i16" -> IntN 16 | "read_i32" -> IntN 32 | "read_i64" -> IntN 64
  | "read_u8" -> UintN 8 | "read_u16" -> UintN 16 | "read_u32" -> UintN 32 | "read_u64" -> UintN 64
  | "read_f32" -> DecN 32 | "read_f64" -> DecN 64
  | "read_cstr" -> Str
  | "write_cstr" -> IntN 64
  | _ -> Int

and cstruct_field_info g id field =
  match assoc_opt id g.sem.cstruct_sigs_by_id with
  | None -> None
  | Some s ->
      (match List.find_opt (fun f -> f.cfs_name = field) s.cs_fields with
       | Some f -> Some (f.cfs_offset, f.cfs_ty)
       | None -> None)

and cstruct_id_of_type_arg g e =
  match e.kind with
  | Path { segments = [ n ]; _ } ->
      (match assoc_opt n g.sem.cstruct_sigs_by_name with Some s -> Some s.cs_id | None -> None)
  | Grouping inner -> cstruct_id_of_type_arg g inner
  | _ -> None

and cstruct_call_sem_type g op args =
  match op, args with
  | "alloc", [ ty ] | "view", [ _; ty ] ->
      (match cstruct_id_of_type_arg g ty with Some id -> CStruct id | None -> Error)
  | "ptr", _ -> Cptr
  | _ -> Int

and cast_result_type in_ty width =
  match in_ty with
  | Uint | UintN _ -> if is_valid_int_width width then Some (UintN width) else None
  | Dec | DecN _ -> if is_valid_int_width width then Some (DecN width) else None
  | Int | IntN _ | NumUnknown -> if is_valid_int_width width then Some (IntN width) else None
  | _ -> None

let array_runtime_kind_for_expr g expr =
  match sem_type_of_expr g expr with
  | ArrayInt _ | ArrayUint _ | ArrayDec -> ArrayNum
  | ArrayBool -> ArrayBoolKind
  | ArrayStruct _ -> ArrayStructKind
  | ArrayFnPtr _ -> ArrayFnPtrKind
  | _ -> ArrayStrKind

let array_num_runtime_suffix_for_expr g expr =
  match sem_type_of_expr g expr with ArrayUint _ -> "uint" | _ -> "num"

let array_int_elem_info_for_expr g expr =
  match sem_type_of_expr g expr with
  | ArrayInt (Some width) when width <= 64 -> Some (width, true)
  | ArrayUint (Some width) when width <= 64 -> Some (width, false)
  | ArrayInt None when g.fn_uses_fast_i64_int -> Some (64, true)
  | _ -> None

let map_runtime_kind_for_expr g expr =
  match sem_type_of_expr g expr with
  | MapInt | MapDec | MapKeyed (_, (ValueInt | ValueDec)) -> MapNum
  | MapBool | MapKeyed (_, ValueBool) -> MapBoolKind
  | MapStruct _ | MapKeyed (_, ValueStruct _) -> MapStructKind
  | MapFnPtr _ | MapKeyed (_, ValueFnPtr _) -> MapFnPtrKind
  | _ -> MapStrKind

let map_key_runtime_kind_for_expr g expr =
  match sem_type_of_expr g expr with
  | MapKeyed ((KeyInt | KeyDec), _) -> MapKeyNum
  | MapKeyed (KeyBool, _) -> MapKeyBool
  | _ -> MapKeyStr

let fast_i64_int_ty g ty = g.fn_uses_fast_i64_int && ty = Int

let local_native_int_info ?name g ty =
  match native_width_of ty with
  | Some w -> Some (w, native_signed_of ty)
  | None ->
      if fast_i64_int_ty g ty
         || (ty = Int
             && Option.fold ~none:false
                  ~some:(fun local -> StringSet.mem local g.effect_i64_locals)
                  name)
      then Some (64, true)
      else None

let emit_adaptive_state_num g state =
  let result_slot = new_temp_named g "adaptive.result.addr" in
  emit_entry_alloca g result_slot;
  let boxed = new_temp g in
  emit_load g boxed "i1" state.boxed_flag_slot 1;
  let boxed_label = new_label g "adaptive.boxed" in
  let native_label = new_label g "adaptive.native" in
  let done_label = new_label g "adaptive.done" in
  emit_br_cond g boxed boxed_label native_label;
  emit_label g native_label;
  let native = new_temp g in
  emit_load g native "i64" state.native_slot 8;
  let native_box = emit_call_ptr g "@xi_num_from_i64" [ arg_i64 native ] in
  emit_store g "ptr" native_box result_slot 8;
  emit_br g done_label;
  emit_label g boxed_label;
  let boxed_value = new_temp g in
  emit_load g boxed_value "ptr" state.boxed_slot 8;
  emit_store g "ptr" boxed_value result_slot 8;
  emit_br g done_label;
  emit_label g done_label;
  let result = new_temp g in
  emit_load g result "ptr" result_slot 8;
  result

let rec gen_expr g expr =
  match expr.kind with
  | Literal (LitInt v) ->
      (match parse_i64 v with
       | Some parsed -> Num (emit_num_from_i64_64 g parsed)
       | None ->
           let s = emit_cstr_ptr g v in
           Num (emit_call_ptr g "@xi_num_from_decimal" [ arg_ptr s ]))
  | Literal (LitDec v) ->
      let s = emit_cstr_ptr g v in
      Num (emit_call_ptr g "@xi_num_dec_from_decimal" [ arg_ptr s ])
  | Literal (LitStr s) -> StrV (emit_cstr_ptr g s)
  | Literal (LitBool b) -> BoolV (if b then "true" else "false")
  | ArrayLiteral arr ->
      let out = emit_call_ptr g "@xi_arr_new" [] in
      let kind = array_runtime_kind_for_expr g expr in
      List.iter
        (fun item ->
          match kind with
          | ArrayNum ->
              let suffix = array_num_runtime_suffix_for_expr g expr in
              (match item.kind with
               | Literal (LitInt literal) ->
                   (match parse_i64 literal with
                    | Some native ->
                        emit_call_void g ("@xi_arr_push_" ^ suffix ^ "_i64")
                          [ arg_ptr out; arg_i64 (Int64.to_string native) ]
                    | None ->
                        emit_call_void g ("@xi_arr_push_" ^ suffix)
                          [ arg_ptr out; arg_ptr (gen_expr_as_num g item) ])
               | _ ->
                   (match gen_expr g item with
                    | NativeInt (native, width, signed) when width <= 64 ->
                        emit_call_void g ("@xi_arr_push_" ^ suffix ^ "_i64")
                          [ arg_ptr out; arg_i64 (native_to_i64 g native width signed) ]
                    | boxed ->
                        emit_call_void g ("@xi_arr_push_" ^ suffix)
                          [ arg_ptr out; arg_ptr (ensure_num g boxed) ]))
          | ArrayBoolKind ->
              let value = gen_expr_as_bool g item in
              emit_call_void g "@xi_arr_push_bool" [ (arg_ptr out); (arg_i1 value) ]
          | ArrayStructKind ->
              let value = gen_expr_as_struct g item in
              emit_call_void g "@xi_arr_push_struct" [ (arg_ptr out); (arg_ptr value) ]
          | ArrayFnPtrKind ->
              let value = gen_expr_as_fn_ptr g item in
              emit_call_void g "@xi_arr_push_raw" [ (arg_ptr out); (arg_ptr value) ]
          | ArrayStrKind ->
              let value = gen_expr_as_str g item in
              emit_call_void g "@xi_arr_push_str" [ (arg_ptr out); (arg_ptr value) ])
        arr.items;
      Arr out
  | StructLiteral lit ->
      let out = emit_call_ptr g "@xi_struct_new" [] in
      let sig_ = assoc_opt lit.name g.sem.struct_sigs_by_name in
      Option.iter
        (fun sig_ ->
          List.iter
            (fun field ->
              let key = emit_cstr_ptr g field.sfs_name in
              let value = default_value_for_type g field.sfs_ty in
              emit_struct_set g out key value field.sfs_ty)
            sig_.ss_fields)
        sig_;
      List.iter
        (fun field ->
          let key = emit_cstr_ptr g field.name in
          let field_ty =
            match sig_ with
            | Some sig_ ->
                (match List.find_opt (fun f -> f.sfs_name = field.name) sig_.ss_fields with
                 | Some f -> f.sfs_ty
                 | None -> Error)
            | None -> Error
          in
          let value = ensure_value_for_type g (gen_expr g field.value) field_ty in
          emit_struct_set g out key value field_ty)
        lit.fields;
      StructV out
  | Closure closure -> FnPtrV (gen_closure_expr g closure)
  | Path path -> gen_path g path
  | Call call -> gen_call g call
  | Index index ->
      let base_ty = sem_type_of_expr g index.base in
      if is_array_sem_type base_ty then gen_array_get g index.base index.index false
      else if is_map_sem_type base_ty then gen_map_get g index.base index.index
      else Num (emit_num_from_i64 g 0)
  | Member member ->
      (match sem_type_of_expr g member.base with
       | CStruct cstruct_id -> gen_cstruct_field_get g member.base cstruct_id member.field
       | Struct struct_id ->
           let base = gen_expr_as_struct g member.base in
           let key = emit_cstr_ptr g member.field in
           let raw = emit_call_ptr g "@xi_struct_get" [ arg_ptr base; arg_ptr key ] in
           (match struct_field_type g struct_id member.field with
            | Some Str -> StrV raw
            | Some Cptr -> CptrV raw
            | Some ty when is_array_sem_type ty -> Arr raw
            | Some ty when is_map_sem_type ty -> MapV raw
            | Some (Struct _) -> StructV raw
            | Some (FnPtr _) -> FnPtrV raw
            | Some Bool ->
                let as_i64 = emit_call_i64 g "@xi_num_as_i64" [ arg_ptr raw ] in
                let b = new_temp g in
                emit_icmp g b "ne" "i64" as_i64 "0";
                BoolV b
            | Some (IntN w) -> NativeInt (unbox_native_int g raw w true, w, true)
            | Some (UintN w) -> NativeInt (unbox_native_int g raw w false, w, false)
            | Some (DecN w) when is_native_float_width w ->
                let d = emit_call_typed g "double" "@xi_num_as_f64" [ arg_ptr raw ] in
                NativeFloat (double_to_native_float g d w, w)
            | _ -> Num raw)
       | _ -> Num (emit_num_from_i64 g 0))
  | Unary unary -> gen_unary g unary
  | Binary binary -> gen_binary g binary
  | Cast cast -> gen_cast g cast
  | Try inner -> gen_try g inner
  | Grouping inner -> gen_expr g inner

and gen_path g path =
  match path.segments with
  | [ name ] ->
      (match lookup_adaptive_state g name with
       | Some state -> Num (emit_adaptive_state_num g state)
       | None ->
      match lookup_local_slot g name with
       | Some slot ->
           let local_ty = lookup_local_ty g name |> Option.value ~default:Int in
           (match local_native_int_info ~name g local_ty with
            | Some (width, signed) ->
                let tmp = new_temp g in
                emit_load g tmp (llvm_int_ty width) slot (native_align width);
                NativeInt (tmp, width, signed)
            | None ->
                (match native_float_width_of local_ty with
                 | Some width ->
                     let tmp = new_temp g in
                     emit_load g tmp (llvm_float_ty width) slot (max 1 (width / 8));
                     NativeFloat (tmp, width)
                 | None when local_ty = Bool ->
                     let tmp = new_temp g in
                     emit_load g tmp "i1" slot 1;
                     BoolV tmp
                 | None ->
                     let tmp = new_temp g in
                     emit_load g tmp "ptr" slot 8;
                     (match local_ty with
                      | Str -> StrV tmp
                      | Cptr -> CptrV tmp
                      | CStruct _ -> CStructV tmp
                      | ty when is_array_sem_type ty -> Arr tmp
                      | ty when is_map_sem_type ty -> MapV tmp
                      | Struct _ | Result _ | ResultUnknown -> StructV tmp
                      | FnPtr _ -> FnPtrV tmp
                      | _ -> Num tmp)))
       | None ->
           if has_function g name then FnPtrV (gen_named_function_closure g name)
           else Num (emit_num_from_i64 g 0))
  | [ module_; name ] ->
      (match module_const_opt g module_ name with
       | Some c -> gen_module_const g c
       | None ->
           let fn_key = module_ ^ "::" ^ name in
           if has_function g fn_key then FnPtrV (gen_named_function_closure g fn_key)
           else Num (emit_num_from_i64 g 0))
  | _ -> Num (emit_num_from_i64 g 0)

and gen_named_function_closure g name =
  let adapter = ensure_fn_adapter g name in
  emit_call_ptr g "@xi_closure_new"
    [ arg_ptr ("@" ^ adapter); arg_ptr "null"; arg_i64 "0" ]

and ensure_fn_adapter g name =
  match Hashtbl.find_opt g.fn_adapter_names name with
  | Some adapter -> adapter
  | None ->
      let adapter = "__xi_fn_adapter_" ^ sanitize_ident name in
      Hashtbl.add g.fn_adapter_names name adapter;
      (match function_sig_opt g name with
       | None -> ()
       | Some sig_ ->
           let typed_params =
             List.map
               (fun p -> (lir_ty (ir_return_ty p.ps_ty), "%" ^ sanitize_ident p.ps_name))
               sig_.fs_params
           in
           let params = (Lir.Ptr, "%env") :: typed_params in
           let ret = lir_ty (ir_return_ty sig_.fs_return_ty) in
           let call_args =
             List.map (fun (ty, name) -> (ty, Lir.Temp (Lir.temp name))) typed_params
           in
           let call_arg_exts =
             List.map
               (fun p -> if p.ps_binding = Ref then Lir.No_ext else c_abi_ext p.ps_ty)
               sig_.fs_params
           in
           let call_ret_ext = c_abi_ext sig_.fs_return_ty in
           let callee = Lir.Direct (sanitize_ident name) in
           let instrs, term =
             if ret = Lir.Void then
               ( [ Lir.Call
                     { dst = None
                     ; ret = Lir.Void
                     ; ret_ext = Lir.No_ext
                     ; callee
                     ; args = call_args
                     ; arg_exts = call_arg_exts
                     }
                 ]
               , Lir.Ret None )
             else
               ( [ Lir.Call
                     { dst = Some "%ret"
                     ; ret
                     ; ret_ext = call_ret_ext
                     ; callee
                     ; args = call_args
                     ; arg_exts = call_arg_exts
                     }
                 ]
               , Lir.Ret (Some (ret, Lir.Temp (Lir.temp "%ret"))) )
           in
           g.lir_generated_funcs <-
             { Lir.name = adapter
             ; ret
             ; ret_ext = Lir.No_ext
             ; params
             ; param_exts = Lir.no_exts (List.length params)
             ; blocks = [ { Lir.label = "entry"; instrs; term } ]
             }
             :: g.lir_generated_funcs);
      adapter

and gen_closure_expr g closure =
  let helper = "__xi_closure_" ^ string_of_int g.closure_counter in
  g.closure_counter <- g.closure_counter + 1;
  let ret = sem_type_from_typeref closure.return_ty g.sem |> ir_return_ty |> lir_ty in
  let params =
    (Lir.Ptr, "%env")
    :: List.map
         (fun (p : Ast.param) ->
           ( lir_ty (ir_return_ty (sem_type_from_typeref p.ty g.sem))
           , "%" ^ sanitize_ident p.name ))
         closure.params
  in
  let saved_return = g.fn_return_ty
  and saved_main = g.fn_is_entry_main
  and saved_fast_i64 = g.fn_uses_fast_i64_int
  and saved_fast_result_out = g.fn_fast_result_out
  and saved_bindings = g.local_scopes.bindings
  and saved_undo = g.local_scopes.undo
  and saved_lir = (g.lir_label, g.lir_instrs, g.lir_blocks, g.lir_entry_allocas) in
  lir_reset_function g;
  lir_open g "entry";
  g.fn_return_ty <- sem_type_from_typeref closure.return_ty g.sem;
  g.fn_is_entry_main <- false;
  g.fn_uses_fast_i64_int <- false;
  g.fn_fast_result_out <- None;
  g.local_scopes.bindings <- Hashtbl.create 64;
  g.local_scopes.undo <- [];
  push_scope g;
  List.iter
    (fun (p : Ast.param) ->
      let pname = sanitize_ident p.name in
      let ty = sem_type_from_typeref p.ty g.sem in
      match native_width_of ty, native_float_width_of ty, ty with
      | Some width, _, _ ->
          let slot = new_temp_named g (pname ^ ".addr") in
          emit_entry_alloca_native g slot width;
          emit_store g (llvm_int_ty width) ("%" ^ pname) slot (native_align width);
          declare_local g p.name slot p.binding ty
      | _, Some width, _ ->
          let slot = new_temp_named g (pname ^ ".addr") in
          emit_entry_alloca_float g slot width;
          emit_store g (llvm_float_ty width) ("%" ^ pname) slot (max 1 (width / 8));
          declare_local g p.name slot p.binding ty
      | _, _, Bool ->
          let slot = new_temp_named g (pname ^ ".addr") in
          emit_entry_alloca_bool g slot;
          emit_store g "i1" ("%" ^ pname) slot 1;
          declare_local g p.name slot p.binding ty
      | _ ->
          let slot = new_temp_named g (pname ^ ".addr") in
          emit_entry_alloca g slot;
          emit_store g "ptr" ("%" ^ pname) slot 8;
          declare_local g p.name slot p.binding ty)
    closure.params;
  let term = gen_stmt g closure.body in
  if not term then emit_default_return g;
  lir_finish_function ~generated:true g helper ret params;
  g.fn_return_ty <- saved_return;
  g.fn_is_entry_main <- saved_main;
  g.fn_uses_fast_i64_int <- saved_fast_i64;
  g.fn_fast_result_out <- saved_fast_result_out;
  g.local_scopes.bindings <- saved_bindings;
  g.local_scopes.undo <- saved_undo;
  (let label, instrs, blocks, allocas = saved_lir in
   g.lir_label <- label;
   g.lir_instrs <- instrs;
   g.lir_blocks <- blocks;
   g.lir_entry_allocas <- allocas);
  emit_call_ptr g "@xi_closure_new"
    [ arg_ptr ("@" ^ helper); arg_ptr "null"; arg_i64 "0" ]

and gen_module_const g konst =
  let ret_ty = extern_abi_ty konst.mcs_ty in
  if ret_ty = "void" then Num (emit_num_from_i64 g 0)
  else
    let tmp =
      emit_call_typed ~ret_ext:(c_abi_ext konst.mcs_ty)
        g ret_ty ("@" ^ konst.mcs_symbol) []
    in
    code_value_from_return tmp konst.mcs_ty

and code_value_from_return value = function
  | Str -> StrV value
  | Cptr -> CptrV value
  | CStruct _ -> CStructV value
  | ty when is_array_sem_type ty -> Arr value
  | ty when is_map_sem_type ty -> MapV value
  | Struct _ | Result _ | ResultUnknown -> StructV value
  | FnPtr _ -> FnPtrV value
  | IntN w -> NativeInt (value, w, true)
  | UintN w -> NativeInt (value, w, false)
  | DecN w when is_native_float_width w -> NativeFloat (value, w)
  | Bool -> BoolV value
  | _ -> Num value

and gen_module_call g sig_ call =
  let args =
    List.mapi
      (fun idx arg ->
        match List.nth_opt sig_.mfs_params idx with
        | Some param when param.ps_binding = Ref ->
            arg_ptr (expr_as_local_slot g arg |> Option.value ~default:"null")
        | Some param -> extern_arg g param.ps_ty arg
        | None -> extern_arg g Int arg)
      call.args
  in
  let arg_exts =
    List.mapi
      (fun idx _ ->
        match List.nth_opt sig_.mfs_params idx with
        | Some param when param.ps_binding <> Ref -> c_abi_ext param.ps_ty
        | _ -> Lir.No_ext)
      call.args
  in
  let ret_ty = extern_abi_ty sig_.mfs_return_ty in
  if ret_ty = "void" then begin
    emit_call_void ~arg_exts g ("@" ^ sig_.mfs_symbol) args;
    Num (emit_num_from_i64 g 0)
  end else
    let tmp =
      emit_call_typed ~ret_ext:(c_abi_ext sig_.mfs_return_ty) ~arg_exts
        g ret_ty ("@" ^ sig_.mfs_symbol) args
    in
    code_value_from_return tmp sig_.mfs_return_ty

and extern_arg g param_ty arg =
  match param_ty with
  | Bool -> arg_i1 (gen_expr_as_bool g arg)
  | IntN w -> arg_int w (gen_expr_as_native_int g arg w true)
  | UintN w -> arg_int w (gen_expr_as_native_int g arg w false)
  | DecN w when is_native_float_width w ->
      arg_float w (gen_expr_as_native_float g arg w)
  | Str -> arg_ptr (gen_expr_as_str g arg)
  | Cptr -> arg_ptr (gen_expr_as_cptr g arg)
  | CStruct _ -> arg_ptr (gen_expr_as_cstruct g arg)
  | ty when is_array_sem_type ty -> arg_ptr (gen_expr_as_arr g arg)
  | ty when is_map_sem_type ty -> arg_ptr (gen_expr_as_map g arg)
  | Struct _ | Result _ | ResultUnknown -> arg_ptr (gen_expr_as_struct g arg)
  | FnPtr _ -> arg_ptr (gen_expr_as_fn_ptr g arg)
  | Uint ->
      let value = ensure_value_for_type g (gen_expr g arg) Uint in
      arg_ptr value
  | _ -> arg_ptr (gen_expr_as_num g arg)

and fn_ptr_arg g param_ty arg =
  match param_ty with
  | IntN w -> arg_int w (gen_expr_as_native_int g arg w true)
  | UintN w -> arg_int w (gen_expr_as_native_int g arg w false)
  | DecN w when is_native_float_width w ->
      arg_float w (gen_expr_as_native_float g arg w)
  | Bool -> arg_i1 (gen_expr_as_bool g arg)
  | DecN w -> arg_ptr (gen_expr_as_softfloat g arg w)
  | Str -> arg_ptr (gen_expr_as_str g arg)
  | ty when is_array_sem_type ty -> arg_ptr (gen_expr_as_arr g arg)
  | ty when is_map_sem_type ty -> arg_ptr (gen_expr_as_map g arg)
  | Struct _ | Result _ | ResultUnknown -> arg_ptr (gen_expr_as_struct g arg)
  | FnPtr _ -> arg_ptr (gen_expr_as_fn_ptr g arg)
  | Uint -> arg_ptr (ensure_value_for_type g (gen_expr g arg) Uint)
  | _ -> arg_ptr (gen_expr_as_num g arg)

and gen_fn_ptr_call g fn_ty callee args =
  match fn_ty with
  | FnPtr sig_id ->
      (match List.nth_opt g.sem.fn_ptr_sigs sig_id with
       | None -> Num (emit_num_from_i64 g 0)
       | Some sig_ ->
           let closure_ptr = gen_expr_as_fn_ptr g callee in
           let code = emit_call_ptr g "@xi_closure_code" [ arg_ptr closure_ptr ] in
           let env = emit_call_ptr g "@xi_closure_env" [ arg_ptr closure_ptr ] in
           let args_ir =
             arg_ptr env
             :: List.mapi
                  (fun idx arg ->
                    let param_ty =
                      List.nth_opt sig_.fps_params idx |> Option.value ~default:Int
                    in
                    fn_ptr_arg g param_ty arg)
                  args
           in
           let ret_ty = ir_return_ty sig_.fps_return_ty in
           if ret_ty = "void" then begin
             emit_call_void g code args_ir;
             Num (emit_num_from_i64 g 0)
           end
           else code_value_from_return (emit_call_typed g ret_ty code args_ir) sig_.fps_return_ty)
  | _ -> Num (emit_num_from_i64 g 0)

and gen_call g call =
  match indirect_call_callee_type g call.callee with
  | Some fn_ty -> gen_fn_ptr_call g fn_ty call.callee call.args
  | None ->
  match path_segments call.callee with
  | Some [ "type" ] ->
      (match call.args with
       | arg :: _ -> StrV (emit_cstr_ptr g (xi_type_name g.sem (sem_type_of_expr g arg)))
       | [] -> StrV "null")
  | Some [ "size" ] ->
      (match call.args with
       | arg :: _ ->
           (match fixed_numeric_bit_width (sem_type_of_expr g arg) with
            | Some width -> Num (emit_num_from_i64 g width)
            | None ->
                let signed = if is_unsigned_int (sem_type_of_expr g arg) then 0 else 1 in
                let value = gen_expr_as_num g arg in
                Num
                  (emit_call_ptr g "@xi_num_size_bits"
                     [ arg_ptr value; arg_i1 (string_of_int signed) ]))
       | [] -> Num (emit_num_from_i64 g 0))
  | Some [ "copy" ] when not (has_function g "copy") ->
      (match call.args with
       | [ arg ] ->
           let value = gen_expr g arg in
           (match value with
            | Arr v -> Arr (emit_call_ptr g "@xi_arr_copy_deep" [ arg_ptr v ])
            | MapV v -> MapV (emit_call_ptr g "@xi_map_copy_deep" [ arg_ptr v ])
            | StructV v -> StructV (emit_call_ptr g "@xi_struct_copy_deep" [ arg_ptr v ])
            | other -> other)
       | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "cast" ] ->
      (match call.args with
       | value :: width_expr :: _ ->
           let width = cast_width_literal width_expr in
           (match width,
                  cast_result_type (sem_type_of_expr g value)
                    (Option.value ~default:0 width)
            with
            | Some _, Some (IntN w) -> NativeInt (gen_expr_as_native_int g value w true, w, true)
            | Some _, Some (UintN w) -> NativeInt (gen_expr_as_native_int g value w false, w, false)
            | Some _, Some (DecN w) when is_native_float_width w -> NativeFloat (gen_expr_as_native_float g value w, w)
            | Some _, Some (DecN w) -> Num (gen_expr_as_softfloat g value w)
            | _ -> Num (emit_num_from_i64 g 0))
       | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "res"; "ok" ] ->
      (match call.args with
       | [ value_expr ] ->
           let value = gen_expr g value_expr in
           let payload_ty = storage_ty_of_code_value value (sem_type_of_expr g value_expr) in
           let out = emit_call_ptr g "@xi_struct_new" [] in
           emit_result_fields g out "true" (ensure_value_for_type g value payload_ty) payload_ty (emit_cstr_ptr g "");
           StructV out
       | _ -> StructV (emit_call_ptr g "@xi_struct_new" []))
  | Some [ "res"; "err" ] ->
      (match call.args with
       | [ message ] ->
           let out = emit_call_ptr g "@xi_struct_new" [] in
           emit_result_fields g out "false" (default_value_for_type g Int) Int (gen_expr_as_str g message);
           StructV out
       | _ -> StructV (emit_call_ptr g "@xi_struct_new" []))
  | Some [ "res"; "is_ok" ] ->
      (match call.args with
       | [ r ] -> BoolV (gen_result_ok_bit g r)
       | _ -> BoolV "false")
  | Some [ "res"; "is_err" ] ->
      (match call.args with
       | [ r ] ->
           let ok = gen_result_ok_bit g r in
           let flipped = new_temp g in
           emit_bin g flipped "xor" "i1" ok "true";
           BoolV flipped
       | _ -> BoolV "false")
  | Some [ "res"; "error" ] ->
      (match call.args with
       | [ r ] -> StrV (gen_result_field_raw g r "error")
       | _ -> StrV (emit_cstr_ptr g ""))
  | Some [ "res"; "value" ] ->
      (match call.args with
       | [ r ] ->
           let payload_ty = match sem_type_of_expr g r with Result payload -> payload | _ -> Int in
           decode_boxed_value g (gen_result_field_raw g r "value") payload_ty
       | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "res"; "or" ] ->
      (match call.args with
       | [ r; fallback ] ->
           let payload_ty =
             match sem_type_of_expr g r with
             | Result payload when payload <> NumUnknown -> payload
             | _ -> sem_type_of_expr g fallback
           in
           let result_slot = new_temp_named g "res.or.result.addr" in
           emit_entry_alloca g result_slot;
           let result_value = gen_expr_as_struct g r in
           emit_store g "ptr" result_value result_slot 8;

           let slot = new_temp_named g "res.or.addr" in
           emit_entry_alloca g slot;
           emit_store g "ptr" (ensure_value_for_type g (gen_expr g fallback) payload_ty) slot 8;

           let ok_raw = emit_call_ptr g "@xi_struct_get" [ arg_ptr result_value; arg_ptr (emit_cstr_ptr g "ok") ] in
           let ok_i64 = emit_call_i64 g "@xi_num_as_i64" [ arg_ptr ok_raw ] in
           let ok = new_temp g in
           emit_icmp g ok "ne" "i64" ok_i64 "0";

           let then_label = new_label g "res.or.ok" in
           let end_label = new_label g "res.or.end" in
           emit_br_cond g ok then_label end_label;
           emit_label g then_label;
           let held = new_temp g in
           emit_load g held "ptr" result_slot 8;
           let value_raw = emit_call_ptr g "@xi_struct_get" [ arg_ptr held; arg_ptr (emit_cstr_ptr g "value") ] in
           emit_store g "ptr" value_raw slot 8;
           emit_br g end_label;
           emit_label g end_label;
           let raw = new_temp g in
           emit_load g raw "ptr" slot 8;
           decode_boxed_value g raw payload_ty
       | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "arr"; "new" ] -> Arr (emit_call_ptr g "@xi_arr_new" [])
  | Some [ "arr"; "push" ] ->
      (match call.args with arr :: value :: _ -> gen_array_push g arr value | _ -> ());
      Num (emit_num_from_i64 g 0)
  | Some [ "arr"; "pop" ] ->
      (match call.args with arr :: _ -> gen_array_pop g arr | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "arr"; "insert" ] ->
      (match call.args with arr :: idx :: value :: _ -> gen_array_insert g arr idx value | _ -> ());
      Num (emit_num_from_i64 g 0)
  | Some [ "arr"; "remove" ] ->
      (match call.args with arr :: idx :: _ -> gen_array_remove g arr idx | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "arr"; "clear" ] ->
      (match call.args with arr :: _ -> emit_call_void g "@xi_arr_clear" [ (arg_ptr (gen_expr_as_arr g arr)) ] | _ -> ());
      Num (emit_num_from_i64 g 0)
  | Some [ "arr"; "contains" ] ->
      (match call.args with arr :: value :: _ -> gen_array_contains g arr value | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "arr"; "index_of" ] ->
      (match call.args with arr :: value :: _ -> gen_array_index_of g arr value | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "arr"; "len" ] ->
      (match call.args with arr :: _ -> NativeInt (emit_call_i64 g "@xi_arr_len_i64" [ arg_ptr (gen_expr_as_arr g arr) ], 64, true) | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "arr"; "get" ] ->
      (match call.args with arr :: idx :: _ -> gen_array_get g arr idx false | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "arr"; "get_unchecked" ] ->
      (match call.args with arr :: idx :: _ -> gen_array_get g arr idx true | _ -> NativeInt ("0", 64, true))
  | Some [ "arr"; "set" ] | Some [ "arr"; "set_unchecked" ] ->
      let unchecked = path_is call.callee [ "arr"; "set_unchecked" ] in
      (match call.args with arr :: idx :: value :: _ -> gen_array_set g arr idx value unchecked | _ -> ());
      Num (emit_num_from_i64 g 0)
  | Some [ "arr"; "slice" ] ->
      (match call.args with arr :: start :: len :: _ -> Arr (gen_array_slice g arr start (Some len)) | _ -> Arr "null")
  | Some [ "arr"; "slice_from" ] ->
      (match call.args with arr :: start :: _ -> Arr (gen_array_slice g arr start None) | _ -> Arr "null")
  | Some [ "__index"; "set" ] ->
      (match call.args with
       | base :: idx :: value :: _ ->
           if is_array_sem_type (sem_type_of_expr g base) then begin
             (* The parser represents [a[i] op= k] as
                [__index::set(a, i, a[i] op k)] and deliberately shares the
                target AST nodes. Fuse the common adaptive-int/literal case
                into one exact runtime operation. Physical identity, or the
                same two pure paths, keeps this from changing evaluation count
                for effectful hand-written target expressions. *)
             let fused =
               match sem_type_of_expr g base, value.kind with
               | ArrayInt None,
                 Binary { op = Add;
                          lhs = { kind = Index old; _ };
                          rhs = { kind = Binary {
                            op = Mul;
                            lhs = { kind = Index source; _ };
                            rhs = { kind = Literal (LitInt scale_text); _ }
                          }; _ } }
                 when (old.base == base
                       || (match old.base.kind, base.kind with
                           | Path a, Path b -> a.segments = b.segments
                           | _ -> false))
                      && (old.index == idx || expr_same_pure_index old.index idx)
                      && (match source.base.kind, base.kind with
                          | Path a, Path b -> a.segments = b.segments
                          | _ -> false)
                      && Option.is_some (expr_as_local_name base)
                      && expr_is_pure_native_index idx
                      && expr_is_pure_native_index source.index ->
                   (match parse_i64 scale_text with
                    | Some scale ->
                        let arrv = gen_expr_as_arr g base in
                        let dstv = gen_expr_as_native_int g idx 64 true in
                        let srcv = gen_expr_as_native_int g source.index 64 true in
                        emit_call_void g "@xi_arr_add_scaled_num_i64_at_i64"
                          [ arg_ptr arrv; arg_i64 dstv; arg_i64 srcv;
                            arg_i64 (Int64.to_string scale) ];
                        true
                    | None -> false)
               | ArrayInt None,
                 Binary { op = (Add | Sub | Mul as op);
                          lhs = { kind = Index old; _ };
                          rhs = { kind = Literal (LitInt text); _ } }
                 when (old.base == base
                       || (match old.base.kind, base.kind with
                           | Path a, Path b -> a.segments = b.segments
                           | _ -> false))
                      && (old.index == idx
                          || (match old.index.kind, idx.kind with
                              | Path a, Path b -> a.segments = b.segments
                              | _ -> false)) ->
                   (match parse_i64 text with
                    | Some rhs ->
                        let arrv = gen_expr_as_arr g base in
                        let idxv = gen_expr_as_native_int g idx 64 true in
                        let name =
                          match op with
                          | Add -> "@xi_arr_add_num_i64_at_i64"
                          | Sub -> "@xi_arr_sub_num_i64_at_i64"
                          | Mul -> "@xi_arr_mul_num_i64_at_i64"
                          | _ -> assert false
                        in
                        let lir_op =
                          match op with
                          | Add -> Lir.Add | Sub -> Lir.Sub | Mul -> Lir.Mul
                          | _ -> assert false
                        in
                        if g.inline_guarded_packed_ops then
                          emit_guarded_packed_update g arrv idxv
                            (Int64.to_string rhs) lir_op name
                        else
                          emit_call_void g name
                            [ arg_ptr arrv; arg_i64 idxv;
                              arg_i64 (Int64.to_string rhs) ];
                        true
                    | None -> false)
               | _ -> false
             in
             if not fused then gen_array_set g base idx value false
           end
           else gen_map_set g base idx value
       | _ -> ());
      (match g.discarded_call with
       | Some discarded when discarded == call -> Num "null"
       | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "__member"; "set" ] ->
      (match call.args with
       | base :: field :: value :: _ -> gen_member_set g base field value
       | _ -> ());
      Num (emit_num_from_i64 g 0)
  | Some [ "map"; "new" ] -> MapV (emit_call_ptr g "@xi_map_new" [])
  | Some [ "map"; "set" ] ->
      (match call.args with map :: key :: value :: _ -> gen_map_set g map key value | _ -> ());
      Num (emit_num_from_i64 g 0)
  | Some [ "map"; "get" ] ->
      (match call.args with map :: key :: _ -> gen_map_get g map key | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "map"; "get_or" ] ->
      (match call.args with map :: key :: fallback :: _ -> gen_map_get_or g map key fallback | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "map"; "has" ] ->
      (match call.args with
       | map :: key :: _ ->
           let mapv = gen_expr_as_map g map in
           let keyv = gen_expr_as_map_key g map key in
           Num (emit_call_ptr g "@xi_map_has" [ arg_ptr mapv; arg_ptr keyv ])
       | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "map"; "del" ] ->
      (match call.args with
       | map :: key :: _ ->
           let mapv = gen_expr_as_map g map in
           let keyv = gen_expr_as_map_key g map key in
           emit_call_void g "@xi_map_del" [ (arg_ptr mapv); (arg_ptr keyv) ]
       | _ -> ());
      Num (emit_num_from_i64 g 0)
  | Some [ "map"; "clear" ] ->
      (match call.args with map :: _ -> emit_call_void g "@xi_map_clear" [ (arg_ptr (gen_expr_as_map g map)) ] | _ -> ());
      Num (emit_num_from_i64 g 0)
  | Some [ "map"; "len" ] ->
      (match call.args with map :: _ -> Num (emit_call_ptr g "@xi_map_len" [ arg_ptr (gen_expr_as_map g map) ]) | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "map"; "keys" ] ->
      (match call.args with
       | map :: _ ->
           let func =
             match sem_type_of_expr g map with
             | MapKeyed (KeyInt, _) -> "@xi_map_keys_num"
             | MapKeyed (KeyDec, _) -> "@xi_map_keys_dec"
             | MapKeyed (KeyBool, _) -> "@xi_map_keys_bool"
             | _ -> "@xi_map_keys"
           in
           Arr (emit_call_ptr g func [ arg_ptr (gen_expr_as_map g map) ])
       | _ -> Arr "null")
  | Some [ "map"; "values" ] ->
      (match call.args with map :: _ -> Arr (gen_map_values g map) | _ -> Arr "null")
  | Some [ "mem"; ("copy" | "set" | "cmp" | "read_be" | "read_le" | "write_be" | "write_le") ] ->
      gen_mem_call g call
  | Some [ "gc"; "collect" ] ->
      emit_call_void g "@xi_gc_collect" [];
      Num (emit_num_from_i64 g 0)
  | Some [ "gc"; "collections" ] ->
      Num (box_native_int g (emit_call_i64 g "@xi_gc_collection_count" []) 64 true)
  | Some [ "gc"; "live" ] ->
      Num (box_native_int g (emit_call_i64 g "@xi_gc_live_count" []) 64 true)
  | Some [ "str"; "len" ] ->
      (match call.args with str :: _ -> NativeInt (emit_call_i64 g "@xi_str_len_i64" [ arg_ptr (gen_expr_as_str g str) ], 64, true) | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "str"; "char_code_at" ] ->
      (match call.args with
       | str :: idx :: _ ->
           let strv = gen_expr_as_str g str in
           let idxv = gen_expr_as_native_int g idx 64 true in
           NativeInt (emit_call_i64 g "@xi_str_char_code_at_i64" [ arg_ptr strv; arg_i64 idxv ], 64, true)
       | _ -> Num (emit_num_from_i64 g 0))
  | Some [ "str"; "from_char_code" ] ->
      (match call.args with code :: _ -> StrV (emit_call_ptr g "@xi_str_from_char_code_i64" [ arg_i64 (gen_expr_as_native_int g code 64 true) ]) | _ -> StrV "null")
  | Some [ "cptr"; op ] -> gen_cptr_call g op call.args
  | Some [ "cstruct"; op ] -> gen_cstruct_call g op call.args
  | Some [ "cfn"; "ptr" ] ->
      (match call.args with
       | [ { kind = Path { segments = [ name ]; _ }; _ } ] ->
           CptrV ("@" ^ sanitize_ident name)
       | _ -> CptrV "null")
  | Some [ module_; name ] ->
      let fn_key = module_ ^ "::" ^ name in
      (match function_sig_opt g fn_key with
       | Some _ ->
           let direct_call = { call with callee = { kind = Path { segments = [ fn_key ]; span = call.callee.span }; span = call.callee.span } } in
           gen_call g direct_call
       | None ->
           (match module_fn_opt g module_ name with
            | Some sig_ -> gen_module_call g sig_ call
            | None -> Num (emit_num_from_i64 g 0)))
  | Some [ name ] ->
      (match function_sig_opt g name with
       | Some _ -> gen_direct_call g name call.args
       | None ->
           (match module_fn_opt g "" name with
            | Some sig_ -> gen_module_call g sig_ call
            | None -> gen_direct_call g name call.args))
  | _ -> Num (emit_num_from_i64 g 0)

and gen_direct_call g fname args =
  let sig_ = function_sig_opt g fname in
  let callee_fast =
    g.fn_uses_fast_i64_int && StringSet.mem fname g.fast_i64_int_functions
  in
  let args_ir =
    List.mapi
      (fun idx arg ->
        match sig_ with
        | Some sig_ ->
            let param = List.nth_opt sig_.fs_params idx in
            let param_ty = Option.map (fun p -> p.ps_ty) param |> Option.value ~default:Int in
            let is_ref = match param with Some p -> p.ps_binding = Ref | None -> false in
            if is_ref then arg_ptr (expr_as_local_slot g arg |> Option.value ~default:"null")
            else if callee_fast && param_ty = Int then arg_i64 (gen_expr_as_native_int g arg 64 true)
            else extern_arg g param_ty arg
        | None -> arg_ptr (gen_expr_as_num g arg))
      args
  in
  let arg_exts =
    List.mapi
      (fun idx _ ->
        match sig_ with
        | Some sig_ -> (
            match List.nth_opt sig_.fs_params idx with
            | Some p when p.ps_binding <> Ref
                         && not (callee_fast && p.ps_ty = Int) ->
                c_abi_ext p.ps_ty
            | _ -> Lir.No_ext)
        | None -> Lir.No_ext)
      args
  in
  let ret_sem_ty = Option.map (fun s -> s.fs_return_ty) sig_ |> Option.value ~default:Int in
  let ret_ty = ir_return_ty ret_sem_ty in
  if callee_fast && ret_sem_ty = Int then begin
    let result_slot = new_temp_named g (sanitize_ident fname ^ ".fast.result") in
    emit_entry_alloca_native g result_slot 64;
    let fast_symbol = "@__xi_i64_" ^ sanitize_ident fname in
    let ok =
      emit_call_typed ~arg_exts:(arg_exts @ [ Lir.No_ext ]) g "i1" fast_symbol
        (args_ir @ [ arg_ptr result_slot ])
    in
    let success_label = new_label g "i64.call.success" in
    let overflow_label =
      Option.value g.fn_i64_overflow_label ~default:success_label
    in
    emit_br_cond g ok success_label overflow_label;
    emit_label g success_label;
    let result = new_temp g in
    emit_load g result "i64" result_slot 8;
    NativeInt (result, 64, true)
  end else if ret_ty = "void" then begin
    emit_call_void ~arg_exts g ("@" ^ sanitize_ident fname) args_ir;
    Num (emit_num_from_i64 g 0)
  end else
    let ret_ext = c_abi_ext ret_sem_ty in
    let tmp =
      emit_call_typed ~ret_ext ~arg_exts g ret_ty
        ("@" ^ sanitize_ident fname) args_ir
    in
    match ret_sem_ty with
    | _ -> code_value_from_return tmp ret_sem_ty

and expr_as_local_slot g expr = Option.bind (expr_as_local_name expr) (lookup_local_slot g)

and gen_expr_as_num g expr = ensure_num g (gen_expr g expr)
and gen_expr_as_bool g expr = ensure_bool g (gen_expr g expr)
and gen_expr_as_str g expr = ensure_str (gen_expr g expr)
and gen_expr_as_arr g expr = ensure_arr (gen_expr g expr)
and gen_expr_as_map g expr = ensure_map (gen_expr g expr)
and gen_expr_as_struct g expr = ensure_struct (gen_expr g expr)
and gen_expr_as_fn_ptr g expr = ensure_fn_ptr (gen_expr g expr)
and gen_expr_as_cptr g expr = ensure_cptr (gen_expr g expr)
and gen_expr_as_cstruct g expr = ensure_cstruct (gen_expr g expr)

and gen_expr_as_map_key g map_expr key_expr =
  match map_key_runtime_kind_for_expr g map_expr with
  | MapKeyStr -> gen_expr_as_str g key_expr
  | MapKeyNum ->
      let key = gen_expr_as_num g key_expr in
      emit_call_ptr g "@xi_map_key_num" [ arg_ptr key ]
  | MapKeyBool ->
      let key = gen_expr_as_bool g key_expr in
      emit_call_ptr g "@xi_map_key_bool" [ arg_i1 key ]

and gen_expr_as_native_int g expr width signed =
  match expr.kind with
  | Index index
    when width = 64 && signed
         && array_runtime_kind_for_expr g index.base = ArrayNum
         && array_num_runtime_suffix_for_expr g index.base = "num" ->
      let arrv = gen_expr_as_arr g index.base in
      let idxv = gen_expr_as_native_int g index.index 64 true in
      if g.inline_guarded_packed_ops then emit_guarded_packed_read_i64 g arrv idxv
      else emit_call_i64 g "@xi_arr_get_num_i64_at_i64" [ arg_ptr arrv; arg_i64 idxv ]
  | Literal (LitInt v) ->
      let boxed () = unbox_native_int g (gen_expr_as_num g expr) width signed in
      (match parse_i64 v with
       | Some parsed ->
           let fits =
             if width >= 64 then true
             else if signed then
               let max = Int64.sub (Int64.shift_left 1L (width - 1)) 1L in
               let min = Int64.neg (Int64.shift_left 1L (width - 1)) in
               Int64.compare parsed min >= 0 && Int64.compare parsed max <= 0
             else
               let umax = Int64.sub (Int64.shift_left 1L width) 1L in
               Int64.compare parsed 0L >= 0 && Int64.compare parsed umax <= 0
           in
           if fits then Int64.to_string parsed else boxed ()
       | None -> if width >= 128 && Fold.literal_fits_i128 v then v else boxed ())
  | _ ->
      (match gen_expr g expr with
       | NativeInt (v, w, src_signed) -> coerce_native_width g v w width src_signed
       | other -> unbox_native_int g (ensure_num g other) width signed)

and gen_expr_as_native_float g expr width =
  match expr.kind with
  | (Literal (LitDec v) | Literal (LitInt v)) when width = 16 ->
      let tmp = new_temp g in
      emit_conv g tmp "fptrunc" "double" (llvm_float_const 64 v) "half";
      tmp
  | Literal (LitDec v) | Literal (LitInt v) -> llvm_float_const width v
  | _ ->
      (match gen_expr g expr with
       | NativeFloat (v, w) -> coerce_native_float_width g v w width
       | NativeInt (v, w, src_signed) ->
           let tmp = new_temp g in
           emit_conv g tmp (if src_signed then "sitofp" else "uitofp") (llvm_int_ty w) v (llvm_float_ty width);
           tmp
       | other ->
           let boxed = ensure_num g other in
           let d = emit_call_typed g "double" "@xi_num_as_f64" [ arg_ptr boxed ] in
           double_to_native_float g d width)

and gen_expr_as_softfloat g expr width =
  match expr.kind with
  | Literal (LitDec v) | Literal (LitInt v) ->
      let s = emit_cstr_ptr g v in
      emit_call_ptr g "@xi_softfloat_from_decimal" [ arg_ptr s; arg_i32 (string_of_int width) ]
  | _ ->
      let boxed = gen_expr_as_num g expr in
      emit_call_ptr g "@xi_softfloat_coerce" [ arg_ptr boxed; arg_i32 (string_of_int width) ]

and gen_unary g unary =
  match unary.op with
  | Neg ->
      (match sem_type_of_expr g unary.rhs, g.fn_i64_overflow_label with
       | Int, Some overflow_label when g.fn_uses_fast_i64_int ->
           let rhs = gen_expr_as_native_int g unary.rhs 64 true in
           NativeInt (emit_checked_i64_neg g rhs overflow_label, 64, true)
       | _ ->
           let rhs = gen_expr_as_num g unary.rhs in
           let zero = emit_num_from_i64 g 0 in
           Num (emit_call_ptr g "@xi_num_sub" [ arg_ptr zero; arg_ptr rhs ]))
  | Not ->
      let rhs = gen_expr_as_bool g unary.rhs in
      let tmp = new_temp g in
      emit_bin g tmp "xor" "i1" rhs "true";
      BoolV tmp
  | BitNot ->
      let native =
        match sem_type_of_expr g unary.rhs with
        | IntN w -> Some (w, true)
        | UintN w -> Some (w, false)
        | Int when g.fn_uses_fast_i64_int -> Some (64, true)
        | _ -> None
      in
      (match native with
       | Some (width, signed) ->
           let v = gen_expr_as_native_int g unary.rhs width signed in
           let tmp = new_temp g in
           emit_bin g tmp "xor" (llvm_int_ty width) v "-1";
           NativeInt (tmp, width, signed)
       | None ->
           let rhs = gen_expr_as_num g unary.rhs in
           Num (emit_call_ptr g "@xi_num_bnot" [ arg_ptr rhs ]))

and native_binary_width g lhs rhs =
  let classify expr =
    match expr.kind with
    | Literal (LitInt _) -> Some None
    | _ ->
        (match sem_type_of_expr g expr with
         | Int when g.fn_uses_fast_i64_int -> Some (Some (64, true))
         | IntN w -> Some (Some (w, true))
         | UintN w -> Some (Some (w, false))
         | Int | NumUnknown ->
             (match expr.kind with
              | Binary binary ->
                  (match native_binary_width g binary.lhs binary.rhs with
                   | Some width -> Some (Some width)
                   | None -> Some None)
              | _ -> Some None)
         | _ -> None)
  in
  match classify lhs, classify rhs with
  | Some lw, Some rw ->
      (match lw, rw with
       | Some (a, sa), Some (b, sb) -> Some (max a b, sa && sb)
       | Some x, None | None, Some x -> Some x
       | None, None -> None)
  | _ -> None

(* Some (Some w) = a hardware-sized decimal; Some None = a literal or integer
   operand that adapts into whatever float width the other side brings; None =
   not float-eligible at all.

   Requiring `DecN` on *both* sides -- which is what this did before -- meant
   `x > 0.0` on a `dec16` fell through to the boxed path entirely, because a
   decimal literal is `Dec`, not `DecN`. One sized operand is enough. *)
and native_float_binary_width g lhs rhs =
  let classify (expr : Ast.expr) =
    match expr.kind with
    | Literal (LitInt _) | Literal (LitDec _) -> Some None
    | _ ->
        (match sem_type_of_expr g expr with
         | DecN w when is_native_float_width w -> Some (Some w)
         | Dec ->
             (match expr.kind with
              | Binary binary ->
                  (match native_float_binary_width g binary.lhs binary.rhs with
                   | Some width -> Some (Some width)
                   | None -> Some None)
              | _ -> Some None)
         | IntN _ | Int | NumUnknown -> Some None
         | _ -> None)
  in
  match classify lhs, classify rhs with
  | Some lw, Some rw ->
      (match lw, rw with
       | Some a, Some b -> Some (max a b)
       | Some x, None | None, Some x -> Some x
       | None, None -> None)
  | _ -> None

and gen_binary g binary =
  match binary.lhs.kind, binary.rhs.kind, binary.op with
  | Index index, Literal (LitInt text), (Eq | Ne | Lt | Le | Gt | Ge as op)
    when sem_type_of_expr g index.base = ArrayInt None ->
      (match parse_i64 text with
       | Some rhs ->
           let arrv = gen_expr_as_arr g index.base in
           let idxv = gen_expr_as_native_int g index.index 64 true in
           let callee =
             match op with
             | Eq -> "@xi_arr_eq_num_i64_at_i64"
             | Ne -> "@xi_arr_ne_num_i64_at_i64"
             | Lt -> "@xi_arr_lt_num_i64_at_i64"
             | Le -> "@xi_arr_le_num_i64_at_i64"
             | Gt -> "@xi_arr_gt_num_i64_at_i64"
             | Ge -> "@xi_arr_ge_num_i64_at_i64"
             | _ -> assert false
           in
           let pred =
             match op with
             | Eq -> "eq" | Ne -> "ne" | Lt -> "slt" | Le -> "sle"
             | Gt -> "sgt" | Ge -> "sge" | _ -> assert false
           in
           BoolV
             (if g.inline_guarded_packed_ops then
                emit_guarded_packed_cmp g arrv idxv
                  (Int64.to_string rhs) pred callee
              else
                emit_call_i1 g callee
                  [ arg_ptr arrv; arg_i64 idxv; arg_i64 (Int64.to_string rhs) ])
       | None -> gen_binary_general g binary)
  | _ -> gen_binary_general g binary

and gen_binary_general g binary =
  let rec effect_i64_range expr =
    match expr.kind with
    | Path { segments = [ name ]; _ } -> Hashtbl.find_opt g.effect_i64_bounds name
    | Literal (LitInt value) -> Option.map (fun n -> n, n) (parse_i64 value)
    | Grouping inner -> effect_i64_range inner
    | Binary nested -> effect_i64_binary_range nested
    | _ -> None
  and effect_i64_binary_range nested =
    match effect_i64_range nested.lhs, effect_i64_range nested.rhs with
    | Some (alo, ahi), Some (blo, bhi) ->
        let nonnegative =
          Int64.compare alo 0L >= 0 && Int64.compare blo 0L >= 0
        in
        (match nested.op with
         | Add when nonnegative
                    && Int64.compare ahi (Int64.sub Int64.max_int bhi) <= 0 ->
             Some (Int64.add alo blo, Int64.add ahi bhi)
         | Sub when nonnegative ->
             Some (Int64.sub alo bhi, Int64.sub ahi blo)
         | Mul when nonnegative
                    && (bhi = 0L
                        || Int64.compare ahi (Int64.div Int64.max_int bhi) <= 0) ->
             Some (Int64.mul alo blo, Int64.mul ahi bhi)
         | BitAnd | BitOr | BitXor when nonnegative ->
             Some (0L, Int64.max_int)
         | _ -> None)
    | _ -> None
  in
  let effect_i64_add_one =
    binary.op = Add
    && ((Option.fold ~none:false
           ~some:(fun name ->
             StringSet.mem name g.effect_i64_locals
             && StringSet.mem name g.proven_i64_add_one)
           (expr_as_local_name binary.lhs)
         && expr_is_one binary.rhs)
        || (Option.fold ~none:false
              ~some:(fun name ->
                StringSet.mem name g.effect_i64_locals
                && StringSet.mem name g.proven_i64_add_one)
              (expr_as_local_name binary.rhs)
            && expr_is_one binary.lhs))
  in
  let effect_i64_constant_update =
    (binary.op = Add || binary.op = Sub)
    && Option.fold ~none:false
         ~some:(fun name -> StringSet.mem name g.effect_i64_locals)
         (expr_as_local_name binary.lhs)
    && (match binary.rhs.kind with
        | Literal (LitInt text) -> Option.is_some (parse_i64 text)
        | _ -> false)
  in
  let effect_i64_width =
    match binary.op with
    | Lt | Le | Gt | Ge | Eq | Ne
      when Option.is_some (effect_i64_range binary.lhs)
           && Option.is_some (effect_i64_range binary.rhs) ->
        Some (64, true)
    | Add when effect_i64_add_one -> Some (64, true)
    | Add | Sub when effect_i64_constant_update -> Some (64, true)
    | Add | Sub | Mul | BitAnd | BitOr | BitXor
      when Option.is_some (effect_i64_binary_range binary) ->
        Some (64, true)
    | _ -> None
  in
  match Option.fold ~none:(native_binary_width g binary.lhs binary.rhs)
          ~some:(fun width -> Some width) effect_i64_width with
  | Some (width, signed) ->
      let arith =
        match binary.op with
        | Add -> Some "add" | Sub -> Some "sub" | Mul -> Some "mul"
        | Div -> Some (if signed then "sdiv" else "udiv")
        | Mod -> Some (if signed then "srem" else "urem")
        | Pow when g.fn_uses_fast_i64_int
                   && sem_type_of_expr g binary.lhs = Int
                   && sem_type_of_expr g binary.rhs = Int -> Some "mul"
        | BitAnd -> Some "and" | BitOr -> Some "or" | BitXor -> Some "xor"
        | _ -> None
      in
      (match arith with
       | Some op ->
           let lhs = gen_expr_as_native_int g binary.lhs width signed in
           let rhs = gen_expr_as_native_int g binary.rhs width signed in
           (match binary.op, width, signed, g.fn_i64_overflow_label,
                  sem_type_of_expr g binary.lhs, sem_type_of_expr g binary.rhs with
            | (Add | Sub | Mul), 64, true, Some overflow_label, Int, Int
              when g.fn_uses_fast_i64_int ->
                let proven_add_one =
                  binary.op = Add
                  && ((match binary.lhs.kind with
                       | Path { segments = [ name ]; _ } ->
                           StringSet.mem name g.proven_i64_add_one
                           && expr_is_one binary.rhs
                       | _ -> false)
                      || (match binary.rhs.kind with
                          | Path { segments = [ name ]; _ } ->
                              StringSet.mem name g.proven_i64_add_one
                              && expr_is_one binary.lhs
                          | _ -> false))
                in
                if binary.op = Mul then
                  NativeInt
                    (emit_checked_i64_mul g lhs rhs overflow_label, 64, true)
                else if proven_add_one then begin
                  let result = new_temp g in
                  emit_bin g result "add" "i64" lhs rhs;
                  NativeInt (result, 64, true)
                end
                else
                  let runtime_fn =
                    if binary.op = Add then "@xi_i64_add_checked"
                    else "@xi_i64_sub_checked"
                  in
                  NativeInt
                    (emit_checked_i64_binary g runtime_fn lhs rhs overflow_label,
                     64, true)
            | Mod, 64, true, Some _, Int, Int when g.fn_uses_fast_i64_int ->
                NativeInt (emit_safe_i64_mod g lhs rhs, 64, true)
            | Pow, 64, true, Some overflow_label, Int, Int
              when g.fn_uses_fast_i64_int ->
                NativeInt (emit_checked_i64_pow g lhs rhs overflow_label, 64, true)
            | _ ->
                let tmp = new_temp g in
                emit_bin g tmp op (llvm_int_ty width) lhs rhs;
                NativeInt (tmp, width, signed))
       | None ->
           let cmp =
             match binary.op with
             | Lt -> Some (if signed then "slt" else "ult")
             | Le -> Some (if signed then "sle" else "ule")
             | Gt -> Some (if signed then "sgt" else "ugt")
             | Ge -> Some (if signed then "sge" else "uge")
             | Eq -> Some "eq"
             | Ne -> Some "ne"
             | _ -> None
           in
           (match cmp with
            | Some op ->
                let lhs = gen_expr_as_native_int g binary.lhs width signed in
                let rhs = gen_expr_as_native_int g binary.rhs width signed in
                let tmp = new_temp g in
                emit_icmp g tmp op (llvm_int_ty width) lhs rhs;
                BoolV tmp
            | None -> gen_boxed_binary g binary))
  | None ->
      (match native_float_binary_width g binary.lhs binary.rhs with
       | Some width ->
           let arith = match binary.op with Add -> Some "fadd" | Sub -> Some "fsub" | Mul -> Some "fmul" | Div -> Some "fdiv" | _ -> None in
           (match arith with
            | Some op ->
                let lhs = gen_expr_as_native_float g binary.lhs width in
                let rhs = gen_expr_as_native_float g binary.rhs width in
                let tmp = new_temp g in
                emit_bin g tmp op (llvm_float_ty width) lhs rhs;
                NativeFloat (tmp, width)
            | None ->
                let cmp =
                  match binary.op with
                  | Lt -> Some "olt" | Le -> Some "ole" | Gt -> Some "ogt" | Ge -> Some "oge"
                  | Eq -> Some "oeq" | Ne -> Some "une"
                  | _ -> None
                in
                (match cmp with
                 | Some op ->
                     let lhs = gen_expr_as_native_float g binary.lhs width in
                     let rhs = gen_expr_as_native_float g binary.rhs width in
                     let tmp = new_temp g in
                     emit_fcmp g tmp op (llvm_float_ty width) lhs rhs;
                     BoolV tmp
                 | None -> gen_boxed_binary g binary))
       | None -> gen_boxed_binary g binary)

and gen_boxed_binary g binary =
  let gen_mixed_operand expr =
    match expr.kind with
    | Literal (LitInt text) ->
        (match parse_i64 text with
         | Some value -> NativeInt (Int64.to_string value, 64, true)
         | None -> gen_expr g expr)
    | _ -> gen_expr g expr
  in
  let mixed_i64 = function
    | NativeInt (value, width, signed) when width < 64 || signed ->
        Some (native_to_i64 g value width signed)
    | _ -> None
  in
  match binary.op with
  | Add ->
      let lhs = gen_mixed_operand binary.lhs in
      let rhs = gen_mixed_operand binary.rhs in
      let lhs_i64 = mixed_i64 lhs in
      let rhs_i64 = mixed_i64 rhs in
      (match lhs, rhs, lhs_i64, rhs_i64 with
       | StrV lhs, StrV rhs, _, _ -> StrV (emit_call_ptr g "@xi_str_concat" [ arg_ptr lhs; arg_ptr rhs ])
       | boxed, _, _, Some native ->
           Num (emit_call_ptr g "@xi_num_add_i64"
             [ arg_ptr (ensure_num g boxed); arg_i64 native ])
       | _, boxed, Some native, _ ->
           Num (emit_call_ptr g "@xi_num_add_i64"
             [ arg_ptr (ensure_num g boxed); arg_i64 native ])
       | _, _, _, _ ->
           let lhs = ensure_num g lhs in
           let rhs = ensure_num g rhs in
           Num (emit_call_ptr g "@xi_num_add" [ arg_ptr lhs; arg_ptr rhs ]))
  | Sub | Mul ->
      let lhs = gen_mixed_operand binary.lhs in
      let rhs = gen_mixed_operand binary.rhs in
      (match mixed_i64 lhs, mixed_i64 rhs with
       | None, Some native ->
           Num (emit_call_ptr g
             (if binary.op = Sub then "@xi_num_sub_i64" else "@xi_num_mul_i64")
             [ arg_ptr (ensure_num g lhs); arg_i64 native ])
       | Some native, None ->
           if binary.op = Sub then
             Num (emit_call_ptr g "@xi_num_i64_sub"
               [ arg_i64 native; arg_ptr (ensure_num g rhs) ])
           else
             Num (emit_call_ptr g "@xi_num_mul_i64"
               [ arg_ptr (ensure_num g rhs); arg_i64 native ])
       | _ ->
           Num (emit_call_ptr g (if binary.op = Sub then "@xi_num_sub" else "@xi_num_mul")
             [ arg_ptr (ensure_num g lhs); arg_ptr (ensure_num g rhs) ]))
  | Div | Mod | Pow | BitAnd | BitOr | BitXor ->
      let fn_name =
        match binary.op with
        | Sub -> "@xi_num_sub" | Mul -> "@xi_num_mul" | Div -> "@xi_num_div" | Mod -> "@xi_num_mod"
        | Pow -> "@xi_num_pow" | BitAnd -> "@xi_num_band" | BitOr -> "@xi_num_bor"
        | BitXor -> "@xi_num_bxor" | _ -> assert false
      in
      let lhs = gen_expr_as_num g binary.lhs in
      let rhs = gen_expr_as_num g binary.rhs in
      Num (emit_call_ptr g fn_name [ arg_ptr lhs; arg_ptr rhs ])
  | Eq | Ne ->
      let lhs = gen_expr g binary.lhs in
      let rhs = gen_expr g binary.rhs in
      (match lhs, rhs with
       | StrV lhs, StrV rhs ->
           let eq = emit_call_i1 g "@xi_str_eq" [ arg_ptr lhs; arg_ptr rhs ] in
           if binary.op = Eq then BoolV eq
           else
             let out = new_temp g in
             emit_bin g out "xor" "i1" eq "true";
             BoolV out
       | _ ->
           let lhs = ensure_num g lhs in
           let rhs = ensure_num g rhs in
           BoolV
             (emit_call_i1 g
                (if binary.op = Eq then "@xi_num_eq" else "@xi_num_ne")
                [ arg_ptr lhs; arg_ptr rhs ]))
  | Lt | Le | Gt | Ge ->
      let fn_name = match binary.op with Lt -> "@xi_num_lt" | Le -> "@xi_num_le" | Gt -> "@xi_num_gt" | Ge -> "@xi_num_ge" | _ -> assert false in
      let lhs = gen_expr_as_num g binary.lhs in
      let rhs = gen_expr_as_num g binary.rhs in
      BoolV (emit_call_i1 g fn_name [ arg_ptr lhs; arg_ptr rhs ])
  | And | Or ->
      let lhs = gen_expr_as_bool g binary.lhs in
      let rhs = gen_expr_as_bool g binary.rhs in
      let tmp = new_temp g in
      emit_bin g tmp (if binary.op = And then "and" else "or") "i1" lhs rhs;
      BoolV tmp

and gen_cast g cast =
  let target = sem_type_from_typeref cast.ty g.sem in
  match target with
  | IntN w -> NativeInt (gen_expr_as_native_int g cast.value w true, w, true)
  | UintN w -> NativeInt (gen_expr_as_native_int g cast.value w false, w, false)
  | DecN w when is_native_float_width w -> NativeFloat (gen_expr_as_native_float g cast.value w, w)
  | DecN w -> Num (gen_expr_as_softfloat g cast.value w)
  | Dec -> Num (ensure_value_for_type g (gen_expr g cast.value) Dec)
  | Int ->
      let boxed = gen_expr_as_num g cast.value in
      Num (emit_call_ptr g "@xi_num_trunc_to_int" [ arg_ptr boxed ])
  | Uint ->
      let boxed = gen_expr_as_num g cast.value in
      let trunc = emit_call_ptr g "@xi_num_trunc_to_int" [ arg_ptr boxed ] in
      Num (emit_call_ptr g "@xi_num_clamp_nonneg" [ arg_ptr trunc ])
  | _ -> Num (emit_num_from_i64 g 0)

and gen_array_push g arr value =
  let arrv = gen_expr_as_arr g arr in
  match array_runtime_kind_for_expr g arr with
  | ArrayNum ->
      let suffix = array_num_runtime_suffix_for_expr g arr in
      (match gen_expr g value with
       | NativeInt (native, width, signed) when width <= 64 ->
           emit_call_void g ("@xi_arr_push_" ^ suffix ^ "_i64")
             [ arg_ptr arrv; arg_i64 (native_to_i64 g native width signed) ]
       | boxed ->
           emit_call_void g ("@xi_arr_push_" ^ suffix)
             [ arg_ptr arrv; arg_ptr (ensure_num g boxed) ])
  | ArrayBoolKind -> emit_call_void g "@xi_arr_push_bool" [ (arg_ptr arrv); (arg_i1 (gen_expr_as_bool g value)) ]
  | ArrayStructKind -> emit_call_void g "@xi_arr_push_struct" [ (arg_ptr arrv); (arg_ptr (gen_expr_as_struct g value)) ]
  | ArrayFnPtrKind -> emit_call_void g "@xi_arr_push_raw" [ (arg_ptr arrv); (arg_ptr (gen_expr_as_fn_ptr g value)) ]
  | ArrayStrKind -> emit_call_void g "@xi_arr_push_str" [ (arg_ptr arrv); (arg_ptr (gen_expr_as_str g value)) ]

and gen_array_pop g arr =
  let arrv = gen_expr_as_arr g arr in
  match array_runtime_kind_for_expr g arr with
  | ArrayNum -> Num (emit_call_ptr g ("@xi_arr_pop_" ^ array_num_runtime_suffix_for_expr g arr) [ arg_ptr arrv ])
  | ArrayBoolKind -> BoolV (emit_call_i1 g "@xi_arr_pop_bool" [ arg_ptr arrv ])
  | ArrayStructKind -> StructV (emit_call_ptr g "@xi_arr_pop_struct" [ arg_ptr arrv ])
  | ArrayFnPtrKind -> FnPtrV (emit_call_ptr g "@xi_arr_pop_raw" [ arg_ptr arrv ])
  | ArrayStrKind -> StrV (emit_call_ptr g "@xi_arr_pop_str" [ arg_ptr arrv ])

and gen_array_insert g arr idx value =
  let arrv = gen_expr_as_arr g arr in
  let idxv = gen_expr g idx in
  let valuev = gen_expr g value in
  match array_runtime_kind_for_expr g arr with
  | ArrayNum ->
      let suffix = array_num_runtime_suffix_for_expr g arr in
      (match idxv, valuev with
       | NativeInt (native_idx, idx_width, idx_signed),
         NativeInt (native_value, value_width, value_signed)
         when (idx_width < 64 || idx_signed)
              && (value_width < 64 || value_signed) ->
           emit_call_void g ("@xi_arr_insert_" ^ suffix ^ "_i64_at_i64")
             [ arg_ptr arrv
             ; arg_i64 (native_to_i64 g native_idx idx_width idx_signed)
             ; arg_i64 (native_to_i64 g native_value value_width value_signed)
             ]
       | _ ->
           emit_call_void g ("@xi_arr_insert_" ^ suffix)
             [ arg_ptr arrv; arg_ptr (ensure_num g idxv); arg_ptr (ensure_num g valuev) ])
  | ArrayBoolKind -> emit_call_void g "@xi_arr_insert_bool" [ (arg_ptr arrv); (arg_ptr (ensure_num g idxv)); (arg_i1 (ensure_bool g valuev)) ]
  | ArrayStructKind -> emit_call_void g "@xi_arr_insert_struct" [ (arg_ptr arrv); (arg_ptr (ensure_num g idxv)); (arg_ptr (ensure_struct valuev)) ]
  | ArrayFnPtrKind -> emit_call_void g "@xi_arr_insert_raw" [ (arg_ptr arrv); (arg_ptr (ensure_num g idxv)); (arg_ptr (ensure_fn_ptr valuev)) ]
  | ArrayStrKind -> emit_call_void g "@xi_arr_insert_str" [ (arg_ptr arrv); (arg_ptr (ensure_num g idxv)); (arg_ptr (ensure_str valuev)) ]

and gen_array_remove g arr idx =
  let arrv = gen_expr_as_arr g arr in
  let idxv = gen_expr g idx in
  match array_runtime_kind_for_expr g arr with
  | ArrayNum ->
      let suffix = array_num_runtime_suffix_for_expr g arr in
      (match idxv with
       | NativeInt (native, width, signed) when width < 64 || signed ->
           Num (emit_call_ptr g ("@xi_arr_remove_" ^ suffix ^ "_at_i64")
             [ arg_ptr arrv; arg_i64 (native_to_i64 g native width signed) ])
       | boxed ->
           Num (emit_call_ptr g ("@xi_arr_remove_" ^ suffix)
             [ arg_ptr arrv; arg_ptr (ensure_num g boxed) ]))
  | ArrayBoolKind -> BoolV (emit_call_i1 g "@xi_arr_remove_bool" [ arg_ptr arrv; arg_ptr (ensure_num g idxv) ])
  | ArrayStructKind -> StructV (emit_call_ptr g "@xi_arr_remove_struct" [ arg_ptr arrv; arg_ptr (ensure_num g idxv) ])
  | ArrayFnPtrKind -> FnPtrV (emit_call_ptr g "@xi_arr_remove_raw" [ arg_ptr arrv; arg_ptr (ensure_num g idxv) ])
  | ArrayStrKind -> StrV (emit_call_ptr g "@xi_arr_remove_str" [ arg_ptr arrv; arg_ptr (ensure_num g idxv) ])

and gen_array_contains g arr value =
  let arrv = gen_expr_as_arr g arr in
  match array_runtime_kind_for_expr g arr with
  | ArrayNum ->
      (match gen_expr g value with
       | NativeInt (native, width, signed) when width < 64 || signed ->
           Num (emit_call_ptr g "@xi_arr_contains_num_i64"
             [ arg_ptr arrv; arg_i64 (native_to_i64 g native width signed) ])
       | boxed ->
           Num (emit_call_ptr g "@xi_arr_contains_num"
             [ arg_ptr arrv; arg_ptr (ensure_num g boxed) ]))
  | ArrayBoolKind -> Num (emit_call_ptr g "@xi_arr_contains_bool" [ arg_ptr arrv; arg_i1 (gen_expr_as_bool g value) ])
  | ArrayStructKind -> Num (emit_call_ptr g "@xi_arr_contains_struct" [ arg_ptr arrv; arg_ptr (gen_expr_as_struct g value) ])
  | ArrayFnPtrKind -> Num (emit_call_ptr g "@xi_arr_contains_raw" [ arg_ptr arrv; arg_ptr (gen_expr_as_fn_ptr g value) ])
  | ArrayStrKind -> Num (emit_call_ptr g "@xi_arr_contains_str" [ arg_ptr arrv; arg_ptr (gen_expr_as_str g value) ])

and gen_array_index_of g arr value =
  let arrv = gen_expr_as_arr g arr in
  match array_runtime_kind_for_expr g arr with
  | ArrayNum ->
      (match gen_expr g value with
       | NativeInt (native, width, signed) when width < 64 || signed ->
           Num (emit_call_ptr g "@xi_arr_index_of_num_i64"
             [ arg_ptr arrv; arg_i64 (native_to_i64 g native width signed) ])
       | boxed ->
           Num (emit_call_ptr g "@xi_arr_index_of_num"
             [ arg_ptr arrv; arg_ptr (ensure_num g boxed) ]))
  | ArrayBoolKind -> Num (emit_call_ptr g "@xi_arr_index_of_bool" [ arg_ptr arrv; arg_i1 (gen_expr_as_bool g value) ])
  | ArrayStructKind -> Num (emit_call_ptr g "@xi_arr_index_of_struct" [ arg_ptr arrv; arg_ptr (gen_expr_as_struct g value) ])
  | ArrayFnPtrKind -> Num (emit_call_ptr g "@xi_arr_index_of_raw" [ arg_ptr arrv; arg_ptr (gen_expr_as_fn_ptr g value) ])
  | ArrayStrKind -> Num (emit_call_ptr g "@xi_arr_index_of_str" [ arg_ptr arrv; arg_ptr (gen_expr_as_str g value) ])

and emit_result_fields g out ok_bit value value_ty message =
  emit_struct_set g out (emit_cstr_ptr g "ok") (ensure_value_for_type g (BoolV ok_bit) Bool) Bool;
  emit_struct_set g out (emit_cstr_ptr g "value") value value_ty;
  emit_struct_set g out (emit_cstr_ptr g "error") message Str

and gen_result_field_raw g result_expr field =
  let base = gen_expr_as_struct g result_expr in
  let key = emit_cstr_ptr g field in
  emit_call_ptr g "@xi_struct_get" [ arg_ptr base; arg_ptr key ]

and gen_result_ok_bit g result_expr =
  let raw = gen_result_field_raw g result_expr "ok" in
  let as_i64 = emit_call_i64 g "@xi_num_as_i64" [ arg_ptr raw ] in
  let b = new_temp g in
  emit_icmp g b "ne" "i64" as_i64 "0";
  b

and storage_ty_of_code_value value fallback =
  match value with
  | StrV _ -> Str
  | Arr _ -> (match fallback with ty when is_array_sem_type ty -> ty | _ -> ArrayUnknown)
  | MapV _ -> (match fallback with ty when is_map_sem_type ty -> ty | _ -> MapUnknown)
  | StructV _ -> (match fallback with Struct _ | Result _ | ResultUnknown -> fallback | _ -> fallback)
  | FnPtrV _ -> fallback
  | CptrV _ -> Cptr
  | CStructV _ -> fallback
  | BoolV _ -> Bool
  | NativeInt (_, w, true) -> IntN w
  | NativeInt (_, w, false) -> UintN w
  | NativeFloat (_, w) -> DecN w
  | Num _ -> (match fallback with Str -> Int | ty -> ty)

and decode_boxed_value g raw ty =
  match ty with
  | Str -> StrV raw
  | Cptr -> CptrV raw
  | ty when is_array_sem_type ty -> Arr raw
  | ty when is_map_sem_type ty -> MapV raw
  | Struct _ | Result _ | ResultUnknown -> StructV raw
  | FnPtr _ -> FnPtrV raw
  | Bool ->
      let as_i64 = emit_call_i64 g "@xi_num_as_i64" [ arg_ptr raw ] in
      let b = new_temp g in
      emit_icmp g b "ne" "i64" as_i64 "0";
      BoolV b
  | IntN w -> NativeInt (unbox_native_int g raw w true, w, true)
  | UintN w -> NativeInt (unbox_native_int g raw w false, w, false)
  | DecN w when is_native_float_width w ->
      let d = emit_call_typed g "double" "@xi_num_as_f64" [ arg_ptr raw ] in
      NativeFloat (double_to_native_float g d w, w)
  | _ -> Num raw

and gen_try g inner =
  let inner_ty = sem_type_of_expr g inner in
  let payload_ty = match inner_ty with Result payload -> payload | _ -> Int in
  let value = gen_expr_as_struct g inner in

  let slot = new_temp_named g "try.addr" in
  emit_entry_alloca g slot;
  emit_store g "ptr" value slot 8;

  let ok_raw = emit_call_ptr g "@xi_struct_get" [ arg_ptr value; arg_ptr (emit_cstr_ptr g "ok") ] in
  let ok_i64 = emit_call_i64 g "@xi_num_as_i64" [ arg_ptr ok_raw ] in
  let ok = new_temp g in
  emit_icmp g ok "ne" "i64" ok_i64 "0";

  let ok_label = new_label g "try.ok" in
  let err_label = new_label g "try.err" in
  emit_br_cond g ok ok_label err_label;

  emit_label g err_label;
  let failed = new_temp g in
  emit_load g failed "ptr" slot 8;
  let message = emit_call_ptr g "@xi_struct_get" [ arg_ptr failed; arg_ptr (emit_cstr_ptr g "error") ] in
  let out_payload = match g.fn_return_ty with Result payload -> payload | _ -> Int in
  let forwarded = emit_call_ptr g "@xi_struct_new" [] in
  emit_result_fields g forwarded "false" (default_value_for_type g out_payload) out_payload message;
  emit_ret g "ptr" forwarded;

  emit_label g ok_label;
  let succeeded = new_temp g in
  emit_load g succeeded "ptr" slot 8;
  let raw = emit_call_ptr g "@xi_struct_get" [ arg_ptr succeeded; arg_ptr (emit_cstr_ptr g "value") ] in
  decode_boxed_value g raw payload_ty

and gen_array_get g arr idx unchecked =
  let arrv = gen_expr_as_arr g arr in
  match array_runtime_kind_for_expr g arr with
  | ArrayNum ->
      let idxv = gen_expr_as_native_int g idx 64 true in
      (match array_int_elem_info_for_expr g arr with
       | Some (width, signed) ->
           let suffix = if signed then "num" else "uint" in
           let func =
             Printf.sprintf "@xi_arr_get_%s_i64_at_i64%s" suffix (if unchecked then "_unchecked" else "")
           in
           let raw = emit_call_i64 g func [ arg_ptr arrv; arg_i64 idxv ] in
           NativeInt (coerce_native_width g raw 64 width signed, width, signed)
       | None ->
           Num
             (emit_call_ptr g
                ("@xi_arr_get_" ^ array_num_runtime_suffix_for_expr g arr
                 ^ "_at_i64")
                [ arg_ptr arrv; arg_i64 idxv ]))
  | ArrayBoolKind ->
      BoolV (emit_call_i1 g "@xi_arr_get_bool" [ arg_ptr arrv; arg_ptr (gen_expr_as_num g idx) ])
  | ArrayStructKind ->
      StructV (emit_call_ptr g "@xi_arr_get_struct" [ arg_ptr arrv; arg_ptr (gen_expr_as_num g idx) ])
  | ArrayFnPtrKind ->
      FnPtrV (emit_call_ptr g "@xi_arr_get_raw" [ arg_ptr arrv; arg_ptr (gen_expr_as_num g idx) ])
  | ArrayStrKind ->
      StrV (emit_call_ptr g "@xi_arr_get_str" [ arg_ptr arrv; arg_ptr (gen_expr_as_num g idx) ])

and gen_array_set g arr idx value unchecked =
  let arrv = gen_expr_as_arr g arr in
  match array_runtime_kind_for_expr g arr with
  | ArrayNum ->
      let idxv = gen_expr_as_native_int g idx 64 true in
      (match array_int_elem_info_for_expr g arr with
       | Some (width, signed) ->
           let v = gen_expr_as_native_int g value width signed in
           let as_i64 = native_to_i64 g v width signed in
           let suffix = if signed then "num" else "uint" in
           let func =
             Printf.sprintf "@xi_arr_set_%s_i64_at_i64%s" suffix (if unchecked then "_unchecked" else "")
           in
           emit_call_void g func [ (arg_ptr arrv); (arg_i64 idxv); (arg_i64 as_i64) ]
       | None ->
           let suffix = array_num_runtime_suffix_for_expr g arr in
           let literal_i64 =
             match value.kind with
             | Literal (LitInt text) -> parse_i64 text
             | _ -> None
           in
           if suffix = "num" then
             match literal_i64 with
             | Some 0L ->
                 emit_call_void g "@xi_arr_set_num_zero_at_i64"
                   [ arg_ptr arrv; arg_i64 idxv ]
             | Some v ->
                 emit_call_void g "@xi_arr_set_num_i64_at_i64"
                   [ arg_ptr arrv; arg_i64 idxv; arg_i64 (Int64.to_string v) ]
             | None when fast_i64_int_ty g (sem_type_of_expr g value) ->
                 let v = gen_expr_as_native_int g value 64 true in
                 emit_call_void g "@xi_arr_set_num_i64_at_i64"
                   [ arg_ptr arrv; arg_i64 idxv; arg_i64 v ]
             | None ->
                 let v = gen_expr_as_num g value in
                 emit_call_void g "@xi_arr_set_num_at_i64"
                   [ arg_ptr arrv; arg_i64 idxv; arg_ptr v ]
           else
             let v = gen_expr_as_num g value in
             emit_call_void g (Printf.sprintf "@xi_arr_set_%s_at_i64" suffix)
               [ arg_ptr arrv; arg_i64 idxv; arg_ptr v ])
  | ArrayBoolKind ->
      let idxv = gen_expr_as_num g idx in
      let v = gen_expr_as_bool g value in
      emit_call_void g "@xi_arr_set_bool" [ (arg_ptr arrv); (arg_ptr idxv); (arg_i1 v) ]
  | ArrayStructKind ->
      let idxv = gen_expr_as_num g idx in
      let v = gen_expr_as_struct g value in
      emit_call_void g "@xi_arr_set_struct" [ (arg_ptr arrv); (arg_ptr idxv); (arg_ptr v) ]
  | ArrayFnPtrKind ->
      let idxv = gen_expr_as_num g idx in
      let v = gen_expr_as_fn_ptr g value in
      emit_call_void g "@xi_arr_set_raw" [ (arg_ptr arrv); (arg_ptr idxv); (arg_ptr v) ]
  | ArrayStrKind ->
      let idxv = gen_expr_as_num g idx in
      let v = gen_expr_as_str g value in
      emit_call_void g "@xi_arr_set_str" [ (arg_ptr arrv); (arg_ptr idxv); (arg_ptr v) ]

and gen_array_slice g arr start len =
  let arrv = gen_expr_as_arr g arr in
  let suffix =
    match array_runtime_kind_for_expr g arr with
    | ArrayNum -> "num"
    | ArrayBoolKind -> "bool"
    | ArrayStructKind -> "struct"
    | ArrayFnPtrKind -> "raw"
    | ArrayStrKind -> "str"
  in
  match len with
  | Some len ->
      let startv = gen_expr_as_num g start in
      let lenv = gen_expr_as_num g len in
      emit_call_ptr g ("@xi_arr_slice_" ^ suffix)
        [ arg_ptr arrv; arg_ptr startv; arg_ptr lenv ]
  | None ->
      emit_call_ptr g ("@xi_arr_slice_from_" ^ suffix)
        [ arg_ptr arrv; arg_ptr (gen_expr_as_num g start) ]

and gen_map_set g map key value =
  let mapv = gen_expr_as_map g map in
  let keyv = gen_expr_as_map_key g map key in
  match map_runtime_kind_for_expr g map with
  | MapNum -> emit_call_void g "@xi_map_set_num" [ (arg_ptr mapv); (arg_ptr keyv); (arg_ptr (gen_expr_as_num g value)) ]
  | MapBoolKind -> emit_call_void g "@xi_map_set_bool" [ (arg_ptr mapv); (arg_ptr keyv); (arg_i1 (gen_expr_as_bool g value)) ]
  | MapStructKind -> emit_call_void g "@xi_map_set_struct" [ (arg_ptr mapv); (arg_ptr keyv); (arg_ptr (gen_expr_as_struct g value)) ]
  | MapFnPtrKind -> emit_call_void g "@xi_map_set_raw" [ (arg_ptr mapv); (arg_ptr keyv); (arg_ptr (gen_expr_as_fn_ptr g value)) ]
  | MapStrKind -> emit_call_void g "@xi_map_set_str" [ (arg_ptr mapv); (arg_ptr keyv); (arg_ptr (gen_expr_as_str g value)) ]

and gen_map_get g map key =
  let mapv = gen_expr_as_map g map in
  let keyv = gen_expr_as_map_key g map key in
  match map_runtime_kind_for_expr g map with
  | MapNum -> Num (emit_call_ptr g "@xi_map_get_num" [ arg_ptr mapv; arg_ptr keyv ])
  | MapBoolKind -> BoolV (emit_call_i1 g "@xi_map_get_bool" [ arg_ptr mapv; arg_ptr keyv ])
  | MapStructKind -> StructV (emit_call_ptr g "@xi_map_get_struct" [ arg_ptr mapv; arg_ptr keyv ])
  | MapFnPtrKind -> FnPtrV (emit_call_ptr g "@xi_map_get_raw" [ arg_ptr mapv; arg_ptr keyv ])
  | MapStrKind -> StrV (emit_call_ptr g "@xi_map_get_str" [ arg_ptr mapv; arg_ptr keyv ])

and gen_map_get_or g map key fallback =
  let mapv = gen_expr_as_map g map in
  let keyv = gen_expr_as_map_key g map key in
  match map_runtime_kind_for_expr g map with
  | MapNum -> Num (emit_call_ptr g "@xi_map_get_or_num" [ arg_ptr mapv; arg_ptr keyv; arg_ptr (gen_expr_as_num g fallback) ])
  | MapBoolKind -> BoolV (emit_call_i1 g "@xi_map_get_or_bool" [ arg_ptr mapv; arg_ptr keyv; arg_i1 (gen_expr_as_bool g fallback) ])
  | MapStructKind -> StructV (emit_call_ptr g "@xi_map_get_or_struct" [ arg_ptr mapv; arg_ptr keyv; arg_ptr (gen_expr_as_struct g fallback) ])
  | MapFnPtrKind -> FnPtrV (emit_call_ptr g "@xi_map_get_or_raw" [ arg_ptr mapv; arg_ptr keyv; arg_ptr (gen_expr_as_fn_ptr g fallback) ])
  | MapStrKind -> StrV (emit_call_ptr g "@xi_map_get_or_str" [ arg_ptr mapv; arg_ptr keyv; arg_ptr (gen_expr_as_str g fallback) ])

and gen_map_values g map =
  let mapv = gen_expr_as_map g map in
  let func =
    match map_runtime_kind_for_expr g map with
    | MapNum -> "@xi_map_values_num"
    | MapBoolKind -> "@xi_map_values_bool"
    | MapStructKind -> "@xi_map_values_struct"
    | MapFnPtrKind -> "@xi_map_values_raw"
    | MapStrKind -> "@xi_map_values_str"
  in
  emit_call_ptr g func [ arg_ptr mapv ]

and gen_member_set g base field value =
  match sem_type_of_expr g base with
  | CStruct cstruct_id ->
      let field_name = match field.kind with Literal (LitStr s) -> s | _ -> "" in
      gen_cstruct_field_set g base cstruct_id field_name value
  | Struct struct_id ->
      let basev = gen_expr_as_struct g base in
      let field_name = match field.kind with Literal (LitStr s) -> s | _ -> "" in
      let key = emit_cstr_ptr g field_name in
      let field_ty = struct_field_type g struct_id field_name |> Option.value ~default:Error in
      let value = ensure_value_for_type g (gen_expr g value) field_ty in
      emit_struct_set g basev key value field_ty
  | _ -> ()

and struct_field_type g struct_id field =
  Option.bind (assoc_opt struct_id g.sem.struct_sigs_by_id)
    (fun sig_ -> List.find_opt (fun f -> f.sfs_name = field) sig_.ss_fields)
  |> Option.map (fun f -> f.sfs_ty)

and gen_mem_call g call =
  match path_segments call.callee, call.args with
  | Some [ "mem"; "copy" ], [ dst; dst_off; src; src_off; count ] ->
      let dstv = gen_expr_as_arr g dst in
      let dst_offv = gen_expr_as_num g dst_off in
      let srcv = gen_expr_as_arr g src in
      let src_offv = gen_expr_as_num g src_off in
      let countv = gen_expr_as_num g count in
      emit_call_void g "@xi_mem_copy" [ (arg_ptr dstv); (arg_ptr dst_offv); (arg_ptr srcv); (arg_ptr src_offv); (arg_ptr countv) ];
      Num (emit_num_from_i64 g 0)
  | Some [ "mem"; "set" ], [ buf; off; value; count ] ->
      let bufv = gen_expr_as_arr g buf in
      let offv = gen_expr_as_num g off in
      let valuev = gen_expr_as_num g value in
      let countv = gen_expr_as_num g count in
      emit_call_void g "@xi_mem_set" [ (arg_ptr bufv); (arg_ptr offv); (arg_ptr valuev); (arg_ptr countv) ];
      Num (emit_num_from_i64 g 0)
  | Some [ "mem"; "cmp" ], [ a; a_off; b; b_off; count ] ->
      let av = gen_expr_as_arr g a in
      let a_offv = gen_expr_as_num g a_off in
      let bv = gen_expr_as_arr g b in
      let b_offv = gen_expr_as_num g b_off in
      let countv = gen_expr_as_num g count in
      Num
        (emit_call_ptr g "@xi_mem_cmp"
           [ arg_ptr av
           ; arg_ptr a_offv
           ; arg_ptr bv
           ; arg_ptr b_offv
           ; arg_ptr countv
           ])
  | Some [ "mem"; ("read_be" | "read_le" as op) ], [ buf; off; nbytes ] ->
      let bufv = gen_expr_as_arr g buf in
      let offv = gen_expr_as_num g off in
      let nbytesv = gen_expr_as_num g nbytes in
      Num
        (emit_call_ptr g
           (if op = "read_be" then "@xi_mem_read_be" else "@xi_mem_read_le")
           [ arg_ptr bufv
           ; arg_ptr offv
           ; arg_ptr nbytesv
           ])
  | Some [ "mem"; ("write_be" | "write_le" as op) ], [ buf; off; value; nbytes ] ->
      let bufv = gen_expr_as_arr g buf in
      let offv = gen_expr_as_num g off in
      let valuev = gen_expr_as_num g value in
      let nbytesv = gen_expr_as_num g nbytes in
      emit_call_void g (if op = "write_be" then "@xi_mem_write_be" else "@xi_mem_write_le") [ (arg_ptr bufv); (arg_ptr offv); (arg_ptr valuev); (arg_ptr nbytesv) ];
      Num (emit_num_from_i64 g 0)
  | _ -> Num (emit_num_from_i64 g 0)

and gen_cptr_gep g p_expr off_expr =
  let p = gen_expr_as_cptr g p_expr in
  let off = gen_expr_as_native_int g off_expr 64 true in
  let tmp = new_temp g in
  emit_gep_byte g tmp p off;
  tmp

and gen_cptr_read_int g args width signed =
  match args with
  | [ p; off ] ->
      let addr = gen_cptr_gep g p off in
      let tmp = new_temp g in
      emit_load g tmp (llvm_int_ty width) addr 1;
      NativeInt (tmp, width, signed)
  | _ -> NativeInt ("0", width, signed)

and gen_cptr_read_float g args width =
  match args with
  | [ p; off ] ->
      let addr = gen_cptr_gep g p off in
      let tmp = new_temp g in
      emit_load g tmp (llvm_float_ty width) addr 1;
      NativeFloat (tmp, width)
  | _ -> NativeFloat ("0.0", width)

and gen_cptr_write_int g args width signed =
  (match args with
   | [ p; off; value ] ->
       let addr = gen_cptr_gep g p off in
       let v = gen_expr_as_native_int g value width signed in
       emit_store g (llvm_int_ty width) v addr 1
   | _ -> ());
  Num (emit_num_from_i64 g 0)

and gen_cptr_write_float g args width =
  (match args with
   | [ p; off; value ] ->
       let addr = gen_cptr_gep g p off in
       let v = gen_expr_as_native_float g value width in
       emit_store g (llvm_float_ty width) v addr 1
   | _ -> ());
  Num (emit_num_from_i64 g 0)

and gen_cptr_call g op args =
  match op, args with
  | "alloc", [ n ] ->
      CptrV (emit_call_ptr g "@xi_cptr_alloc" [ arg_i64 (gen_expr_as_native_int g n 64 true) ])
  | "free", [ p ] ->
      emit_call_void g "@xi_cptr_free" [ (arg_ptr (gen_expr_as_cptr g p)) ];
      Num (emit_num_from_i64 g 0)
  | "null", [] -> CptrV "null"
  | "is_null", [ p ] ->
      let v = gen_expr_as_cptr g p in
      let tmp = new_temp g in
      emit_icmp g tmp "eq" "ptr" v "null";
      BoolV tmp
  | "addr", [ p ] ->
      let v = gen_expr_as_cptr g p in
      let tmp = new_temp g in
      emit_conv g tmp "ptrtoint" "ptr" v "i64";
      NativeInt (tmp, 64, false)
  | "offset", [ p; n ] -> CptrV (gen_cptr_gep g p n)
  | "from_addr", [ n ] ->
      let v = gen_expr_as_native_int g n 64 true in
      let t = new_temp g in
      emit_conv g t "inttoptr" "i64" v "ptr";
      CptrV t
  | "read_i8", _ -> gen_cptr_read_int g args 8 true
  | "read_i16", _ -> gen_cptr_read_int g args 16 true
  | "read_i32", _ -> gen_cptr_read_int g args 32 true
  | "read_i64", _ -> gen_cptr_read_int g args 64 true
  | "read_u8", _ -> gen_cptr_read_int g args 8 false
  | "read_u16", _ -> gen_cptr_read_int g args 16 false
  | "read_u32", _ -> gen_cptr_read_int g args 32 false
  | "read_u64", _ -> gen_cptr_read_int g args 64 false
  | "read_f32", _ -> gen_cptr_read_float g args 32
  | "read_f64", _ -> gen_cptr_read_float g args 64
  | "read_ptr", [ p; off ] ->
      let addr = gen_cptr_gep g p off in
      let tmp = new_temp g in
      emit_load g tmp "ptr" addr 1;
      CptrV tmp
  | "read_cstr", [ p; off ] ->
      let pv = gen_expr_as_cptr g p in
      let offv = gen_expr_as_native_int g off 64 true in
      StrV
        (emit_call_ptr g "@xi_cptr_read_cstr"
           [ arg_ptr pv; arg_i64 offv ])
  | "write_i8", _ -> gen_cptr_write_int g args 8 true
  | "write_i16", _ -> gen_cptr_write_int g args 16 true
  | "write_i32", _ -> gen_cptr_write_int g args 32 true
  | "write_i64", _ -> gen_cptr_write_int g args 64 true
  | "write_u8", _ -> gen_cptr_write_int g args 8 false
  | "write_u16", _ -> gen_cptr_write_int g args 16 false
  | "write_u32", _ -> gen_cptr_write_int g args 32 false
  | "write_u64", _ -> gen_cptr_write_int g args 64 false
  | "write_f32", _ -> gen_cptr_write_float g args 32
  | "write_f64", _ -> gen_cptr_write_float g args 64
  | "write_ptr", [ p; off; value ] ->
      let addr = gen_cptr_gep g p off in
      emit_store g "ptr" (gen_expr_as_cptr g value) addr 1;
      Num (emit_num_from_i64 g 0)
  | "write_cstr", [ p; off; value ] ->
      let pv = gen_expr_as_cptr g p in
      let offv = gen_expr_as_native_int g off 64 true in
      let valuev = gen_expr_as_str g value in
      NativeInt
        ( emit_call_i64 g "@xi_cptr_write_cstr"
            [ arg_ptr pv
            ; arg_i64 offv
            ; arg_ptr valuev
            ]
        , 64, true )
  | _ -> Num (emit_num_from_i64 g 0)

and cstruct_size g id = match assoc_opt id g.sem.cstruct_sigs_by_id with Some s -> s.cs_size | None -> 0

and cstruct_field_addr g base offset =
  if offset = 0 then base
  else begin
    let t = new_temp g in
    emit_gep_byte g t base (string_of_int offset);
    t
  end

and gen_cstruct_field_get g base cstruct_id field =
  let base_ptr = gen_expr_as_cstruct g base in
  match cstruct_field_info g cstruct_id field with
  | None -> Num (emit_num_from_i64 g 0)
  | Some (offset, ty) ->
      let addr = cstruct_field_addr g base_ptr offset in
      let load ll =
        let t = new_temp g in
        emit_load g t ll addr 1;
        t
      in
      (match ty with
       | CStruct _ -> CStructV addr
       | Cptr -> CptrV (load "ptr")
       | Bool ->
           let byte = load "i8" in
           let b = new_temp g in
           emit_icmp g b "ne" "i8" byte "0";
           BoolV b
       | IntN w -> NativeInt (load (llvm_int_ty w), w, true)
       | UintN w -> NativeInt (load (llvm_int_ty w), w, false)
       | DecN w -> NativeFloat (load (llvm_float_ty w), w)
       | _ -> Num (emit_num_from_i64 g 0))

and gen_cstruct_field_set g base cstruct_id field value =
  let base_ptr = gen_expr_as_cstruct g base in
  match cstruct_field_info g cstruct_id field with
  | None -> ()
  | Some (offset, ty) ->
      let addr = cstruct_field_addr g base_ptr offset in
      let store ll v = emit_store g ll v addr 1 in
      (match ty with
       | Cptr -> store "ptr" (gen_expr_as_cptr g value)
       | Bool ->
           let b = gen_expr_as_bool g value in
           let t = new_temp g in
           emit_conv g t "zext" "i1" b "i8";
           store "i8" t
       | IntN w -> store (llvm_int_ty w) (gen_expr_as_native_int g value w true)
       | UintN w -> store (llvm_int_ty w) (gen_expr_as_native_int g value w false)
       | DecN w -> store (llvm_float_ty w) (gen_expr_as_native_float g value w)
       | _ -> ())

and gen_cstruct_call g op args =
  match op, args with
  | "size", [ ty ] ->
      Num (emit_num_from_i64 g (match cstruct_id_of_type_arg g ty with Some id -> cstruct_size g id | None -> 0))
  | "alloc", [ ty ] ->
      let size = (match cstruct_id_of_type_arg g ty with Some id -> cstruct_size g id | None -> 0) in
      CStructV (emit_call_ptr g "@xi_cptr_alloc" [ arg_i64 (string_of_int size) ])
  | "view", [ p; _ ] -> CStructV (gen_expr_as_cptr g p)
  | "ptr", [ x ] -> CptrV (gen_expr_as_cstruct g x)
  | "free", [ x ] ->
      emit_call_void g "@xi_cptr_free" [ (arg_ptr (gen_expr_as_cstruct g x)) ];
      Num (emit_num_from_i64 g 0)
  | _ -> Num (emit_num_from_i64 g 0)

(* The process exit code for `main`.

   `main` is always emitted as `define i32 @main()` whatever its declared or
   inferred Xi return type, because the value it returns *is* the exit status the
   CRT hands to the OS. Reducing that value to an i32 here is what stops a boxed
   return leaking out as an exit code: `f main() { return 0; }` infers a boxed
   `int`, whose LLVM type is `ptr`, so returning it directly made the process
   exit with a heap address (cptr_basics.xi exited 1770052792). A numeric return
   becomes the exit code C-style; anything that is not a number cannot be one, so
   it exits 0 for success. *)
and emit_entry_main_return g value =
  match value with
  | None -> emit_ret g "i32" "0"
  | Some expr -> (
      match gen_expr g expr with
      | NativeInt (v, width, signed) ->
          emit_ret g "i32" (coerce_native_width g v width 32 signed)
      | BoolV b ->
          let tmp = new_temp g in
          emit_conv g tmp "zext" "i1" b "i32";
          emit_ret g "i32" tmp
      | Num v ->
          let as_i64 = emit_call_i64 g "@xi_num_as_i64" [ arg_ptr v ] in
          let tmp = new_temp g in
          emit_conv g tmp "trunc" "i64" as_i64 "i32";
          emit_ret g "i32" tmp
      | NativeFloat (v, width) ->
          let d = native_float_to_double g v width in
          let tmp = new_temp g in
          emit_conv g tmp "fptosi" "double" d "i32";
          emit_ret g "i32" tmp
      | StrV _ | Arr _ | MapV _ | StructV _ | FnPtrV _ | CptrV _ | CStructV _ ->
          emit_ret g "i32" "0")

and emit_default_return g =
  if g.fn_is_entry_main then emit_entry_main_return g None
  else
  match g.fn_return_ty with
  | Void -> emit_ret_void g
  | ty when native_width_of ty <> None ->
      let width = match native_width_of ty with Some width -> width | None -> 64 in
      emit_ret g (llvm_int_ty width) "0"
  | Int when g.fn_uses_fast_i64_int ->
      (match g.fn_fast_result_out with
       | Some out -> emit_store g "i64" "0" out 8; emit_ret g "i1" "true"
       | None -> emit_ret g "i64" "0")
  | ty when native_float_width_of ty <> None ->
      let width = match native_float_width_of ty with Some width -> width | None -> 64 in
      emit_ret g (llvm_float_ty width) "0.0"
  | Bool -> emit_ret g "i1" "false"
  | ty ->
      let zero = default_value_for_type g ty in
      emit_ret g "ptr" zero

and gen_stmt g stmt =
  match stmt with
  | Block block ->
      push_scope g;
      let terminated = gen_stmt_list g block.statements in
      pop_scope g;
      terminated
  | VarDecl decl ->
      let decl_ty = sem_type_from_typeref decl.ty g.sem in
      if decl.binding = Ref then begin
        Option.iter
          (fun init -> Option.iter (fun slot -> declare_local g decl.name slot decl.binding decl_ty) (expr_as_local_slot g init))
          decl.init;
        false
      end else if decl.binding = Link then begin
        (match Option.bind decl.init expr_as_local_name with
         | Some init_name ->
             (match lookup_local_slot g init_name with
              | Some slot ->
                  declare_local g decl.name slot decl.binding decl_ty;
                  merge_link_groups g decl.name init_name;
                  false
              | None -> gen_var_decl_slot g decl decl_ty)
         | None -> gen_var_decl_slot g decl decl_ty)
      end else gen_var_decl_slot g decl decl_ty
  | Assign assign ->
      (match assign.target.segments with
       | [ target_name ] ->
           let lhs_ty = lookup_local_ty g target_name |> Option.value ~default:Int in
           if assign.mode = ExplicitLink then begin
             Option.iter
               (fun rhs_name ->
                 merge_link_groups g target_name rhs_name;
                 Option.iter (retarget_link_group_to_slot g target_name) (lookup_local_slot g rhs_name))
               (expr_as_local_name assign.value)
           end else if lookup_local_binding g target_name = Some Link then begin
             match Option.bind (expr_as_local_name assign.value) (lookup_local_slot g) with
             | Some rhs_slot -> retarget_link_group_to_slot g target_name rhs_slot
             | None ->
                 let value = ensure_value_for_type g (gen_expr g assign.value) lhs_ty in
                 let slot = new_temp_named g (sanitize_ident target_name ^ ".link.addr") in
                 emit_entry_alloca g slot;
                 emit_store g "ptr" value slot 8;
                 retarget_link_group_to_slot g target_name slot
           end else (match lookup_adaptive_state g target_name with
           | Some state -> gen_adaptive_state_assignment g target_name state assign.value
           | None ->
             Option.iter
               (fun slot ->
                 match local_native_int_info ~name:target_name g lhs_ty with
                 | Some (width, signed) ->
                     let value = gen_expr_as_native_int g assign.value width signed in
                     emit_store g (llvm_int_ty width) value slot (native_align width)
                 | None ->
                     (match native_float_width_of lhs_ty with
                      | Some width ->
                          let value = gen_expr_as_native_float g assign.value width in
                          emit_store g (llvm_float_ty width) value slot (max 1 (width / 8))
                      | None when lhs_ty = Bool ->
                          emit_store g "i1" (gen_expr_as_bool g assign.value) slot 1
                      | None ->
                          let value = ensure_value_for_type g (gen_expr g assign.value) lhs_ty in
                          emit_store g "ptr" value slot 8))
               (lookup_local_slot g target_name))
       | _ -> ());
      false
  | Return ret ->
      gen_return g ret;
      true
  | Break _ ->
      (match g.loop_targets with
       | (_, break_target) :: _ ->
           emit_br g break_target;
           true
       | [] -> false)
  | Continue _ ->
      (match g.loop_targets with
       | (continue_target, _) :: _ ->
           emit_br g continue_target;
           true
       | [] -> false)
  | ExprStmt expr_stmt ->
      (match expr_stmt.expr.kind with
       | Call call ->
           g.discarded_call <- Some call;
           ignore (gen_expr g expr_stmt.expr);
           g.discarded_call <- None
       | _ -> ignore (gen_expr g expr_stmt.expr));
      false
  | IoChain io ->
      if io.anchor = Wrtr then emit_call_void g "@xi_io_wrtr_begin" [];
      List.iter
        (function
          | Endl _ -> emit_call_void g "@xi_io_newline" []
          | IoExpr expr -> emit_io_expr g expr)
        io.items;
      (match io.anchor with
       | Wrt -> ()
       | Wrtl -> emit_call_void g "@xi_io_newline" []
       | Wrtr -> emit_call_void g "@xi_io_wrtr_end" []);
      false
  | If if_stmt -> gen_if g if_stmt
  | Switch switch_stmt -> gen_switch g switch_stmt
  | While while_stmt ->
      gen_while g while_stmt;
      false
  | ForEach for_each ->
      gen_for_each g for_each;
      false
  | For for_stmt ->
      gen_for g for_stmt;
      false

and gen_stmt_list g statements =
  match statements with
  | [] -> false
  | stmt :: rest -> if gen_stmt g stmt then true else gen_stmt_list g rest

and gen_adaptive_state_assignment g target_name state value =
  let self_literal_update =
    match value.kind with
    | Binary
        { op = (Add | Sub | Mul as op)
        ; lhs = { kind = Path { segments = [ name ]; _ }; _ }
        ; rhs = { kind = Literal (LitInt text); _ }
        }
      when name = target_name -> Option.map (fun rhs -> op, rhs) (parse_i64 text)
    | _ -> None
  in
  match self_literal_update with
  | None ->
      let boxed = ensure_num g (gen_expr g value) in
      emit_store g "ptr" boxed state.boxed_slot 8;
      emit_store g "i1" "true" state.boxed_flag_slot 1
  | Some (op, rhs) ->
      let boxed_flag = new_temp g in
      emit_load g boxed_flag "i1" state.boxed_flag_slot 1;
      let native_label = new_label g "adaptive.update.native" in
      let boxed_label = new_label g "adaptive.update.boxed" in
      let overflow_label = new_label g "adaptive.update.overflow" in
      let done_label = new_label g "adaptive.update.done" in
      emit_br_cond g boxed_flag boxed_label native_label;
      emit_label g native_label;
      let old = new_temp g in
      emit_load g old "i64" state.native_slot 8;
      let lir_op, runtime_fn =
        match op with
        | Add -> Lir.Add, "@xi_num_add_i64"
        | Sub -> Lir.Sub, "@xi_num_sub_i64"
        | Mul -> Lir.Mul, "@xi_num_mul_i64"
        | _ -> assert false
      in
      let native = emit_checked_i64_arith g lir_op old (Int64.to_string rhs) overflow_label in
      emit_store g "i64" native state.native_slot 8;
      emit_br g done_label;
      emit_label g overflow_label;
      let old_boxed = emit_call_ptr g "@xi_num_from_i64" [ arg_i64 old ] in
      let overflowed = emit_call_ptr g runtime_fn
        [ arg_ptr old_boxed; arg_i64 (Int64.to_string rhs) ] in
      emit_store g "ptr" overflowed state.boxed_slot 8;
      emit_store g "i1" "true" state.boxed_flag_slot 1;
      emit_br g done_label;
      emit_label g boxed_label;
      let old_boxed = new_temp g in
      emit_load g old_boxed "ptr" state.boxed_slot 8;
      let updated = emit_call_ptr g runtime_fn
        [ arg_ptr old_boxed; arg_i64 (Int64.to_string rhs) ] in
      emit_store g "ptr" updated state.boxed_slot 8;
      emit_br g done_label;
      emit_label g done_label

and gen_var_decl_slot g decl decl_ty =
  let slot = new_temp_named g (sanitize_ident decl.name ^ ".addr") in
  if decl_ty = Int && StringSet.mem decl.name g.adaptive_state_locals then begin
    let native_slot = new_temp_named g (sanitize_ident decl.name ^ ".native.addr") in
    let boxed_slot = slot in
    let boxed_flag_slot = new_temp_named g (sanitize_ident decl.name ^ ".boxed.addr") in
    emit_entry_alloca_native g native_slot 64;
    emit_entry_alloca g boxed_slot;
    emit_entry_alloca_bool g boxed_flag_slot;
    let init =
      match decl.init with
      | Some { kind = Literal (LitInt text); _ } ->
          (match parse_i64 text with Some value -> Int64.to_string value | None -> "0")
      | _ -> "0"
    in
    emit_store g "i64" init native_slot 8;
    emit_store g "ptr" "null" boxed_slot 8;
    emit_store g "i1" "false" boxed_flag_slot 1;
    declare_local
      ~adaptive_state:{ native_slot; boxed_slot; boxed_flag_slot }
      g decl.name boxed_slot decl.binding decl_ty
  end else begin
  (match local_native_int_info ~name:decl.name g decl_ty with
   | Some (width, signed) ->
       emit_entry_alloca_native g slot width;
       let init = match decl.init with Some e -> gen_expr_as_native_int g e width signed | None -> "0" in
       emit_store g (llvm_int_ty width) init slot (native_align width)
   | None ->
       (match native_float_width_of decl_ty with
        | Some width ->
            emit_entry_alloca_float g slot width;
            let init = match decl.init with Some e -> gen_expr_as_native_float g e width | None -> "0.0" in
            emit_store g (llvm_float_ty width) init slot (max 1 (width / 8))
        | None when decl_ty = Bool ->
            emit_entry_alloca_bool g slot;
            let init = match decl.init with Some e -> gen_expr_as_bool g e | None -> "false" in
            emit_store g "i1" init slot 1
        | None ->
            emit_entry_alloca g slot;
            let init = match decl.init with Some e -> ensure_value_for_type g (gen_expr g e) decl_ty | None -> default_value_for_type g decl_ty in
            emit_store g "ptr" init slot 8));
  declare_local g decl.name slot decl.binding decl_ty
  end;
  false

and gen_return g ret =
  (* `main` returns an exit status, not a Xi value, so it is handled uniformly
     for every return type rather than falling through the cases below — those
     would hand back whatever LLVM type the Xi type maps to, which for a boxed
     type is a pointer. *)
  if g.fn_is_entry_main then emit_entry_main_return g ret.value
  else
  match ret.value, g.fn_return_ty with
  | None, Void -> emit_ret_void g
  | None, (IntN w | UintN w) -> emit_ret g (llvm_int_ty w) "0"
  | None, Int when g.fn_uses_fast_i64_int ->
      (match g.fn_fast_result_out with
       | Some out -> emit_store g "i64" "0" out 8; emit_ret g "i1" "true"
       | None -> emit_ret g "i64" "0")
  | None, DecN w when is_native_float_width w -> emit_ret g (llvm_float_ty w) "0.0"
  | None, Bool -> emit_ret g "i1" "false"
  | None, ty -> emit_ret g "ptr" (default_value_for_type g ty)
  | Some expr, Void ->
      ignore (gen_expr g expr);
      emit_ret_void g
  | Some expr, IntN w -> emit_ret g (llvm_int_ty w) (gen_expr_as_native_int g expr w true)
  | Some expr, UintN w -> emit_ret g (llvm_int_ty w) (gen_expr_as_native_int g expr w false)
  | Some expr, Int when g.fn_uses_fast_i64_int ->
      let value = gen_expr_as_native_int g expr 64 true in
      (match g.fn_fast_result_out with
       | Some out -> emit_store g "i64" value out 8; emit_ret g "i1" "true"
       | None -> emit_ret g "i64" value)
  | Some expr, DecN w when is_native_float_width w -> emit_ret g (llvm_float_ty w) (gen_expr_as_native_float g expr w)
  | Some expr, Bool -> emit_ret g "i1" (gen_expr_as_bool g expr)
  | Some expr, ty ->
      let value = ensure_value_for_type g (gen_expr g expr) ty in
      emit_ret g "ptr" value

and emit_io_expr g expr =
  match gen_expr g expr with
  | Num v -> emit_call_void g "@xi_io_write_num" [ (arg_ptr v) ]
  | NativeInt (v, width, signed) ->
      if width <= 64 then
        emit_call_void g (if signed then "@xi_io_write_i64" else "@xi_io_write_u64") [ (arg_i64 (native_to_i64 g v width signed)) ]
      else emit_wide_print g v width signed
  | NativeFloat (v, width) ->
      emit_call_void g "@xi_io_write_f64w" [ (arg_double (native_float_to_double g v width)); (arg_i32 (string_of_int width)) ]
  | StrV ptr -> emit_call_void g "@xi_io_write_cstr" [ (arg_ptr ptr) ]
  | BoolV b ->
      let tmp = new_temp g in
      emit_conv g tmp "zext" "i1" b "i32";
      emit_call_void g "@xi_io_write_bool" [ (arg_i32 tmp) ]
  | CptrV v | CStructV v ->
      let tmp = new_temp g in
      emit_conv g tmp "ptrtoint" "ptr" v "i64";
      emit_call_void g "@xi_io_write_u64" [ (arg_i64 tmp) ]
  | Arr _ | MapV _ | StructV _ ->
      emit_call_void g "@xi_io_write_cstr" [ arg_ptr "null" ]
  | FnPtrV _ ->
      let label = emit_cstr_ptr g "<fn>" in
      emit_call_void g "@xi_io_write_cstr" [ (arg_ptr label) ]

and gen_if g if_stmt =
  let then_label = new_label g "if.then" in
  let else_label = new_label g (if Option.is_some if_stmt.else_branch then "if.else" else "if.false") in
  let end_label = new_label g "if.end" in
  let cond = gen_expr_as_bool g if_stmt.cond in
  emit_br_cond g cond then_label else_label;
  emit_label g then_label;
  let then_term = gen_stmt g if_stmt.then_branch in
  if not then_term then emit_br g end_label;
  emit_label g else_label;
  let else_term = match if_stmt.else_branch with Some branch -> gen_stmt g branch | None -> false in
  if not else_term then emit_br g end_label;
  if then_term && else_term then true
  else begin
    emit_label g end_label;
    false
  end

and int_literal_i64 expr =
  match expr.kind with
  | Literal (LitInt v) ->
      (match parse_i64 v with
       | Some n -> Some n
       | None -> parse_u64_as_i64 v)
  | Grouping inner -> int_literal_i64 inner
  | Unary { op = Neg; rhs = { kind = Literal (LitInt v); _ } } ->
      (match parse_i64 v with
       | Some n when n <> Int64.min_int -> Some (Int64.neg n)
       | _ -> None)
  | _ -> None

and gen_switch g (switch_stmt : Ast.switch_stmt) =
  let default_label = if Option.is_some switch_stmt.default then new_label g "switch.default" else "" in
  let end_label = new_label g "switch.end" in
  let selector = gen_expr_as_native_int g switch_stmt.selector 64 true in
  let labels = List.map (fun _ -> new_label g "switch.case") switch_stmt.cases in
  let default_target = if Option.is_some switch_stmt.default then default_label else end_label in
  let cases =
    List.combine switch_stmt.cases labels
    |> List.filter_map
         (fun ((case : Ast.switch_case), label) ->
           Option.map (fun value -> (Int64.to_string value, label)) (int_literal_i64 case.value))
  in
  emit_switch g "i64" selector default_target cases;
  let all_cases_term =
    List.combine switch_stmt.cases labels
    |> List.fold_left
         (fun all_term ((case : Ast.switch_case), label) ->
           emit_label g label;
           push_scope g;
           let term = gen_stmt_list g case.body in
           pop_scope g;
           if not term then emit_br g end_label;
           all_term && term)
         (switch_stmt.cases <> [])
  in
  let default_term =
    match switch_stmt.default with
    | Some d ->
        emit_label g default_label;
        push_scope g;
        let term = gen_stmt_list g d.body in
        pop_scope g;
        if not term then emit_br g end_label;
        term
    | None -> false
  in
  if all_cases_term && default_term then true
  else begin
    emit_label g end_label;
    false
  end

and gen_while g while_stmt =
  match packed_scan_loop_parts while_stmt with
  | Some (array_expr, index_expr)
    when sem_type_of_expr g array_expr = ArrayInt None ->
      let fast_dispatch = new_label g "while.packed.dispatch" in
      let slow_cond = new_label g "while.generic.cond" in
      let slow_body = new_label g "while.generic.body" in
      let end_label = new_label g "while.versioned.end" in
      let array = gen_expr_as_arr g array_expr in
      let packed = emit_call_i1 g "@xi_arr_is_packed_signed_i64" [ arg_ptr array ] in
      emit_br_cond g packed fast_dispatch slow_cond;

      let emit_body cond_label =
        let saved_add_one = g.proven_i64_add_one in
        g.loop_targets <- (cond_label, end_label) :: g.loop_targets;
        let body_term = gen_stmt g while_stmt.body in
        g.loop_targets <- List.tl g.loop_targets;
        g.proven_i64_add_one <- saved_add_one;
        if not body_term then emit_br g cond_label
      in

      emit_label g fast_dispatch;
      let data = emit_array_header_load g array 24 "ptr" 8 "while.packed.data" in
      let bits = emit_array_header_load g array 32 "i16" 2 "while.packed.bits" in
      let versions =
        List.map
          (fun (width, bytes) ->
            width, bytes, new_label g "while.packed.cond",
            new_label g "while.packed.body", new_label g "while.packed.bounds")
          packed_widths
      in
      emit_switch g "i16" bits slow_cond
        (List.map (fun (width, _, cond, _, _) -> string_of_int width, cond) versions);
      List.iter
        (fun (width, bytes, cond_label, body_label, bounds_label) ->
          emit_label g cond_label;
          let index = gen_expr_as_native_int g index_expr 64 true in
          let nonnegative = new_temp g in
          emit_icmp g nonnegative "sge" "i64" index "0";
          let upper_label = new_label g "while.packed.upper" in
          emit_br_cond g nonnegative upper_label bounds_label;
          emit_label g upper_label;
          let len = emit_array_header_load g array 0 "i64" 8 "while.packed.len" in
          let in_bounds = new_temp g in
          emit_icmp g in_bounds "ult" "i64" index len;
          let load_label = new_label g "while.packed.load" in
          emit_br_cond g in_bounds load_label bounds_label;
          emit_label g load_label;
          let addr = emit_packed_element_addr g data index bytes in
          let value = emit_packed_signed_load g addr width in
          let nonzero = new_temp g in
          emit_icmp g nonzero "ne" "i64" value "0";
          emit_br_cond g nonzero body_label end_label;
          emit_label g bounds_label;
          ignore (emit_call_i1 g "@xi_arr_packed_nonzero_at_i64"
            [ arg_ptr array; arg_i64 index ]);
          emit_br g end_label;
          emit_label g body_label;
          emit_body cond_label)
        versions;

      emit_label g slow_cond;
      let cond = gen_expr_as_bool g while_stmt.cond in
      emit_br_cond g cond slow_body end_label;
      emit_label g slow_body;
      emit_body slow_cond;
      emit_label g end_label
  | _ -> gen_while_general g while_stmt

and gen_while_general g while_stmt =
  let cond_label = new_label g "while.cond" in
  let body_label = new_label g "while.body" in
  let end_label = new_label g "while.end" in
  emit_br g cond_label;
  emit_label g cond_label;
  let cond = gen_expr_as_bool g while_stmt.cond in
  emit_br_cond g cond body_label end_label;
  emit_label g body_label;
  let saved_add_one = g.proven_i64_add_one in
  (match lt_condition_name while_stmt.cond with
   | Some name
     when lookup_local_ty g name = Some Int
          && assignments_to_name name while_stmt.body = (1, 1) ->
       g.proven_i64_add_one <- StringSet.add name g.proven_i64_add_one
   | _ -> ());
  g.loop_targets <- (cond_label, end_label) :: g.loop_targets;
  let body_term = gen_stmt g while_stmt.body in
  g.loop_targets <- List.tl g.loop_targets;
  g.proven_i64_add_one <- saved_add_one;
  if not body_term then emit_br g cond_label;
  emit_label g end_label

(* `for v in xs { body }` becomes the three-part loop

       for (; i < arr::len(coll); i = i + 1) { v = coll[i]; body }

   over two hidden locals. Declaring those locals here rather than desugaring in
   the parser is what makes the element type available: the parser has no types,
   while here `v` can be given the array's element type directly.

   The collection is evaluated ONCE into `coll`, so `for v in build()` builds one
   array rather than one per iteration. `coll` is a handle, so the loop walks the
   same object the caller has; the length is re-read each iteration, which means
   removing elements during the loop shortens it rather than reading off the end.
   Reusing `gen_for` also gets `break`/`continue` right for free: `continue`
   targets the step block, so it still advances. *)
and gen_for_each g (for_each : for_each_stmt) =
  push_scope g;
  let span = for_each.span in
  let uid = g.label_counter in
  g.label_counter <- g.label_counter + 1;
  let coll_name = "__fe_coll_" ^ string_of_int uid in
  let index_name = "__fe_i_" ^ string_of_int uid in

  let coll_ty = sem_type_of_expr g for_each.collection in
  let coll_slot = new_temp_named g (sanitize_ident coll_name ^ ".addr") in
  emit_entry_alloca g coll_slot;
  emit_store g "ptr" (ensure_arr (gen_expr g for_each.collection)) coll_slot 8;
  declare_local g coll_name coll_slot Value coll_ty;

  let index_ty = IntN 64 in
  let index_slot = new_temp_named g (sanitize_ident index_name ^ ".addr") in
  emit_entry_alloca_native g index_slot 64;
  emit_store g "i64" "0" index_slot 8;
  declare_local g index_name index_slot Value index_ty;

  let elem_ty =
    match array_element_sem_type coll_ty with Some ty -> ty | None -> Error
  in
  let elem_slot = new_temp_named g (sanitize_ident for_each.name ^ ".addr") in
  (match local_native_int_info g elem_ty with
   | Some (width, _) ->
       emit_entry_alloca_native g elem_slot width;
       emit_store g (llvm_int_ty width) "0" elem_slot (native_align width)
   | None ->
       (match native_float_width_of elem_ty with
        | Some width ->
            emit_entry_alloca_float g elem_slot width;
            emit_store g (llvm_float_ty width) "0.0" elem_slot (max 1 (width / 8))
        | None when elem_ty = Bool ->
            emit_entry_alloca_bool g elem_slot;
            emit_store g "i1" "false" elem_slot 1
        | None ->
            emit_entry_alloca g elem_slot;
            emit_store g "ptr" (default_value_for_type g elem_ty) elem_slot 8));
  declare_local g for_each.name elem_slot Value elem_ty;

  let path name = { kind = Path ({ segments = [ name ]; span } : path_expr); span } in
  let coll_path = path coll_name in
  let index_path = path index_name in
  let len_call =
    { kind =
        Call
          ({ callee = { kind = Path ({ segments = [ "arr"; "len" ]; span } : path_expr); span }
           ; args = [ coll_path ]
           }
            : call_expr)
    ; span
    }
  in
  let cond = { kind = Binary ({ op = Lt; lhs = index_path; rhs = len_call } : binary_expr); span } in
  let step =
    ForStepAssign
      ({ target = { segments = [ index_name ]; span }
       ; value =
           { kind =
               Binary
                 ({ op = Add; lhs = index_path; rhs = { kind = Literal (LitInt "1"); span } }
                   : binary_expr)
           ; span
           }
       ; mode = Normal
       ; span
       }
        : assign_stmt)
  in
  let bind_element =
    Assign
      ({ target = { segments = [ for_each.name ]; span }
       ; value = { kind = Index ({ base = coll_path; index = index_path } : index_expr); span }
       ; mode = Normal
       ; span
       }
        : assign_stmt)
  in
  let body = Block ({ statements = [ bind_element; for_each.body ]; span } : block_stmt) in
  gen_for g ({ init = None; cond = Some cond; step = Some step; body; span } : for_stmt);
  pop_scope g

and gen_for g for_stmt =
  push_scope g;
  Option.iter
    (function
      | ForInitVarDecl v -> ignore (gen_stmt g (VarDecl v))
      | ForInitAssign a -> ignore (gen_stmt g (Assign a))
      | ForInitExpr e -> ignore (gen_expr g e))
    for_stmt.init;
  let cond_label = new_label g "for.cond" in
  let body_label = new_label g "for.body" in
  let step_label = new_label g "for.step" in
  let end_label = new_label g "for.end" in
  emit_br g cond_label;
  emit_label g cond_label;
  (match for_stmt.cond with
   | Some cond ->
       let v = gen_expr_as_bool g cond in
       emit_br_cond g v body_label end_label
   | None -> emit_br g body_label);
  emit_label g body_label;
  let saved_add_one = g.proven_i64_add_one in
  (match for_stmt.cond, for_stmt.step with
   | Some cond, Some (ForStepAssign step) ->
       (match lt_condition_name cond with
        | Some name
          when lookup_local_ty g name = Some Int
               && assignments_to_name name for_stmt.body = (0, 0)
               && assignment_is_add_one name step ->
            g.proven_i64_add_one <- StringSet.add name g.proven_i64_add_one
        | _ -> ())
   | _ -> ());
  g.loop_targets <- (step_label, end_label) :: g.loop_targets;
  let body_term = gen_stmt g for_stmt.body in
  g.loop_targets <- List.tl g.loop_targets;
  if not body_term then emit_br g step_label;
  emit_label g step_label;
  Option.iter
    (function
      | ForStepAssign a -> ignore (gen_stmt g (Assign a))
      | ForStepExpr e -> ignore (gen_expr g e))
    for_stmt.step;
  g.proven_i64_add_one <- saved_add_one;
  emit_br g cond_label;
  emit_label g end_label;
  pop_scope g

let runtime_decls =
  [ "declare void @xi_gc_set_mode(i32)"
  ; "declare void @xi_gc_collect()"
  ; "declare i64 @xi_gc_collection_count()"
  ; "declare i64 @xi_gc_live_count()"
  ; "declare ptr @xi_num_from_i64(i64)"
  ; "declare ptr @xi_num_from_decimal(ptr)"
  ; "declare ptr @xi_num_dec_from_decimal(ptr)"
  ; "declare ptr @xi_num_trunc_to_int(ptr)"
  ; "declare ptr @xi_num_from_f64(double)"
  ; "declare double @xi_num_as_f64(ptr)"
  ; "declare ptr @xi_softfloat_from_decimal(ptr, i32)"
  ; "declare ptr @xi_softfloat_coerce(ptr, i32)"
  ; "declare ptr @xi_softfloat_zero(i32)"
  ; "declare i64 @xi_num_as_i64(ptr)"
  ; "declare i1 @xi_num_try_i64(ptr, ptr)"
  ; "declare ptr @xi_num_size_bits(ptr, i1)"
  ; "declare ptr @xi_num_from_wide(ptr, i64, i1)"
  ; "declare void @xi_num_to_wide(ptr, ptr, i64)"
  ; "declare ptr @xi_num_add(ptr, ptr)"
  ; "declare ptr @xi_num_add_i64(ptr, i64)"
  ; "declare ptr @xi_num_sub(ptr, ptr)"
  ; "declare ptr @xi_num_sub_i64(ptr, i64)"
  ; "declare ptr @xi_num_i64_sub(i64, ptr)"
  ; "declare ptr @xi_num_mul(ptr, ptr)"
  ; "declare ptr @xi_num_mul_i64(ptr, i64)"
  ; "declare ptr @xi_num_div(ptr, ptr)"
  ; "declare ptr @xi_num_mod(ptr, ptr)"
  ; "declare ptr @xi_num_pow(ptr, ptr)"
  ; "declare ptr @xi_num_band(ptr, ptr)"
  ; "declare ptr @xi_num_bor(ptr, ptr)"
  ; "declare ptr @xi_num_bxor(ptr, ptr)"
  ; "declare ptr @xi_num_bnot(ptr)"
  ; "declare ptr @xi_num_clamp_nonneg(ptr)"
  ; "declare i1 @xi_num_lt(ptr, ptr)"
  ; "declare i1 @xi_num_le(ptr, ptr)"
  ; "declare i1 @xi_num_gt(ptr, ptr)"
  ; "declare i1 @xi_num_ge(ptr, ptr)"
  ; "declare i1 @xi_num_eq(ptr, ptr)"
  ; "declare i1 @xi_num_ne(ptr, ptr)"
  ; "declare i1 @xi_num_truthy(ptr)"
  ; "declare ptr @xi_str_concat(ptr, ptr)"
  ; "declare i1 @xi_str_eq(ptr, ptr)"
  ; "declare i64 @xi_str_len_i64(ptr)"
  ; "declare i64 @xi_str_char_code_at_i64(ptr, i64)"
  ; "declare ptr @xi_str_from_char_code_i64(i64)"
  ; "declare void @xi_mem_copy(ptr, ptr, ptr, ptr, ptr)"
  ; "declare void @xi_mem_set(ptr, ptr, ptr, ptr)"
  ; "declare ptr @xi_mem_cmp(ptr, ptr, ptr, ptr, ptr)"
  ; "declare ptr @xi_mem_read_be(ptr, ptr, ptr)"
  ; "declare ptr @xi_mem_read_le(ptr, ptr, ptr)"
  ; "declare void @xi_mem_write_be(ptr, ptr, ptr, ptr)"
  ; "declare void @xi_mem_write_le(ptr, ptr, ptr, ptr)"
  ; "declare ptr @xi_cptr_alloc(i64)"
  ; "declare void @xi_cptr_free(ptr)"
  ; "declare ptr @xi_cptr_read_cstr(ptr, i64)"
  ; "declare i64 @xi_cptr_write_cstr(ptr, i64, ptr)"
  ; "declare ptr @xi_closure_env_alloc(i64)"
  ; "declare ptr @xi_closure_new(ptr, ptr, i64)"
  ; "declare ptr @xi_closure_code(ptr)"
  ; "declare ptr @xi_closure_env(ptr)"
  ; "declare void @xi_io_write_num(ptr)"
  ; "declare void @xi_io_write_i64(i64)"
  ; "declare void @xi_io_write_u64(i64)"
  ; "declare void @xi_io_write_f64w(double, i32)"
  ; "declare void @xi_io_write_wide(ptr, i64, i1)"
  ; "declare void @xi_io_write_cstr(ptr)"
  ; "declare void @xi_io_write_bool(i32)"
  ; "declare void @xi_io_newline()"
  ; "declare void @xi_io_wrtr_begin()"
  ; "declare void @xi_io_wrtr_end()"
  ; "declare void @xi_io_flush()"
  ; "declare ptr @xi_arr_new()"
  ; "declare void @xi_arr_push_num(ptr, ptr)"
  ; "declare void @xi_arr_push_uint(ptr, ptr)"
  ; "declare void @xi_arr_push_num_i64(ptr, i64)"
  ; "declare void @xi_arr_push_uint_i64(ptr, i64)"
  ; "declare void @xi_arr_push_str(ptr, ptr)"
  ; "declare void @xi_arr_push_bool(ptr, i1)"
  ; "declare void @xi_arr_push_struct(ptr, ptr)"
  ; "declare void @xi_arr_push_raw(ptr, ptr)"
  ; "declare ptr @xi_arr_pop_num(ptr)"
  ; "declare ptr @xi_arr_pop_uint(ptr)"
  ; "declare ptr @xi_arr_pop_str(ptr)"
  ; "declare i1 @xi_arr_pop_bool(ptr)"
  ; "declare ptr @xi_arr_pop_struct(ptr)"
  ; "declare ptr @xi_arr_pop_raw(ptr)"
  ; "declare void @xi_arr_insert_num(ptr, ptr, ptr)"
  ; "declare void @xi_arr_insert_uint(ptr, ptr, ptr)"
  ; "declare void @xi_arr_insert_num_i64_at_i64(ptr, i64, i64)"
  ; "declare void @xi_arr_insert_uint_i64_at_i64(ptr, i64, i64)"
  ; "declare void @xi_arr_insert_str(ptr, ptr, ptr)"
  ; "declare void @xi_arr_insert_bool(ptr, ptr, i1)"
  ; "declare void @xi_arr_insert_struct(ptr, ptr, ptr)"
  ; "declare void @xi_arr_insert_raw(ptr, ptr, ptr)"
  ; "declare ptr @xi_arr_remove_num(ptr, ptr)"
  ; "declare ptr @xi_arr_remove_uint(ptr, ptr)"
  ; "declare ptr @xi_arr_remove_num_at_i64(ptr, i64)"
  ; "declare ptr @xi_arr_remove_uint_at_i64(ptr, i64)"
  ; "declare ptr @xi_arr_remove_str(ptr, ptr)"
  ; "declare i1 @xi_arr_remove_bool(ptr, ptr)"
  ; "declare ptr @xi_arr_remove_struct(ptr, ptr)"
  ; "declare ptr @xi_arr_remove_raw(ptr, ptr)"
  ; "declare void @xi_arr_clear(ptr)"
  ; "declare ptr @xi_arr_contains_num(ptr, ptr)"
  ; "declare ptr @xi_arr_contains_num_i64(ptr, i64)"
  ; "declare ptr @xi_arr_contains_str(ptr, ptr)"
  ; "declare ptr @xi_arr_contains_bool(ptr, i1)"
  ; "declare ptr @xi_arr_contains_struct(ptr, ptr)"
  ; "declare ptr @xi_arr_contains_raw(ptr, ptr)"
  ; "declare ptr @xi_arr_index_of_num(ptr, ptr)"
  ; "declare ptr @xi_arr_index_of_num_i64(ptr, i64)"
  ; "declare ptr @xi_arr_index_of_str(ptr, ptr)"
  ; "declare ptr @xi_arr_index_of_bool(ptr, i1)"
  ; "declare ptr @xi_arr_index_of_struct(ptr, ptr)"
  ; "declare ptr @xi_arr_index_of_raw(ptr, ptr)"
  ; "declare ptr @xi_arr_len(ptr)"
  ; "declare i64 @xi_arr_len_i64(ptr)"
  ; "declare i1 @xi_arr_all_num_fit_i64(ptr)"
  ; "declare ptr @xi_arr_get_num(ptr, ptr)"
  ; "declare ptr @xi_arr_get_uint(ptr, ptr)"
  ; "declare ptr @xi_arr_get_num_at_i64(ptr, i64)"
  ; "declare ptr @xi_arr_get_uint_at_i64(ptr, i64)"
  ; "declare i64 @xi_arr_get_num_i64_at_i64(ptr, i64)"
  ; "declare i64 @xi_arr_get_uint_i64_at_i64(ptr, i64)"
  ; "declare i64 @xi_arr_get_num_i64_at_i64_unchecked(ptr, i64)"
  ; "declare i64 @xi_arr_get_uint_i64_at_i64_unchecked(ptr, i64)"
  ; "declare ptr @xi_arr_get_str(ptr, ptr)"
  ; "declare i1 @xi_arr_get_bool(ptr, ptr)"
  ; "declare ptr @xi_arr_get_struct(ptr, ptr)"
  ; "declare ptr @xi_arr_get_raw(ptr, ptr)"
  ; "declare void @xi_arr_set_num(ptr, ptr, ptr)"
  ; "declare void @xi_arr_set_uint(ptr, ptr, ptr)"
  ; "declare void @xi_arr_set_num_at_i64(ptr, i64, ptr)"
  ; "declare void @xi_arr_set_uint_at_i64(ptr, i64, ptr)"
  ; "declare void @xi_arr_set_num_i64_at_i64(ptr, i64, i64)"
  ; "declare void @xi_arr_set_uint_i64_at_i64(ptr, i64, i64)"
  ; "declare void @xi_arr_set_num_zero_at_i64(ptr, i64)"
  ; "declare void @xi_arr_add_num_i64_at_i64(ptr, i64, i64)"
  ; "declare void @xi_arr_add_scaled_num_i64_at_i64(ptr, i64, i64, i64)"
  ; "declare void @xi_arr_sub_num_i64_at_i64(ptr, i64, i64)"
  ; "declare void @xi_arr_mul_num_i64_at_i64(ptr, i64, i64)"
  ; "declare i1 @xi_arr_cmp_num_i64_at_i64(ptr, i64, i64, i32)"
  ; "declare i1 @xi_arr_eq_num_i64_at_i64(ptr, i64, i64)"
  ; "declare i1 @xi_arr_ne_num_i64_at_i64(ptr, i64, i64)"
  ; "declare i1 @xi_arr_lt_num_i64_at_i64(ptr, i64, i64)"
  ; "declare i1 @xi_arr_le_num_i64_at_i64(ptr, i64, i64)"
  ; "declare i1 @xi_arr_gt_num_i64_at_i64(ptr, i64, i64)"
  ; "declare i1 @xi_arr_ge_num_i64_at_i64(ptr, i64, i64)"
  ; "declare i1 @xi_arr_num_nonzero_at_i64(ptr, i64)"
  ; "declare i1 @xi_arr_is_packed_signed_i64(ptr)"
  ; "declare i1 @xi_arr_packed_nonzero_at_i64(ptr, i64)"
  ; "declare void @xi_arr_set_num_i64_at_i64_unchecked(ptr, i64, i64)"
  ; "declare void @xi_arr_set_uint_i64_at_i64_unchecked(ptr, i64, i64)"
  ; "declare void @xi_arr_set_str(ptr, ptr, ptr)"
  ; "declare void @xi_arr_set_bool(ptr, ptr, i1)"
  ; "declare void @xi_arr_set_struct(ptr, ptr, ptr)"
  ; "declare void @xi_arr_set_raw(ptr, ptr, ptr)"
  ; "declare ptr @xi_arr_slice_num(ptr, ptr, ptr)"
  ; "declare ptr @xi_arr_slice_str(ptr, ptr, ptr)"
  ; "declare ptr @xi_arr_slice_bool(ptr, ptr, ptr)"
  ; "declare ptr @xi_arr_slice_struct(ptr, ptr, ptr)"
  ; "declare ptr @xi_arr_slice_raw(ptr, ptr, ptr)"
  ; "declare ptr @xi_arr_slice_from_num(ptr, ptr)"
  ; "declare ptr @xi_arr_slice_from_str(ptr, ptr)"
  ; "declare ptr @xi_arr_slice_from_bool(ptr, ptr)"
  ; "declare ptr @xi_arr_slice_from_struct(ptr, ptr)"
  ; "declare ptr @xi_arr_slice_from_raw(ptr, ptr)"
  ; "declare ptr @xi_map_new()"
  ; "declare ptr @xi_map_key_num(ptr)"
  ; "declare ptr @xi_map_key_bool(i1)"
  ; "declare void @xi_map_set_num(ptr, ptr, ptr)"
  ; "declare void @xi_map_set_str(ptr, ptr, ptr)"
  ; "declare void @xi_map_set_bool(ptr, ptr, i1)"
  ; "declare void @xi_map_set_struct(ptr, ptr, ptr)"
  ; "declare void @xi_map_set_raw(ptr, ptr, ptr)"
  ; "declare ptr @xi_map_get_num(ptr, ptr)"
  ; "declare ptr @xi_map_get_str(ptr, ptr)"
  ; "declare i1 @xi_map_get_bool(ptr, ptr)"
  ; "declare ptr @xi_map_get_struct(ptr, ptr)"
  ; "declare ptr @xi_map_get_raw(ptr, ptr)"
  ; "declare ptr @xi_map_get_or_num(ptr, ptr, ptr)"
  ; "declare ptr @xi_map_get_or_str(ptr, ptr, ptr)"
  ; "declare i1 @xi_map_get_or_bool(ptr, ptr, i1)"
  ; "declare ptr @xi_map_get_or_struct(ptr, ptr, ptr)"
  ; "declare ptr @xi_map_get_or_raw(ptr, ptr, ptr)"
  ; "declare ptr @xi_map_has(ptr, ptr)"
  ; "declare void @xi_map_del(ptr, ptr)"
  ; "declare void @xi_map_clear(ptr)"
  ; "declare ptr @xi_map_len(ptr)"
  ; "declare ptr @xi_map_keys(ptr)"
  ; "declare ptr @xi_map_keys_num(ptr)"
  ; "declare ptr @xi_map_keys_dec(ptr)"
  ; "declare ptr @xi_map_keys_bool(ptr)"
  ; "declare ptr @xi_map_values_num(ptr)"
  ; "declare ptr @xi_map_values_str(ptr)"
  ; "declare ptr @xi_map_values_bool(ptr)"
  ; "declare ptr @xi_map_values_struct(ptr)"
  ; "declare ptr @xi_map_values_raw(ptr)"
  ; "declare ptr @xi_struct_new()"
  ; "declare void @xi_struct_set(ptr, ptr, ptr)"
  ; "declare void @xi_struct_set_str(ptr, ptr, ptr)"
  ; "declare ptr @xi_struct_get(ptr, ptr)"
  ; "declare ptr @xi_arr_copy_deep(ptr)"
  ; "declare ptr @xi_map_copy_deep(ptr)"
  ; "declare ptr @xi_struct_copy_deep(ptr)"
  ]

(* ---------------------------------------------------------------------------
   Native-i64 promotion.

   Xi's bare `int` is arbitrary precision, so by default it is a boxed `xi_num`.
   The machinery below was written to promote a function's `int` parameters,
   locals, array accesses and return value to machine `i64`.

   Semantic invariant: promotion from adaptive `int` is legal only when every
   value that uses the promoted representation is proved to remain in the i64
   range on every path. It is not enough for literals to fit: parameters, call
   results and adaptive-array reads are runtime values, and `+`, `-`, `*` and
   unary negation can leave the range even when their inputs fit. On overflow
   Xi must grow the integer; wrapping, truncating or trapping is never permitted.

   The current whole-function analysis does not establish that invariant. It
   only applies the following useful-but-insufficient syntactic filters:

     - no `/`; checked `+`, `-`, `*`, and non-negative integer `^` restart in
       the boxed version on signed-i64 overflow, while negative exponents fall
       back for Xi's decimal-power semantics;
     - `%` guards zero and negative one before using the native remainder
       instruction, matching Xi's defined zero result without machine traps;
     - no casts at all, and no literal that does not itself fit in an i64;
     - no `dec` literal;
     - no `ref int` parameter or local, and no call that would bind one: a
       promoted slot holds a raw i64, so a boxed value aliased through a `ref`
       would be observed at the wrong representation;
     - indirect calls themselves cannot use the private fast ABI. Address-taken
       functions may still be versioned: their public symbol retains the boxed
       ABI expected by the generic function-pointer adapter and performs the
       same exact guards as any other public call.

   Promotion therefore uses versioning rather than replacing the boxed ABI. A
   public wrapper checks adaptive arguments exactly, a private i64 clone uses
   checked arithmetic and exact array-element reads, and any failed guard or
   overflow restarts in the boxed implementation. Only restart-safe functions
   are cloned, so fallback cannot duplicate observable effects.

   Every function is considered independently. Earlier versions gated the
   entire analysis on whether any function in the program indexed an array or
   walked a string. That made an unrelated array access determine whether a
   pure arithmetic function received a fast clone, and left arithmetic-only
   programs fully boxed despite satisfying all of the soundness checks below.

   Ported from the reference `xic` (Xi-rs, `xic/src/ir.rs`).
   --------------------------------------------------------------------------- *)

let is_hot_call_path segments =
  match segments with
  | [ "str"; ("len" | "char_code_at" | "from_char_code") ] -> true
  | [ "arr"; ("len" | "get" | "set" | "get_unchecked" | "set_unchecked") ] -> true
  | [ "__index"; "set" ] -> true
  | _ -> false

let rec stmt_has_i64_hotspot (stmt : Ast.stmt) =
  let opt_expr = function Some e -> expr_has_i64_hotspot e | None -> false in
  match stmt with
  | VarDecl v -> opt_expr v.init
  | Assign a -> expr_has_i64_hotspot a.value
  | Return r -> opt_expr r.value
  | Break _ | Continue _ -> false
  | If i ->
      expr_has_i64_hotspot i.cond
      || stmt_has_i64_hotspot i.then_branch
      || (match i.else_branch with Some s -> stmt_has_i64_hotspot s | None -> false)
  | Switch s ->
      expr_has_i64_hotspot s.selector
      || List.exists
           (fun (case : Ast.switch_case) ->
             expr_has_i64_hotspot case.value || List.exists stmt_has_i64_hotspot case.body)
           s.cases
      || (match s.default with
          | Some (d : Ast.switch_default) -> List.exists stmt_has_i64_hotspot d.body
          | None -> false)
  | While w -> expr_has_i64_hotspot w.cond || stmt_has_i64_hotspot w.body
  | ForEach f -> expr_has_i64_hotspot f.collection || stmt_has_i64_hotspot f.body
  | For f ->
      (match f.init with
       | Some (ForInitVarDecl v) -> opt_expr v.init
       | Some (ForInitAssign a) -> expr_has_i64_hotspot a.value
       | Some (ForInitExpr e) -> expr_has_i64_hotspot e
       | None -> false)
      || opt_expr f.cond
      || (match f.step with
          | Some (ForStepAssign a) -> expr_has_i64_hotspot a.value
          | Some (ForStepExpr e) -> expr_has_i64_hotspot e
          | None -> false)
      || stmt_has_i64_hotspot f.body
  | IoChain io ->
      List.exists (function IoExpr e -> expr_has_i64_hotspot e | Endl _ -> false) io.items
  | ExprStmt e -> expr_has_i64_hotspot e.expr
  | Block b -> List.exists stmt_has_i64_hotspot b.statements

and expr_has_i64_hotspot (expr : Ast.expr) =
  match expr.kind with
  | Call call ->
      let hot = match call.callee.kind with Path p -> is_hot_call_path p.segments | _ -> false in
      hot || expr_has_i64_hotspot call.callee || List.exists expr_has_i64_hotspot call.args
  | Index _ -> true
  | ArrayLiteral arr -> List.exists expr_has_i64_hotspot arr.items
  | StructLiteral st ->
      List.exists (fun (f : Ast.struct_literal_field_expr) -> expr_has_i64_hotspot f.value) st.fields
  | Closure c -> stmt_has_i64_hotspot c.body
  | Member m -> expr_has_i64_hotspot m.base
  | Unary u -> expr_has_i64_hotspot u.rhs
  | Binary b -> expr_has_i64_hotspot b.lhs || expr_has_i64_hotspot b.rhs
  | Cast c -> expr_has_i64_hotspot c.value
  | Grouping inner | Try inner -> expr_has_i64_hotspot inner
  | Literal _ | Path _ -> false

let rec address_taken_stmt (stmt : Ast.stmt) names out =
  let expr e = address_taken_expr e names out in
  let stmt' s = address_taken_stmt s names out in
  match stmt with
  | VarDecl v -> Option.iter expr v.init
  | Assign a -> expr a.value
  | Return r -> Option.iter expr r.value
  | Break _ | Continue _ -> ()
  | If i ->
      expr i.cond;
      stmt' i.then_branch;
      Option.iter stmt' i.else_branch
  | Switch s ->
      expr s.selector;
      List.iter
        (fun (case : Ast.switch_case) ->
          expr case.value;
          List.iter stmt' case.body)
        s.cases;
      Option.iter (fun (d : Ast.switch_default) -> List.iter stmt' d.body) s.default
  | While w ->
      expr w.cond;
      stmt' w.body
  | ForEach f ->
      expr f.collection;
      stmt' f.body
  | For f ->
      Option.iter
        (function
          | ForInitVarDecl v -> Option.iter expr v.init
          | ForInitAssign a -> expr a.value
          | ForInitExpr e -> expr e)
        f.init;
      Option.iter expr f.cond;
      Option.iter (function ForStepAssign a -> expr a.value | ForStepExpr e -> expr e) f.step;
      stmt' f.body
  | IoChain io -> List.iter (function IoExpr e -> expr e | Endl _ -> ()) io.items
  | ExprStmt e -> expr e.expr
  | Block b -> List.iter stmt' b.statements

and address_taken_expr (expr : Ast.expr) names out =
  let recur e = address_taken_expr e names out in
  match expr.kind with
  | Path { segments = [ name ]; _ } -> if StringSet.mem name names then out := StringSet.add name !out
  | Call call ->
      (match call.callee.kind with
       | Path { segments = [ _ ]; _ } -> ()
       | _ -> recur call.callee);
      List.iter recur call.args
  | Index i ->
      recur i.base;
      recur i.index
  | Member m -> recur m.base
  | Unary u -> recur u.rhs
  | Binary b ->
      recur b.lhs;
      recur b.rhs
  | Cast c -> recur c.value
  | Grouping inner | Try inner -> recur inner
  | ArrayLiteral arr -> List.iter recur arr.items
  | StructLiteral st -> List.iter (fun (f : Ast.struct_literal_field_expr) -> recur f.value) st.fields
  | Closure c -> address_taken_stmt c.body names out
  | Literal _ | Path _ -> ()

let collect_address_taken_functions (program : Ast.program) =
  let names =
    List.fold_left
      (fun acc (f : Ast.function_decl) -> StringSet.add f.name acc)
      StringSet.empty program.functions
  in
  let out = ref StringSet.empty in
  List.iter (fun (f : Ast.function_decl) -> address_taken_stmt f.body names out) program.functions;
  !out

(* Whether a literal is representable as an i64, which is what decides promotion.
   Same parse as everywhere else; see `parse_i64`. *)
let literal_fits_i64 v = parse_i64 v <> None

let is_bare_int (ty : Ast.type_ref) = ty.kind = Ast.Int None

(* Positions of every `ref int` parameter, per function name. A call that binds
   one of these disqualifies the *caller*: the argument's slot would have to
   hold a boxed value for the callee to write through it. *)
let collect_ref_int_params (program : Ast.program) =
  let table = Hashtbl.create 16 in
  List.iter
    (fun (f : Ast.function_decl) ->
      let positions =
        List.mapi (fun i p -> (i, p)) f.params
        |> List.filter_map (fun (i, (p : Ast.param)) ->
               if p.binding = Ref && is_bare_int p.ty then Some i else None)
      in
      Hashtbl.replace table f.name positions)
    program.functions;
  table

type fast_i64_env =
  { fi_ref_int_params : (string, int list) Hashtbl.t
  ; fi_oversized : StringSet.t
  }

let call_feeds_ref_int (call : Ast.call_expr) env =
  match call.callee.kind with
  | Path { segments = [ name ]; _ } ->
      (match Hashtbl.find_opt env.fi_ref_int_params name with
       | Some positions ->
           let argc = List.length call.args in
           List.exists (fun i -> i < argc) positions
       | None -> false)
  | _ -> false

let builtin_call_may_be_oversized segments =
  match segments with
  | [ "bit"; ("shl" | "rotl" | "rotr") ] -> true
  | [ "mem"; ("read_be" | "read_le") ] -> true
  | [ "cptr"; ("read_u64" | "addr") ] -> true
  | [ "cast" ] -> true
  | _ -> false

let rec expr_may_be_oversized (expr : Ast.expr) oversized =
  let recur e = expr_may_be_oversized e oversized in
  match expr.kind with
  | Literal (LitInt v) -> not (literal_fits_i64 v)
  | Grouping inner | Try inner -> recur inner
  | Unary u -> (match u.op with Not -> false | Neg | BitNot -> recur u.rhs)
  | Binary b -> recur b.lhs || recur b.rhs
  | Call call ->
      (match path_segments call.callee with
       | Some segments ->
           builtin_call_may_be_oversized segments
           || StringSet.mem (String.concat "::" segments) oversized
           || List.exists recur call.args
       | None -> List.exists recur call.args)
  | _ -> false

let rec stmt_returns_oversized (stmt : Ast.stmt) oversized =
  let recur s = stmt_returns_oversized s oversized in
  match stmt with
  | Return r ->
      (match r.value with Some e -> expr_may_be_oversized e oversized | None -> false)
  | If i ->
      recur i.then_branch
      || (match i.else_branch with Some s -> recur s | None -> false)
  | Switch s ->
      List.exists (fun (case : Ast.switch_case) -> List.exists recur case.body) s.cases
      || (match s.default with
          | Some (d : Ast.switch_default) -> List.exists recur d.body
          | None -> false)
  | While w -> recur w.body
  | ForEach f -> recur f.body
  | For f -> recur f.body
  | Block b -> List.exists recur b.statements
  | VarDecl _ | Assign _ | Break _ | Continue _ | IoChain _ | ExprStmt _ -> false

let collect_oversized_functions (program : Ast.program) =
  let oversized = ref StringSet.empty in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (f : Ast.function_decl) ->
        if (not (StringSet.mem f.name !oversized))
           && stmt_returns_oversized f.body !oversized
        then begin
          oversized := StringSet.add f.name !oversized;
          changed := true
        end)
      program.functions
  done;
  !oversized

let call_result_fits_i64 (call : Ast.call_expr) env =
  match path_segments call.callee with
  | None -> true
  | Some segments ->
      not (builtin_call_may_be_oversized segments
           || StringSet.mem (String.concat "::" segments) env.fi_oversized)

let rec stmt_allows_fast_i64_int (stmt : Ast.stmt) env =
  let allows_expr e = expr_allows_fast_i64_int e env in
  let allows_stmt s = stmt_allows_fast_i64_int s env in
  let allows_opt = function Some e -> allows_expr e | None -> true in
  match stmt with
  | VarDecl v -> if v.binding <> Value && is_bare_int v.ty then false else allows_opt v.init
  | Assign a -> a.mode = Normal && allows_expr a.value
  | Return r -> allows_opt r.value
  | Break _ | Continue _ -> true
  | If i ->
      allows_expr i.cond
      && allows_stmt i.then_branch
      && (match i.else_branch with Some s -> allows_stmt s | None -> true)
  | Switch s ->
      allows_expr s.selector
      && List.for_all
           (fun (case : Ast.switch_case) ->
             allows_expr case.value && List.for_all allows_stmt case.body)
           s.cases
      && (match s.default with
          | Some (d : Ast.switch_default) -> List.for_all allows_stmt d.body
          | None -> true)
  | While w -> allows_expr w.cond && allows_stmt w.body
  | ForEach f -> allows_expr f.collection && allows_stmt f.body
  | For f ->
      (match f.init with
       | Some (ForInitVarDecl v) ->
           (v.binding = Value || not (is_bare_int v.ty)) && allows_opt v.init
       | Some (ForInitAssign a) -> a.mode = Normal && allows_expr a.value
       | Some (ForInitExpr e) -> allows_expr e
       | None -> true)
      && allows_opt f.cond
      && (match f.step with
          | Some (ForStepAssign a) -> a.mode = Normal && allows_expr a.value
          | Some (ForStepExpr e) -> allows_expr e
          | None -> true)
      && allows_stmt f.body
  | IoChain io -> List.for_all (function IoExpr e -> allows_expr e | Endl _ -> true) io.items
  | ExprStmt e -> allows_expr e.expr
  | Block b -> List.for_all allows_stmt b.statements

and expr_allows_fast_i64_int (expr : Ast.expr) env =
  let allows e = expr_allows_fast_i64_int e env in
  match expr.kind with
  | Literal (LitInt v) -> literal_fits_i64 v
  | Literal (LitDec _) -> false
  | Literal _ | Path _ -> true
  | ArrayLiteral arr -> List.for_all allows arr.items
  | StructLiteral st -> List.for_all (fun (f : Ast.struct_literal_field_expr) -> allows f.value) st.fields
  | Closure c -> stmt_allows_fast_i64_int c.body env
  | Call call ->
      (not (call_feeds_ref_int call env))
      && call_result_fits_i64 call env
      && allows call.callee
      && List.for_all allows call.args
  | Index i -> allows i.base && allows i.index
  | Member m -> allows m.base
  | Unary u -> (match u.op with Not -> true | Neg | BitNot -> allows u.rhs)
  | Binary b ->
      let op_ok =
        match b.op with
        | Div -> false
        | Pow -> true
        | Mod -> true
        | _ -> true
      in
      op_ok && allows b.lhs && allows b.rhs
  | Cast _ -> false
  | Grouping inner -> allows inner
  | Try _ -> false

let function_allows_fast_i64_int (f : Ast.function_decl) env =
  List.for_all (fun (p : Ast.param) -> p.binding = Value || not (is_bare_int p.ty)) f.params
  && stmt_allows_fast_i64_int f.body env

let rec expr_calls_only_fast program_names fast_names (expr : Ast.expr) =
  let recur = expr_calls_only_fast program_names fast_names in
  match expr.kind with
  | Call call ->
      let callee_ok =
        match path_segments call.callee with
        | Some [ name ] when StringSet.mem name program_names ->
            StringSet.mem name fast_names
        | _ -> recur call.callee
      in
      callee_ok && List.for_all recur call.args
  | Index i -> recur i.base && recur i.index
  | Member m -> recur m.base
  | Unary u -> recur u.rhs
  | Binary b -> recur b.lhs && recur b.rhs
  | Cast c -> recur c.value
  | Grouping inner | Try inner -> recur inner
  | ArrayLiteral a -> List.for_all recur a.items
  | StructLiteral s ->
      List.for_all (fun (f : Ast.struct_literal_field_expr) -> recur f.value) s.fields
  | Closure c -> stmt_calls_only_fast program_names fast_names c.body
  | Literal _ | Path _ -> true

and stmt_calls_only_fast program_names fast_names (stmt : Ast.stmt) =
  let expr = expr_calls_only_fast program_names fast_names in
  let recur = stmt_calls_only_fast program_names fast_names in
  let opt = function Some e -> expr e | None -> true in
  match stmt with
  | VarDecl v -> opt v.init
  | Assign a -> expr a.value
  | Return r -> opt r.value
  | Break _ | Continue _ -> true
  | If i -> expr i.cond && recur i.then_branch && Option.fold ~none:true ~some:recur i.else_branch
  | Switch s ->
      expr s.selector
      && List.for_all
           (fun (c : Ast.switch_case) -> expr c.value && List.for_all recur c.body)
           s.cases
      && Option.fold ~none:true
           ~some:(fun (d : Ast.switch_default) -> List.for_all recur d.body)
           s.default
  | While w -> expr w.cond && recur w.body
  | ForEach f -> expr f.collection && recur f.body
  | For f ->
      (match f.init with
       | None -> true
       | Some (ForInitVarDecl v) -> opt v.init
       | Some (ForInitAssign a) -> expr a.value
       | Some (ForInitExpr e) -> expr e)
      && opt f.cond
      && (match f.step with
          | None -> true
          | Some (ForStepAssign a) -> expr a.value
          | Some (ForStepExpr e) -> expr e)
      && recur f.body
  | IoChain io ->
      List.for_all (function IoExpr e -> expr e | Endl _ -> true) io.items
  | ExprStmt e -> expr e.expr
  | Block b -> List.for_all recur b.statements

let rec expr_is_restart_safe restart_safe_calls array_names (expr : Ast.expr) =
  match expr.kind with
  | Literal _ | Path _ -> true
  | Grouping inner -> expr_is_restart_safe restart_safe_calls array_names inner
  | Unary u -> expr_is_restart_safe restart_safe_calls array_names u.rhs
  | Binary b ->
      expr_is_restart_safe restart_safe_calls array_names b.lhs
      && expr_is_restart_safe restart_safe_calls array_names b.rhs
  | Index i ->
      (match i.base.kind with
       | Path { segments = [ name ]; _ } -> StringSet.mem name array_names
       | _ -> false)
      && expr_is_restart_safe restart_safe_calls array_names i.index
  | Member _ -> false
  | Call call ->
      (match path_segments call.callee with
       | Some [ "arr"; ("get" | "get_unchecked" | "len") ]
       | Some [ "str"; ("len" | "char_code_at") ] ->
           List.for_all (expr_is_restart_safe restart_safe_calls array_names) call.args
       | Some [ name ] when StringSet.mem name restart_safe_calls ->
           List.for_all (expr_is_restart_safe restart_safe_calls array_names) call.args
       | _ -> false)
  | ArrayLiteral _ | StructLiteral _ | Closure _ | Cast _ | Try _ -> false

let rec stmt_is_restart_safe restart_safe_calls array_names (stmt : Ast.stmt) =
  let expr = expr_is_restart_safe restart_safe_calls array_names in
  let recur = stmt_is_restart_safe restart_safe_calls array_names in
  let opt = function Some e -> expr e | None -> true in
  match stmt with
  | VarDecl v -> v.binding = Value && opt v.init
  | Assign a -> a.mode = Normal && expr a.value
  | Return r -> opt r.value
  | Break _ | Continue _ -> true
  | If i ->
      expr i.cond
      && recur i.then_branch
      && Option.fold ~none:true ~some:recur i.else_branch
  | Switch s ->
      expr s.selector
      && List.for_all
           (fun (case : Ast.switch_case) ->
             expr case.value && List.for_all recur case.body)
           s.cases
      && Option.fold ~none:true
           ~some:(fun (d : Ast.switch_default) ->
             List.for_all recur d.body)
           s.default
  | While w -> expr w.cond && recur w.body
  | ForEach f -> expr f.collection && recur f.body
  | For f ->
      (match f.init with
       | None -> true
       | Some (ForInitVarDecl v) -> v.binding = Value && opt v.init
       | Some (ForInitAssign a) -> a.mode = Normal && expr a.value
       | Some (ForInitExpr e) -> expr e)
      && opt f.cond
      && (match f.step with
          | None -> true
          | Some (ForStepAssign a) -> a.mode = Normal && expr a.value
          | Some (ForStepExpr e) -> expr e)
      && recur f.body
  | Block b -> List.for_all recur b.statements
  | IoChain _ | ExprStmt _ -> false

let adaptive_int_array_names (f : Ast.function_decl) =
  let arrays = ref StringSet.empty in
  let others = ref StringSet.empty in
  let add name (ty : Ast.type_ref) =
    match ty.kind with
    | Ast.Array (Ast.Int None) -> arrays := StringSet.add name !arrays
    | _ -> others := StringSet.add name !others
  in
  List.iter (fun (p : Ast.param) -> add p.name p.ty) f.params;
  let rec walk = function
    | VarDecl v -> add v.name v.ty
    | If i -> walk i.then_branch; Option.iter walk i.else_branch
    | Switch s ->
        List.iter (fun (c : Ast.switch_case) -> List.iter walk c.body) s.cases;
        Option.iter (fun (d : Ast.switch_default) -> List.iter walk d.body) s.default
    | While w -> walk w.body
    | ForEach x -> others := StringSet.add x.name !others; walk x.body
    | For x ->
        (match x.init with Some (ForInitVarDecl v) -> add v.name v.ty | _ -> ());
        walk x.body
    | Block b -> List.iter walk b.statements
    | Assign _ | Return _ | Break _ | Continue _ | IoChain _ | ExprStmt _ -> ()
  in
  walk f.body;
  StringSet.diff !arrays !others

let collect_index_cursor_i64_locals (f : Ast.function_decl) =
  let declarations = Hashtbl.create 16 in
  let candidates = Hashtbl.create 16 in
  let rec collect = function
    | VarDecl v ->
        Hashtbl.replace declarations v.name
          (1 + Option.value (Hashtbl.find_opt declarations v.name) ~default:0);
        (match v.binding, v.ty.kind, v.init with
         | Value, Ast.Int None, Some { kind = Literal (LitInt text); _ }
           when Option.is_some (parse_i64 text) -> Hashtbl.replace candidates v.name ()
         | _ -> ())
    | If x -> collect x.then_branch; Option.iter collect x.else_branch
    | Switch x ->
        List.iter (fun (c : Ast.switch_case) -> List.iter collect c.body) x.cases;
        Option.iter (fun (d : Ast.switch_default) -> List.iter collect d.body) x.default
    | While x -> collect x.body
    | ForEach x -> collect x.body
    | For x ->
        Option.iter (function ForInitVarDecl v -> collect (VarDecl v) | _ -> ()) x.init;
        collect x.body
    | Block x -> List.iter collect x.statements
    | Assign _ | Return _ | Break _ | Continue _ | IoChain _ | ExprStmt _ -> ()
  in
  collect f.body;
  let is_local name expr =
    match expr.kind with Path { segments = [ found ]; _ } -> found = name | _ -> false
  in
  let is_fitting_literal expr =
    match expr.kind with Literal (LitInt text) -> Option.is_some (parse_i64 text) | _ -> false
  in
  let rec expr_ok name ~as_index expr =
    match expr.kind with
    | Path { segments = [ found ]; _ } -> found <> name || as_index
    | Index x -> expr_ok name ~as_index:false x.base && expr_ok name ~as_index:true x.index
    | Call x ->
        (match path_segments x.callee, x.args with
         | Some [ "__index"; "set" ], base :: index :: value :: _ ->
             expr_ok name ~as_index:false base
             && expr_ok name ~as_index:true index
             && expr_ok name ~as_index:false value
         | Some [ "arr"; ("get" | "get_unchecked") ], arr :: index :: _ ->
             expr_ok name ~as_index:false arr && expr_ok name ~as_index:true index
         | Some [ "arr"; ("set" | "set_unchecked") ], arr :: index :: value :: _ ->
             expr_ok name ~as_index:false arr
             && expr_ok name ~as_index:true index
             && expr_ok name ~as_index:false value
         | _ ->
             expr_ok name ~as_index:false x.callee
             && List.for_all (expr_ok name ~as_index:false) x.args)
    | Binary x when as_index ->
        expr_ok name ~as_index:true x.lhs && expr_ok name ~as_index:true x.rhs
    | Binary x -> expr_ok name ~as_index:false x.lhs && expr_ok name ~as_index:false x.rhs
    | Unary x -> expr_ok name ~as_index x.rhs
    | Grouping inner -> expr_ok name ~as_index inner
    | Member x -> expr_ok name ~as_index:false x.base
    | Cast x -> expr_ok name ~as_index:false x.value
    | Try inner -> expr_ok name ~as_index:false inner
    | ArrayLiteral x -> List.for_all (expr_ok name ~as_index:false) x.items
    | StructLiteral x ->
        List.for_all
          (fun (field : Ast.struct_literal_field_expr) ->
            expr_ok name ~as_index:false field.value)
          x.fields
    | Closure _ -> false
    | Literal _ | Path _ -> true
  in
  let update_ok name value =
    match value.kind with
    | Binary { op = (Add | Sub); lhs; rhs } ->
        is_local name lhs && is_fitting_literal rhs
    | _ -> false
  in
  let rec stmt_ok name = function
    | VarDecl v ->
        v.name = name || Option.fold ~none:true ~some:(expr_ok name ~as_index:false) v.init
    | Assign a ->
        (match a.target.segments with
         | [ target ] when target = name -> a.mode = Normal && update_ok name a.value
         | _ -> expr_ok name ~as_index:false a.value)
    | Return x -> Option.fold ~none:true ~some:(expr_ok name ~as_index:false) x.value
    | If x ->
        expr_ok name ~as_index:false x.cond && stmt_ok name x.then_branch
        && Option.fold ~none:true ~some:(stmt_ok name) x.else_branch
    | Switch x ->
        expr_ok name ~as_index:false x.selector
        && List.for_all
             (fun (c : Ast.switch_case) ->
               expr_ok name ~as_index:false c.value && List.for_all (stmt_ok name) c.body)
             x.cases
        && Option.fold ~none:true
             ~some:(fun (d : Ast.switch_default) -> List.for_all (stmt_ok name) d.body)
             x.default
    | While x -> expr_ok name ~as_index:false x.cond && stmt_ok name x.body
    | ForEach x -> expr_ok name ~as_index:false x.collection && stmt_ok name x.body
    | For x ->
        Option.fold ~none:true
          ~some:(function
            | ForInitVarDecl v -> stmt_ok name (VarDecl v)
            | ForInitAssign a -> stmt_ok name (Assign a)
            | ForInitExpr e -> expr_ok name ~as_index:false e)
          x.init
        && Option.fold ~none:true ~some:(expr_ok name ~as_index:false) x.cond
        && Option.fold ~none:true
             ~some:(function
               | ForStepAssign a -> stmt_ok name (Assign a)
               | ForStepExpr e -> expr_ok name ~as_index:false e)
             x.step
        && stmt_ok name x.body
    | IoChain x ->
        List.for_all
          (function IoExpr e -> expr_ok name ~as_index:false e | Endl _ -> true)
          x.items
    | ExprStmt x -> expr_ok name ~as_index:false x.expr
    | Block x -> List.for_all (stmt_ok name) x.statements
    | Break _ | Continue _ -> true
  in
  Hashtbl.fold
    (fun name () out ->
      if Hashtbl.find_opt declarations name = Some 1 && stmt_ok name f.body
      then StringSet.add name out else out)
    candidates StringSet.empty

(* Effectful functions cannot use restart-on-overflow versioning: replaying a
   print, mutation, or foreign call would be observable.  A canonical bounded
   induction variable needs no restart, however.  If it starts at an i64
   constant, is modified only by [i = i + 1] inside [while (i < LIMIT)], and
   LIMIT itself fits i64, then the increment is mathematically bounded by
   LIMIT.  Keep just that local in an i64 slot; uses at boxed boundaries are
   converted normally by [ensure_num]. *)
let collect_effect_i64_locals (f : Ast.function_decl) =
  let declarations = Hashtbl.create 16 in
  let initializers = Hashtbl.create 16 in
  let rec collect_decls = function
    | VarDecl v ->
        Hashtbl.replace declarations v.name
          (1 + Option.value (Hashtbl.find_opt declarations v.name) ~default:0);
        (match v.binding, v.ty.kind, v.init with
         | Value, Ast.Int None, Some { kind = Literal (LitInt value); _ } ->
             Option.iter (fun parsed -> Hashtbl.replace initializers v.name parsed)
               (parse_i64 value)
         | _ -> ())
    | If i -> collect_decls i.then_branch; Option.iter collect_decls i.else_branch
    | Switch s ->
        List.iter (fun (c : Ast.switch_case) -> List.iter collect_decls c.body) s.cases;
        Option.iter (fun (d : Ast.switch_default) -> List.iter collect_decls d.body) s.default
    | While w -> collect_decls w.body
    | ForEach x -> collect_decls x.body
    | For x ->
        Option.iter
          (function ForInitVarDecl v -> collect_decls (VarDecl v) | _ -> ())
          x.init;
        collect_decls x.body
    | Block b -> List.iter collect_decls b.statements
    | Assign _ | Return _ | Break _ | Continue _ | IoChain _ | ExprStmt _ -> ()
  in
  collect_decls f.body;
  let rec expr_contains name expr =
    let recur = expr_contains name in
    match expr.kind with
    | Path { segments = [ found ]; _ } -> found = name
    | Call c -> recur c.callee || List.exists recur c.args
    | Index i -> recur i.base || recur i.index
    | Member m -> recur m.base
    | Unary u -> recur u.rhs
    | Binary b -> recur b.lhs || recur b.rhs
    | Cast c -> recur c.value
    | Grouping inner | Try inner -> recur inner
    | ArrayLiteral a -> List.exists recur a.items
    | StructLiteral s ->
        List.exists (fun (field : Ast.struct_literal_field_expr) -> recur field.value) s.fields
    | Closure c -> stmt_contains name c.body
    | Literal _ | Path _ -> false
  and stmt_contains name stmt =
    let expr = expr_contains name in
    let recur = stmt_contains name in
    match stmt with
    | VarDecl v -> Option.fold ~none:false ~some:expr v.init
    | Assign a -> List.mem name a.target.segments || expr a.value
    | Return r -> Option.fold ~none:false ~some:expr r.value
    | If i -> expr i.cond || recur i.then_branch || Option.fold ~none:false ~some:recur i.else_branch
    | Switch s ->
        expr s.selector
        || List.exists
             (fun (c : Ast.switch_case) -> expr c.value || List.exists recur c.body)
             s.cases
        || Option.fold ~none:false
             ~some:(fun (d : Ast.switch_default) -> List.exists recur d.body)
             s.default
    | While w -> expr w.cond || recur w.body
    | ForEach x -> expr x.collection || recur x.body
    | For x ->
        Option.fold ~none:false
          ~some:(function
            | ForInitVarDecl v -> Option.fold ~none:false ~some:expr v.init
            | ForInitAssign a -> expr a.value
            | ForInitExpr e -> expr e)
          x.init
        || Option.fold ~none:false ~some:expr x.cond
        || Option.fold ~none:false
             ~some:(function ForStepAssign a -> expr a.value | ForStepExpr e -> expr e)
             x.step
        || recur x.body
    | IoChain io -> List.exists (function IoExpr e -> expr e | Endl _ -> false) io.items
    | ExprStmt e -> expr e.expr
    | Block b -> List.exists recur b.statements
    | Break _ | Continue _ -> false
  in
  let rec has_hazard name stmt =
    let expr_hazard expr =
      let rec walk expr =
        match expr.kind with
        | Call c ->
            let may_alias_local =
              match path_segments c.callee with
              | Some [ _ ] | None -> List.exists (expr_contains name) c.args
              | Some _ -> false
            in
            may_alias_local || walk c.callee || List.exists walk c.args
        | Closure c -> stmt_contains name c.body || has_hazard name c.body
        | Index i -> walk i.base || walk i.index
        | Member m -> walk m.base
        | Unary u -> walk u.rhs
        | Binary b -> walk b.lhs || walk b.rhs
        | Cast c -> walk c.value
        | Grouping inner | Try inner -> walk inner
        | ArrayLiteral a -> List.exists walk a.items
        | StructLiteral s ->
            List.exists (fun (field : Ast.struct_literal_field_expr) -> walk field.value) s.fields
        | Literal _ | Path _ -> false
      in
      walk expr
    in
    let recur = has_hazard name in
    match stmt with
    | VarDecl v ->
        ((v.binding = Ref || v.binding = Link)
         && Option.fold ~none:false ~some:(expr_contains name) v.init)
        || Option.fold ~none:false ~some:expr_hazard v.init
    | Assign a -> expr_hazard a.value
    | Return r -> Option.fold ~none:false ~some:expr_hazard r.value
    | If i -> expr_hazard i.cond || recur i.then_branch || Option.fold ~none:false ~some:recur i.else_branch
    | Switch s ->
        expr_hazard s.selector
        || List.exists
             (fun (c : Ast.switch_case) -> expr_hazard c.value || List.exists recur c.body)
             s.cases
        || Option.fold ~none:false
             ~some:(fun (d : Ast.switch_default) -> List.exists recur d.body)
             s.default
    | While w -> expr_hazard w.cond || recur w.body
    | ForEach x -> expr_hazard x.collection || recur x.body
    | For x ->
        Option.fold ~none:false
          ~some:(function
            | ForInitVarDecl v -> Option.fold ~none:false ~some:expr_hazard v.init
            | ForInitAssign a -> expr_hazard a.value
            | ForInitExpr e -> expr_hazard e)
          x.init
        || Option.fold ~none:false ~some:expr_hazard x.cond
        || Option.fold ~none:false
             ~some:(function ForStepAssign a -> expr_hazard a.value | ForStepExpr e -> expr_hazard e)
             x.step
        || recur x.body
    | IoChain io -> List.exists (function IoExpr e -> expr_hazard e | Endl _ -> false) io.items
    | ExprStmt e -> expr_hazard e.expr
    | Block b -> List.exists recur b.statements
    | Break _ | Continue _ -> false
  in
  let bounded = ref StringSet.empty in
  let bounds = Hashtbl.create 16 in
  let rec resolve_bound expr =
    match expr.kind with
    | Literal (LitInt limit) -> Option.map (fun upper -> upper, None) (parse_i64 limit)
    | Grouping inner -> resolve_bound inner
    | Path { segments = [ bound_name ]; _ } ->
        (match Hashtbl.find_opt initializers bound_name with
         | Some upper
           when Hashtbl.find_opt declarations bound_name = Some 1
                && assignments_to_name bound_name f.body = (0, 0)
                && not (has_hazard bound_name f.body) ->
             Some (upper, Some bound_name)
         | _ -> None)
    | _ -> None
  in
  let add_if_bounded name bound assignments_in_loop =
    match Hashtbl.find_opt initializers name, resolve_bound bound with
    | Some initial, Some (upper, bound_name)
      when Hashtbl.find_opt declarations name = Some 1
           && Int64.compare initial upper <= 0
           && assignments_to_name name f.body = (1, 1)
           && assignments_in_loop = (1, 1)
           && not (has_hazard name f.body) ->
        bounded := StringSet.add name !bounded;
        Hashtbl.replace bounds name (initial, upper);
        Option.iter
          (fun local ->
            bounded := StringSet.add local !bounded;
            Hashtbl.replace bounds local (upper, upper))
          bound_name
    | _ -> ()
  in
  let rec find_loops = function
    | While w ->
        (match w.cond.kind with
         | Binary { op = Lt; lhs; rhs }
           when Option.is_some (expr_as_local_name lhs) ->
             let name = Option.get (expr_as_local_name lhs) in
             add_if_bounded name rhs (assignments_to_name name w.body)
         | _ -> ());
        find_loops w.body
    | If i -> find_loops i.then_branch; Option.iter find_loops i.else_branch
    | Switch s ->
        List.iter (fun (c : Ast.switch_case) -> List.iter find_loops c.body) s.cases;
        Option.iter (fun (d : Ast.switch_default) -> List.iter find_loops d.body) s.default
    | ForEach x -> find_loops x.body
    | For x ->
        (match x.init, x.cond, x.step with
         | Some (ForInitVarDecl init),
           Some { kind = Binary { op = Lt; lhs; rhs }; _ },
           Some (ForStepAssign step)
           when init.name = Option.value (expr_as_local_name lhs) ~default:""
                && assignment_is_add_one init.name step ->
             add_if_bounded init.name rhs
               (let body_count = assignments_to_name init.name x.body in
                (fst body_count + 1, snd body_count + 1))
         | _ -> ());
        find_loops x.body
    | Block b -> List.iter find_loops b.statements
    | VarDecl _ | Assign _ | Return _ | Break _ | Continue _ | IoChain _ | ExprStmt _ -> ()
  in
  find_loops f.body;
  StringSet.union !bounded (collect_index_cursor_i64_locals f), bounds

let collect_adaptive_state_locals (f : Ast.function_decl) =
  let declarations = Hashtbl.create 16 in
  let candidates = ref StringSet.empty in
  let hazards = ref StringSet.empty in
  let rec names_in_expr expr =
    match expr.kind with
    | Path { segments = [ name ]; _ } -> StringSet.singleton name
    | Call c ->
        List.fold_left
          (fun acc arg -> StringSet.union acc (names_in_expr arg))
          (names_in_expr c.callee) c.args
    | Index x -> StringSet.union (names_in_expr x.base) (names_in_expr x.index)
    | Member x -> names_in_expr x.base
    | Unary x -> names_in_expr x.rhs
    | Binary x -> StringSet.union (names_in_expr x.lhs) (names_in_expr x.rhs)
    | Cast x -> names_in_expr x.value
    | Grouping x | Try x -> names_in_expr x
    | ArrayLiteral x ->
        List.fold_left (fun acc e -> StringSet.union acc (names_in_expr e))
          StringSet.empty x.items
    | StructLiteral x ->
        List.fold_left
          (fun acc (field : Ast.struct_literal_field_expr) ->
            StringSet.union acc (names_in_expr field.value))
          StringSet.empty x.fields
    | Closure _ | Literal _ | Path _ -> StringSet.empty
  in
  let rec expr_hazards expr =
    match expr.kind with
    | Call c ->
        List.iter
          (fun arg -> hazards := StringSet.union !hazards (names_in_expr arg))
          c.args;
        expr_hazards c.callee;
        List.iter expr_hazards c.args
    | Closure c -> hazards := StringSet.union !hazards (names_in_stmt c.body)
    | Index x -> expr_hazards x.base; expr_hazards x.index
    | Member x -> expr_hazards x.base
    | Unary x -> expr_hazards x.rhs
    | Binary x -> expr_hazards x.lhs; expr_hazards x.rhs
    | Cast x -> expr_hazards x.value
    | Grouping x | Try x -> expr_hazards x
    | ArrayLiteral x -> List.iter expr_hazards x.items
    | StructLiteral x ->
        List.iter (fun (field : Ast.struct_literal_field_expr) -> expr_hazards field.value) x.fields
    | Literal _ | Path _ -> ()
  and names_in_stmt stmt =
    let add_expr acc expr = StringSet.union acc (names_in_expr expr) in
    match stmt with
    | VarDecl v -> Option.fold ~none:StringSet.empty ~some:names_in_expr v.init
    | Assign a -> names_in_expr a.value
    | Return r -> Option.fold ~none:StringSet.empty ~some:names_in_expr r.value
    | If x ->
        let names = add_expr (names_in_stmt x.then_branch) x.cond in
        Option.fold ~none:names ~some:(fun s -> StringSet.union names (names_in_stmt s)) x.else_branch
    | Switch x ->
        List.fold_left
          (fun acc (c : Ast.switch_case) ->
            List.fold_left (fun a s -> StringSet.union a (names_in_stmt s))
              (add_expr acc c.value) c.body)
          (names_in_expr x.selector) x.cases
    | While x -> add_expr (names_in_stmt x.body) x.cond
    | ForEach x -> add_expr (names_in_stmt x.body) x.collection
    | For x -> names_in_stmt x.body
    | IoChain x ->
        List.fold_left
          (fun acc -> function IoExpr e -> add_expr acc e | Endl _ -> acc)
          StringSet.empty x.items
    | ExprStmt x -> names_in_expr x.expr
    | Block x ->
        List.fold_left (fun acc s -> StringSet.union acc (names_in_stmt s))
          StringSet.empty x.statements
    | Break _ | Continue _ -> StringSet.empty
  and walk stmt =
    match stmt with
    | VarDecl v ->
        Hashtbl.replace declarations v.name
          (1 + Option.value (Hashtbl.find_opt declarations v.name) ~default:0);
        (match v.binding, v.ty.kind, v.init with
         | Value, Ast.Int None, Some { kind = Literal (LitInt text); _ }
           when Option.is_some (parse_i64 text) ->
             candidates := StringSet.add v.name !candidates
         | (Ref | Link), _, Some init ->
             hazards := StringSet.union !hazards (names_in_expr init)
         | _ -> ());
        Option.iter expr_hazards v.init
    | Assign a -> expr_hazards a.value
    | Return r -> Option.iter expr_hazards r.value
    | If x -> expr_hazards x.cond; walk x.then_branch; Option.iter walk x.else_branch
    | Switch x ->
        expr_hazards x.selector;
        List.iter (fun (c : Ast.switch_case) -> expr_hazards c.value; List.iter walk c.body) x.cases;
        Option.iter (fun (d : Ast.switch_default) -> List.iter walk d.body) x.default
    | While x -> expr_hazards x.cond; walk x.body
    | ForEach x -> expr_hazards x.collection; walk x.body
    | For x -> walk x.body
    | IoChain x -> List.iter (function IoExpr e -> expr_hazards e | Endl _ -> ()) x.items
    | ExprStmt x -> expr_hazards x.expr
    | Block x -> List.iter walk x.statements
    | Break _ | Continue _ -> ()
  in
  walk f.body;
  StringSet.filter
    (fun name -> Hashtbl.find_opt declarations name = Some 1
                 && not (StringSet.mem name !hazards))
    !candidates

let collect_fast_i64_int_functions (program : Ast.program) (sem : semantic_result) =
  let env =
    { fi_ref_int_params = collect_ref_int_params program
    ; fi_oversized = collect_oversized_functions program
    }
  in
  let restart_safe_calls =
    let safe =
      ref
        (List.fold_left
           (fun names (f : Ast.function_decl) -> StringSet.add f.name names)
           StringSet.empty program.functions)
    in
    let changed = ref true in
    while !changed do
      changed := false;
      List.iter
        (fun (f : Ast.function_decl) ->
          if StringSet.mem f.name !safe
             && not
                  (stmt_is_restart_safe !safe (adaptive_int_array_names f) f.body)
          then begin
            safe := StringSet.remove f.name !safe;
            changed := true
          end)
        program.functions
    done;
    !safe
  in
  let signature_is_supported name =
    match List.assoc_opt name sem.function_sigs with
    | Some sig_ ->
        sig_.fs_return_ty = Int
        && List.for_all
             (fun p ->
               p.ps_binding = Value
               && native_width_of p.ps_ty = None
               && native_float_width_of p.ps_ty = None
               && p.ps_ty <> Bool)
             sig_.fs_params
    | None -> false
  in
  let program_names =
    List.fold_left
      (fun names (f : Ast.function_decl) -> StringSet.add f.name names)
      StringSet.empty program.functions
  in
  let fast =
    ref
      (List.fold_left
         (fun acc (f : Ast.function_decl) ->
           if f.name <> "main"
              && signature_is_supported f.name
              && List.for_all (fun (p : Ast.param) -> p.binding = Value) f.params
              && StringSet.mem f.name restart_safe_calls
              && function_allows_fast_i64_int f env
           then StringSet.add f.name acc
           else acc)
         StringSet.empty program.functions)
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (f : Ast.function_decl) ->
        if StringSet.mem f.name !fast
           && not (stmt_calls_only_fast program_names !fast f.body)
        then begin
          fast := StringSet.remove f.name !fast;
          changed := true
        end)
      program.functions
  done;
  !fast

let make_generator ~fast_i64_int_functions ~gc_mode program (sem : semantic_result) =
  let string_literals, next_string_id = collect_string_literals program in
  let function_sigs =
    Hashtbl.of_seq (List.to_seq sem.function_sigs)
  in
  let module_fns =
    Hashtbl.create (max 16 (List.length sem.modules.mt_fns))
  in
  List.iter
    (fun sig_ ->
      Hashtbl.replace module_fns (sig_.mfs_module, sig_.mfs_name) sig_)
    sem.modules.mt_fns;
  let module_consts =
    Hashtbl.create (max 8 (List.length sem.modules.mt_consts))
  in
  List.iter
    (fun sig_ ->
      Hashtbl.replace module_consts (sig_.mcs_module, sig_.mcs_name) sig_)
    sem.modules.mt_consts;
  { program
  ; sem
  ; gc_mode
  ; function_sigs
  ; module_fns
  ; module_consts
  ; temp_counter = 0
  ; label_counter = 0
  ; local_scopes = { bindings = Hashtbl.create 64; undo = [] }
  ; link_groups = Hashtbl.create 16
  ; group_members = Hashtbl.create 16
  ; next_group_id = 0
  ; fn_return_ty = Void
  ; fn_is_entry_main = false
  ; string_literals
  ; next_string_id
  ; fast_i64_int_functions
  ; fn_uses_fast_i64_int = false
  ; fn_fast_result_out = None
  ; fn_i64_overflow_label = None
  ; proven_i64_add_one = StringSet.empty
  ; effect_i64_locals = StringSet.empty
  ; effect_i64_bounds = Hashtbl.create 16
  ; adaptive_state_locals = StringSet.empty
  ; inline_guarded_packed_ops = true
  ; bounds_proofs = []
  ; discarded_call = None
  ; loop_targets = []
  ; closure_counter = 0
  ; fn_adapter_names = Hashtbl.create 16
  ; lir_label = ""
  ; lir_instrs = []
  ; lir_blocks = []
  ; lir_entry_allocas = []
  ; lir_funcs = []
  ; lir_generated_funcs = []
  ; lir_decls = []
  ; lir_function_analysis_time = 0.
  ; lir_optimization_time = 0.
  ; lir_sink = None
  }

let decl_symbol line =
  match String.index_opt line '@' with
  | None -> None
  | Some at ->
      let rest = String.sub line (at + 1) (String.length line - at - 1) in
      (match String.index_opt rest '(' with
       | Some p -> Some (String.sub rest 0 p)
       | None -> Some rest)

let runtime_decl_symbols =
  List.fold_left
    (fun acc line -> match decl_symbol line with Some s -> StringSet.add s acc | None -> acc)
    StringSet.empty runtime_decls

let runtime_lir_decls = List.map Lir.decl_of_string runtime_decls

let emit_lir_decl g decl =
  g.lir_decls <- decl :: g.lir_decls

let emit_runtime_decls g =
  List.iter (emit_lir_decl g) runtime_lir_decls;
  let decls =
    List.filter_map
      (fun sig_ ->
        if StringSet.mem sig_.mfs_symbol runtime_decl_symbols then None
        else begin
          let d_params =
            List.map
              (fun p -> lir_ty (if p.ps_binding = Ref then "ptr" else extern_abi_ty p.ps_ty))
              sig_.mfs_params
          in
          let d_param_exts =
            List.map
              (fun p -> if p.ps_binding = Ref then Lir.No_ext else c_abi_ext p.ps_ty)
              sig_.mfs_params
          in
          Some
            { Lir.d_name = sig_.mfs_symbol
            ; d_ret = lir_ty (extern_abi_ty sig_.mfs_return_ty)
            ; d_ret_ext = c_abi_ext sig_.mfs_return_ty
            ; d_params
            ; d_param_exts
            }
        end)
      g.sem.modules.mt_fns
    @ List.filter_map
        (fun c ->
          if StringSet.mem c.mcs_symbol runtime_decl_symbols then None
          else
            Some
              { Lir.d_name = c.mcs_symbol
              ; d_ret = lir_ty (extern_abi_ty c.mcs_ty)
              ; d_ret_ext = c_abi_ext c.mcs_ty
              ; d_params = []
              ; d_param_exts = []
              })
        g.sem.modules.mt_consts
  in
  decls |> List.sort_uniq compare |> List.iter (emit_lir_decl g)

let fallback_function_sig (function_ : Ast.function_decl) =
  { fs_name = function_.name
  ; fs_params = []
  ; fs_return_ty = Void
  ; fs_span = function_.span
  }

let rec statement_count = function
  | If x ->
      1 + statement_count x.then_branch
      + Option.fold ~none:0 ~some:statement_count x.else_branch
  | Switch x ->
      1
      + List.fold_left
          (fun total (c : Ast.switch_case) ->
            total + List.fold_left (fun n stmt -> n + statement_count stmt) 0 c.body)
          0 x.cases
      + Option.fold ~none:0
          ~some:(fun (d : Ast.switch_default) ->
            List.fold_left (fun n stmt -> n + statement_count stmt) 0 d.body)
          x.default
  | While x -> 1 + statement_count x.body
  | ForEach x -> 1 + statement_count x.body
  | For x -> 1 + statement_count x.body
  | Block x ->
      1 + List.fold_left (fun total stmt -> total + statement_count stmt) 0 x.statements
  | VarDecl _ | Assign _ | Return _ | Break _ | Continue _ | IoChain _ | ExprStmt _ -> 1

let emit_function ?symbol ?(fast = false) ?boxed_fallback g
    (function_ : Ast.function_decl) =
  let analysis_started = Clock.now () in
  Hashtbl.reset g.local_scopes.bindings;
  g.local_scopes.undo <- [];
  Hashtbl.clear g.link_groups;
  Hashtbl.clear g.group_members;
  g.next_group_id <- 0;
  lir_reset_function g;
  push_scope g;
  let sig_ =
    match function_sig_opt g function_.name with
    | Some sig_ -> sig_
    | None -> fallback_function_sig function_
  in
  g.fn_return_ty <- sig_.fs_return_ty;
  g.fn_is_entry_main <- function_.name = "main" && not fast;
  g.fn_uses_fast_i64_int <- fast;
  g.inline_guarded_packed_ops <- statement_count function_.body <= 512;
  g.fn_fast_result_out <- if fast && g.fn_return_ty = Int then Some "%__xi_result" else None;
  g.fn_i64_overflow_label <-
    (match boxed_fallback with Some _ -> Some (new_label g "i64.overflow") | None -> None);
  g.proven_i64_add_one <- StringSet.empty;
  let effect_locals, effect_bounds =
    if fast then StringSet.empty, Hashtbl.create 0
    else collect_effect_i64_locals function_
  in
  g.effect_i64_locals <- effect_locals;
  g.effect_i64_bounds <- effect_bounds;
  g.adaptive_state_locals <-
    if fast then StringSet.empty
    else StringSet.diff (collect_adaptive_state_locals function_) effect_locals;
  g.lir_function_analysis_time <-
    g.lir_function_analysis_time +. (Clock.now () -. analysis_started);
  let params =
    List.map
      (fun (param : Ast.param) ->
        let pname = sanitize_ident param.name in
        let param_ty = sem_type_from_typeref param.ty g.sem in
        let pname = "%" ^ pname in
        if param.binding = Ref then (Lir.Ptr, pname ^ ".slot")
        else if fast_i64_int_ty g param_ty then (Lir.I 64, pname)
        else
          match native_width_of param_ty, native_float_width_of param_ty, param_ty with
          | Some width, _, _ -> (Lir.I width, pname)
          | _, Some width, _ -> (Lir.F width, pname)
          | _, _, Bool -> (Lir.I 1, pname)
          | _ -> (Lir.Ptr, pname))
      function_.params
    @ (match g.fn_fast_result_out with Some out -> [ (Lir.Ptr, out) ] | None -> [])
  in
  let ret =
    if g.fn_is_entry_main then Lir.I 32
    else if g.fn_fast_result_out <> None then Lir.I 1
    else if g.fn_uses_fast_i64_int && g.fn_return_ty = Int then Lir.I 64
    else lir_ty (ir_return_ty g.fn_return_ty)
  in
  emit_label g "entry";
  if g.fn_is_entry_main then
    emit_call_void g "@xi_gc_set_mode" [ arg_i32 (string_of_int g.gc_mode) ];
  List.iter
    (fun (param : Ast.param) ->
      let pname = sanitize_ident param.name in
      let param_ty = sem_type_from_typeref param.ty g.sem in
      if param.binding = Ref then
        declare_local g param.name ("%" ^ pname ^ ".slot") param.binding param_ty
      else if fast_i64_int_ty g param_ty then begin
        let slot = new_temp_named g (pname ^ ".addr") in
        emit_entry_alloca_native g slot 64;
        emit_store g "i64" ("%" ^ pname) slot 8;
        declare_local g param.name slot param.binding param_ty
      end
      else
        match native_width_of param_ty, native_float_width_of param_ty, param_ty with
        | Some width, _, _ ->
            let slot = new_temp_named g (pname ^ ".addr") in
            emit_entry_alloca_native g slot width;
            emit_store g (llvm_int_ty width) ("%" ^ pname) slot (native_align width);
            declare_local g param.name slot param.binding param_ty
        | _, Some width, _ ->
            let slot = new_temp_named g (pname ^ ".addr") in
            emit_entry_alloca_float g slot width;
            emit_store g (llvm_float_ty width) ("%" ^ pname) slot (max 1 (width / 8));
            declare_local g param.name slot param.binding param_ty
        | _, _, Bool ->
            let slot = new_temp_named g (pname ^ ".addr") in
            emit_entry_alloca_bool g slot;
            emit_store g "i1" ("%" ^ pname) slot 1;
            declare_local g param.name slot param.binding param_ty
        | _ ->
            let slot = new_temp_named g (pname ^ ".addr") in
            emit_entry_alloca g slot;
            emit_store g "ptr" ("%" ^ pname) slot 8;
            declare_local g param.name slot param.binding param_ty)
    function_.params;
  let terminated = gen_stmt g function_.body in
  if not terminated then emit_default_return g;
  (match boxed_fallback, g.fn_i64_overflow_label with
   | Some _, Some overflow_label ->
       emit_label g overflow_label;
       emit_ret g "i1" "false"
   | _ -> ());
  let param_exts =
    List.map
      (fun (param : Ast.param) ->
        let param_ty = sem_type_from_typeref param.ty g.sem in
        if param.binding = Ref || fast_i64_int_ty g param_ty then Lir.No_ext
        else c_abi_ext param_ty)
      function_.params
    @ (match g.fn_fast_result_out with Some _ -> [ Lir.No_ext ] | None -> [])
  in
  let ret_ext =
    if g.fn_is_entry_main || g.fn_fast_result_out <> None
       || (g.fn_uses_fast_i64_int && g.fn_return_ty = Int)
    then Lir.No_ext
    else c_abi_ext g.fn_return_ty
  in
  lir_finish_function ~ret_ext ~param_exts g
    (Option.value symbol ~default:(sanitize_ident function_.name)) ret params

let emit_i64_wrapper g (function_ : Ast.function_decl) ~fast_symbol ~boxed_symbol =
  lir_reset_function g;
  let sig_ =
    match function_sig_opt g function_.name with
    | Some sig_ -> sig_
    | None -> fallback_function_sig function_
  in
  let params =
    List.map
      (fun (param : Ast.param) ->
        let ty = sem_type_from_typeref param.ty g.sem in
        let name = "%" ^ sanitize_ident param.name in
        match native_width_of ty, native_float_width_of ty, ty with
        | Some width, _, _ -> (Lir.I width, name)
        | _, Some width, _ -> (Lir.F width, name)
        | _, _, Bool -> (Lir.I 1, name)
        | _ -> (Lir.Ptr, name))
      function_.params
  in
  let param_exts =
    List.map
      (fun (param : Ast.param) ->
        c_abi_ext (sem_type_from_typeref param.ty g.sem))
      function_.params
  in
  emit_label g "entry";
  let slow_label = new_label g "i64.guard.slow" in
  let exact_slots = Hashtbl.create (List.length function_.params) in
  List.iter
    (fun (param : Ast.param) ->
      let ty = sem_type_from_typeref param.ty g.sem in
      if ty = Int then begin
        let slot = new_temp_named g (sanitize_ident param.name ^ ".exact.i64") in
        emit_entry_alloca_native g slot 64;
        Hashtbl.add exact_slots param.name slot;
        let boxed = "%" ^ sanitize_ident param.name in
        let ok = emit_call_i1 g "@xi_num_try_i64" [ arg_ptr boxed; arg_ptr slot ] in
        let next_label = new_label g "i64.guard.next" in
        emit_br_cond g ok next_label slow_label;
        emit_label g next_label
      end
      else if ty = ArrayInt None then begin
        let array = "%" ^ sanitize_ident param.name in
        let ok = emit_call_i1 g "@xi_arr_all_num_fit_i64" [ arg_ptr array ] in
        let next_label = new_label g "i64.array.guard.next" in
        emit_br_cond g ok next_label slow_label;
        emit_label g next_label
      end)
    function_.params;
  let fast_args =
    List.map
      (fun (param : Ast.param) ->
        let ty = sem_type_from_typeref param.ty g.sem in
        let name = "%" ^ sanitize_ident param.name in
        if ty = Int then begin
          let value = new_temp g in
          emit_load g value "i64" (Hashtbl.find exact_slots param.name) 8;
          arg_i64 value
        end
        else
          match native_width_of ty, native_float_width_of ty, ty with
          | Some width, _, _ -> arg_int width name
          | _, Some width, _ -> arg_float width name
          | _, _, Bool -> arg_i1 name
          | _ -> arg_ptr name)
      function_.params
  in
  let fast_result_slot = new_temp_named g "i64.fast.result" in
  emit_entry_alloca_native g fast_result_slot 64;
  let fast_ok =
    emit_call_i1 g ("@" ^ fast_symbol) (fast_args @ [ arg_ptr fast_result_slot ])
  in
  let fast_success_label = new_label g "i64.fast.success" in
  emit_br_cond g fast_ok fast_success_label slow_label;
  emit_label g fast_success_label;
  let fast_result = new_temp g in
  emit_load g fast_result "i64" fast_result_slot 8;
  emit_ret g "ptr" (box_native_int g fast_result 64 true);

  emit_label g slow_label;
  let boxed_args =
    List.map
      (fun (param : Ast.param) ->
        let ty = sem_type_from_typeref param.ty g.sem in
        let name = "%" ^ sanitize_ident param.name in
        match native_width_of ty, native_float_width_of ty, ty with
        | Some width, _, _ -> arg_int width name
        | _, Some width, _ -> arg_float width name
        | _, _, Bool -> arg_i1 name
        | _ -> arg_ptr name)
      function_.params
  in
  let boxed_result = emit_call_ptr g ("@" ^ boxed_symbol) boxed_args in
  emit_ret g "ptr" boxed_result;
  lir_finish_function ~ret_ext:(c_abi_ext sig_.fs_return_ty) ~param_exts g
    (sanitize_ident function_.name) Lir.Ptr params

let lir_globals g =
  Hashtbl.fold (fun value name acc -> (value, name) :: acc) g.string_literals []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.map (fun (value, global_name) ->
         { Lir.g_name = strip_at global_name; g_bytes = value ^ "\000"; g_align = 1 })

type lir_generation_timings =
  { native_i64_promotion : float
  ; setup_and_declarations : float
  ; function_analysis : float
  ; ast_and_helper_generation : float
  ; lir_optimization : float
  ; module_finalization : float
  ; validation : float
  }

let generate_lir_with_optimization_timing ?on_func ?(gc_mode = 0) (program : Ast.program) sem =
  let optimization_started = Clock.now () in
  let fast_i64_int_functions = collect_fast_i64_int_functions program sem in
  let native_i64_promotion =
    Clock.now () -. optimization_started
  in
  let setup_started = Clock.now () in
  let g = make_generator ~fast_i64_int_functions ~gc_mode program sem in
  g.lir_sink <- on_func;
  emit_runtime_decls g;
  let setup_and_declarations = Clock.now () -. setup_started in
  let functions_started = Clock.now () in
  List.iter
    (fun (function_ : Ast.function_decl) ->
      if StringSet.mem function_.name fast_i64_int_functions then begin
        let stem = sanitize_ident function_.name in
        let boxed_symbol = "__xi_boxed_" ^ stem in
        let fast_symbol = "__xi_i64_" ^ stem in
        emit_function ~symbol:boxed_symbol g function_;
        emit_function ~symbol:fast_symbol ~fast:true
          ~boxed_fallback:boxed_symbol g function_;
        emit_i64_wrapper g function_ ~fast_symbol ~boxed_symbol
      end
      else emit_function g function_)
    program.functions;
  let functions_elapsed = Clock.now () -. functions_started in
  let finalization_started = Clock.now () in
  (match on_func with
   | Some sink -> List.iter sink (List.rev g.lir_generated_funcs)
   | None -> ());
  let lir =
    { Lir.decls = List.rev g.lir_decls
    ; funcs = List.rev g.lir_funcs @ List.rev g.lir_generated_funcs
    ; globals = lir_globals g
    }
  in
  let module_finalization = Clock.now () -. finalization_started in
  let validation =
    if Build_config.preset = "debug" then begin
      let started = Clock.now () in
      Lir.validate lir;
      Clock.now () -. started
    end
    else 0.
  in
  let timings =
    { native_i64_promotion
    ; setup_and_declarations
    ; function_analysis = g.lir_function_analysis_time
    ; ast_and_helper_generation =
        max 0.
          (functions_elapsed
           -. g.lir_function_analysis_time
           -. g.lir_optimization_time)
    ; lir_optimization = g.lir_optimization_time
    ; module_finalization
    ; validation
    }
  in
  (lir, timings)

let generate_lir ?on_func ?gc_mode program sem =
  fst (generate_lir_with_optimization_timing ?on_func ?gc_mode program sem)

let generate_ir program sem = Lir.to_llvm_text (generate_lir program sem)

let generate_ir_and_lir program sem =
  let lir = generate_lir program sem in
  (Lir.to_llvm_text lir, lir)
