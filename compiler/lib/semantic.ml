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

type param_sig = { name : string; ty : sem_type; binding : binding_kind }

type function_sig =
  { name : string
  ; params : param_sig list
  ; return_ty : sem_type
  ; span : Span.t
  }

type fn_ptr_sig = { params : sem_type list; return_ty : sem_type }
type struct_field_sig = { name : string; ty : sem_type }

type struct_sig =
  { id : int
  ; name : string
  ; fields : struct_field_sig list
  ; field_index : (string * int) list
  ; span : Span.t
  }

type cstruct_field_sig = { cfs_name : string; cfs_ty : sem_type; cfs_offset : int }

type cstruct_sig =
  { cs_id : int
  ; cs_name : string
  ; cs_fields : cstruct_field_sig list
  ; cs_size : int
  ; cs_align : int
  ; cs_span : Span.t
  }

type module_fn_sig =
  { module_ : string
  ; name : string
  ; params : param_sig list
  ; return_ty : sem_type
  ; symbol : string
  }

type module_const_sig =
  { module_ : string
  ; name : string
  ; ty : sem_type
  ; symbol : string
  }

type module_table =
  { fns : ((string * string) * module_fn_sig) list
  ; consts : ((string * string) * module_const_sig) list
  ; intrinsics : (string * string) list
  }

type semantic_result =
  { function_sigs : (string * function_sig) list
  ; fn_ptr_sigs : fn_ptr_sig list
  ; struct_sigs_by_name : (string * struct_sig) list
  ; struct_sigs_by_id : (int * struct_sig) list
  ; cstruct_sigs_by_name : (string * cstruct_sig) list
  ; cstruct_sigs_by_id : (int * cstruct_sig) list
  ; modules : module_table
  ; uses_gc : bool
  ; gc_use_spans : Span.t list
  ; diagnostics : Diagnostic.t list
  }

type local_symbol = { local_ty : sem_type; local_binding : binding_kind }

type local_scope =
  { mutable bindings : (string * local_symbol) list
  ; mutable index : (string, local_symbol) Hashtbl.t option
  }

type analyzer =
  { function_sigs : (string, function_sig) Hashtbl.t
  ; mutable fn_ptr_sigs : fn_ptr_sig list
  ; mutable struct_ids : (string * int) list
  ; mutable struct_sigs_by_name : (string * struct_sig) list
  ; mutable struct_sigs_by_id : (int * struct_sig) list
  ; mutable cstruct_ids : (string * int) list
  ; mutable cstruct_sigs_by_name : (string * cstruct_sig) list
  ; mutable cstruct_sigs_by_id : (int * cstruct_sig) list
  ; mutable cstruct_decls : Ast.struct_decl list
  ; mutable modules : module_table
  ; module_fns : (string * string, module_fn_sig) Hashtbl.t
  ; module_consts : (string * string, module_const_sig) Hashtbl.t
  ; module_intrinsics : (string * string, unit) Hashtbl.t
  ; included_modules : StringSet.t
  ; mutable diagnostics : Diagnostic.t list
  ; mutable scopes : local_scope list
  ; mutable current_return_exprs : sem_type list
  ; mutable current_return_void : bool
  ; mutable cfn_refs : (string * Span.t) list
  ; mutable loop_depth : int
  ; mutable uses_try : bool
  ; mutable uses_gc : bool
  ; mutable gc_use_spans : Span.t list
  }

let empty_modules = { fns = []; consts = []; intrinsics = [] }

let diag code message span = Diagnostic.make code message span
let push_diag a code message span = a.diagnostics <- diag code message span :: a.diagnostics
let assoc_opt key xs = List.find_map (fun (k, v) -> if k = key then Some v else None) xs

let replace_assoc key value xs =
  let rec loop acc = function
    | [] -> List.rev ((key, value) :: acc)
    | (k, _) :: rest when k = key -> List.rev_append acc ((key, value) :: rest)
    | item :: rest -> loop (item :: acc) rest
  in
  loop [] xs

let mem_assoc key xs = List.exists (fun (k, _) -> k = key) xs

let function_sig_opt a name = Hashtbl.find_opt a.function_sigs name
let has_function a name = Hashtbl.mem a.function_sigs name

let string_exists f s =
  let rec loop i = i < String.length s && (f s.[i] || loop (i + 1)) in
  loop 0

let is_valid_int_width width =
  width >= 8 && width mod 8 = 0 && width <= 65535

let is_native_float_width = function 16 | 32 | 64 -> true | _ -> false

let c_abi_boundary_ok ~allow_void = function
  | Bool | Str | Cptr | CStruct _ -> true
  | IntN w | UintN w -> List.mem w [ 8; 16; 32; 64 ]
  | DecN w -> is_native_float_width w
  | Void -> allow_void
  | Error -> true
  | _ -> false

let fixed_numeric_bit_width = function
  | IntN width | UintN width | DecN width -> Some width
  | _ -> None

let is_unsigned_int = function Uint | UintN _ -> true | _ -> false

let rec sem_type_to_string = function
  | Int -> "Int"
  | IntN w -> Printf.sprintf "IntN(%d)" w
  | Uint -> "Uint"
  | UintN w -> Printf.sprintf "UintN(%d)" w
  | Dec -> "Dec"
  | DecN w -> Printf.sprintf "DecN(%d)" w
  | Bool -> "Bool"
  | Str -> "Str"
  | Cptr -> "Cptr"
  | ArrayInt None -> "ArrayInt"
  | ArrayInt (Some w) -> Printf.sprintf "ArrayInt(%d)" w
  | ArrayUint None -> "ArrayUint"
  | ArrayUint (Some w) -> Printf.sprintf "ArrayUint(%d)" w
  | ArrayDec -> "ArrayDec"
  | ArrayBool -> "ArrayBool"
  | ArrayStr -> "ArrayStr"
  | ArrayStruct id -> Printf.sprintf "ArrayStruct(%d)" id
  | ArrayFnPtr id -> Printf.sprintf "ArrayFnPtr(%d)" id
  | ArrayUnknown -> "ArrayUnknown"
  | MapInt -> "MapInt"
  | MapDec -> "MapDec"
  | MapBool -> "MapBool"
  | MapStr -> "MapStr"
  | MapStruct id -> Printf.sprintf "MapStruct(%d)" id
  | MapFnPtr id -> Printf.sprintf "MapFnPtr(%d)" id
  | MapKeyed (key, value) ->
      Printf.sprintf "MapKeyed(%s,%s)" (map_key_type_name key) (map_value_type_name value [])
  | MapUnknown -> "MapUnknown"
  | Struct id -> Printf.sprintf "Struct(%d)" id
  | CStruct id -> Printf.sprintf "CStruct(%d)" id
  | FnPtr id -> Printf.sprintf "FnPtr(%d)" id
  | Result payload -> Printf.sprintf "Result(%s)" (sem_type_to_string payload)
  | ResultUnknown -> "ResultUnknown"
  | Void -> "Void"
  | NumUnknown -> "NumUnknown"
  | Error -> "Error"

and map_key_type_name = function KeyInt -> "int" | KeyDec -> "dec" | KeyBool -> "bool"

and map_value_type_name value (structs : (int * struct_sig) list) =
  match value with
  | ValueInt -> "int"
  | ValueDec -> "dec"
  | ValueBool -> "bool"
  | ValueStr -> "string"
  | ValueStruct id ->
      (match assoc_opt id structs with Some sig_ -> sig_.name | None -> "struct")
  | ValueFnPtr _ -> "fn"
  | ValueUnknown -> "unknown"

let rec xi_type_name (structs : (int * struct_sig) list) = function
  | Int | IntN _ -> "int"
  | Uint | UintN _ -> "uint"
  | Dec | DecN _ -> "dec"
  | Bool -> "bool"
  | Str -> "string"
  | Cptr -> "cptr"
  | ArrayInt None -> "array<int>"
  | ArrayInt (Some width) -> Printf.sprintf "array<int%d>" width
  | ArrayUint None -> "array<uint>"
  | ArrayUint (Some width) -> Printf.sprintf "array<uint%d>" width
  | ArrayDec -> "array<dec>"
  | ArrayBool -> "array<bool>"
  | ArrayStr -> "array<string>"
  | ArrayStruct id ->
      Printf.sprintf "array<%s>"
        (match assoc_opt id structs with Some sig_ -> sig_.name | None -> "struct")
  | ArrayFnPtr _ -> "array<fn>"
  | ArrayUnknown -> "array<unknown>"
  | MapInt -> "map<string, int>"
  | MapDec -> "map<string, dec>"
  | MapBool -> "map<string, bool>"
  | MapStr -> "map<string, string>"
  | MapStruct id ->
      Printf.sprintf "map<string, %s>"
        (match assoc_opt id structs with Some sig_ -> sig_.name | None -> "struct")
  | MapFnPtr _ -> "map<string, fn>"
  | MapKeyed (key, value) ->
      Printf.sprintf "map<%s, %s>" (map_key_type_name key) (map_value_type_name value structs)
  | MapUnknown -> "map<string, unknown>"
  | Struct id -> (match assoc_opt id structs with Some sig_ -> sig_.name | None -> "struct")
  | CStruct _ -> "cstruct"
  | FnPtr _ -> "fn"
  | Result payload -> Printf.sprintf "result<%s>" (xi_type_name structs payload)
  | ResultUnknown -> "result<unknown>"
  | Void -> "void"
  | NumUnknown -> "number"
  | Error -> "error"

let boundary_type_desc structs = function
  | IntN w -> Printf.sprintf "int%d" w
  | UintN w -> Printf.sprintf "uint%d" w
  | DecN w -> Printf.sprintf "dec%d" w
  | ty -> xi_type_name structs ty

let sem_type_from_map_value_kind = function
  | ValueInt -> Some Int
  | ValueDec -> Some Dec
  | ValueBool -> Some Bool
  | ValueStr -> Some Str
  | ValueStruct id -> Some (Struct id)
  | ValueFnPtr id -> Some (FnPtr id)
  | ValueUnknown -> None

let is_numeric = function
  | Int | IntN _ | Uint | UintN _ | Dec | DecN _ | NumUnknown -> true
  | _ -> false

let is_integer = function
  | Int | IntN _ | Uint | UintN _ | NumUnknown -> true
  | _ -> false

let is_result_type = function Result _ | ResultUnknown -> true | _ -> false

let is_array_type = function
  | ArrayInt _ | ArrayUint _ | ArrayDec | ArrayBool | ArrayStr | ArrayStruct _ | ArrayFnPtr _
  | ArrayUnknown -> true
  | _ -> false

let is_numeric_array = function
  | ArrayInt _ | ArrayUint _ | ArrayDec | ArrayUnknown -> true
  | _ -> false

let is_map_type = function
  | MapInt | MapDec | MapBool | MapStr | MapStruct _ | MapFnPtr _ | MapKeyed _ | MapUnknown -> true
  | _ -> false

let cast_result_type value_ty width =
  match value_ty with
  | Int | IntN _ | NumUnknown -> Some (IntN width)
  | Uint | UintN _ -> Some (UintN width)
  | Dec | DecN _ -> Some (DecN width)
  | _ -> None

let cast_width_literal expr =
  match expr.kind with Literal (LitInt v) -> int_of_string_opt v | _ -> None

let map_keys_array_type = function
  | MapKeyed (KeyInt, _) -> ArrayInt None
  | MapKeyed (KeyDec, _) -> ArrayDec
  | MapKeyed (KeyBool, _) -> ArrayBool
  | _ -> ArrayStr

let map_value_type = function
  | MapInt -> Some Int
  | MapDec -> Some Dec
  | MapBool -> Some Bool
  | MapStr -> Some Str
  | MapStruct id -> Some (Struct id)
  | MapFnPtr id -> Some (FnPtr id)
  | MapKeyed (_, value) -> sem_type_from_map_value_kind value
  | _ -> None

let array_element_type = function
  | ArrayInt width -> Some (match width with Some width -> IntN width | None -> Int)
  | ArrayUint width -> Some (match width with Some width -> UintN width | None -> Uint)
  | ArrayDec -> Some Dec
  | ArrayBool -> Some Bool
  | ArrayStr -> Some Str
  | ArrayStruct id -> Some (Struct id)
  | ArrayFnPtr id -> Some (FnPtr id)
  | _ -> None

let array_type_from_element = function
  | Str -> ArrayStr
  | Bool -> ArrayBool
  | Struct id -> ArrayStruct id
  | FnPtr id -> ArrayFnPtr id
  | Dec | DecN _ -> ArrayDec
  | Int -> ArrayInt None
  | IntN width -> ArrayInt (Some width)
  | Uint -> ArrayUint None
  | UintN width -> ArrayUint (Some width)
  | NumUnknown -> ArrayUnknown
  | _ -> ArrayUnknown

let is_array_element_value ty =
  ty = Str || ty = Bool || is_numeric ty || match ty with Struct _ | FnPtr _ -> true | _ -> false

let unify_numeric a b =
  match a, b with
  | Dec, _ | _, Dec -> Dec
  | DecN wa, DecN wb -> DecN (max wa wb)
  | DecN w, (Int | IntN _ | Uint | UintN _ | NumUnknown)
  | (Int | IntN _ | Uint | UintN _ | NumUnknown), DecN w -> DecN w
  | IntN wa, IntN wb -> IntN (max wa wb)
  | IntN w, NumUnknown | NumUnknown, IntN w -> IntN w
  | IntN _, Int | Int, IntN _ -> Int
  | Int, Int -> Int
  | NumUnknown, Int | Int, NumUnknown -> Int
  | UintN wa, UintN wb -> UintN (max wa wb)
  | UintN w, NumUnknown | NumUnknown, UintN w -> UintN w
  | IntN wa, UintN wb | UintN wa, IntN wb -> UintN (max wa wb)
  | Uint, Uint -> Uint
  | Uint, NumUnknown | NumUnknown, Uint -> Uint
  | Uint, UintN _ | UintN _, Uint -> Uint
  | Int, (Uint | UintN _) | (Uint | UintN _), Int -> Int
  | NumUnknown, NumUnknown -> NumUnknown
  | _ -> Error

let unify_array_element_type a b =
  match a, b with
  | Str, Str -> Some Str
  | Bool, Bool -> Some Bool
  | Struct a_id, Struct b_id when a_id = b_id -> Some (Struct a_id)
  | FnPtr a_id, FnPtr b_id when a_id = b_id -> Some (FnPtr a_id)
  | _ when is_numeric a && is_numeric b ->
      let ty = unify_numeric a b in
      if ty = Error then None else Some ty
  | _ -> None

let rec is_assignable target value =
  if target = Error || value = Error then true
  else
    match target, value with
    | Cptr, Cptr -> true
    | CStruct a, CStruct b -> a = b
    | FnPtr a, FnPtr b -> a = b
    | DecN _, v when is_numeric v -> true
    | t, DecN _ when is_numeric t -> true
    | Int, Int -> true
    | IntN _, IntN _ -> true
    | IntN _, (Int | NumUnknown) -> true
    | Int, IntN _ -> true
    | Dec, IntN _ -> true
    | NumUnknown, IntN _ -> true
    | Uint, Uint -> true
    | UintN _, UintN _ -> true
    | UintN _, (Uint | NumUnknown) -> true
    | Uint, (UintN _ | NumUnknown) -> true
    | (Uint | UintN _), (Int | IntN _) -> true
    | (Int | IntN _), (Uint | UintN _) -> true
    | Dec, (Uint | UintN _) -> true
    | NumUnknown, (Uint | UintN _) -> true
    | Dec, (Int | Dec | NumUnknown) -> true
    | Int, NumUnknown -> true
    | NumUnknown, (Int | Dec | NumUnknown) -> true
    | Str, NumUnknown -> true
    | Str, Str -> true
    | target, value when is_array_type target && is_array_type value ->
        arrays_assignable target value
    | target, value when is_map_type target && is_map_type value ->
        target = value || target = MapUnknown || value = MapUnknown
    | Struct a, Struct b -> a = b
    | ResultUnknown, ResultUnknown -> true
    | Result _, ResultUnknown | ResultUnknown, Result _ -> true
    | Result a, Result b -> is_assignable a b
    | Bool, Bool -> true
    | Void, Void -> true
    | _ -> false

and unify_result a b =
  match a, b with
  | ResultUnknown, other | other, ResultUnknown -> other
  | Result NumUnknown, Result payload | Result payload, Result NumUnknown -> Result payload
  | Result x, Result y when x = y -> Result x
  | Result x, Result y when is_numeric x && is_numeric y -> Result (unify_numeric x y)
  | Result x, Result y -> if is_assignable x y then Result x else Result y
  | _ -> ResultUnknown

and arrays_assignable target value =
  target = ArrayUnknown
  || value = ArrayUnknown
  ||
  match array_element_type target, array_element_type value with
  | Some target_elem, Some value_elem -> is_assignable target_elem value_elem
  | _ -> false

let can_store_in_array array_ty value_ty =
  match array_ty with
  | ArrayInt width -> is_assignable (match width with Some w -> IntN w | None -> Int) value_ty
  | ArrayUint width -> is_assignable (match width with Some w -> UintN w | None -> Uint) value_ty
  | ArrayDec -> is_assignable Dec value_ty
  | ArrayBool -> is_assignable Bool value_ty
  | ArrayStr -> is_assignable Str value_ty
  | ArrayStruct id -> is_assignable (Struct id) value_ty
  | ArrayFnPtr id -> is_assignable (FnPtr id) value_ty
  | ArrayUnknown -> is_array_element_value value_ty
  | _ -> false

let can_store_in_map map_ty value_ty =
  match map_ty with
  | MapInt -> is_assignable Int value_ty
  | MapDec -> is_assignable Dec value_ty
  | MapBool -> is_assignable Bool value_ty
  | MapStruct id -> is_assignable (Struct id) value_ty
  | MapFnPtr id -> is_assignable (FnPtr id) value_ty
  | MapKeyed (_, value) ->
      (match sem_type_from_map_value_kind value with
       | Some expected -> is_assignable expected value_ty
       | None -> false)
  | MapStr | MapUnknown -> is_assignable Str value_ty
  | _ -> false

let can_use_map_key map_ty key_ty =
  match map_ty with
  | MapKeyed (KeyInt, _) -> is_integer key_ty
  | MapKeyed (KeyDec, _) -> is_numeric key_ty
  | MapKeyed (KeyBool, _) -> key_ty = Bool
  | MapInt | MapDec | MapBool | MapStr | MapStruct _ | MapFnPtr _ | MapUnknown -> key_ty = Str
  | _ -> false

let expr_as_local_name expr =
  match expr.kind with
  | Path path -> (match path.segments with [ name ] -> Some name | _ -> None)
  | _ -> None

let rec expr_is_statically_negative expr =
  match expr.kind with
  | Grouping inner -> expr_is_statically_negative inner
  | Unary { op = Neg; rhs } ->
      (match rhs.kind with
       | Literal (LitInt v) | Literal (LitDec v) ->
           string_exists (fun c -> c >= '1' && c <= '9') v
       | _ -> false)
  | _ -> false

let rec int_literal_value expr =
  match expr.kind with
  | Grouping inner -> int_literal_value inner
  | Literal (LitInt v) -> int_of_string_opt v
  | Unary { op = Neg; rhs = { kind = Literal (LitInt v); _ } } ->
      Option.map (fun n -> -n) (int_of_string_opt v)
  | _ -> None

let module_exports a module_ name =
  let key = module_, name in
  Hashtbl.mem a.module_fns key
  || Hashtbl.mem a.module_consts key
  || Hashtbl.mem a.module_intrinsics key

let new_scope () = { bindings = []; index = None }

let push_scope a = a.scopes <- new_scope () :: a.scopes
let pop_scope a = match a.scopes with _ :: rest -> a.scopes <- rest | [] -> ()

let declare_local a name ty binding span =
  match a.scopes with
  | scope :: _ ->
      let exists =
        match scope.index with
        | Some index -> Hashtbl.mem index name
        | None -> List.mem_assoc name scope.bindings
      in
      if exists then
        push_diag a "E390" (Printf.sprintf "duplicate variable `%s` in this scope" name) span
      else begin
        let symbol = { local_ty = ty; local_binding = binding } in
        scope.bindings <- (name, symbol) :: scope.bindings;
        match scope.index with
        | Some index -> Hashtbl.add index name symbol
        | None when List.length scope.bindings >= 4 ->
            scope.index <- Some (Hashtbl.of_seq (List.to_seq scope.bindings))
        | None -> ()
      end
  | [] ->
      let scope = new_scope () in
      scope.bindings <- [ name, { local_ty = ty; local_binding = binding } ];
      a.scopes <- [ scope ]

let lookup_local a name =
  List.find_map
    (fun scope ->
      match scope.index with
      | Some index -> Hashtbl.find_opt index name
      | None -> assoc_opt name scope.bindings)
    a.scopes

let lookup_local_ty a name = Option.map (fun symbol -> symbol.local_ty) (lookup_local a name)
let lookup_local_binding a name = Option.map (fun symbol -> symbol.local_binding) (lookup_local a name)

let intern_fn_ptr_sig a sig_ =
  let rec find idx = function
    | [] ->
        let id = List.length a.fn_ptr_sigs in
        a.fn_ptr_sigs <- a.fn_ptr_sigs @ [ sig_ ];
        FnPtr id
    | existing :: rest -> if existing = sig_ then FnPtr idx else find (idx + 1) rest
  in
  find 0 a.fn_ptr_sigs

let fn_ptr_type_from_function_sig a (sig_ : function_sig) span =
  if List.exists (fun (p : param_sig) -> p.binding <> Value) sig_.params then begin
    push_diag a "E371"
      (Printf.sprintf
         "function `%s` cannot be used as a function pointer because it has ref/link parameters"
         sig_.name)
      span;
    Error
  end else
    intern_fn_ptr_sig a
      { params = List.map (fun (p : param_sig) -> p.ty) sig_.params
      ; return_ty = sig_.return_ty
      }

let rec resolve_sem_type a (ty : Ast.type_ref) =
  match ty.kind with
  | Ast.Int None -> Int
  | Ast.Int (Some width) ->
      if is_valid_int_width width then IntN width
      else begin
        push_diag a "E339"
          (Printf.sprintf
             "int width %d is not supported (sized ints must be a multiple of 8, from 8 up to 65528)"
             width)
          ty.span;
        Error
      end
  | Ast.Uint None -> Uint
  | Ast.Uint (Some width) ->
      if is_valid_int_width width then UintN width
      else begin
        push_diag a "E339"
          (Printf.sprintf
             "uint width %d is not supported (sized ints must be a multiple of 8, from 8 up to 65528)"
             width)
          ty.span;
        Error
      end
  | Ast.Dec None -> Dec
  | Ast.Dec (Some width) ->
      if is_valid_int_width width then DecN width
      else begin
        push_diag a "E339"
          (Printf.sprintf
             "dec width %d is not supported (sized decimals must be a multiple of 8, from 8 up to 65528)"
             width)
          ty.span;
        Error
      end
  | Ast.Str -> Str
  | Ast.Bool -> Bool
  | Ast.Array elem -> array_type_from_typeref_element a elem ty.span
  | Ast.Map (key, value) -> map_type_from_typeref a key value ty.span
  | Ast.Result payload -> Result (resolve_sem_type a { kind = payload; span = ty.span })
  | Ast.Fn (params, ret) ->
      let params =
        List.map (fun kind -> resolve_sem_type a { kind; span = ty.span }) params
      in
      let return_ty = resolve_sem_type a { kind = ret; span = ty.span } in
      intern_fn_ptr_sig a { params; return_ty }
  | Ast.Named name ->
      if name = "map" then MapStr
      else if name = "cptr" then Cptr
      else if name = "void" then Void
      else
        (match assoc_opt name a.cstruct_ids with
         | Some id -> CStruct id
         | None ->
             (match assoc_opt name a.struct_ids with
              | Some id -> Struct id
              | None ->
                  push_diag a "E431" (Printf.sprintf "unknown type `%s`" name) ty.span;
                  Error))

and array_type_from_typeref_element a (elem : Ast.type_kind) span =
  match elem with
  | Ast.Int None -> ArrayInt None
  | Ast.Int (Some width) when is_valid_int_width width -> ArrayInt (Some width)
  | Ast.Int (Some width) ->
      push_diag a "E339"
        (Printf.sprintf
           "int width %d is not supported (sized ints must be a multiple of 8, from 8 up to 65528)"
           width)
        span;
      Error
  | Ast.Uint None -> ArrayUint None
  | Ast.Uint (Some width) when is_valid_int_width width -> ArrayUint (Some width)
  | Ast.Uint (Some width) ->
      push_diag a "E339"
        (Printf.sprintf
           "uint width %d is not supported (sized uints must be a multiple of 8, from 8 up to 65528)"
           width)
        span;
      Error
  | Ast.Dec _ -> ArrayDec
  | Ast.Str -> ArrayStr
  | Ast.Bool -> ArrayBool
  | Ast.Fn (params, ret) ->
      (match resolve_sem_type a { kind = Ast.Fn (params, ret); span } with
       | FnPtr id -> ArrayFnPtr id
       | Error -> Error
       | _ -> ArrayUnknown)
  | Ast.Named name ->
      (match assoc_opt name a.struct_ids with
       | Some id -> ArrayStruct id
       | None ->
           push_diag a "E431" (Printf.sprintf "unknown type `%s`" name) span;
           Error)
  | Ast.Array _ | Ast.Map _ -> ArrayUnknown
  | Ast.Result _ ->
      push_diag a "E535" "`array<result<T>>` is not supported yet; store the payload or a struct instead" span;
      Error

and map_type_from_typeref_value a (value : Ast.type_kind) span =
  match value with
  | Ast.Int _ | Ast.Uint _ -> MapInt
  | Ast.Dec _ -> MapDec
  | Ast.Str -> MapStr
  | Ast.Bool -> MapBool
  | Ast.Fn (params, ret) ->
      (match resolve_sem_type a { kind = Ast.Fn (params, ret); span } with
       | FnPtr id -> MapFnPtr id
       | Error -> Error
       | _ -> MapUnknown)
  | Ast.Named name ->
      (match assoc_opt name a.struct_ids with
       | Some id -> MapStruct id
       | None ->
           push_diag a "E431" (Printf.sprintf "unknown type `%s`" name) span;
           Error)
  | Ast.Array _ | Ast.Map _ -> MapUnknown
  | Ast.Result _ ->
      push_diag a "E535" "`map<K, result<T>>` is not supported yet; store the payload or a struct instead" span;
      Error

and map_type_from_typeref a (key : Ast.type_kind) (value : Ast.type_kind) span =
  if key = Ast.Str then map_type_from_typeref_value a value span
  else
    match map_key_type_from_typekind a key span with
    | None -> Error
    | Some key_ty ->
        let value_ty = map_value_kind_from_typekind a value span in
        if value_ty = ValueUnknown then MapUnknown else MapKeyed (key_ty, value_ty)

and map_key_type_from_typekind a (key : Ast.type_kind) span =
  match key with
  | Ast.Int _ | Ast.Uint _ -> Some KeyInt
  | Ast.Dec _ -> Some KeyDec
  | Ast.Bool -> Some KeyBool
  | Ast.Str -> None
  | Ast.Named _ | Ast.Array _ | Ast.Map _ | Ast.Fn _ | Ast.Result _ ->
      push_diag a "E442" "`map<K, V>` keys currently support `int`, `dec`, `bool`, or `string`" span;
      None

and map_value_kind_from_typekind a (value : Ast.type_kind) span =
  match value with
  | Ast.Int _ | Ast.Uint _ -> ValueInt
  | Ast.Dec _ -> ValueDec
  | Ast.Str -> ValueStr
  | Ast.Bool -> ValueBool
  | Ast.Fn (params, ret) ->
      (match resolve_sem_type a { kind = Ast.Fn (params, ret); span } with
       | FnPtr id -> ValueFnPtr id
       | _ -> ValueUnknown)
  | Ast.Named name ->
      (match assoc_opt name a.struct_ids with
       | Some id -> ValueStruct id
       | None ->
           push_diag a "E431" (Printf.sprintf "unknown type `%s`" name) span;
           ValueUnknown)
  | Ast.Array _ | Ast.Map _ -> ValueUnknown
  | Ast.Result _ ->
      push_diag a "E535" "`map<K, result<T>>` is not supported yet; store the payload or a struct instead" span;
      ValueUnknown

let struct_field_type a struct_id field =
  match assoc_opt struct_id a.struct_sigs_by_id with
  | None -> None
  | Some (sig_ : struct_sig) ->
      Option.bind (assoc_opt field sig_.field_index) (fun idx -> List.nth_opt sig_.fields idx)
      |> Option.map (fun (field : struct_field_sig) -> field.ty)

(* Byte size and alignment of a C-layout field type. `None` = not permitted in a
   `cstruct` (arbitrary-precision box, non-native width, Xi aggregate, …). *)
let rec cstruct_type_size_align a = function
  | Bool -> Some (1, 1)
  | IntN w | UintN w when w = 8 || w = 16 || w = 32 || w = 64 -> Some (w / 8, w / 8)
  | DecN 16 -> Some (2, 2)
  | DecN 32 -> Some (4, 4)
  | DecN 64 -> Some (8, 8)
  | Cptr -> Some (8, 8)
  | CStruct id -> Option.map (fun (s : cstruct_sig) -> s.cs_size, s.cs_align) (ensure_cstruct_layout a id)
  | Error -> Some (0, 1)
  | _ -> None

and ensure_cstruct_layout ?(visiting = []) a id =
  match assoc_opt id a.cstruct_sigs_by_id with
  | Some sig_ -> Some sig_
  | None ->
      if List.mem id visiting then None
      else
        match List.find_opt (fun (d : Ast.struct_decl) -> assoc_opt d.name a.cstruct_ids = Some id) a.cstruct_decls with
        | None -> None
        | Some decl ->
            let visiting = id :: visiting in
            let offset = ref 0 in
            let align = ref 1 in
            let seen = ref [] in
            let fields = ref [] in
            List.iter
              (fun (field : Ast.struct_field_decl) ->
                if List.mem field.name !seen then
                  push_diag a "E302" (Printf.sprintf "duplicate field `%s` in cstruct `%s`" field.name decl.name) field.span
                else begin
                  seen := field.name :: !seen;
                  let ty = resolve_sem_type a field.ty in
                  let sz_al =
                    match ty with
                    | CStruct nested ->
                        (match ensure_cstruct_layout ~visiting a nested with
                         | Some s -> Some (s.cs_size, s.cs_align)
                         | None ->
                             push_diag a "E512"
                               (Printf.sprintf "cstruct `%s` contains itself (directly or via a cycle); use a `cptr` for recursive references" decl.name)
                               field.span;
                             None)
                    | _ -> cstruct_type_size_align a ty
                  in
                  match sz_al with
                  | None ->
                      (match ty with
                       | Error | CStruct _ -> ()
                       | _ ->
                      push_diag a "E510"
                          (Printf.sprintf "cstruct field `%s.%s` has type `%s`, which has no fixed C layout. Use `int8/16/32/64`, `uint8/…/64`, `dec16/32/64`, `bool`, `cptr`, or another cstruct"
                             decl.name field.name (boundary_type_desc a.struct_sigs_by_id ty))
                          field.span)
                  | Some (sz, al) ->
                      let off = ((!offset + al - 1) / al) * al in
                      fields := { cfs_name = field.name; cfs_ty = ty; cfs_offset = off } :: !fields;
                      offset := off + sz;
                      if al > !align then align := al
                end)
              decl.fields;
            let size = ((!offset + !align - 1) / !align) * !align in
            let sig_ =
              { cs_id = id; cs_name = decl.name; cs_fields = List.rev !fields
              ; cs_size = size; cs_align = !align; cs_span = decl.span }
            in
            a.cstruct_sigs_by_name <- replace_assoc decl.name sig_ a.cstruct_sigs_by_name;
            a.cstruct_sigs_by_id <- replace_assoc id sig_ a.cstruct_sigs_by_id;
            Some sig_

let cstruct_field a cstruct_id field =
  match assoc_opt cstruct_id a.cstruct_sigs_by_id with
  | None -> None
  | Some (sig_ : cstruct_sig) -> List.find_opt (fun f -> f.cfs_name = field) sig_.cs_fields

let collect_structs a (program : Ast.program) =
  let dynamic_decls = List.filter (fun (d : Ast.struct_decl) -> not d.c_layout) program.structs in
  let cstruct_decls = List.filter (fun (d : Ast.struct_decl) -> d.c_layout) program.structs in
  a.cstruct_decls <- cstruct_decls;
  List.iter
    (fun (decl : Ast.struct_decl) ->
      if mem_assoc decl.name a.cstruct_ids || mem_assoc decl.name a.struct_ids then
        push_diag a "E301" (Printf.sprintf "duplicate cstruct `%s`" decl.name) decl.span
      else
        let id = List.length a.cstruct_ids in
        a.cstruct_ids <- (decl.name, id) :: a.cstruct_ids)
    cstruct_decls;
  List.iter
    (fun (decl : Ast.struct_decl) ->
      if mem_assoc decl.name a.struct_ids || mem_assoc decl.name a.cstruct_ids then
        push_diag a "E301" (Printf.sprintf "duplicate struct `%s`" decl.name) decl.span
      else
        let id = List.length a.struct_ids in
        a.struct_ids <- (decl.name, id) :: a.struct_ids)
    dynamic_decls;
  List.iter
    (fun (decl : Ast.struct_decl) ->
      match assoc_opt decl.name a.cstruct_ids with
      | Some id -> ignore (ensure_cstruct_layout a id)
      | None -> ())
    cstruct_decls;
  List.iter
    (fun (decl : Ast.struct_decl) ->
      match assoc_opt decl.name a.struct_ids with
      | None -> ()
      | Some id ->
          let fields = ref [] in
          let field_index = ref [] in
          List.iter
            (fun (field : Ast.struct_field_decl) ->
              if List.mem_assoc field.name !field_index then
                push_diag a "E302"
                  (Printf.sprintf "duplicate field `%s` in struct `%s`" field.name decl.name)
                  field.span
              else begin
                let ty = resolve_sem_type a field.ty in
                field_index := (field.name, List.length !fields) :: !field_index;
                fields := !fields @ [ { name = field.name; ty } ]
              end)
            decl.fields;
          let sig_ =
            { id
            ; name = decl.name
            ; fields = !fields
            ; field_index = List.rev !field_index
            ; span = decl.span
            }
          in
          a.struct_sigs_by_name <- replace_assoc decl.name sig_ a.struct_sigs_by_name;
          a.struct_sigs_by_id <- replace_assoc id sig_ a.struct_sigs_by_id)
    program.structs

let collect_modules a (program : Ast.program) =
  let register_module module_ (mod_ast : Ast.program) =
    List.iter
      (fun (fn : Ast.extern_fn_decl) ->
        let boundary_form =
          if String.starts_with ~prefix:"xi_inline_c_" fn.symbol then "c f"
          else "cextern f"
        in
        let resolved = List.map (fun (p : Ast.param) -> p, resolve_sem_type a p.ty) fn.params in
        let params =
          List.map (fun ((p : Ast.param), ty) -> { name = p.name; ty; binding = p.binding }) resolved
        in
        let return_ty = resolve_sem_type a fn.return_ty in
        if fn.c_abi then begin
          List.iter
            (fun ((p : Ast.param), ty) ->
              if p.binding <> Value then
                push_diag a "E497"
                  (Printf.sprintf
                     "`%s` parameter `%s` cannot be `ref`/`link`; pass a `cptr` for C out-parameters"
                     boundary_form p.name)
                  p.span
              else if not (c_abi_boundary_ok ~allow_void:false ty) then
                push_diag a "E495"
                  (Printf.sprintf
                     "`%s %s`: parameter `%s` has type `%s`, which cannot cross the C ABI. Use `int8/16/32/64`, `uint8/16/32/64`, `dec16/32/64`, `string`, `cptr`, or `bool` — bare and wider numeric values are not native C ABI values"
                     boundary_form fn.name p.name (boundary_type_desc a.struct_sigs_by_id ty))
                  p.span)
            resolved;
          if not (c_abi_boundary_ok ~allow_void:true return_ty) then
            push_diag a "E496"
              (Printf.sprintf
                 "`%s %s`: return type `%s` cannot cross the C ABI. Use an 8/16/32/64-bit integer, `dec16/32/64`, `string`, `cptr`, `bool`, or `void`"
                 boundary_form fn.name (boundary_type_desc a.struct_sigs_by_id return_ty))
              fn.return_ty.span
        end;
        let actual_module, actual_name =
          if module_ = "" then
            match String.split_on_char ':' fn.name with
            | [ namespace; ""; name ] -> namespace, name
            | _ -> module_, fn.name
          else module_, fn.name
        in
        let sig_ = { module_ = actual_module; name = actual_name; params; return_ty; symbol = fn.symbol } in
        Hashtbl.replace a.module_fns (actual_module, actual_name) sig_;
        a.modules <-
          { a.modules with
            fns = replace_assoc (actual_module, actual_name) sig_ a.modules.fns
          })
      mod_ast.externs;
    List.iter
      (fun (konst : Ast.extern_const_decl) ->
        let ty = resolve_sem_type a konst.ty in
        if konst.c_abi && not (c_abi_boundary_ok ~allow_void:false ty) then
          push_diag a "E499"
            (Printf.sprintf
               "`cextern %s`: type `%s` cannot cross the C ABI. Use a fixed-width type, `string`, or `cptr`"
               konst.name (boundary_type_desc a.struct_sigs_by_id ty))
            konst.span;
        let sig_ = { module_; name = konst.name; ty; symbol = konst.symbol } in
        Hashtbl.replace a.module_consts (module_, konst.name) sig_;
        a.modules <- { a.modules with consts = replace_assoc (module_, konst.name) sig_ a.modules.consts })
      mod_ast.extern_consts;
    List.iter
      (fun (intr : Ast.intrinsic_decl) ->
        if not (Hashtbl.mem a.module_intrinsics (module_, intr.name)) then begin
          Hashtbl.add a.module_intrinsics (module_, intr.name) ();
          a.modules <- { a.modules with intrinsics = (module_, intr.name) :: a.modules.intrinsics }
        end)
      mod_ast.intrinsics
  in
  let seen = Hashtbl.create 8 in
  register_module "" program;
  List.iter
    (fun (inc : Ast.include_decl) ->
      if not (Hashtbl.mem seen inc.module_) then begin
        Hashtbl.add seen inc.module_ ();
        match Module_loader.load inc.module_ with
        | Some mod_ast -> register_module inc.module_ mod_ast
        | None -> ()
      end)
    program.includes

let collect_signatures a (program : Ast.program) =
  List.iter
    (fun (function_ : Ast.function_decl) ->
      if has_function a function_.name then
        push_diag a "E300" (Printf.sprintf "duplicate function `%s`" function_.name) function_.span
      else begin
        let params =
          List.map
            (fun (param : Ast.param) ->
              let ty = resolve_sem_type a param.ty in
              if param.binding = Link then
                push_diag a "E201" "`link` parameters are forbidden by spec at function boundaries" param.span;
              { name = param.name; ty; binding = param.binding })
            function_.params
        in
        let sig_ = { name = function_.name; params; return_ty = NumUnknown; span = function_.span } in
        Hashtbl.add a.function_sigs function_.name sig_
      end)
    program.functions

let infer_return_type a name span =
  if a.current_return_void && a.current_return_exprs <> [] then begin
    push_diag a "E320"
      (Printf.sprintf "function `%s` mixes `return;` and `return <expr>;`, which is not allowed" name)
      span;
    Void
  end else
    match List.rev a.current_return_exprs with
    | [] -> Void
    | tys ->
        let rec loop inferred = function
          | [] -> Option.value inferred ~default:Void
          | ty :: rest ->
              let next =
                match inferred with
                | None -> Some ty
                | Some NumUnknown -> Some ty
                | Some existing when ty = NumUnknown -> Some existing
                | Some existing when existing = ty -> Some existing
                | Some existing when is_numeric existing && is_numeric ty -> Some (unify_numeric existing ty)
                | Some existing when is_result_type existing && is_result_type ty ->
                    Some (unify_result existing ty)
                | Some _ ->
                    if not a.uses_try then
                      push_diag a "E321"
                        (Printf.sprintf
                           "function `%s` has incompatible return types; v1 supports a single inferred type (numeric, string, array, map, or void)"
                           name)
                        span;
                    Some Void
              in
              if next = Some Void && inferred <> Some Void then Void else loop next rest
        in
        loop None tys

let require_boolish a ty span message =
  if ty <> Error && ty <> Bool && not (is_numeric ty) then push_diag a "E380" message span

let rec analyze_stmt a stmt =
  match stmt with
  | VarDecl decl -> analyze_var_decl a decl
  | Assign assign -> analyze_assign a assign
  | Return ret -> analyze_return a ret
  | If if_stmt ->
      let cond_ty = analyze_expr a if_stmt.cond in
      require_boolish a cond_ty if_stmt.cond.span "if condition must be numeric or bool";
      analyze_stmt a if_stmt.then_branch;
      Option.iter (analyze_stmt a) if_stmt.else_branch
  | Switch switch_stmt -> analyze_switch a switch_stmt
  | While while_stmt ->
      let cond_ty = analyze_expr a while_stmt.cond in
      require_boolish a cond_ty while_stmt.cond.span "while condition must be numeric or bool";
      analyze_loop_body a while_stmt.body
  | Break jump ->
      if a.loop_depth = 0 then
        push_diag a "E322" "`break` is only valid inside a `while` or `for` loop" jump.span
  | Continue jump ->
      if a.loop_depth = 0 then
        push_diag a "E323" "`continue` is only valid inside a `while` or `for` loop" jump.span
  | For for_stmt -> analyze_for a for_stmt
  | ForEach for_each -> analyze_for_each a for_each
  | IoChain io -> analyze_io_chain a io
  | ExprStmt expr_stmt -> ignore (analyze_expr a expr_stmt.expr)
  | Block block ->
      push_scope a;
      List.iter (analyze_stmt a) block.statements;
      pop_scope a

and analyze_var_decl a decl =
  let ty = resolve_sem_type a decl.ty in
  (match decl.binding with
   | Ref ->
       (match decl.init with
        | None ->
            push_diag a "E334" (Printf.sprintf "`ref` variable `%s` requires an initializer" decl.name) decl.span
        | Some init ->
            (match expr_as_local_name init with
             | None ->
                 push_diag a "E335"
                   (Printf.sprintf "`ref` variable `%s` must initialize from a local variable path" decl.name)
                   init.span
             | Some target_name ->
                 (match lookup_local_ty a target_name with
                  | None ->
                      push_diag a "E336"
                        (Printf.sprintf "`ref` variable `%s` references undefined variable `%s`" decl.name target_name)
                        init.span
                  | Some target_ty ->
                      if ty <> target_ty then
                        push_diag a "E337"
                          (Printf.sprintf "`ref` variable `%s` type %s must match target `%s` type %s"
                             decl.name (sem_type_to_string ty) target_name (sem_type_to_string target_ty))
                          init.span)))
   | Link ->
       (match decl.init with
        | None ->
            push_diag a "E203" (Printf.sprintf "`link` variable `%s` requires an initializer" decl.name) decl.span
        | Some init ->
            (match expr_as_local_name init with
             | None ->
                 push_diag a "E204"
                   (Printf.sprintf "`link` variable `%s` must initialize from a local variable path" decl.name)
                   init.span
             | Some target_name ->
                 (match lookup_local_ty a target_name with
                  | None ->
                      push_diag a "E205"
                        (Printf.sprintf "`link` variable `%s` references undefined variable `%s`" decl.name target_name)
                        init.span
                  | Some target_ty ->
                      if ty <> target_ty then
                        push_diag a "E206"
                          (Printf.sprintf "`link` variable `%s` type %s must match target `%s` type %s"
                             decl.name (sem_type_to_string ty) target_name (sem_type_to_string target_ty))
                          init.span)))
   | Value ->
       Option.iter
         (fun init ->
           let rhs_ty = analyze_expr a init in
           if ty = Uint && expr_is_statically_negative init then
             push_diag a "E338"
               (Printf.sprintf
                  "cannot assign a negative literal to adaptive `uint` variable `%s` (use a sized `uintN` to wrap, or `as uint` to saturate to 0)"
                  decl.name)
               init.span
           else if not (is_assignable ty rhs_ty) then
             push_diag a "E330"
               (Printf.sprintf "cannot assign %s to variable `%s` of type %s"
                  (sem_type_to_string rhs_ty) decl.name (sem_type_to_string ty))
               init.span)
         decl.init);
  declare_local a decl.name ty decl.binding decl.span

and analyze_assign a assign =
  match assign.target.segments with
  | [ name ] ->
      (match lookup_local_ty a name with
       | None -> push_diag a "E332" (Printf.sprintf "undefined variable `%s`" name) assign.target.span
       | Some lhs_ty ->
           let lhs_binding = lookup_local_binding a name |> Option.value ~default:Value in
           if assign.mode = ExplicitLink then begin
             if lhs_binding <> Link then
               push_diag a "E207"
                 (Printf.sprintf "explicit `link` assignment target `%s` must be a `link` variable" name)
                 assign.target.span;
             match expr_as_local_name assign.value with
             | None ->
                 push_diag a "E208"
                   "explicit `link` assignment requires a local variable on the right-hand side"
                   assign.value.span
             | Some rhs_name ->
                 (match lookup_local_ty a rhs_name with
                  | None ->
                      push_diag a "E209"
                        (Printf.sprintf "undefined variable `%s` in explicit `link` assignment" rhs_name)
                        assign.value.span
                  | Some rhs_ty ->
                      if lhs_ty <> rhs_ty then
                        push_diag a "E210"
                          (Printf.sprintf "cannot `link` target `%s` (%s) with `%s` (%s)"
                             name (sem_type_to_string lhs_ty) rhs_name (sem_type_to_string rhs_ty))
                          assign.span)
           end else begin
             let rhs_ty = analyze_expr a assign.value in
             if lhs_ty = Uint && expr_is_statically_negative assign.value then
               push_diag a "E338"
                 (Printf.sprintf
                    "cannot assign a negative literal to adaptive `uint` variable `%s` (use a sized `uintN` to wrap, or `as uint` to saturate to 0)"
                    name)
                 assign.value.span
             else if not (is_assignable lhs_ty rhs_ty) then
               push_diag a "E333"
                 (Printf.sprintf "cannot assign %s to variable `%s` of type %s"
                    (sem_type_to_string rhs_ty) name (sem_type_to_string lhs_ty))
                 assign.value.span
           end)
  | _ -> push_diag a "E331" "assignment target must be a local variable name" assign.target.span

and analyze_return a ret =
  match ret.value with
  | Some expr -> a.current_return_exprs <- analyze_expr a expr :: a.current_return_exprs
  | None -> a.current_return_void <- true

and analyze_for a for_stmt =
  push_scope a;
  Option.iter
    (function
      | ForInitVarDecl decl -> analyze_var_decl a decl
      | ForInitAssign assign -> analyze_assign a assign
      | ForInitExpr expr -> ignore (analyze_expr a expr))
    for_stmt.init;
  Option.iter
    (fun cond ->
      let cond_ty = analyze_expr a cond in
      require_boolish a cond_ty cond.span "for condition must be numeric or bool")
    for_stmt.cond;
  Option.iter
    (function
      | ForStepAssign assign -> analyze_assign a assign
      | ForStepExpr expr -> ignore (analyze_expr a expr))
    for_stmt.step;
  analyze_loop_body a for_stmt.body;
  pop_scope a

and analyze_for_each a (for_each : Ast.for_each_stmt) =
  let collection_ty = analyze_expr a for_each.collection in
  let element_ty =
    if collection_ty = Error then Error
    else
      match array_element_type collection_ty with
      | Some ty -> ty
      | None ->
          if collection_ty = ArrayUnknown then NumUnknown
          else begin
            push_diag a "E324"
              (Printf.sprintf
                 "`for %s in ...` needs an array; %s is not iterable (for a map, walk `map::keys(m)`)"
                 for_each.name (xi_type_name a.struct_sigs_by_id collection_ty))
              for_each.collection.span;
            Error
          end
  in
  push_scope a;
  declare_local a for_each.name element_ty Value for_each.name_span;
  analyze_loop_body a for_each.body;
  pop_scope a

and analyze_loop_body a body =
  a.loop_depth <- a.loop_depth + 1;
  analyze_stmt a body;
  a.loop_depth <- a.loop_depth - 1

and analyze_switch a (switch_stmt : Ast.switch_stmt) =
  let selector_ty = analyze_expr a switch_stmt.selector in
  if selector_ty <> Error && not (is_integer selector_ty) then
    push_diag a "E462" "switch selector must be an integer value" switch_stmt.selector.span;
  let seen = ref [] in
  List.iter
    (fun (case : Ast.switch_case) ->
      let case_ty = analyze_expr a case.value in
      let case_is_integer = case_ty <> Error && is_integer case_ty in
      if case_ty <> Error && not case_is_integer then
        push_diag a "E463" "switch case value must be an integer literal" case.value.span;
      (match int_literal_value case.value with
       | Some value ->
           if List.mem value !seen then
             push_diag a "E464" (Printf.sprintf "duplicate switch case `%d`" value) case.value.span
           else seen := value :: !seen
       | None ->
           if case_is_integer then
             push_diag a "E463" "switch case value must be an integer literal" case.value.span);
      push_scope a;
      List.iter (analyze_stmt a) case.body;
      pop_scope a)
    switch_stmt.cases;
  Option.iter
    (fun (default : Ast.switch_default) ->
      push_scope a;
      List.iter (analyze_stmt a) default.body;
      pop_scope a)
    switch_stmt.default

and analyze_io_chain a io =
  if not (StringSet.mem "io" a.included_modules) then
    push_diag a "E458" "`io::wrt` / `io::wrtl` / `io::wrtr` require `#include io`" io.span;
  List.iter
    (function
      | IoExpr expr ->
          let ty = analyze_expr a expr in
          if ty <> Str && ty <> Bool && ty <> Error && not (is_numeric ty) then
            push_diag a "E340" "io chain items must be numeric, bool, or string values" expr.span
      | Endl _ -> ())
    io.items

and analyze_expr a expr =
  match expr.kind with
  | Literal (LitInt _) -> Int
  | Literal (LitDec _) -> Dec
  | Literal (LitStr _) -> Str
  | Literal (LitBool _) -> Bool
  | ArrayLiteral arr ->
      let had_error = ref false in
      let elem_ty = ref None in
      List.iter
        (fun item ->
          let item_ty = analyze_expr a item in
          if item_ty = Error then had_error := true
          else if not (is_array_element_value item_ty) then begin
            push_diag a "E413" "array literal elements must be all-string or all-numeric values" item.span;
            had_error := true
          end else
            match !elem_ty with
            | None -> elem_ty := Some item_ty
            | Some prev ->
                (match unify_array_element_type prev item_ty with
                 | Some next -> elem_ty := Some next
                 | None ->
                     push_diag a "E413" "array literal elements must be all-string or all-numeric values" item.span;
                     had_error := true))
        arr.items;
      if !had_error then Error else Option.map array_type_from_element !elem_ty |> Option.value ~default:ArrayUnknown
  | StructLiteral lit -> analyze_struct_literal a lit expr.span
  | Closure closure -> analyze_closure a closure expr.span
  | Path path -> analyze_path a path
  | Call call -> analyze_call a call expr.span
  | Index index -> analyze_index a index
  | Member member -> analyze_member a member expr.span
  | Unary unary -> analyze_unary a unary
  | Binary binary -> analyze_binary a binary expr.span
  | Cast cast -> analyze_cast a cast
  | Try inner -> analyze_try a inner expr.span
  | Grouping inner -> analyze_expr a inner

and analyze_struct_literal a (lit : Ast.struct_literal_expr) span =
  match assoc_opt lit.name a.struct_ids with
  | None ->
      push_diag a "E438" (Printf.sprintf "unknown struct type `%s` in struct literal" lit.name) span;
      Error
  | Some struct_id ->
      (match assoc_opt struct_id a.struct_sigs_by_id with
       | None -> Error
       | Some sig_ ->
           let had_error = ref false in
           let seen = ref [] in
           List.iter
             (fun (field : Ast.struct_literal_field_expr) ->
               if List.mem_assoc field.name !seen then begin
                 push_diag a "E439"
                   (Printf.sprintf "duplicate field `%s` in struct literal `%s`" field.name lit.name)
                   field.span;
                 had_error := true
               end else begin
                 seen := (field.name, field.span) :: !seen;
                 match assoc_opt field.name sig_.field_index with
                 | None ->
                     push_diag a "E433" (Printf.sprintf "unknown struct field `%s`" field.name) field.span;
                     had_error := true;
                     ignore (analyze_expr a field.value)
                 | Some idx ->
                     let field_ty =
                       List.nth_opt sig_.fields idx
                       |> Option.map (fun (f : struct_field_sig) -> f.ty)
                       |> Option.value ~default:Error
                     in
                     let value_ty = analyze_expr a field.value in
                     if not (is_assignable field_ty value_ty) then begin
                       push_diag a "E440"
                         (Printf.sprintf "cannot assign %s to field `%s` of type %s"
                            (sem_type_to_string value_ty) field.name (sem_type_to_string field_ty))
                         field.value.span;
                       had_error := true
                     end
               end)
             lit.fields;
           List.iter
             (fun (field : struct_field_sig) ->
               if not (List.mem_assoc field.name !seen) then begin
                 push_diag a "E441"
                   (Printf.sprintf "missing field `%s` in struct literal `%s`" field.name lit.name)
                   span;
                 had_error := true
               end)
             sig_.fields;
           if !had_error then Error else Struct struct_id)

and analyze_index a index =
  let base_ty = analyze_expr a index.base in
  let idx_ty = analyze_expr a index.index in
  if base_ty <> Error && not (is_array_type base_ty) && not (is_map_type base_ty) then begin
    push_diag a "E406" "index base must be an array or map value" index.base.span;
    Error
  end else if base_ty <> Error && idx_ty <> Error && is_array_type base_ty && not (is_numeric idx_ty) then begin
    push_diag a "E407" "array index expression must be numeric" index.index.span;
    Error
  end else if base_ty <> Error && idx_ty <> Error && is_map_type base_ty && not (can_use_map_key base_ty idx_ty) then begin
    push_diag a "E408" "map index expression must match the map key type" index.index.span;
    Error
  end else if base_ty = Error || idx_ty = Error then Error
  else if is_array_type base_ty then Option.value (array_element_type base_ty) ~default:NumUnknown
  else if is_map_type base_ty then Option.value (map_value_type base_ty) ~default:Str
  else Error

and analyze_member a member span =
  match analyze_expr a member.base with
  | Struct struct_id ->
      (match struct_field_type a struct_id member.field with
       | Some field_ty -> field_ty
       | None ->
           push_diag a "E433" (Printf.sprintf "unknown struct field `%s`" member.field) span;
           Error)
  | CStruct cstruct_id ->
      (match cstruct_field a cstruct_id member.field with
       | Some field -> field.cfs_ty
       | None ->
           push_diag a "E433" (Printf.sprintf "unknown cstruct field `%s`" member.field) span;
           Error)
  | Error -> Error
  | _ ->
      push_diag a "E432" "member access base must be a struct value" member.base.span;
      Error

and analyze_unary a unary =
  let rhs_ty = analyze_expr a unary.rhs in
  match unary.op with
  | Neg ->
      if not (is_numeric rhs_ty) then begin
        push_diag a "E350" "unary `-` requires a numeric operand" unary.rhs.span;
        Error
      end else rhs_ty
  | Not ->
      require_boolish a rhs_ty unary.rhs.span "unary `!` requires numeric or bool";
      Bool
  | BitNot ->
      if not (is_integer rhs_ty) then begin
        push_diag a "E354" "bitwise `~` requires an integer operand" unary.rhs.span;
        Error
      end else rhs_ty

and analyze_binary a binary span =
  let lhs_ty = analyze_expr a binary.lhs in
  let rhs_ty = analyze_expr a binary.rhs in
  match binary.op with
  | Add ->
      if lhs_ty = Str || rhs_ty = Str then
        if lhs_ty = Str && rhs_ty = Str then Str
        else begin
          push_diag a "E351" "operator `+` requires both operands to be numeric or both to be string" span;
          Error
        end
      else if not (is_numeric lhs_ty) || not (is_numeric rhs_ty) then begin
        push_diag a "E351" "operator `+` requires both operands to be numeric or both to be string" span;
        Error
      end else unify_numeric lhs_ty rhs_ty
  | Sub | Mul | Div | Pow ->
      if not (is_numeric lhs_ty) || not (is_numeric rhs_ty) then begin
        push_diag a "E351" "arithmetic operators require numeric operands" span;
        Error
      end else unify_numeric lhs_ty rhs_ty
  | Mod ->
      if not (is_integer lhs_ty) || not (is_integer rhs_ty) then begin
        push_diag a "E353" "operator `%` (modulo) requires integer operands" span;
        Error
      end else unify_numeric lhs_ty rhs_ty
  | BitAnd | BitOr | BitXor ->
      if not (is_integer lhs_ty) || not (is_integer rhs_ty) then begin
        push_diag a "E355" "bitwise operators (`&`, `|`, `^^`) require integer operands" span;
        Error
      end else unify_numeric lhs_ty rhs_ty
  | Eq | Ne | Lt | Le | Gt | Ge ->
      let is_string_eq = (binary.op = Eq || binary.op = Ne) && lhs_ty = Str && rhs_ty = Str in
      if is_string_eq then Bool
      else if not (is_numeric lhs_ty) || not (is_numeric rhs_ty) then begin
        push_diag a "E352" "comparison operators require numeric operands (except string `==` and `!=`)" span;
        Error
      end else Bool
  | And | Or ->
      require_boolish a lhs_ty binary.lhs.span "logical operators require numeric or bool operands";
      require_boolish a rhs_ty binary.rhs.span "logical operators require numeric or bool operands";
      Bool

and analyze_cast a cast =
  let value_ty = analyze_expr a cast.value in
  let target_ty = resolve_sem_type a cast.ty in
  if value_ty = Error || target_ty = Error then Error
  else if not (is_numeric value_ty) then begin
    push_diag a "E484" "`as` requires a numeric value on the left" cast.value.span;
    Error
  end else if not (is_numeric target_ty) then begin
    push_diag a "E485" "`as` target type must be numeric (`int`, `dec`, `intN`, or `decN`)" cast.ty.span;
    Error
  end else target_ty

and analyze_path a path =
  match path.segments with
  | [ name ] ->
      (match lookup_local_ty a name with
       | Some ty -> ty
       | None ->
           (match function_sig_opt a name with
            | Some sig_ -> fn_ptr_type_from_function_sig a sig_ path.span
            | None ->
                push_diag a "E360" (Printf.sprintf "undefined symbol `%s`" name) path.span;
                Error))
  | [ module_; name ] ->
      (match Hashtbl.find_opt a.module_consts (module_, name) with
       | Some konst -> konst.ty
       | None ->
           let fn_key = module_ ^ "::" ^ name in
           (match function_sig_opt a fn_key with
            | Some sig_ -> fn_ptr_type_from_function_sig a sig_ path.span
            | None ->
                push_diag a "E361" (Printf.sprintf "unknown namespaced symbol `%s::%s`" module_ name) path.span;
                Error))
  | _ ->
      push_diag a "E361" (Printf.sprintf "unknown namespaced symbol `%s`" (String.concat "::" path.segments)) path.span;
      Error

and analyze_try a inner span =
  let ty = analyze_expr a inner in
  a.uses_try <- true;
  a.current_return_exprs <- ResultUnknown :: a.current_return_exprs;
  if ty = Error then Error
  else
    match ty with
    | Result payload -> payload
    | ResultUnknown -> NumUnknown
    | _ ->
        push_diag a "E536"
          (Printf.sprintf "`try` expects a result; %s cannot fail"
             (xi_type_name a.struct_sigs_by_id ty))
          span;
        Error

and analyze_closure a (closure : Ast.closure_expr) span =
  let params =
    List.map
      (fun (param : Ast.param) ->
        if param.binding <> Value then
          push_diag a "E379" "closure parameters must be value parameters" param.span;
        { name = param.name; ty = resolve_sem_type a param.ty; binding = param.binding })
      closure.params
  in
  let declared_return = resolve_sem_type a closure.return_ty in
  let outer_returns = a.current_return_exprs in
  let outer_void = a.current_return_void in
  let outer_loop_depth = a.loop_depth in
  let outer_uses_try = a.uses_try in
  a.current_return_exprs <- [];
  a.current_return_void <- false;
  a.loop_depth <- 0;
  a.uses_try <- false;
  push_scope a;
  List.iter (fun (p : param_sig) -> declare_local a p.name p.ty p.binding span) params;
  analyze_stmt a closure.body;
  let inferred_return = infer_return_type a "<closure>" span in
  pop_scope a;
  a.current_return_exprs <- outer_returns;
  a.current_return_void <- outer_void;
  a.loop_depth <- outer_loop_depth;
  a.uses_try <- outer_uses_try;
  if not (is_assignable declared_return inferred_return) then begin
    push_diag a "E381"
      (Printf.sprintf "closure returns %s, which cannot be assigned to declared return type %s"
         (sem_type_to_string inferred_return) (sem_type_to_string declared_return))
      closure.return_ty.span;
    Error
  end else
    intern_fn_ptr_sig a
      { params = List.map (fun (p : param_sig) -> p.ty) params
      ; return_ty = declared_return
      }

and indirect_call_callee_type a callee =
  match callee.kind with
  | Path path ->
      (match path.segments with
       | [ name ] ->
           (match lookup_local_ty a name with Some (FnPtr _ as ty) -> Some ty | _ -> None)
       | _ -> None)
  | _ ->
      (match analyze_expr a callee with FnPtr _ as ty -> Some ty | _ -> None)

and analyze_call a call span =
  match indirect_call_callee_type a call.callee with
  | Some fn_ty -> check_fn_ptr_call a fn_ty call span
  | None ->
      (match call.callee.kind with
       | Path path -> analyze_path_call a path call span
       | _ ->
           push_diag a "E370" "call target must be a function path or function pointer" call.callee.span;
           Error)

and analyze_path_call a path call span =
  match path.segments with
  | [ "type" ] -> check_unary_builtin a call span "E459" "`type` expects exactly one argument" Str
  | [ "size" ] ->
      (match call.args with
       | [ arg ] ->
        let arg_ty = analyze_expr a arg in
        if arg_ty = Error then Error
        else if not (is_numeric arg_ty) then begin
          push_diag a "E461" "`size` expects a numeric value" arg.span;
          Error
        end else Int
       | _ ->
        push_diag a "E460" "`size` expects exactly one argument" span;
        Error
      )
  | [ "cast" ] -> analyze_cast_call a call span
  | [ "copy" ] when not (has_function a "copy") ->
      (match call.args with
       | [ arg ] -> analyze_expr a arg
       | _ ->
           push_diag a "E543" "`copy` expects exactly one argument" span;
           Error)
  | [ module_; name ] ->
      let fn_key = module_ ^ "::" ^ name in
      (match function_sig_opt a fn_key with
       | Some sig_ -> check_function_call a sig_ call span fn_key
       | None ->
           (match Hashtbl.find_opt a.module_fns (module_, name) with
            | Some sig_ -> check_module_call a sig_ call span
            | None ->
                if module_ = "arr" then analyze_arr_call a name call span
                else if module_ = "map" then analyze_map_call a name call span
                else if module_ = "mem" then analyze_mem_call a name call span
                else if module_ = "gc" && StringSet.mem "gc" a.included_modules then
                  analyze_gc_call a name call span
                else if module_ = "cptr" then analyze_cptr_call a name call span
                else if module_ = "cstruct" then analyze_cstruct_call a name call span
                else if module_ = "cfn" then analyze_cfn_call a name call span
                else if module_ = "__index" && name = "set" then analyze_index_set_call a call span
                else if module_ = "__member" && name = "set" then analyze_member_set_call a call span
                else if module_ = "str" then analyze_str_call a name call span
                else if module_ = "res" then analyze_res_call a name call span
                else begin
                  if StringSet.mem module_ a.included_modules && not (module_exports a module_ name) then
                    push_diag a "E457" (Printf.sprintf "module `%s` has no member `%s`" module_ name) path.span
                  else
                    push_diag a "E376" (Printf.sprintf "unsupported function path `%s::%s`" module_ name) path.span;
                  Error
                end))
  | [ name ] ->
      (match function_sig_opt a name with
       | Some sig_ -> check_function_call a sig_ call span name
       | None ->
           (match Hashtbl.find_opt a.module_fns ("", name) with
            | Some sig_ -> check_module_call a sig_ call span
            | None ->
                push_diag a "E373" (Printf.sprintf "undefined function `%s`" name) path.span;
                Error))
  | _ ->
      push_diag a "E376" (Printf.sprintf "unsupported function path `%s`" (String.concat "::" path.segments)) path.span;
      Error

and check_unary_builtin a call span code message ret_ty =
  match call.args with
  | [ arg ] ->
    ignore (analyze_expr a arg);
    ret_ty
  | _ ->
    push_diag a code message span;
    Error

and analyze_cast_call a call span =
  match call.args with
  | [ value; width_arg ] ->
    let value_ty = analyze_expr a value in
    if value_ty = Error then Error
    else
      (match cast_width_literal width_arg with
       | None ->
           push_diag a "E481" "`cast` bit-width must be an integer literal" width_arg.span;
           Error
       | Some width ->
           if not (is_valid_int_width width) then begin
             push_diag a "E482"
               (Printf.sprintf
                  "cast width %d is not supported (sized types must be a multiple of 8, from 8 up to 65535)"
                  width)
               width_arg.span;
             Error
           end else
             match cast_result_type value_ty width with
             | Some ty -> ty
             | None ->
                 push_diag a "E483" "`cast` expects a numeric value" value.span;
                 Error)
  | _ ->
    push_diag a "E480" "`cast` expects arguments (value, bit-width)" span;
    Error

and analyze_arr_call a name call span =
  match name, call.args with
  | "new", [] -> ArrayUnknown
  | "new", _ -> push_diag a "E397" "`arr::new` expects no arguments" span; Error
  | "push", [ arr; value ] ->
      let arr_ty = analyze_expr a arr in
      let value_ty = analyze_expr a value in
      if not (is_array_type arr_ty) || not (can_store_in_array arr_ty value_ty) then begin
        push_diag a "E399" "`arr::push` value must match array element type" span;
        Error
      end else Void
  | "pop", [ arr ] -> require_array_call a arr span "E466" "`arr::pop` argument must be array"
  | "insert", [ arr; idx; value ] ->
      let arr_ty = analyze_expr a arr in
      let idx_ty = analyze_expr a idx in
      let value_ty = analyze_expr a value in
      if not (is_array_type arr_ty) || not (is_numeric idx_ty) || not (can_store_in_array arr_ty value_ty) then begin
        push_diag a "E465" "`arr::insert` requires (array, numeric-index, value-matching-array-type)" span;
        Error
      end else Void
  | "remove", [ arr; idx ] ->
      let arr_ty = analyze_expr a arr in
      let idx_ty = analyze_expr a idx in
      if not (is_array_type arr_ty) || not (is_numeric idx_ty) then begin
        push_diag a "E467" "`arr::remove` requires (array, numeric) arguments" span;
        Error
      end else Option.value (array_element_type arr_ty) ~default:NumUnknown
  | "clear", [ arr ] ->
      let arr_ty = analyze_expr a arr in
      if not (is_array_type arr_ty) then begin push_diag a "E469" "`arr::clear` argument must be array" arr.span; Error end
      else Void
  | ("contains" | "index_of"), [ arr; value ] ->
      let arr_ty = analyze_expr a arr in
      let value_ty = analyze_expr a value in
      if not (is_array_type arr_ty) || not (can_store_in_array arr_ty value_ty) then begin
        push_diag a "E471" (Printf.sprintf "`arr::%s` value must match array element type" name) span;
        Error
      end else Int
  | "len", [ arr ] ->
      let arr_ty = analyze_expr a arr in
      if not (is_array_type arr_ty) then begin push_diag a "E401" "`arr::len` argument must be array" arr.span; Error end
      else Int
  | "get", [ arr; idx ] -> analyze_arr_get a arr idx span false
  | "get_unchecked", [ arr; idx ] -> analyze_arr_get a arr idx span true
  | "set", [ arr; idx; value ] -> analyze_arr_set a arr idx value span false
  | "set_unchecked", [ arr; idx; value ] -> analyze_arr_set a arr idx value span true
  | "slice", [ arr; start; len ] ->
      let arr_ty = analyze_expr a arr in
      let start_ty = analyze_expr a start in
      let len_ty = analyze_expr a len in
      if not (is_array_type arr_ty) || not (is_numeric start_ty) || not (is_numeric len_ty) then begin
        push_diag a "E415" "`arr::slice` requires (array, numeric, numeric) arguments" span;
        Error
      end else arr_ty
  | "slice_from", [ arr; start ] ->
      let arr_ty = analyze_expr a arr in
      let start_ty = analyze_expr a start in
      if not (is_array_type arr_ty) || not (is_numeric start_ty) then begin
        push_diag a "E417" "`arr::slice_from` requires (array, numeric) arguments" span;
        Error
      end else arr_ty
  | _ ->
      push_diag a "E376" (Printf.sprintf "unsupported function path `arr::%s`" name) span;
      Error

and require_array_call a arr _span code message =
  let arr_ty = analyze_expr a arr in
  if not (is_array_type arr_ty) then begin push_diag a code message arr.span; Error end
  else Option.value (array_element_type arr_ty) ~default:NumUnknown

and analyze_arr_get a arr idx span unchecked =
  let arr_ty = analyze_expr a arr in
  let idx_ty = analyze_expr a idx in
  if not (is_array_type arr_ty) || not (is_numeric idx_ty) then begin
    push_diag a (if unchecked then "E410" else "E403")
      (if unchecked then "`arr::get_unchecked` requires an int/uint array and a numeric index"
       else "`arr::get` requires (array, numeric) arguments")
      span;
    Error
  end else
    let elem = array_element_type arr_ty |> Option.value ~default:NumUnknown in
    if unchecked && not (match elem with Int | IntN _ | Uint | UintN _ -> true | _ -> false) then begin
      push_diag a "E410" "`arr::get_unchecked` requires an int/uint array and a numeric index" span;
      Error
    end else elem

and analyze_arr_set a arr idx value span unchecked =
  let arr_ty = analyze_expr a arr in
  let idx_ty = analyze_expr a idx in
  let value_ty = analyze_expr a value in
  let elem = array_element_type arr_ty in
  let unchecked_ok =
    not unchecked
    || match elem with Some (Int | IntN _ | Uint | UintN _) -> true | _ -> false
  in
  if not (is_array_type arr_ty) || not (is_numeric idx_ty) || not unchecked_ok || not (can_store_in_array arr_ty value_ty) then begin
    push_diag a (if unchecked then "E409" else "E405")
      (if unchecked then "`arr::set_unchecked` requires an int/uint array, numeric index, and matching value"
       else "`arr::set` value must match array element type")
      span;
    Error
  end else Void

and analyze_res_call a name call span =
  match name, call.args with
  | "ok", [ value ] ->
      let ty = analyze_expr a value in
      if ty = Error then Error
      else if ty = Void then begin
        push_diag a "E530" "`res::ok` needs a value; a void call has none" span;
        Error
      end
      else Result ty
  | "err", [ message ] ->
      let ty = analyze_expr a message in
      if ty = Error then Error
      else if ty <> Str then begin
        push_diag a "E531" "`res::err` expects a string message" span;
        Error
      end
      else ResultUnknown
  | ("is_ok" | "is_err"), [ result ] ->
      let ty = analyze_expr a result in
      if ty = Error then Error
      else if not (is_result_type ty) then begin
        push_diag a "E532" (Printf.sprintf "`res::%s` expects a result" name) span;
        Error
      end
      else Bool
  | "value", [ result ] ->
      let ty = analyze_expr a result in
      if ty = Error then Error
      else (match ty with
            | Result payload -> payload
            | ResultUnknown -> NumUnknown
            | _ ->
                push_diag a "E532" "`res::value` expects a result" span;
                Error)
  | "error", [ result ] ->
      let ty = analyze_expr a result in
      if ty = Error then Error
      else if not (is_result_type ty) then begin
        push_diag a "E532" "`res::error` expects a result" span;
        Error
      end
      else Str
  | "or", [ result; fallback ] ->
      let result_ty = analyze_expr a result in
      let fallback_ty = analyze_expr a fallback in
      if result_ty = Error || fallback_ty = Error then Error
      else if not (is_result_type result_ty) then begin
        push_diag a "E532" "`res::or` expects (result, fallback) arguments" span;
        Error
      end
      else (match result_ty with
            | Result payload when payload <> NumUnknown ->
                if not (is_assignable payload fallback_ty) then begin
                  push_diag a "E533"
                    (Printf.sprintf "`res::or` fallback is %s, which does not match the result payload %s"
                       (xi_type_name a.struct_sigs_by_id fallback_ty)
                       (xi_type_name a.struct_sigs_by_id payload))
                    span;
                  Error
                end
                else payload
            | _ -> fallback_ty)
  | ("ok" | "err" | "is_ok" | "is_err" | "value" | "error" | "or"), _ ->
      push_diag a "E534" (Printf.sprintf "`res::%s` was called with the wrong number of arguments" name) span;
      Error
  | _ ->
      push_diag a "E457" (Printf.sprintf "module `res` has no member `%s`" name) span;
      Error

and analyze_map_call a name call span =
  match name, call.args with
  | "new", [] -> MapUnknown
  | "new", _ -> push_diag a "E418" "`map::new` expects no arguments" span; Error
  | "set", [ map; key; value ] ->
      let map_ty = analyze_expr a map in
      let key_ty = analyze_expr a key in
      let value_ty = analyze_expr a value in
      if not (is_map_type map_ty) || not (can_use_map_key map_ty key_ty) || not (can_store_in_map map_ty value_ty) then begin
        push_diag a "E420" "`map::set` requires (map, key-matching-map-type, value-matching-map-type) arguments" span;
        Error
      end else Void
  | "get", [ map; key ] -> analyze_map_get_like a map key span "E422" "`map::get` requires (map, key-matching-map-type) arguments"
  | "get_or", [ map; key; fallback ] ->
      let map_ty = analyze_expr a map in
      let key_ty = analyze_expr a key in
      let fallback_ty = analyze_expr a fallback in
      if not (is_map_type map_ty) || not (can_use_map_key map_ty key_ty) || not (can_store_in_map map_ty fallback_ty) then begin
        push_diag a "E473" "`map::get_or` requires (map, key-matching-map-type, value-matching-map-type) arguments" span;
        Error
      end else Option.value (map_value_type map_ty) ~default:Str
  | "has", [ map; key ] ->
      let map_ty = analyze_expr a map in
      let key_ty = analyze_expr a key in
      if not (is_map_type map_ty) || not (can_use_map_key map_ty key_ty) then begin
        push_diag a "E424" "`map::has` requires (map, key-matching-map-type) arguments" span;
        Error
      end else Int
  | "del", [ map; key ] ->
      let map_ty = analyze_expr a map in
      let key_ty = analyze_expr a key in
      if not (is_map_type map_ty) || not (can_use_map_key map_ty key_ty) then begin
        push_diag a "E426" "`map::del` requires (map, key-matching-map-type) arguments" span;
        Error
      end else Void
  | "clear", [ map ] ->
      let map_ty = analyze_expr a map in
      if not (is_map_type map_ty) then begin push_diag a "E475" "`map::clear` argument must be map" map.span; Error end
      else Void
  | "len", [ map ] ->
      let map_ty = analyze_expr a map in
      if not (is_map_type map_ty) then begin push_diag a "E428" "`map::len` argument must be map" map.span; Error end
      else Int
  | "keys", [ map ] ->
      let map_ty = analyze_expr a map in
      if not (is_map_type map_ty) then begin push_diag a "E477" "`map::keys` argument must be map" map.span; Error end
      else map_keys_array_type map_ty
  | "values", [ map ] ->
      let map_ty = analyze_expr a map in
      if not (is_map_type map_ty) then begin push_diag a "E479" "`map::values` argument must be map" map.span; Error end
      else array_type_from_element (Option.value (map_value_type map_ty) ~default:Str)
  | _ ->
      push_diag a "E376" (Printf.sprintf "unsupported function path `map::%s`" name) span;
      Error

and analyze_map_get_like a map key span code message =
  let map_ty = analyze_expr a map in
  let key_ty = analyze_expr a key in
  if not (is_map_type map_ty) || not (can_use_map_key map_ty key_ty) then begin
    push_diag a code message span;
    Error
  end else Option.value (map_value_type map_ty) ~default:Str

and analyze_gc_call a name call span =
  a.uses_gc <- true;
  a.gc_use_spans <- span :: a.gc_use_spans;
  match name, call.args with
  | "collect", [] -> Void
  | ("collections" | "live"), [] -> Int
  | ("collect" | "collections" | "live"), _ ->
      push_diag a "E540" (Printf.sprintf "`gc::%s` expects no arguments" name) span;
      Error
  | _ ->
      push_diag a "E457" (Printf.sprintf "module `gc` has no member `%s`" name) span;
      Error

and analyze_mem_call a name call span =
  let args = call.args in
  let require_count n msg code =
    if List.length args <> n then begin push_diag a code msg span; false end else true
  in
  match name, args with
  | "copy", [ dst; dst_off; src; src_off; count ] ->
      let dst_ty = analyze_expr a dst and dst_off_ty = analyze_expr a dst_off in
      let src_ty = analyze_expr a src and src_off_ty = analyze_expr a src_off in
      let count_ty = analyze_expr a count in
      if not (is_numeric_array dst_ty) || not (is_numeric_array src_ty) || not (is_integer dst_off_ty)
         || not (is_integer src_off_ty) || not (is_integer count_ty)
      then begin push_diag a "E551" "`mem::copy` requires (numeric-array, int, numeric-array, int, int)" span; Error end
      else Void
  | "set", [ buf; off; value; count ] ->
      let buf_ty = analyze_expr a buf and off_ty = analyze_expr a off in
      let value_ty = analyze_expr a value and count_ty = analyze_expr a count in
      if not (is_numeric_array buf_ty) || not (is_integer off_ty) || not (is_numeric value_ty) || not (is_integer count_ty)
      then begin push_diag a "E552" "`mem::set` requires (numeric-array, int, numeric, int)" span; Error end
      else Void
  | "cmp", [ a_expr; a_off; b_expr; b_off; count ] ->
      let a_ty = analyze_expr a a_expr and a_off_ty = analyze_expr a a_off in
      let b_ty = analyze_expr a b_expr and b_off_ty = analyze_expr a b_off in
      let count_ty = analyze_expr a count in
      if not (is_numeric_array a_ty) || not (is_numeric_array b_ty) || not (is_integer a_off_ty)
         || not (is_integer b_off_ty) || not (is_integer count_ty)
      then begin push_diag a "E553" "`mem::cmp` requires (numeric-array, int, numeric-array, int, int)" span; Error end
      else Int
  | ("read_be" | "read_le"), [ buf; off; nbytes ] ->
      let buf_ty = analyze_expr a buf and off_ty = analyze_expr a off and nbytes_ty = analyze_expr a nbytes in
      if not (is_numeric_array buf_ty) || not (is_integer off_ty) || not (is_integer nbytes_ty)
      then begin push_diag a "E554" (Printf.sprintf "`mem::%s` requires (numeric-array, int, int)" name) span; Error end
      else Int
  | ("write_be" | "write_le"), [ buf; off; value; nbytes ] ->
      let buf_ty = analyze_expr a buf and off_ty = analyze_expr a off in
      let value_ty = analyze_expr a value and nbytes_ty = analyze_expr a nbytes in
      if not (is_numeric_array buf_ty) || not (is_integer off_ty) || not (is_integer value_ty) || not (is_integer nbytes_ty)
      then begin push_diag a "E555" (Printf.sprintf "`mem::%s` requires (numeric-array, int, int, int)" name) span; Error end
      else Void
  | "copy", _ -> ignore (require_count 5 "`mem::copy` expects five arguments" "E551"); Error
  | "set", _ -> ignore (require_count 4 "`mem::set` expects four arguments" "E552"); Error
  | "cmp", _ -> ignore (require_count 5 "`mem::cmp` expects five arguments" "E553"); Error
  | ("read_be" | "read_le"), _ ->
      ignore (require_count 3 (Printf.sprintf "`mem::%s` expects three arguments" name) "E554"); Error
  | ("write_be" | "write_le"), _ ->
      ignore (require_count 4 (Printf.sprintf "`mem::%s` expects four arguments" name) "E555"); Error
  | _ ->
      push_diag a "E457" (Printf.sprintf "module `mem` has no member `%s`" name) span;
      Error

and cptr_read_result name =
  match name with
  | "read_i8" -> Some (IntN 8) | "read_i16" -> Some (IntN 16)
  | "read_i32" -> Some (IntN 32) | "read_i64" -> Some (IntN 64)
  | "read_u8" -> Some (UintN 8) | "read_u16" -> Some (UintN 16)
  | "read_u32" -> Some (UintN 32) | "read_u64" -> Some (UintN 64)
  | "read_f32" -> Some (DecN 32) | "read_f64" -> Some (DecN 64)
  | "read_ptr" -> Some Cptr
  | _ -> None

and analyze_cptr_call a name call span =
  let cptr_arg e =
    let t = analyze_expr a e in
    if t <> Error && t <> Cptr then
      push_diag a "E490" (Printf.sprintf "`cptr::%s` expects a `cptr` argument" name) e.span
  in
  let int_arg e =
    let t = analyze_expr a e in
    if t <> Error && not (is_integer t) then
      push_diag a "E491" (Printf.sprintf "`cptr::%s` expects an integer argument" name) e.span
  in
  let num_arg e =
    let t = analyze_expr a e in
    if t <> Error && not (is_numeric t) then
      push_diag a "E492" (Printf.sprintf "`cptr::%s` expects a numeric argument" name) e.span
  in
  match name, call.args with
  | "alloc", [ n ] -> int_arg n; Cptr
  | "free", [ p ] -> cptr_arg p; Void
  | "null", [] -> Cptr
  | "is_null", [ p ] -> cptr_arg p; Bool
  | "addr", [ p ] -> cptr_arg p; UintN 64
  | "from_addr", [ n ] -> int_arg n; Cptr
  | "offset", [ p; n ] -> cptr_arg p; int_arg n; Cptr
  | _, [ p; off ] when cptr_read_result name <> None ->
      cptr_arg p; int_arg off; Option.get (cptr_read_result name)
  | "read_cstr", [ p; off ] -> cptr_arg p; int_arg off; Str
  | "write_cstr", [ p; off; s ] ->
      cptr_arg p; int_arg off;
      let s_ty = analyze_expr a s in
      if s_ty <> Error && s_ty <> Str then
        push_diag a "E493" "`cptr::write_cstr` expects a string value" s.span;
      Int
  | "write_ptr", [ p; off; v ] -> cptr_arg p; int_arg off; cptr_arg v; Void
  | ( ("write_i8" | "write_i16" | "write_i32" | "write_i64"
      | "write_u8" | "write_u16" | "write_u32" | "write_u64"), [ p; off; v ] ) ->
      cptr_arg p; int_arg off; int_arg v; Void
  | ("write_f32" | "write_f64"), [ p; off; v ] -> cptr_arg p; int_arg off; num_arg v; Void
  | ("alloc" | "free" | "null" | "is_null" | "addr" | "from_addr" | "offset"
    | "read_i8" | "read_i16" | "read_i32" | "read_i64"
    | "read_u8" | "read_u16" | "read_u32" | "read_u64"
    | "read_f32" | "read_f64" | "read_ptr" | "read_cstr"
    | "write_cstr" | "write_ptr" | "write_i8" | "write_i16" | "write_i32"
    | "write_i64" | "write_u8" | "write_u16" | "write_u32" | "write_u64"
    | "write_f32" | "write_f64"), _ ->
      push_diag a "E494" (Printf.sprintf "unsupported or mis-argumented `cptr::%s`" name) span;
      Error
  | _ ->
      push_diag a "E457" (Printf.sprintf "module `cptr` has no member `%s`" name) span;
      Error

and analyze_cstruct_call a name call span =
  let cstruct_id_of_name_arg e =
    match e.kind with
    | Path { segments = [ n ]; _ } ->
        (match assoc_opt n a.cstruct_ids with
         | Some id -> Some id
         | None ->
             push_diag a "E513" (Printf.sprintf "`%s` is not a `cstruct` type" n) e.span;
             None)
    | _ ->
        push_diag a "E513" (Printf.sprintf "`cstruct::%s` expects a cstruct type name" name) e.span;
        None
  in
  match name, call.args with
  | "size", [ ty ] -> (match cstruct_id_of_name_arg ty with _ -> Int)
  | "alloc", [ ty ] ->
      (match cstruct_id_of_name_arg ty with Some id -> CStruct id | None -> Error)
  | "view", [ p; ty ] ->
      let p_ty = analyze_expr a p in
      if p_ty <> Error && p_ty <> Cptr then
        push_diag a "E514" "`cstruct::view` expects a `cptr` as its first argument" p.span;
      (match cstruct_id_of_name_arg ty with Some id -> CStruct id | None -> Error)
  | "ptr", [ x ] ->
      let x_ty = analyze_expr a x in
      (match x_ty with
       | CStruct _ | Error -> Cptr
       | _ -> push_diag a "E515" "`cstruct::ptr` expects a cstruct value" x.span; Error)
  | "free", [ x ] ->
      let x_ty = analyze_expr a x in
      (match x_ty with
       | CStruct _ | Error -> Void
       | _ -> push_diag a "E516" "`cstruct::free` expects a cstruct value" x.span; Error)
  | ("size" | "alloc" | "view" | "ptr" | "free"), _ ->
      push_diag a "E517" (Printf.sprintf "unsupported or mis-argumented `cstruct::%s`" name) span;
      Error
  | _ ->
      push_diag a "E457" (Printf.sprintf "module `cstruct` has no member `%s`" name) span;
      Error

and analyze_cfn_call a name call span =
  match name, call.args with
  | "ptr", [ f ] ->
      (match f.kind with
       | Path { segments = [ fname ]; _ } when has_function a fname ->
           a.cfn_refs <- (fname, f.span) :: a.cfn_refs;
           Cptr
       | Path { segments = [ fname ]; _ } ->
           push_diag a "E520"
             (Printf.sprintf "`cfn::ptr` expects a top-level function; `%s` is not one" fname) f.span;
           Error
       | _ ->
           push_diag a "E520" "`cfn::ptr` expects a top-level function name" f.span;
           Error)
  | "ptr", _ ->
      push_diag a "E523" "`cfn::ptr` expects exactly one argument" span;
      Error
  | _ ->
      push_diag a "E457" (Printf.sprintf "module `cfn` has no member `%s`" name) span;
      Error

and analyze_str_call a name call span =
  match name, call.args with
  | "len", [ s ] ->
      let ty = analyze_expr a s in
      if ty <> Str then begin push_diag a "E456" "`str::len` expects a string" s.span; Error end else Int
  | "char_code_at", [ s; idx ] ->
      let s_ty = analyze_expr a s in
      let idx_ty = analyze_expr a idx in
      if s_ty <> Str || not (is_integer idx_ty) then begin
        push_diag a "E557" "`str::char_code_at` expects (string, int)" span;
        Error
      end else Int
  | "from_char_code", [ code ] ->
      let code_ty = analyze_expr a code in
      if not (is_integer code_ty) then begin push_diag a "E558" "`str::from_char_code` expects an int" code.span; Error end
      else Str
  | "len", _ -> push_diag a "E456" "`str::len` expects exactly one string argument" span; Error
  | "char_code_at", _ -> push_diag a "E557" "`str::char_code_at` expects (string, int)" span; Error
  | "from_char_code", _ -> push_diag a "E558" "`str::from_char_code` expects one integer" span; Error
  | _ -> push_diag a "E457" (Printf.sprintf "module `str` has no member `%s`" name) span; Error

and analyze_index_set_call a call span =
  match call.args with
  | [ base; idx; value ] ->
      let base_ty = analyze_expr a base in
      let idx_ty = analyze_expr a idx in
      let value_ty = analyze_expr a value in
      if is_array_type base_ty then
        if not (is_numeric idx_ty) || not (can_store_in_array base_ty value_ty) then begin
          push_diag a "E430" "array index assignment requires (array, numeric-index, value-matching-array-type)" span;
          Error
        end else Void
      else if is_map_type base_ty then
        if not (can_use_map_key base_ty idx_ty) || not (can_store_in_map base_ty value_ty) then begin
          push_diag a "E443" "map index assignment requires (map, key-matching-map-type, value-matching-map-type)" span;
          Error
        end else Void
      else begin
        push_diag a "E406" "index base must be an array or map value" base.span;
        Error
      end
  | _ ->
      push_diag a "E429" "index assignment expects (base, index, value)" span;
      Error

and analyze_member_set_call a call span =
  match call.args with
  | [ base; field; value ] ->
      let base_ty = analyze_expr a base in
      let field_name =
        match field.kind with
        | Literal (LitStr s) -> Some s
        | _ ->
            push_diag a "E435" "member assignment requires a string field selector" field.span;
            None
      in
      let value_ty = analyze_expr a value in
      (match base_ty, field_name with
       | Struct struct_id, Some field_name ->
           (match struct_field_type a struct_id field_name with
            | None ->
                push_diag a "E433" (Printf.sprintf "unknown struct field `%s`" field_name) field.span;
                Error
            | Some field_ty ->
                if not (is_assignable field_ty value_ty) then begin
                  push_diag a "E436"
                    (Printf.sprintf "cannot assign %s to field `%s` of type %s"
                       (sem_type_to_string value_ty) field_name (sem_type_to_string field_ty))
                    value.span;
                  Error
                end else Void)
       | CStruct cstruct_id, Some field_name ->
           (match cstruct_field a cstruct_id field_name with
            | None ->
                push_diag a "E433" (Printf.sprintf "unknown cstruct field `%s`" field_name) field.span;
                Error
            | Some field ->
                (match field.cfs_ty with
                 | CStruct _ ->
                     push_diag a "E511"
                       (Printf.sprintf "cannot assign a whole nested cstruct field `%s`; set its scalar fields directly (e.g. `x.%s.field = …`)" field_name field_name)
                       value.span;
                     Error
                 | field_ty ->
                     if not (is_assignable field_ty value_ty) then begin
                       push_diag a "E436"
                         (Printf.sprintf "cannot assign %s to cstruct field `%s` of type %s"
                            (sem_type_to_string value_ty) field_name (sem_type_to_string field_ty))
                         value.span;
                       Error
                     end else Void))
       | Error, _ -> Error
       | _ ->
           push_diag a "E432" "member access base must be a struct value" base.span;
           Error)
  | _ ->
      push_diag a "E434" "member assignment expects (base, field, value)" span;
      Error

and check_fn_ptr_call a fn_ty call span =
  match fn_ty with
  | FnPtr sig_id ->
      (match List.nth_opt a.fn_ptr_sigs sig_id with
       | None -> Error
       | Some sig_ ->
           if List.length call.args <> List.length sig_.params then begin
             push_diag a "E372"
               (Printf.sprintf "function pointer expects %d arguments, got %d"
                  (List.length sig_.params) (List.length call.args))
               span;
             Error
           end else
             let had_error = ref false in
             List.iter2
               (fun arg expected ->
                 let actual = analyze_expr a arg in
                 if not (is_assignable expected actual) then begin
                   push_diag a "E378"
                     (Printf.sprintf "function pointer argument has type %s, expected %s"
                        (sem_type_to_string actual) (sem_type_to_string expected))
                     arg.span;
                   had_error := true
                 end)
               call.args sig_.params;
             if !had_error then Error else sig_.return_ty)
  | _ -> Error

and check_function_call a sig_ call span display_name =
  if List.length sig_.params <> List.length call.args then begin
    push_diag a "E374"
      (Printf.sprintf "function `%s` expects %d args but got %d"
         display_name (List.length sig_.params) (List.length call.args))
      span;
    Error
  end else begin
    List.iter2
      (fun param arg ->
        let arg_ty = analyze_expr a arg in
        if param.binding = Ref && expr_as_local_name arg = None then
          push_diag a "E377"
            (Printf.sprintf "argument for `ref` parameter `%s` must be a local variable path" param.name)
            arg.span;
        if not (is_assignable param.ty arg_ty) then
          push_diag a "E375"
            (Printf.sprintf "arg for parameter `%s` expected %s, got %s"
               param.name (sem_type_to_string param.ty) (sem_type_to_string arg_ty))
            arg.span)
      sig_.params call.args;
    sig_.return_ty
  end

and check_module_call a sig_ call span =
  if List.length sig_.params <> List.length call.args then begin
    push_diag a "E455"
      (Printf.sprintf "`%s::%s` expects %d argument(s) but got %d"
         sig_.module_ sig_.name (List.length sig_.params) (List.length call.args))
      span;
    Error
  end else begin
    List.iter2
      (fun param arg ->
        let arg_ty = analyze_expr a arg in
        if param.binding = Ref && expr_as_local_name arg = None then
          push_diag a "E377"
            (Printf.sprintf "argument for `ref` parameter `%s` must be a local variable path" param.name)
            arg.span;
        if not (is_assignable param.ty arg_ty) then
          push_diag a "E498"
            (Printf.sprintf "`%s::%s` argument `%s` expected %s, got %s"
               sig_.module_ sig_.name param.name (sem_type_to_string param.ty) (sem_type_to_string arg_ty))
            arg.span)
      sig_.params call.args;
    sig_.return_ty
  end

let analyze_functions a (program : Ast.program) =
  List.iter
    (fun (function_ : Ast.function_decl) ->
      a.scopes <- [];
      a.current_return_exprs <- [];
      a.current_return_void <- false;
      a.loop_depth <- 0;
      a.uses_try <- false;
      push_scope a;
      (match function_sig_opt a function_.name with
       | Some sig_ ->
           List.iter
             (fun (param : param_sig) -> declare_local a param.name param.ty param.binding function_.span)
             sig_.params
       | None -> ());
      analyze_stmt a function_.body;
      let return_ty = infer_return_type a function_.name function_.span in
      if a.uses_try then begin
        if function_.name = "main" then
          push_diag a "E537"
            "`main` cannot propagate with `try`; handle the result with `res::is_ok` / `res::or`"
            function_.span
        else if not (is_result_type return_ty) then
          push_diag a "E538"
            (Printf.sprintf
               "`%s` uses `try`, so every `return` in it must be a result; return `res::ok(value)` instead of a bare value"
               function_.name)
            function_.span
      end;
      (match function_sig_opt a function_.name with
       | Some sig_ ->
           Hashtbl.replace a.function_sigs function_.name { sig_ with return_ty }
       | None -> ());
      pop_scope a)
    program.functions

(* A function's return type is inferred from its body, so a call to a function
   that has not been analyzed yet reads back as `NumUnknown` — with declaration
   order deciding which calls those are. That is fine for a numeric result (the
   unknown unifies later) but not for a struct, array, or map: binding one to a
   typed local is E330, and passing one as an argument is E375. Mutually
   recursive functions have no declaration order that avoids it.

   So run the body pass repeatedly until no signature changes. Each pass starts
   from the signatures the previous one produced, so a forward call resolves as
   soon as its callee has been through the analyzer once. Only the final pass
   keeps its diagnostics; earlier ones are discarded because they may complain
   about types that were merely not inferred yet. *)
let analyze_functions_to_fixpoint a (program : Ast.program) =
  let base_diagnostics = a.diagnostics in
  let base_cfn_refs = a.cfn_refs in
  let return_types () =
    List.map
      (fun (function_ : Ast.function_decl) ->
        match function_sig_opt a function_.name with
        | Some sig_ -> sig_.return_ty
        | None -> Error)
      program.functions
  in
  let max_passes = List.length program.functions + 1 in
  let rec loop pass =
    a.diagnostics <- base_diagnostics;
    a.cfn_refs <- base_cfn_refs;
    let before = return_types () in
    analyze_functions a program;
    if return_types () <> before && pass < max_passes then loop (pass + 1)
  in
  loop 1

let validate_cfn_refs a =
  List.iter
    (fun (fname, span) ->
      match function_sig_opt a fname with
      | None -> ()
      | Some sig_ ->
          List.iter
            (fun (p : param_sig) ->
              if p.binding <> Value then
                push_diag a "E521"
                  (Printf.sprintf "callback `%s` cannot have a `ref`/`link` parameter (`%s`)" fname p.name)
                  span
              else if not (c_abi_boundary_ok ~allow_void:false p.ty) then
                push_diag a "E521"
                  (Printf.sprintf "callback `%s`: parameter `%s` has type `%s`, which is not C-ABI-safe. Callback parameters must use 8/16/32/64-bit integers, `cptr`, `bool`, `dec16/32/64`, or a cstruct"
                     fname p.name (boundary_type_desc a.struct_sigs_by_id p.ty))
                  span)
            sig_.params;
          if not (c_abi_boundary_ok ~allow_void:true sig_.return_ty) then
            push_diag a "E522"
              (Printf.sprintf "callback `%s`: return type `%s` is not C-ABI-safe. Return a fixed-width value (e.g. `int64`) or `void` — bare `int`/`dec` are arbitrary-precision boxes (a bare `return 0;` infers `int`; use `cast(0, 64)` or an `int64` local)"
                 fname (boundary_type_desc a.struct_sigs_by_id sig_.return_ty))
              span)
    a.cfn_refs

let analyze (program : Ast.program) =
  let included_modules =
    List.fold_left
      (fun acc (inc : Ast.include_decl) -> StringSet.add inc.module_ acc)
      StringSet.empty program.includes
  in
  let analyzer =
    { function_sigs = Hashtbl.create (max 16 (List.length program.functions))
    ; fn_ptr_sigs = []
    ; struct_ids = []
    ; struct_sigs_by_name = []
    ; struct_sigs_by_id = []
    ; cstruct_ids = []
    ; cstruct_sigs_by_name = []
    ; cstruct_sigs_by_id = []
    ; cstruct_decls = []
    ; modules = empty_modules
    ; module_fns = Hashtbl.create 64
    ; module_consts = Hashtbl.create 16
    ; module_intrinsics = Hashtbl.create 64
    ; included_modules
    ; diagnostics = []
    ; scopes = []
    ; current_return_exprs = []
    ; current_return_void = false
    ; cfn_refs = []
    ; loop_depth = 0
    ; uses_try = false
    ; uses_gc = false
    ; gc_use_spans = []
    }
  in
  collect_structs analyzer program;
  collect_modules analyzer program;
  collect_signatures analyzer program;
  analyze_functions_to_fixpoint analyzer program;
  validate_cfn_refs analyzer;
  { function_sigs =
      List.filter_map
        (fun (function_ : Ast.function_decl) ->
          Option.map
            (fun sig_ -> function_.name, sig_)
            (function_sig_opt analyzer function_.name))
        program.functions
  ; fn_ptr_sigs = analyzer.fn_ptr_sigs
  ; struct_sigs_by_name = List.rev analyzer.struct_sigs_by_name
  ; struct_sigs_by_id = List.rev analyzer.struct_sigs_by_id
  ; cstruct_sigs_by_name = List.rev analyzer.cstruct_sigs_by_name
  ; cstruct_sigs_by_id = List.rev analyzer.cstruct_sigs_by_id
  ; modules =
      { fns = List.rev analyzer.modules.fns
      ; consts = List.rev analyzer.modules.consts
      ; intrinsics = List.rev analyzer.modules.intrinsics
      }
  ; uses_gc = analyzer.uses_gc
  ; gc_use_spans = List.sort_uniq compare analyzer.gc_use_spans
  ; diagnostics = List.rev analyzer.diagnostics
  }

let rec to_ir_sem_type = function
  | Int -> Ir.Int
  | IntN w -> Ir.IntN w
  | Uint -> Ir.Uint
  | UintN w -> Ir.UintN w
  | Dec -> Ir.Dec
  | DecN w -> Ir.DecN w
  | Bool -> Ir.Bool
  | Str -> Ir.Str
  | Cptr -> Ir.Cptr
  | ArrayInt w -> Ir.ArrayInt w
  | ArrayUint w -> Ir.ArrayUint w
  | ArrayDec -> Ir.ArrayDec
  | ArrayBool -> Ir.ArrayBool
  | ArrayStr -> Ir.ArrayStr
  | ArrayStruct id -> Ir.ArrayStruct id
  | ArrayFnPtr id -> Ir.ArrayFnPtr id
  | ArrayUnknown -> Ir.ArrayUnknown
  | MapInt -> Ir.MapInt
  | MapDec -> Ir.MapDec
  | MapBool -> Ir.MapBool
  | MapStr -> Ir.MapStr
  | MapStruct id -> Ir.MapStruct id
  | MapFnPtr id -> Ir.MapFnPtr id
  | MapKeyed (key, value) -> Ir.MapKeyed (to_ir_map_key_type key, to_ir_map_value_type value)
  | MapUnknown -> Ir.MapUnknown
  | Struct id -> Ir.Struct id
  | Result payload -> Ir.Result (to_ir_sem_type payload)
  | ResultUnknown -> Ir.ResultUnknown
  | CStruct id -> Ir.CStruct id
  | FnPtr id -> Ir.FnPtr id
  | Void -> Ir.Void
  | NumUnknown -> Ir.NumUnknown
  | Error -> Ir.Error

and to_ir_map_key_type = function KeyInt -> Ir.KeyInt | KeyDec -> Ir.KeyDec | KeyBool -> Ir.KeyBool

and to_ir_map_value_type = function
  | ValueInt -> Ir.ValueInt
  | ValueDec -> Ir.ValueDec
  | ValueBool -> Ir.ValueBool
  | ValueStr -> Ir.ValueStr
  | ValueStruct id -> Ir.ValueStruct id
  | ValueFnPtr id -> Ir.ValueFnPtr id
  | ValueUnknown -> Ir.ValueUnknown

let to_ir_result (sem : semantic_result) =
  let param (p : param_sig) = { Ir.ps_name = p.name; ps_ty = to_ir_sem_type p.ty; ps_binding = p.binding } in
  let fn_sig (name, (sig_ : function_sig)) =
    ( name
    , { Ir.fs_name = sig_.name
      ; fs_params = List.map param sig_.params
      ; fs_return_ty = to_ir_sem_type sig_.return_ty
      ; fs_span = sig_.span
      } )
  in
  let fn_ptr (sig_ : fn_ptr_sig) =
    { Ir.fps_params = List.map to_ir_sem_type sig_.params
    ; fps_return_ty = to_ir_sem_type sig_.return_ty
    }
  in
  let struct_field (field : struct_field_sig) = { Ir.sfs_name = field.name; sfs_ty = to_ir_sem_type field.ty } in
  let struct_sig (_, (sig_ : struct_sig)) =
    { Ir.ss_id = sig_.id
    ; ss_name = sig_.name
    ; ss_fields = List.map struct_field sig_.fields
    ; ss_span = sig_.span
    }
  in
  let module_fn ((module_, name), (sig_ : module_fn_sig)) =
    ( (module_, name)
    , { Ir.mfs_module = sig_.module_
      ; mfs_name = sig_.name
      ; mfs_params = List.map param sig_.params
      ; mfs_return_ty = to_ir_sem_type sig_.return_ty
      ; mfs_symbol = sig_.symbol
      } )
  in
  let module_const ((module_, name), (sig_ : module_const_sig)) =
    ( (module_, name)
    , { Ir.mcs_module = sig_.module_
      ; mcs_name = sig_.name
      ; mcs_ty = to_ir_sem_type sig_.ty
      ; mcs_symbol = sig_.symbol
      } )
  in
  let cstruct_field (field : cstruct_field_sig) =
    { Ir.cfs_name = field.cfs_name; cfs_ty = to_ir_sem_type field.cfs_ty; cfs_offset = field.cfs_offset }
  in
  let cstruct_sig (_, (sig_ : cstruct_sig)) =
    { Ir.cs_id = sig_.cs_id
    ; cs_name = sig_.cs_name
    ; cs_fields = List.map cstruct_field sig_.cs_fields
    ; cs_size = sig_.cs_size
    ; cs_align = sig_.cs_align
    }
  in
  { Ir.function_sigs = List.map fn_sig sem.function_sigs
  ; fn_ptr_sigs = List.map fn_ptr sem.fn_ptr_sigs
  ; struct_sigs_by_name =
      List.map (fun (name, sig_) -> name, struct_sig (name, sig_)) sem.struct_sigs_by_name
  ; struct_sigs_by_id =
      List.map (fun (id, sig_) -> id, struct_sig (string_of_int id, sig_)) sem.struct_sigs_by_id
  ; cstruct_sigs_by_name =
      List.map (fun (name, sig_) -> name, cstruct_sig (name, sig_)) sem.cstruct_sigs_by_name
  ; cstruct_sigs_by_id =
      List.map (fun (id, sig_) -> id, cstruct_sig (string_of_int id, sig_)) sem.cstruct_sigs_by_id
  ; modules =
      { Ir.mt_fns = List.map module_fn sem.modules.fns |> List.map snd
      ; mt_consts = List.map module_const sem.modules.consts |> List.map snd
      ; mt_intrinsics = sem.modules.intrinsics
      }
  ; diagnostics = sem.diagnostics
  }
