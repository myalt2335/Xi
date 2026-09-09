type program =
  { includes : include_decl list
  ; links : link_decl list
  ; subsystems : subsystem_decl list
  ; c_preambles : c_preamble_decl list
  ; c_functions : inline_c_fn_decl list
  ; structs : struct_decl list
  ; functions : function_decl list
  ; externs : extern_fn_decl list
  ; extern_consts : extern_const_decl list
  ; intrinsics : intrinsic_decl list
  }

and include_decl = { module_ : string; span : Span.t }

and link_decl = { library : string; span : Span.t }

and subsystem_decl = { name : string; span : Span.t }

and c_preamble_decl =
  { body : string
  ; body_offset : int
  ; origin : string option
  ; line : int option
  ; span : Span.t
  }

and inline_c_fn_decl =
  { name : string
  ; params : param list
  ; return_ty : type_ref
  ; symbol : string
  ; body : string
  ; body_offset : int
  ; origin : string option
  ; line : int option
  ; span : Span.t
  }

and extern_fn_decl =
  { name : string
  ; params : param list
  ; return_ty : type_ref
  ; symbol : string
  ; c_abi : bool
  ; span : Span.t
  }

and extern_const_decl =
  { name : string
  ; ty : type_ref
  ; symbol : string
  ; c_abi : bool
  ; span : Span.t
  }

and intrinsic_decl = { name : string; span : Span.t }

and struct_decl =
  { name : string
  ; fields : struct_field_decl list
  ; c_layout : bool
  ; span : Span.t
  }

and struct_field_decl =
  { ty : type_ref
  ; name : string
  ; span : Span.t
  }

and function_decl =
  { name : string
  ; params : param list
  ; body : stmt
  ; span : Span.t
  }

and binding_kind = Value | Ref | Link

and param =
  { binding : binding_kind
  ; ty : type_ref
  ; name : string
  ; span : Span.t
  }

and type_ref = { kind : type_kind; span : Span.t }

and type_kind =
  | Int of int option
  | Uint of int option
  | Dec of int option
  | Str
  | Bool
  | Array of type_kind
  | Map of type_kind * type_kind
  | Result of type_kind
  | Fn of type_kind list * type_kind
  | Named of string

and stmt =
  | VarDecl of var_decl_stmt
  | Assign of assign_stmt
  | Return of return_stmt
  | If of if_stmt
  | Switch of switch_stmt
  | While of while_stmt
  | For of for_stmt
  | ForEach of for_each_stmt
  | Break of jump_stmt
  | Continue of jump_stmt
  | IoChain of io_chain_stmt
  | ExprStmt of expr_stmt
  | Block of block_stmt

and var_decl_stmt =
  { binding : binding_kind
  ; ty : type_ref
  ; name : string
  ; init : expr option
  ; span : Span.t
  }

and assign_mode = Normal | ExplicitLink

and assign_stmt =
  { target : path_expr
  ; value : expr
  ; mode : assign_mode
  ; span : Span.t
  }

and return_stmt = { value : expr option; span : Span.t }

and jump_stmt = { span : Span.t }

and if_stmt =
  { cond : expr
  ; then_branch : stmt
  ; else_branch : stmt option
  ; span : Span.t
  }

and switch_stmt =
  { selector : expr
  ; cases : switch_case list
  ; default : switch_default option
  ; span : Span.t
  }

and switch_case =
  { value : expr
  ; body : stmt list
  ; span : Span.t
  }

and switch_default = { body : stmt list; span : Span.t }

and while_stmt =
  { cond : expr
  ; body : stmt
  ; span : Span.t
  }

and for_stmt =
  { init : for_init option
  ; cond : expr option
  ; step : for_step option
  ; body : stmt
  ; span : Span.t
  }

and for_each_stmt =
  { name : string
  ; name_span : Span.t
  ; collection : expr
  ; body : stmt
  ; span : Span.t
  }

and for_init =
  | ForInitVarDecl of var_decl_stmt
  | ForInitAssign of assign_stmt
  | ForInitExpr of expr

and for_step = ForStepAssign of assign_stmt | ForStepExpr of expr

and io_anchor = Wrt | Wrtl | Wrtr

and io_item = IoExpr of expr | Endl of Span.t

and io_chain_stmt =
  { anchor : io_anchor
  ; items : io_item list
  ; span : Span.t
  }

and expr_stmt = { expr : expr; span : Span.t }

and block_stmt = { statements : stmt list; span : Span.t }

and expr = { kind : expr_kind; span : Span.t }

and expr_kind =
  | Literal of literal
  | ArrayLiteral of array_literal_expr
  | StructLiteral of struct_literal_expr
  | Closure of closure_expr
  | Path of path_expr
  | Call of call_expr
  | Index of index_expr
  | Member of member_expr
  | Unary of unary_expr
  | Binary of binary_expr
  | Cast of cast_expr
  | Try of expr
  | Grouping of expr

and literal = LitInt of string | LitDec of string | LitStr of string | LitBool of bool

and path_expr = { segments : string list; span : Span.t }

and call_expr = { callee : expr; args : expr list }

and index_expr = { base : expr; index : expr }

and member_expr = { base : expr; field : string }

and array_literal_expr = { items : expr list }

and struct_literal_expr =
  { name : string
  ; fields : struct_literal_field_expr list
  }

and struct_literal_field_expr =
  { name : string
  ; value : expr
  ; span : Span.t
  }

and closure_expr =
  { params : param list
  ; return_ty : type_ref
  ; body : stmt
  }

and unary_expr = { op : unary_op; rhs : expr }

and unary_op = Neg | Not | BitNot

and binary_expr =
  { op : binary_op
  ; lhs : expr
  ; rhs : expr
  }

and cast_expr = { value : expr; ty : type_ref }

and binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  | BitAnd
  | BitOr
  | BitXor
