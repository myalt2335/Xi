(* Lir: the low-level IR the native backends consume.

   This is deliberately shaped to the code `Ir` already generates rather than to
   LLVM in general: no phi nodes, no aggregates in registers, no vectors, no
   invoke/landingpad, no varargs, no indirect calls. Every value produced by an
   instruction is consumed inside the same basic block, because `Ir` routes
   everything that outlives a block through an `alloca` slot (see
   `gen_var_decl_slot` and the parameter prologue in `emit_function`). Backends
   may exploit that, but are not required to.

   `Ir` constructs Lir directly. `--emit=llvm` prints that structure and
   `--emit=asm` consumes it directly, so lowering has one authoritative output
   and no parsing or parallel textual sink. *)

type ty =
  | I of int
  | F of int (* half = 16, float = 32, double = 64 *)
  | Ptr
  | Void

let size_bytes = function
  | I 1 -> 1
  | I bits -> max 1 (bits / 8)
  | F bits -> max 1 (bits / 8)
  | Ptr -> 8
  | Void -> 0

let is_float = function F _ -> true | _ -> false

let ty_name = function
  | I bits -> Printf.sprintf "i%d" bits
  | F 16 -> "half"
  | F 32 -> "float"
  | F _ -> "double"
  | Ptr -> "ptr"
  | Void -> "void"

type temp = { tname : string; mutable tid : int }

let temp tname = { tname; tid = -1 }

type value =
  | Temp of temp
  | Int_imm of string
  | Bool_imm of bool
  | Float_bits of string
  | Null
  | Global of string

type operand = ty * value

type abi_ext = No_ext | Sign_ext | Zero_ext

(* Most calls use no ABI extension metadata. These immutable lists are shared
   by arity so lowering does not allocate one redundant cons cell per argument
   for every runtime call. *)
let no_exts = function
  | 0 -> []
  | 1 -> [ No_ext ]
  | 2 -> [ No_ext; No_ext ]
  | 3 -> [ No_ext; No_ext; No_ext ]
  | 4 -> [ No_ext; No_ext; No_ext; No_ext ]
  | 5 -> [ No_ext; No_ext; No_ext; No_ext; No_ext ]
  | count -> List.init count (fun _ -> No_ext)

type icmp_pred = Eq | Ne | Slt | Sle | Sgt | Sge | Ult | Ule | Ugt | Uge

type fcmp_pred = Foeq | Fone | Folt | Fole | Fogt | Foge | Fueq | Fune

type binop =
  | Add
  | Sub
  | Mul
  | Sdiv
  | Udiv
  | Srem
  | Urem
  | And
  | Or
  | Xor
  | Shl
  | Lshr
  | Ashr

type fbinop = Fadd | Fsub | Fmul | Fdiv

type conv =
  | Trunc
  | Zext
  | Sext
  | Fptrunc
  | Fpext
  | Sitofp
  | Uitofp
  | Fptosi
  | Fptoui
  | Ptrtoint
  | Inttoptr
  | Bitcast

type instr =
  | Alloca of { dst : string; ty : ty; align : int }
  | Load of { dst : string; ty : ty; src : value; align : int }
  | Store of { ty : ty; src : value; dst : value; align : int }
  | Bin of { dst : string; op : binop; ty : ty; lhs : value; rhs : value }
  | Checked_arith of
      { dst : string; overflow : value; op : binop; lhs : value; rhs : value }
  | Fbin of { dst : string; op : fbinop; ty : ty; lhs : value; rhs : value }
  | Icmp of { dst : string; pred : icmp_pred; ty : ty; lhs : value; rhs : value }
  | Fcmp of { dst : string; pred : fcmp_pred; ty : ty; lhs : value; rhs : value }
  | Conv of { dst : string; op : conv; from_ty : ty; src : value; to_ty : ty }
  | Gep_global of { dst : string; sym : string }
  | Gep_byte of { dst : string; base : value; index : operand }
  (* dst = None for a void call. *)
  | Call of
      { dst : string option
      ; ret : ty
      ; ret_ext : abi_ext
      ; callee : callee
      ; args : operand list
      ; arg_exts : abi_ext list
      }

and callee = Direct of string | Indirect of value

type term =
  | Ret of operand option
  | Br of string
  | Br_cond of { cond : value; then_label : string; else_label : string }
  | Switch of
      { ty : ty
      ; selector : value
      ; default_label : string
      ; cases : (string * string) list
      }
  | Unreachable

type block = { label : string; instrs : instr list; term : term }

type func =
  { name : string
  ; ret : ty
  ; ret_ext : abi_ext
  ; params : (ty * string) list
  ; param_exts : abi_ext list
  ; blocks : block list
  }

type global = { g_name : string; g_bytes : string; g_align : int }

type decl =
  { d_name : string
  ; d_ret : ty
  ; d_ret_ext : abi_ext
  ; d_params : ty list
  ; d_param_exts : abi_ext list
  }

type modul = { decls : decl list; funcs : func list; globals : global list }

let empty = { decls = []; funcs = []; globals = [] }

exception Malformed of string

let malformed fmt = Printf.ksprintf (fun s -> raise (Malformed s)) fmt

let ty_of_fragment text : ty =
  match text with
  | "ptr" -> Ptr
  | "void" -> Void
  | "i1" -> I 1
  | "i8" -> I 8
  | "i16" -> I 16
  | "i32" -> I 32
  | "i64" -> I 64
  | "half" -> F 16
  | "float" -> F 32
  | "double" -> F 64
  | other ->
      let n = String.length other in
      if n > 1 && other.[0] = 'i'
         && String.for_all (fun c -> c >= '0' && c <= '9') (String.sub other 1 (n - 1))
      then I (int_of_string (String.sub other 1 (n - 1)))
      else malformed "unsupported type %S" other

let ty_of_string text : ty =
  ty_of_fragment (String.trim text)

let half_bits_of_float d =
  let b = Int64.bits_of_float d in
  let sign = Int64.to_int (Int64.logand (Int64.shift_right_logical b 63) 1L) in
  let exp = Int64.to_int (Int64.logand (Int64.shift_right_logical b 52) 0x7FFL) in
  let mant = Int64.logand b 0xFFFFFFFFFFFFFL in
  let sbit = sign lsl 15 in
  let round m rest width =
    let tie = Int64.shift_left 1L (width - 1) in
    let c = Int64.compare rest tie in
    if c > 0 || (c = 0 && m land 1 = 1) then m + 1 else m
  in
  if exp = 0x7FF then
    sbit lor 0x7C00 lor (if Int64.equal mant 0L then 0 else 0x200)
  else if exp = 0 then sbit
  else
    let e = exp - 1023 in
    if e > 15 then sbit lor 0x7C00
    else if e >= -14 then
      let m = Int64.to_int (Int64.shift_right_logical mant 42) in
      let rest = Int64.logand mant 0x3FFFFFFFFFFL in
      sbit lor round (((e + 15) lsl 10) lor m) rest 42
    else if e >= -25 then
      (* Subnormal: shift the implicit-1 significand down into the 10-bit field.
         At e = -25 the value is exactly half the smallest subnormal, so
         ties-to-even correctly flushes it to zero. *)
      let significand = Int64.logor 0x10000000000000L mant in
      let shift = 28 - e in
      let m = Int64.to_int (Int64.shift_right_logical significand shift) in
      let rest = Int64.logand significand (Int64.sub (Int64.shift_left 1L shift) 1L) in
      sbit lor round m rest shift
    else sbit

let bits_in_width width d =
  match width with
  | 16 -> Printf.sprintf "%04X" (half_bits_of_float d)
  | 32 -> Printf.sprintf "%08lX" (Int32.bits_of_float d)
  | 64 -> Printf.sprintf "%016LX" (Int64.bits_of_float d)
  | w -> malformed "unsupported float width %d" w

let float_bits_of_decimal width text = bits_in_width width (float_of_string text)

let float_bits_of_hex width hex =
  bits_in_width width (Int64.float_of_bits (Int64.of_string ("0x" ^ hex)))

let value_of_fragment (ty : ty) text : value =
  if text = "" then malformed "empty value"
  else
    match text.[0] with
    | '%' -> Temp (temp text)
    | '@' -> Global (String.sub text 1 (String.length text - 1))
    | _ -> (
        match text with
        | "null" -> Null
        | "true" -> Bool_imm true
        | "false" -> Bool_imm false
        | "undef" | "poison" -> Int_imm "0"
        | _ -> (
            match ty with
            | F width ->
                let n = String.length text in
                if n > 2 && text.[0] = '0' && text.[1] = 'x' then
                  Float_bits (float_bits_of_hex width (String.sub text 2 (n - 2)))
                else Float_bits (float_bits_of_decimal width text)
            | _ -> Int_imm text))

let value_of_string (ty : ty) text : value =
  value_of_fragment ty (String.trim text)

let icmp_pred_of_string = function
  | "eq" -> Eq | "ne" -> Ne
  | "slt" -> Slt | "sle" -> Sle | "sgt" -> Sgt | "sge" -> Sge
  | "ult" -> Ult | "ule" -> Ule | "ugt" -> Ugt | "uge" -> Uge
  | p -> malformed "unsupported icmp predicate %S" p

let fcmp_pred_of_string = function
  | "oeq" -> Foeq | "one" -> Fone | "olt" -> Folt | "ole" -> Fole
  | "ogt" -> Fogt | "oge" -> Foge | "ueq" -> Fueq | "une" -> Fune
  | p -> malformed "unsupported fcmp predicate %S" p

let binop_of_string_opt = function
  | "add" -> Some Add | "sub" -> Some Sub | "mul" -> Some Mul
  | "sdiv" -> Some Sdiv | "udiv" -> Some Udiv
  | "srem" -> Some Srem | "urem" -> Some Urem
  | "and" -> Some And | "or" -> Some Or | "xor" -> Some Xor
  | "shl" -> Some Shl | "lshr" -> Some Lshr | "ashr" -> Some Ashr
  | _ -> None

let fbinop_of_string_opt = function
  | "fadd" -> Some Fadd | "fsub" -> Some Fsub
  | "fmul" -> Some Fmul | "fdiv" -> Some Fdiv
  | _ -> None

let conv_of_string_opt = function
  | "trunc" -> Some Trunc | "zext" -> Some Zext | "sext" -> Some Sext
  | "fptrunc" -> Some Fptrunc | "fpext" -> Some Fpext
  | "sitofp" -> Some Sitofp | "uitofp" -> Some Uitofp
  | "fptosi" -> Some Fptosi | "fptoui" -> Some Fptoui
  | "ptrtoint" -> Some Ptrtoint | "inttoptr" -> Some Inttoptr
  | "bitcast" -> Some Bitcast
  | _ -> None

let binop_of_string name =
  match binop_of_string_opt name with
  | Some op -> op
  | None -> malformed "unsupported binary op %S" name

let fbinop_of_string name =
  match fbinop_of_string_opt name with
  | Some op -> op
  | None -> malformed "unsupported float binary op %S" name

let conv_of_string name =
  match conv_of_string_opt name with
  | Some op -> op
  | None -> malformed "unsupported conversion %S" name

let decl_of_string line : decl =
  let line = String.trim line in
  let rest =
    if String.length line >= 8 && String.sub line 0 8 = "declare " then
      String.sub line 8 (String.length line - 8)
    else line
  in
  match String.index_opt rest ' ' with
  | None -> malformed "declaration without a return type: %S" line
  | Some sp -> (
      let ret = ty_of_string (String.sub rest 0 sp) in
      let after = String.trim (String.sub rest (sp + 1) (String.length rest - sp - 1)) in
      match String.index_opt after '(' with
      | None -> malformed "declaration without a parameter list: %S" line
      | Some op ->
          let name = String.trim (String.sub after 0 op) in
          let name =
            if String.length name > 0 && name.[0] = '@' then String.sub name 1 (String.length name - 1)
            else name
          in
          let params =
            match String.rindex_opt after ')' with
            | None -> malformed "declaration with an unterminated parameter list: %S" line
            | Some cl -> String.sub after (op + 1) (cl - op - 1)
          in
          let d_params =
            if String.trim params = "" then []
            else
              String.split_on_char ',' params
              |> List.map (fun p ->
                     let p = String.trim p in
                     match String.index_opt p ' ' with
                     | Some i -> ty_of_string (String.sub p 0 i)
                     | None -> ty_of_string p)
          in
          { d_name = name
          ; d_ret = ret
          ; d_ret_ext = No_ext
          ; d_params
          ; d_param_exts = List.map (fun _ -> No_ext) d_params
          })

let escape_c_string s =
  let out = Buffer.create (String.length s) in
  String.iter
    (fun ch ->
      let code = Char.code ch in
      if code >= 32 && code <= 126 && ch <> '\\' && ch <> '"' then Buffer.add_char out ch
      else Buffer.add_string out (Printf.sprintf "\\%02X" code))
    s;
  Buffer.contents out

let value_text ty = function
  | Temp t -> t.tname
  | Int_imm text -> text
  | Bool_imm b -> if b then "true" else "false"
  | Null -> "null"
  | Global name -> "@" ^ name
  | Float_bits hex -> (
      match ty with
      | F 32 ->
          let f = Int32.float_of_bits (Int32.of_string ("0x" ^ hex)) in
          Printf.sprintf "0x%016LX" (Int64.bits_of_float f)
      | _ -> "0x" ^ hex)

let operand_text (ty, value) = ty_name ty ^ " " ^ value_text ty value

let icmp_pred_text = function
  | Eq -> "eq" | Ne -> "ne"
  | Slt -> "slt" | Sle -> "sle" | Sgt -> "sgt" | Sge -> "sge"
  | Ult -> "ult" | Ule -> "ule" | Ugt -> "ugt" | Uge -> "uge"

let fcmp_pred_text = function
  | Foeq -> "oeq" | Fone -> "one" | Folt -> "olt" | Fole -> "ole"
  | Fogt -> "ogt" | Foge -> "oge" | Fueq -> "ueq" | Fune -> "une"

let binop_text = function
  | Add -> "add" | Sub -> "sub" | Mul -> "mul"
  | Sdiv -> "sdiv" | Udiv -> "udiv" | Srem -> "srem" | Urem -> "urem"
  | And -> "and" | Or -> "or" | Xor -> "xor"
  | Shl -> "shl" | Lshr -> "lshr" | Ashr -> "ashr"

let fbinop_text = function Fadd -> "fadd" | Fsub -> "fsub" | Fmul -> "fmul" | Fdiv -> "fdiv"

let conv_text = function
  | Trunc -> "trunc" | Zext -> "zext" | Sext -> "sext"
  | Fptrunc -> "fptrunc" | Fpext -> "fpext"
  | Sitofp -> "sitofp" | Uitofp -> "uitofp"
  | Fptosi -> "fptosi" | Fptoui -> "fptoui"
  | Ptrtoint -> "ptrtoint" | Inttoptr -> "inttoptr" | Bitcast -> "bitcast"

let callee_text = function Direct name -> "@" ^ name | Indirect v -> value_text Ptr v

let abi_ext_text = function
  | No_ext -> ""
  | Sign_ext -> "signext"
  | Zero_ext -> "zeroext"

let return_text ext ty =
  match abi_ext_text ext with
  | "" -> ty_name ty
  | attr -> attr ^ " " ^ ty_name ty

let operand_text_with_ext ext (ty, value) =
  match abi_ext_text ext with
  | "" -> operand_text (ty, value)
  | attr -> Printf.sprintf "%s %s %s" (ty_name ty) attr (value_text ty value)

let instr_text global_len instr =
  match instr with
  | Alloca { dst; ty; align } -> Printf.sprintf "  %s = alloca %s, align %d" dst (ty_name ty) align
  | Load { dst; ty; src; align } ->
      Printf.sprintf "  %s = load %s, ptr %s, align %d" dst (ty_name ty) (value_text Ptr src) align
  | Store { ty; src; dst; align } ->
      Printf.sprintf "  store %s %s, ptr %s, align %d" (ty_name ty) (value_text ty src)
        (value_text Ptr dst) align
  | Bin { dst; op; ty; lhs; rhs } ->
      Printf.sprintf "  %s = %s %s %s, %s" dst (binop_text op) (ty_name ty) (value_text ty lhs)
        (value_text ty rhs)
  | Checked_arith { dst; overflow; op; lhs; rhs } ->
      let intrinsic =
        match op with
        | Add -> "sadd" | Sub -> "ssub" | Mul -> "smul"
        | _ -> malformed "checked arithmetic does not support %s" (binop_text op)
      in
      Printf.sprintf
        "  %s.checked = call { i64, i1 } @llvm.%s.with.overflow.i64(i64 %s, i64 %s)\n  %s = extractvalue { i64, i1 } %s.checked, 0\n  %s.overflow = extractvalue { i64, i1 } %s.checked, 1\n  store i1 %s.overflow, ptr %s, align 1"
        dst intrinsic (value_text (I 64) lhs) (value_text (I 64) rhs) dst dst dst dst dst
        (value_text Ptr overflow)
  | Fbin { dst; op; ty; lhs; rhs } ->
      Printf.sprintf "  %s = %s %s %s, %s" dst (fbinop_text op) (ty_name ty) (value_text ty lhs)
        (value_text ty rhs)
  | Icmp { dst; pred; ty; lhs; rhs } ->
      Printf.sprintf "  %s = icmp %s %s %s, %s" dst (icmp_pred_text pred) (ty_name ty)
        (value_text ty lhs) (value_text ty rhs)
  | Fcmp { dst; pred; ty; lhs; rhs } ->
      Printf.sprintf "  %s = fcmp %s %s %s, %s" dst (fcmp_pred_text pred) (ty_name ty)
        (value_text ty lhs) (value_text ty rhs)
  | Conv { dst; op; from_ty; src; to_ty } ->
      Printf.sprintf "  %s = %s %s %s to %s" dst (conv_text op) (ty_name from_ty)
        (value_text from_ty src) (ty_name to_ty)
  | Gep_global { dst; sym } ->
      let len =
        match global_len sym with
        | Some n -> n
        | None -> malformed "getelementptr names @%s, which has no global" sym
      in
      Printf.sprintf "  %s = getelementptr inbounds [%d x i8], ptr @%s, i64 0, i64 0" dst len sym
  | Gep_byte { dst; base; index } ->
      Printf.sprintf "  %s = getelementptr inbounds i8, ptr %s, i64 %s" dst (value_text Ptr base)
        (value_text (fst index) (snd index))
  | Call { dst; ret; ret_ext; callee; args; arg_exts } ->
      let args =
        if List.length args <> List.length arg_exts then
          malformed "call ABI metadata has %d entries for %d arguments"
            (List.length arg_exts) (List.length args);
        String.concat ", " (List.map2 operand_text_with_ext arg_exts args)
      in
      (match dst with
       | Some dst ->
           Printf.sprintf "  %s = call %s %s(%s)" dst
             (return_text ret_ext ret) (callee_text callee) args
       | None -> Printf.sprintf "  call void %s(%s)" (callee_text callee) args)

let term_text = function
  | Ret None -> "  ret void"
  | Ret (Some op) -> "  ret " ^ operand_text op
  | Br label -> Printf.sprintf "  br label %%%s" label
  | Br_cond { cond; then_label; else_label } ->
      Printf.sprintf "  br i1 %s, label %%%s, label %%%s" (value_text (I 1) cond) then_label else_label
  | Switch { ty; selector; default_label; cases } ->
      let cases =
        List.map (fun (key, target) -> Printf.sprintf "\n    %s %s, label %%%s" (ty_name ty) key target) cases
        |> String.concat ""
      in
      Printf.sprintf "  switch %s %s, label %%%s [%s\n  ]" (ty_name ty) (value_text ty selector)
        default_label cases
  | Unreachable -> "  unreachable"

let decl_text { d_name; d_ret; d_ret_ext; d_params; d_param_exts } =
  if List.length d_params <> List.length d_param_exts then
    malformed "declaration @%s ABI metadata has %d entries for %d parameters"
      d_name (List.length d_param_exts) (List.length d_params);
  let params =
    List.map2
      (fun ty ext ->
        match abi_ext_text ext with
        | "" -> ty_name ty
        | attr -> ty_name ty ^ " " ^ attr)
      d_params d_param_exts
  in
  Printf.sprintf "declare %s @%s(%s)" (return_text d_ret_ext d_ret) d_name
    (String.concat ", " params)

let global_text { g_name; g_bytes; g_align } =
  Printf.sprintf "@%s = private unnamed_addr constant [%d x i8] c\"%s\", align %d" g_name
    (String.length g_bytes) (escape_c_string g_bytes) g_align

let func_text global_len { name; ret; ret_ext; params; param_exts; blocks } =
  if List.length params <> List.length param_exts then
    malformed "function @%s ABI metadata has %d entries for %d parameters"
      name (List.length param_exts) (List.length params);
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    (Printf.sprintf "define %s @%s(%s) {\n" (return_text ret_ext ret) name
       (String.concat ", "
          (List.map2
             (fun (ty, p) ext ->
               match abi_ext_text ext with
               | "" -> ty_name ty ^ " " ^ p
               | attr -> Printf.sprintf "%s %s %s" (ty_name ty) attr p)
             params param_exts)));
  List.iter
    (fun { label; instrs; term } ->
      Buffer.add_string buf (label ^ ":\n");
      List.iter (fun i -> Buffer.add_string buf (instr_text global_len i ^ "\n")) instrs;
      Buffer.add_string buf (term_text term ^ "\n"))
    blocks;
  Buffer.add_string buf "}\n";
  Buffer.contents buf

let to_llvm_text { decls; funcs; globals } =
  let lengths = Hashtbl.create (max 16 (List.length globals)) in
  List.iter (fun g -> Hashtbl.replace lengths g.g_name (String.length g.g_bytes)) globals;
  let global_len sym = Hashtbl.find_opt lengths sym in
  let buf = Buffer.create 65536 in
  Buffer.add_string buf "; xi v0.6-R3 generated LLVM IR (v1)\n\n";
  let uses_checked_arith =
    List.exists
      (fun f ->
        List.exists
          (fun block ->
            List.exists (function Checked_arith _ -> true | _ -> false) block.instrs)
          f.blocks)
      funcs
  in
  if uses_checked_arith then
    Buffer.add_string buf
      "declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64)\ndeclare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64)\ndeclare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64)\n";
  List.iter (fun d -> Buffer.add_string buf (decl_text d ^ "\n")) decls;
  Buffer.add_char buf '\n';
  List.iter
    (fun f ->
      Buffer.add_string buf (func_text global_len f);
      Buffer.add_char buf '\n')
    funcs;
  List.iter (fun g -> Buffer.add_string buf (global_text g ^ "\n")) globals;
  Buffer.contents buf

let resolve_alias aliases value =
  let rec loop seen = function
    | Temp t as value when not (List.mem t.tname seen) ->
        (match Hashtbl.find_opt aliases t.tname with
         | Some next -> loop (t.tname :: seen) next
         | None -> value)
    | value -> value
  in
  loop [] value

let map_operand aliases (ty, value) = ty, resolve_alias aliases value

let rewrite_instr aliases = function
  | Alloca _ as instr -> instr
  | Load x -> Load { x with src = resolve_alias aliases x.src }
  | Store x ->
      Store { x with src = resolve_alias aliases x.src; dst = resolve_alias aliases x.dst }
  | Bin x ->
      Bin { x with lhs = resolve_alias aliases x.lhs; rhs = resolve_alias aliases x.rhs }
  | Checked_arith x ->
      Checked_arith
        { x with overflow = resolve_alias aliases x.overflow
        ; lhs = resolve_alias aliases x.lhs; rhs = resolve_alias aliases x.rhs }
  | Fbin x ->
      Fbin { x with lhs = resolve_alias aliases x.lhs; rhs = resolve_alias aliases x.rhs }
  | Icmp x ->
      Icmp { x with lhs = resolve_alias aliases x.lhs; rhs = resolve_alias aliases x.rhs }
  | Fcmp x ->
      Fcmp { x with lhs = resolve_alias aliases x.lhs; rhs = resolve_alias aliases x.rhs }
  | Conv x -> Conv { x with src = resolve_alias aliases x.src }
  | Gep_global _ as instr -> instr
  | Gep_byte x ->
      Gep_byte
        { x with base = resolve_alias aliases x.base; index = map_operand aliases x.index }
  | Call x ->
      let callee =
        match x.callee with
        | Direct _ as direct -> direct
        | Indirect value -> Indirect (resolve_alias aliases value)
      in
      Call { x with callee; args = List.map (map_operand aliases) x.args }

let rewrite_term aliases = function
  | Ret (Some operand) -> Ret (Some (map_operand aliases operand))
  | Br_cond x -> Br_cond { x with cond = resolve_alias aliases x.cond }
  | Switch x -> Switch { x with selector = resolve_alias aliases x.selector }
  | (Ret None | Br _ | Unreachable) as term -> term

let value_key = function
  | Temp t -> "t:" ^ t.tname
  | Int_imm x -> "i:" ^ x
  | Bool_imm x -> if x then "b:1" else "b:0"
  | Float_bits x -> "f:" ^ x
  | Null -> "n"
  | Global x -> "g:" ^ x

let key2 tag ty lhs rhs =
  String.concat "|" [ tag; ty_name ty; value_key lhs; value_key rhs ]

let cse_key memory_epoch = function
  | Load x ->
      Some (Printf.sprintf "load|%d|%s|%s|%d" memory_epoch (ty_name x.ty)
              (value_key x.src) x.align)
  | Bin x -> Some (key2 (binop_text x.op) x.ty x.lhs x.rhs)
  | Fbin x -> Some (key2 (fbinop_text x.op) x.ty x.lhs x.rhs)
  | Icmp x -> Some (key2 ("icmp." ^ icmp_pred_text x.pred) x.ty x.lhs x.rhs)
  | Fcmp x -> Some (key2 ("fcmp." ^ fcmp_pred_text x.pred) x.ty x.lhs x.rhs)
  | Conv x ->
      Some (String.concat "|"
        [ "conv." ^ conv_text x.op; ty_name x.from_ty; value_key x.src; ty_name x.to_ty ])
  | Gep_global x -> Some ("global|" ^ x.sym)
  | Gep_byte x ->
      Some (String.concat "|"
        [ "gep"; value_key x.base; ty_name (fst x.index); value_key (snd x.index) ])
  | Alloca _ | Store _ | Checked_arith _ | Call _ -> None

let instr_dst = function
  | Alloca x -> Some x.dst | Load x -> Some x.dst | Bin x -> Some x.dst
  | Checked_arith x -> Some x.dst | Fbin x -> Some x.dst | Icmp x -> Some x.dst
  | Fcmp x -> Some x.dst | Conv x -> Some x.dst | Gep_global x -> Some x.dst
  | Gep_byte x -> Some x.dst | Call { dst; _ } -> dst | Store _ -> None

let instr_values = function
  | Alloca _ | Gep_global _ -> []
  | Load x -> [ x.src ]
  | Store x -> [ x.src; x.dst ]
  | Bin x -> [ x.lhs; x.rhs ]
  | Checked_arith x -> [ x.overflow; x.lhs; x.rhs ]
  | Fbin x -> [ x.lhs; x.rhs ]
  | Icmp x -> [ x.lhs; x.rhs ]
  | Fcmp x -> [ x.lhs; x.rhs ]
  | Conv x -> [ x.src ]
  | Gep_byte x -> [ x.base; snd x.index ]
  | Call x ->
      (match x.callee with Direct _ -> [] | Indirect v -> [ v ])
      @ List.map snd x.args

let term_values = function
  | Ret (Some (_, value)) -> [ value ]
  | Br_cond x -> [ x.cond ]
  | Switch x -> [ x.selector ]
  | Ret None | Br _ | Unreachable -> []

let dead_removable = function
  | Bin { op = (Sdiv | Udiv | Srem | Urem); _ } -> false
  | Bin _ | Fbin _ | Icmp _ | Fcmp _ | Conv _ | Gep_global _ | Gep_byte _ -> true
  | Alloca _ | Load _ | Store _ | Checked_arith _ | Call _ -> false

let eliminate_dead_pure uses instrs =
  let changed = ref false in
  let dec = function
    | Temp t ->
        let count = Option.value (Hashtbl.find_opt uses t.tname) ~default:0 in
        if count <= 1 then Hashtbl.remove uses t.tname
        else Hashtbl.replace uses t.tname (count - 1)
    | _ -> ()
  in
  List.fold_left
    (fun kept instr ->
      match instr_dst instr with
      | Some dst when dead_removable instr && not (Hashtbl.mem uses dst) ->
          changed := true;
          List.iter dec (instr_values instr);
          kept
      | _ -> instr :: kept)
    [] (List.rev instrs), !changed

let optimize_block local_allocas cross_block_used block =
  let aliases = Hashtbl.create 32 in
  let available = Hashtbl.create 32 in
  let stored_values = Hashtbl.create 16 in
  let memory_epoch = ref 0 in
  let memory_key ty address = ty_name ty ^ "|" ^ value_key address in
  let instrs =
    List.filter_map
      (fun original ->
        let instr = rewrite_instr aliases original in
        let result =
          match instr with
          | Load x when not (Hashtbl.mem cross_block_used x.dst) ->
              (match Hashtbl.find_opt stored_values (memory_key x.ty x.src) with
               | Some stored ->
                   Hashtbl.replace aliases x.dst stored;
                   None
               | None ->
                   (match cse_key !memory_epoch instr with
                    | Some key ->
                        (match Hashtbl.find_opt available key with
                         | Some prior -> Hashtbl.replace aliases x.dst prior; None
                         | None ->
                             Hashtbl.replace available key (Temp (temp x.dst));
                             Some instr)
                    | None -> Some instr))
          | _ ->
              (match cse_key !memory_epoch instr, instr_dst instr with
               | Some key, Some dst when not (Hashtbl.mem cross_block_used dst) ->
                   (match Hashtbl.find_opt available key with
                    | Some prior -> Hashtbl.replace aliases dst prior; None
                    | None ->
                        Hashtbl.replace available key (Temp (temp dst));
                        Some instr)
               | _ -> Some instr)
        in
        (match instr with
         | Store x ->
             incr memory_epoch;
             Hashtbl.clear available;
             (match x.dst with
              | Temp t when Hashtbl.mem local_allocas t.tname ->
                  Hashtbl.remove stored_values (memory_key x.ty x.dst)
              | _ -> Hashtbl.clear stored_values);
             Hashtbl.replace stored_values (memory_key x.ty x.dst) x.src
         | Checked_arith _ | Call _ ->
             incr memory_epoch;
             Hashtbl.clear available;
             Hashtbl.clear stored_values
         | _ -> ());
        result)
      block.instrs
  in
  let term = rewrite_term aliases block.term in
  { block with instrs; term }

let optimize_func func =
  let local_allocas = Hashtbl.create 32 in
  let defining_block = Hashtbl.create 128 in
  let cross_block_used = Hashtbl.create 32 in
  List.iter
    (fun block ->
      List.iter
        (fun instr ->
          (match instr with Alloca x -> Hashtbl.replace local_allocas x.dst () | _ -> ());
          Option.iter (fun dst -> Hashtbl.replace defining_block dst block.label) (instr_dst instr))
        block.instrs)
    func.blocks;
  let mark_use block_label = function
    | Temp t ->
        (match Hashtbl.find_opt defining_block t.tname with
         | Some def_label when def_label <> block_label ->
             Hashtbl.replace cross_block_used t.tname ()
         | _ -> ())
    | _ -> ()
  in
  List.iter
    (fun block ->
      List.iter
        (fun instr -> List.iter (mark_use block.label) (instr_values instr))
        block.instrs;
      List.iter (mark_use block.label) (term_values block.term))
    func.blocks;
  let blocks = ref
    (List.map (optimize_block local_allocas cross_block_used) func.blocks) in
  let uses = Hashtbl.create 128 in
  let add = function
    | Temp t -> Hashtbl.replace uses t.tname
        (1 + Option.value (Hashtbl.find_opt uses t.tname) ~default:0)
    | _ -> ()
  in
  List.iter
    (fun block ->
      List.iter (fun instr -> List.iter add (instr_values instr)) block.instrs;
      List.iter add (term_values block.term))
    !blocks;
  let changed = ref true in
  while !changed do
    changed := false;
    blocks :=
      List.map
        (fun block ->
          let instrs, removed = eliminate_dead_pure uses block.instrs in
          if removed then changed := true;
          { block with instrs })
        !blocks
  done;
  { func with blocks = !blocks }

let optimize modul =
  { modul with funcs = List.map optimize_func modul.funcs }

let ident_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
  || c = '_' || c = '.' || c = '$' || c = '-'

let ident_ok s = s <> "" && String.for_all ident_char s

let check_ident what s = if not (ident_ok s) then malformed "%s %S is not an identifier" what s

let local_ok s =
  String.length s > 1
  && s.[0] = '%'
  &&
  let ok = ref true in
  String.iteri (fun i c -> if i > 0 && not (ident_char c) then ok := false) s;
  !ok

let check_local what s =
  if not (local_ok s) then malformed "%s %S is not a local name" what s

let check_int_imm s =
  let n = String.length s in
  let start = if n > 0 && (s.[0] = '-' || s.[0] = '+') then 1 else 0 in
  if not (n > start && String.for_all (fun c -> c >= '0' && c <= '9') (String.sub s start (n - start)))
  then malformed "integer immediate %S is not a decimal integer" s

let check_value ty = function
  | Temp t -> check_local "temporary" t.tname
  | Global name -> check_ident "symbol" name
  | Int_imm s -> check_int_imm s
  | Bool_imm _ | Null -> ()
  | Float_bits hex ->
      let want = match ty with F 16 -> 4 | F 32 -> 8 | F 64 -> 16 | _ -> 16 in
      if String.length hex <> want then
        malformed "float constant %S is %d hex digits, expected %d for %s" hex (String.length hex)
          want (ty_name ty);
      if not (String.for_all (fun c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F')) hex) then
        malformed "float constant %S is not uppercase hex" hex

let check_operand (ty, value) = check_value ty value

let validate_instr instr =
  match instr with
  | Alloca { dst; _ } -> check_local "temporary" dst
  | Load { dst; src; _ } -> check_local "temporary" dst; check_value Ptr src
  | Store { ty; src; dst; _ } -> check_value ty src; check_value Ptr dst
  | Checked_arith { dst; overflow; op; lhs; rhs } ->
      check_local "temporary" dst;
      check_value Ptr overflow;
      check_value (I 64) lhs;
      check_value (I 64) rhs;
      (match op with Add | Sub | Mul -> () | _ -> malformed "invalid checked arithmetic operator")
  | Bin { dst; ty; lhs; rhs; _ } | Fbin { dst; ty; lhs; rhs; _ }
  | Icmp { dst; ty; lhs; rhs; _ } | Fcmp { dst; ty; lhs; rhs; _ } ->
      check_local "temporary" dst; check_value ty lhs; check_value ty rhs
  | Conv { dst; from_ty; src; _ } -> check_local "temporary" dst; check_value from_ty src
  | Gep_global { dst; sym } -> check_local "temporary" dst; check_ident "symbol" sym
  | Gep_byte { dst; base; index } ->
      check_local "temporary" dst; check_value Ptr base; check_operand index
  | Call { dst; callee; args; arg_exts; _ } ->
      Option.iter (check_local "temporary") dst;
      (match callee with Direct name -> check_ident "symbol" name | Indirect v -> check_value Ptr v);
      if List.length args <> List.length arg_exts then
        malformed "call ABI metadata has %d entries for %d arguments"
          (List.length arg_exts) (List.length args);
      List.iter check_operand args

let validate_term = function
  | Ret None | Unreachable -> ()
  | Ret (Some op) -> check_operand op
  | Br label -> check_ident "label" label
  | Br_cond { cond; then_label; else_label } ->
      check_value (I 1) cond;
      check_ident "label" then_label;
      check_ident "label" else_label
  | Switch { ty; selector; default_label; cases } ->
      check_value ty selector;
      check_ident "label" default_label;
      List.iter (fun (key, target) -> check_int_imm key; check_ident "label" target) cases

let validate { decls; funcs; globals } =
  List.iter
    (fun d ->
      check_ident "symbol" d.d_name;
      if List.length d.d_params <> List.length d.d_param_exts then
        malformed "declaration @%s ABI metadata has %d entries for %d parameters"
          d.d_name (List.length d.d_param_exts) (List.length d.d_params))
    decls;
  List.iter (fun g -> check_ident "symbol" g.g_name) globals;
  List.iter
    (fun f ->
      check_ident "symbol" f.name;
      if List.length f.params <> List.length f.param_exts then
        malformed "function @%s ABI metadata has %d entries for %d parameters"
          f.name (List.length f.param_exts) (List.length f.params);
      List.iter (fun (_, p) -> check_local "parameter" p) f.params;
      List.iter
        (fun b ->
          check_ident "label" b.label;
          List.iter validate_instr b.instrs;
          validate_term b.term)
        f.blocks)
    funcs

let operand_of_string text : operand =
  let text = String.trim text in
  let n = String.length text in
  let starts prefix = String.starts_with ~prefix text in
  let ty, value_start =
    if starts "ptr " then Ptr, 4
    else if starts "i64 " then I 64, 4
    else if starts "i32 " then I 32, 4
    else if starts "i16 " then I 16, 4
    else if starts "i8 " then I 8, 3
    else if starts "i1 " then I 1, 3
    else if starts "double " then F 64, 7
    else if starts "float " then F 32, 6
    else if starts "half " then F 16, 5
    else
      match String.index_opt text ' ' with
      | None -> malformed "operand %S has no value" text
      | Some i -> ty_of_string (String.sub text 0 i), i + 1
  in
  let rest = String.trim (String.sub text value_start (n - value_start)) in
  if rest = "" then malformed "operand %S has no value" text;
  (ty, value_of_fragment ty rest)
