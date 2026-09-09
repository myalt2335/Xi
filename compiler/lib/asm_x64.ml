(* Asm_x64: Lir -> x86_64 lowering, rendered in the compiler's bounded
   GAS/AT&T-shaped instruction dialect.

   Normal builds feed per-function machine sinks straight to the in-process
   encoder and ELF/COFF writers. Text assembly remains available for --dump=asm
   and the independent --emit=asm backend. The instruction selector is
   shared by the Windows/MS-x64 and Linux/System-V targets; the handful of ABI
   differences are selected from Build_config.target below.

   Allocation strategy: values that fit a general-purpose register and stay
   within one basic block are assigned non-volatile registers by a small linear
   scan in [plan_frame]. Everything else uses a recycled stack slot. Non-
   volatile registers are important here: `Ir` lowers many operations to calls
   into the Zig runtime, so a volatile-only cache would be flushed precisely
   where it matters most. Cross-block values deliberately fall back to memory;
   this IR routes them through allocas, so graph colouring and phi destruction
   would add complexity without buying useful coverage.

   Slot discipline, which everything below depends on:
     - A temp slot is always written with a full 8-byte `movq` and always read
       with a full 8-byte `movq`.
     - For a value narrower than 64 bits only the low bits are meaningful; the
       high bits are unspecified. Every consumer therefore selects the width it
       needs (`cmpb`/`cmpl`/...) or re-extends explicitly (`Sext`/`Zext`).
     - `alloca` results are not stored anywhere. The alloca's *object* gets a
       frame slot and the pointer is rematerialised with `leaq` at each use,
       which is both smaller and faster than spilling it. *)

open Lir

exception Unsupported of string

let fail fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

module StrTbl = Hashtbl.Make (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
end)

module IntTbl = Hashtbl.Make (struct
  type t = int

  let equal = Int.equal
  let hash = Hashtbl.hash
end)

module ImmTbl = Hashtbl.Make (struct
  type t = bool * int * string

  let equal (s1, b1, t1) (s2, b2, t2) = s1 = s2 && b1 = b2 && String.equal t1 t2

  let hash (_, _, text) = Hashtbl.hash text
end)

let target_parts = String.split_on_char '-' Build_config.target
let windows_abi = List.mem "windows" target_parts
let sysv_abi = List.mem "linux" target_parts

let () =
  if not windows_abi && not sysv_abi then
    fail "unsupported x86_64 object target %s" Build_config.target

(* Scratch set, all volatile (caller-saved) under the MS x64 ABI, so nothing
   here needs saving. RCX/RDX/R8/R9 double as the outgoing argument registers;
   R10/R11 are kept clear of that role so they are always safe to use while
   marshalling a call. *)
type greg =
  | Rax | Rcx | Rdx | R8 | R9 | R10 | R11
  | Rbx | Rsi | Rdi | R12 | R13 | R14 | R15

let gr reg bits =
  match (reg, bits) with
  | Rax, 8 -> "%al" | Rax, 16 -> "%ax" | Rax, 32 -> "%eax" | Rax, 64 -> "%rax"
  | Rcx, 8 -> "%cl" | Rcx, 16 -> "%cx" | Rcx, 32 -> "%ecx" | Rcx, 64 -> "%rcx"
  | Rdx, 8 -> "%dl" | Rdx, 16 -> "%dx" | Rdx, 32 -> "%edx" | Rdx, 64 -> "%rdx"
  | R8, 8 -> "%r8b" | R8, 16 -> "%r8w" | R8, 32 -> "%r8d" | R8, 64 -> "%r8"
  | R9, 8 -> "%r9b" | R9, 16 -> "%r9w" | R9, 32 -> "%r9d" | R9, 64 -> "%r9"
  | R10, 8 -> "%r10b" | R10, 16 -> "%r10w" | R10, 32 -> "%r10d" | R10, 64 -> "%r10"
  | R11, 8 -> "%r11b" | R11, 16 -> "%r11w" | R11, 32 -> "%r11d" | R11, 64 -> "%r11"
  | Rbx, 8 -> "%bl" | Rbx, 16 -> "%bx" | Rbx, 32 -> "%ebx" | Rbx, 64 -> "%rbx"
  | Rsi, 8 -> "%sil" | Rsi, 16 -> "%si" | Rsi, 32 -> "%esi" | Rsi, 64 -> "%rsi"
  | Rdi, 8 -> "%dil" | Rdi, 16 -> "%di" | Rdi, 32 -> "%edi" | Rdi, 64 -> "%rdi"
  | R12, 8 -> "%r12b" | R12, 16 -> "%r12w" | R12, 32 -> "%r12d" | R12, 64 -> "%r12"
  | R13, 8 -> "%r13b" | R13, 16 -> "%r13w" | R13, 32 -> "%r13d" | R13, 64 -> "%r13"
  | R14, 8 -> "%r14b" | R14, 16 -> "%r14w" | R14, 32 -> "%r14d" | R14, 64 -> "%r14"
  | R15, 8 -> "%r15b" | R15, 16 -> "%r15w" | R15, 32 -> "%r15d" | R15, 64 -> "%r15"
  | _ -> fail "no %d-bit alias for this register" bits

type opd = { text : string; op : Asm_x64_obj.operand }

let greg_code = function
  | Rax -> 0 | Rcx -> 1 | Rdx -> 2 | Rbx -> 3 | Rsi -> 6 | Rdi -> 7
  | R8 -> 8 | R9 -> 9 | R10 -> 10 | R11 -> 11 | R12 -> 12 | R13 -> 13
  | R14 -> 14 | R15 -> 15

let bits_slot = function 8 -> 0 | 16 -> 1 | 32 -> 2 | 64 -> 3 | _ -> -1

let gro_table =
  let dummy = { text = ""; op = Asm_x64_obj.name_operand "" } in
  let table = Array.make 64 dummy in
  List.iter
    (fun r ->
      List.iter
        (fun bits ->
          let code = greg_code r in
          table.(code * 4 + bits_slot bits) <-
            { text = gr r bits; op = Asm_x64_obj.gpr_operand ~code ~width:bits })
        [ 8; 16; 32; 64 ])
    [ Rax; Rcx; Rdx; R8; R9; R10; R11; Rbx; Rsi; Rdi; R12; R13; R14; R15 ];
  table

let gro r bits =
  let slot = bits_slot bits in
  if slot < 0 then fail "no %d-bit alias for this register" bits
  else gro_table.(greg_code r * 4 + slot)

let o_rbp = { text = "%rbp"; op = Asm_x64_obj.gpr_operand ~code:5 ~width:64 }
let o_rsp = { text = "%rsp"; op = Asm_x64_obj.gpr_operand ~code:4 ~width:64 }
let o_imm0 = { text = "$0"; op = Asm_x64_obj.imm_operand 0L }
let o_imm1 = { text = "$1"; op = Asm_x64_obj.imm_operand 1L }

let o_star_r11 =
  { text = "*%r11"
  ; op = Asm_x64_obj.indirect_operand (Asm_x64_obj.gpr_operand ~code:11 ~width:64)
  }

let o_star_rax =
  { text = "*%rax"
  ; op = Asm_x64_obj.indirect_operand (Asm_x64_obj.gpr_operand ~code:0 ~width:64)
  }

let imm_int_o n =
  { text = "$" ^ string_of_int n; op = Asm_x64_obj.imm_operand (Int64.of_int n) }

let mem_rbp_o disp =
  { text = string_of_int disp ^ "(%rbp)"; op = Asm_x64_obj.mem_rbp disp }

let mem_rsp_o disp =
  { text = string_of_int disp ^ "(%rsp)"; op = Asm_x64_obj.mem_reg_disp 4 disp }

let mem_index_o base index scale =
  { text = Printf.sprintf "(%s,%s,%d)" (gr base 64) (gr index 64) scale
  ; op =
      Asm_x64_obj.mem_reg_index ~base:(greg_code base) ~index:(greg_code index)
        ~scale
  }

let rip_o sym = { text = sym ^ "(%rip)"; op = Asm_x64_obj.sym_rip sym }
let name_o s = { text = s; op = Asm_x64_obj.name_operand s }

let windows_arg_gregs = [| Rcx; Rdx; R8; R9 |]
let sysv_arg_gregs = [| Rdi; Rsi; Rdx; Rcx; R8; R9 |]

let cache_gregs =
  if sysv_abi then [ Rbx; R12; R13; R14; R15 ]
  else [ Rbx; Rsi; Rdi; R12; R13; R14; R15 ]

let cache_xmms =
  if windows_abi then [ 6; 7; 8; 9; 10; 11; 12; 13; 14; 15 ]
  else [ 9; 10; 11; 12; 13; 14; 15 ]

type arg_home = Gpr of greg | Xmm of int | Stack of int

let classify_args tys =
  if windows_abi then
    List.mapi
      (fun pos ty ->
        if pos >= 4 then Stack (8 * pos)
        else if is_float ty then Xmm pos
        else Gpr windows_arg_gregs.(pos))
      tys
  else begin
    let next_gpr = ref 0 and next_xmm = ref 0 and next_stack = ref 0 in
    List.map
      (fun ty ->
        if is_float ty then
          if !next_xmm < 8 then begin
            let n = !next_xmm in
            incr next_xmm;
            Xmm n
          end
          else begin
            let off = 8 * !next_stack in
            incr next_stack;
            Stack off
          end
        else if !next_gpr < Array.length sysv_arg_gregs then begin
          let r = sysv_arg_gregs.(!next_gpr) in
          incr next_gpr;
          Gpr r
        end
        else begin
          let off = 8 * !next_stack in
          incr next_stack;
          Stack off
        end)
      tys
  end

let incoming_stack_offset = function
  | Stack off -> 16 + off
  | _ -> invalid_arg "incoming_stack_offset"

let outgoing_bytes tys =
  let stack_end =
    List.fold_left
      (fun n -> function Stack off -> max n (off + 8) | _ -> n)
      0 (classify_args tys)
  in
  if windows_abi then max 32 stack_end else stack_end

let bits_of_ty = function
  | I 1 -> 8
  | I bits when bits <= 8 -> 8
  | I bits when bits <= 16 -> 16
  | I bits when bits <= 32 -> 32
  | I bits when bits <= 64 -> 64
  | I bits -> fail "integers wider than 64 bits are not supported yet (i%d)" bits
  | F 16 -> 16
  | F 32 -> 32
  | F 64 -> 64
  | F bits -> fail "unsupported float width %d" bits
  | Ptr -> 64
  | Void -> fail "void has no width"

let exact_bits = function
  | I 1 -> 1
  | I bits -> bits
  | F bits -> bits
  | Ptr -> 64
  | Void -> fail "void has no width"

let mem_bytes ty = size_bytes ty

let is_machine_width bits = bits = 8 || bits = 16 || bits = 32 || bits = 64

let is_wide = function I bits -> bits > 64 | _ -> false

let sfx_of_bits = function
  | 8 -> "b" | 16 -> "w" | 32 -> "l" | 64 -> "q"
  | n -> fail "no operand suffix for %d bits" n

let is_xmm_ty = function F 32 | F 64 -> true | _ -> false

let work_bits ty = if bits_of_ty ty <= 32 then 32 else 64

type fnstate =
  { mutable frame : int
  ;
    ids : int StrTbl.t
  ; mutable names : string array
  ; mutable slots : int array
  ; slot_text : opd IntTbl.t
  ; mutable allocas : int array
  ;
    mutable alloca_cache : greg option array
  ;
    mutable falloca_cache : int array
  ;
    mutable param_cache : greg option array
  ; mutable temp_tys : ty array
  ;
    mutable sret : int option
  ; (* An 8-byte scratch cell for wide multiplication, which is one register
       short of fitting its inner loop. 0 when the function has no wide mul. *)
    mutable mul_scratch : int
  ;
    mutable cache : greg option array
  ;
    mutable xcache : int array
  ;
    mutable saved : (greg * int) list
  ;
    mutable saved_xmms : (int * int) list
  ; fname : string
  ; ret_ty : ty
  ; ret_ext : abi_ext
  }

type output =
  | Text_output of Buffer.t
  | Machine_output of Asm_x64_obj.builder

type state =
  { output : output
  ; scope : int
  ; int_imms : int64 StrTbl.t
  ; imm_text : opd ImmTbl.t
  ;
    pool : (int * string, string) Hashtbl.t
  ; mutable pool_order : (string * int * string) list
  ; mutable pool_next : int
  ;
    wide_pool : (int * string, string) Hashtbl.t
  ; mutable wide_pool_order : (string * string) list
  ; mutable switch_pool_order : (string * string list) list
  ; mutable switch_next : int
  ;
    mutable loop_next : int
  }

let line st fmt =
  Printf.ksprintf
    (fun text ->
      match st.output with
      | Text_output buffer ->
          Buffer.add_char buffer '\t';
          Buffer.add_string buffer text;
          Buffer.add_char buffer '\n'
      | Machine_output builder ->
          Asm_x64_obj.emit_line builder text)
    fmt

let raw st fmt =
  Printf.ksprintf
    (fun text ->
      match st.output with
      | Text_output buffer ->
          Buffer.add_string buffer text;
          Buffer.add_char buffer '\n'
      | Machine_output builder -> Asm_x64_obj.emit_line builder text)
    fmt

let blank st =
  match st.output with
  | Text_output buffer -> Buffer.add_char buffer '\n'
  | Machine_output _ -> ()

let new_state ~scope output =
  { output
  ; scope
  ; int_imms = StrTbl.create 32
  ; imm_text = ImmTbl.create 64
  ; pool = Hashtbl.create 16
  ; pool_order = []
  ; pool_next = 0
  ; wide_pool = Hashtbl.create 8
  ; wide_pool_order = []
  ; switch_pool_order = []
  ; switch_next = 0
  ; loop_next = 0
  }

let new_text_state ~scope ~capacity =
  let buffer = Buffer.create capacity in
  new_state ~scope (Text_output buffer), buffer

let new_machine_state ~scope =
  let builder = Asm_x64_obj.create_builder () in
  new_state ~scope (Machine_output builder), builder

let insn0 st mnemonic =
  match st.output with
  | Text_output buffer ->
      Buffer.add_char buffer '\t';
      Buffer.add_string buffer mnemonic;
      Buffer.add_char buffer '\n'
  | Machine_output builder ->
      Asm_x64_obj.emit_typed_instruction builder mnemonic []

let insn1 st opcode (operand : opd) =
  match st.output with
  | Text_output buffer ->
      Buffer.add_char buffer '\t';
      Buffer.add_string buffer opcode;
      Buffer.add_char buffer '\t';
      Buffer.add_string buffer operand.text;
      Buffer.add_char buffer '\n'
  | Machine_output builder ->
      Asm_x64_obj.emit_typed_instruction builder opcode [ operand.op ]

let insn2 st opcode (lhs : opd) (rhs : opd) =
  match st.output with
  | Text_output buffer ->
      Buffer.add_char buffer '\t';
      Buffer.add_string buffer opcode;
      Buffer.add_char buffer '\t';
      Buffer.add_string buffer lhs.text;
      Buffer.add_string buffer ", ";
      Buffer.add_string buffer rhs.text;
      Buffer.add_char buffer '\n'
  | Machine_output builder ->
      Asm_x64_obj.emit_typed_instruction builder opcode [ lhs.op; rhs.op ]

let insn2_suffix st opcode suffix lhs rhs = insn2 st (opcode ^ suffix) lhs rhs

let deflabel st name =
  match st.output with
  | Text_output buffer ->
      Buffer.add_string buffer name;
      Buffer.add_string buffer ":\n"
  | Machine_output builder -> Asm_x64_obj.define_label builder name

let dir_text st =
  match st.output with
  | Text_output buffer -> Buffer.add_string buffer "\t.text\n"
  | Machine_output builder -> Asm_x64_obj.set_section_text builder

let dir_globl st name =
  match st.output with
  | Text_output buffer ->
      Buffer.add_string buffer "\t.globl\t";
      Buffer.add_string buffer name;
      Buffer.add_char buffer '\n'
  | Machine_output builder -> Asm_x64_obj.set_global builder name

let dir_type_function st name =
  match st.output with
  | Text_output buffer ->
      Buffer.add_string buffer "\t.type\t";
      Buffer.add_string buffer name;
      Buffer.add_string buffer ", @function\n"
  | Machine_output builder -> Asm_x64_obj.mark_function builder name

let dir_size st name =
  match st.output with
  | Text_output buffer ->
      Buffer.add_string buffer "\t.size\t";
      Buffer.add_string buffer name;
      Buffer.add_string buffer ", .-";
      Buffer.add_string buffer name;
      Buffer.add_char buffer '\n'
  | Machine_output builder -> Asm_x64_obj.set_size_here builder name

let dir_p2align16 st =
  match st.output with
  | Text_output buffer -> Buffer.add_string buffer "\t.p2align\t4, 0x90\n"
  | Machine_output builder -> Asm_x64_obj.p2align builder 4

let dir_cfi_startproc st =
  match st.output with
  | Text_output buffer -> Buffer.add_string buffer "\t.cfi_startproc\n"
  | Machine_output builder -> Asm_x64_obj.cfi_startproc builder

let dir_cfi_endproc st =
  match st.output with
  | Text_output buffer -> Buffer.add_string buffer "\t.cfi_endproc\n"
  | Machine_output builder -> Asm_x64_obj.cfi_endproc builder

let dir_cfi_prologue_rbp st =
  match st.output with
  | Text_output buffer ->
      Buffer.add_string buffer "\t.cfi_def_cfa_offset\t16\n";
      Buffer.add_string buffer "\t.cfi_offset\t%rbp, -16\n"
  | Machine_output builder ->
      Asm_x64_obj.cfi_def_cfa_offset builder 16;
      Asm_x64_obj.cfi_offset builder 5 (-16)

let dir_cfi_def_cfa_register_rbp st =
  match st.output with
  | Text_output buffer -> Buffer.add_string buffer "\t.cfi_def_cfa_register\t%rbp\n"
  | Machine_output builder -> Asm_x64_obj.cfi_def_cfa_register builder 5

let dir_cfi_offset st reg_text reg_code offset =
  match st.output with
  | Text_output buffer ->
      Buffer.add_string buffer "\t.cfi_offset\t";
      Buffer.add_string buffer reg_text;
      Buffer.add_string buffer ", ";
      Buffer.add_string buffer (string_of_int offset);
      Buffer.add_char buffer '\n'
  | Machine_output builder -> Asm_x64_obj.cfi_offset builder reg_code offset

let pool_label st width bits =
  let key = (width, bits) in
  match Hashtbl.find_opt st.pool key with
  | Some label -> label
  | None ->
      let label =
        if st.scope < 0 then Printf.sprintf ".Lfp%d" st.pool_next
        else Printf.sprintf ".Lfp%d_%d" st.scope st.pool_next
      in
      st.pool_next <- st.pool_next + 1;
      Hashtbl.add st.pool key label;
      st.pool_order <- (label, width, bits) :: st.pool_order;
      label

let bytes_of_decimal text n =
  let negative = String.length text > 0 && text.[0] = '-' in
  let digits_text = if negative || (String.length text > 0 && text.[0] = '+')
                    then String.sub text 1 (String.length text - 1)
                    else text
  in
  String.iter
    (fun c -> if c < '0' || c > '9' then fail "malformed integer literal %S" text)
    digits_text;
  let d = Array.init (String.length digits_text) (fun i -> Char.code digits_text.[i] - 48) in
  let out = Bytes.make n '\000' in
  let all_zero () = Array.for_all (fun x -> x = 0) d in
  let i = ref 0 in
  while !i < n && not (all_zero ()) do
    let rem = ref 0 in
    Array.iteri
      (fun k digit ->
        let cur = (!rem * 10) + digit in
        d.(k) <- cur / 256;
        rem := cur mod 256)
      d;
    Bytes.set out !i (Char.chr !rem);
    incr i
  done;
  if negative then begin
    let carry = ref 1 in
    for k = 0 to n - 1 do
      let v = (Char.code (Bytes.get out k) lxor 0xFF) + !carry in
      Bytes.set out k (Char.chr (v land 0xFF));
      carry := v lsr 8
    done
  end;
  Bytes.to_string out

let wide_pool_label st nbytes text =
  let key = (nbytes, text) in
  match Hashtbl.find_opt st.wide_pool key with
  | Some label -> label
  | None ->
      let label =
        if st.scope < 0 then Printf.sprintf ".Lwide%d" (Hashtbl.length st.wide_pool)
        else Printf.sprintf ".Lwide%d_%d" st.scope (Hashtbl.length st.wide_pool)
      in
      Hashtbl.add st.wide_pool key label;
      st.wide_pool_order <- (label, bytes_of_decimal text nbytes) :: st.wide_pool_order;
      label

let new_loop_label st base =
  st.loop_next <- st.loop_next + 1;
  if st.scope < 0 then Printf.sprintf ".L%s%d" base st.loop_next
  else Printf.sprintf ".L%s%d_%d" base st.scope st.loop_next

let block_label fn label = Printf.sprintf ".L%s.%s" fn.fname label

let temp_id fn name =
  match StrTbl.find_opt fn.ids name with
  | Some id -> id
  | None -> fail "%s: unknown temporary %%%s" fn.fname name

let slot_of fn id =
  let off = fn.slots.(id) in
  if off >= 0 then off
  else fail "%s: no stack slot for %%%s" fn.fname fn.names.(id)

let mem_slot fn off =
  match IntTbl.find_opt fn.slot_text off with
  | Some operand -> operand
  | None ->
      let operand = mem_rbp_o (-off) in
      IntTbl.add fn.slot_text off operand;
      operand

let instr_dst = function
  | Alloca _ | Store _ -> None
  | Load { dst; _ } | Bin { dst; _ } | Checked_arith { dst; _ }
  | Fbin { dst; _ } | Icmp { dst; _ }
  | Fcmp { dst; _ } | Conv { dst; _ } | Gep_global { dst; _ }
  | Gep_byte { dst; _ } -> Some dst
  | Call { dst; _ } -> dst

let iter_instr_uses f = function
  | Alloca _ | Gep_global _ -> ()
  | Load { src; _ } | Conv { src; _ } -> f src
  | Store { src; dst; _ } -> f src; f dst
  | Bin { lhs; rhs; _ } | Fbin { lhs; rhs; _ } | Icmp { lhs; rhs; _ }
  | Fcmp { lhs; rhs; _ } -> f lhs; f rhs
  | Checked_arith { overflow; lhs; rhs; _ } -> f overflow; f lhs; f rhs
  | Gep_byte { base; index; _ } -> f base; f (snd index)
  | Call { args; callee; _ } ->
      (match callee with Direct _ -> () | Indirect v -> f v);
      List.iter (fun (_, v) -> f v) args

let iter_term_uses f = function
  | Ret (Some (_, v)) -> f v
  | Ret None | Br _ | Unreachable -> ()
  | Br_cond { cond; _ } -> f cond
  | Switch { selector; _ } -> f selector

let value_id = function Temp t -> t.tid | _ -> -1

let instr_def = function
  | Alloca { dst; _ } -> Some dst
  | Store _ -> None
  | Load { dst; _ } | Bin { dst; _ } | Checked_arith { dst; _ }
  | Fbin { dst; _ } | Icmp { dst; _ }
  | Fcmp { dst; _ } | Conv { dst; _ } | Gep_global { dst; _ }
  | Gep_byte { dst; _ } -> Some dst
  | Call { dst; _ } -> dst

let is_alloca = function Alloca _ -> true | _ -> false

let instr_dst_ty = function
  | Alloca _ | Store _ -> None
  | Load { ty; _ } -> Some ty
  | Bin { ty; _ } | Fbin { ty; _ } -> Some ty
  | Checked_arith _ -> Some (I 64)
  | Icmp _ | Fcmp _ -> Some (I 1)
  | Conv { to_ty; _ } -> Some to_ty
  | Gep_global _ | Gep_byte _ -> Some Ptr
  | Call { ret; _ } -> Some ret

(* Outgoing argument bytes needed by instructions that lower to a call without
   being a Call: the `half` conversions, which go to compiler-rt because x86-64
   has no baseline f16 support, and wide division, which goes to the runtime.
   Missing one would let the callee's shadow store overwrite our own locals in a
   function that makes no explicit call. *)
let implicit_call_outgoing = function
  | Conv { op = Fptrunc | Fpext; from_ty = F 16; _ }
  | Conv { op = Fptrunc | Fpext; to_ty = F 16; _ }
  | Fbin { ty = F 16; _ }
  | Fcmp { ty = F 16; _ } ->
      outgoing_bytes [ F 32 ]
  | Bin { op = Sdiv | Srem | Udiv | Urem; ty; _ } when is_wide ty ->
      outgoing_bytes [ Ptr; Ptr; Ptr; Ptr; I 64; I 64 ]
  | _ -> 0

let instr_has_call = function
  | Call _ -> true
  | Conv { op = Fptrunc | Fpext; from_ty = F 16; _ }
  | Conv { op = Fptrunc | Fpext; to_ty = F 16; _ }
  | Fbin { ty = F 16; _ }
  | Fcmp { ty = F 16; _ } -> true
  | Bin { op = Sdiv | Srem | Udiv | Urem; ty; _ } -> is_wide ty
  | _ -> false

let plan_frame (f : func) =
  let fn =
    { frame = 0
    ; ids = StrTbl.create 64
    ; names = [||]
    ; slots = [||]
    ; slot_text = IntTbl.create 32
    ; allocas = [||]
    ; alloca_cache = [||]
    ; falloca_cache = [||]
    ; param_cache = [||]
    ; temp_tys = [||]
    ; sret = None
    ; mul_scratch = 0
    ; cache = [||]
    ; xcache = [||]
    ; saved = []
    ; saved_xmms = []
    ; fname = f.name
    ; ret_ty = f.ret
    ; ret_ext = f.ret_ext
    }
  in
  let blocks = Array.of_list f.blocks in
  let nblocks = Array.length blocks in

  let next_id = ref 0 in
  let rev_names = ref [] in
  let intern name =
    match StrTbl.find_opt fn.ids name with
    | Some id -> id
    | None ->
        let id = !next_id in
        next_id := id + 1;
        StrTbl.add fn.ids name id;
        rev_names := name :: !rev_names;
        id
  in
  let stamp = function Temp t -> t.tid <- intern t.tname | _ -> () in
  List.iter (fun (_, name) -> ignore (intern name)) f.params;
  let block_instrs = Array.make nblocks [||] in
  let block_defs = Array.make nblocks [||] in
  let function_has_call = ref false in
  Array.iteri
    (fun block_index b ->
      let instrs = Array.of_list b.instrs in
      let defs = Array.make (Array.length instrs) (-1) in
      Array.iteri
        (fun i instr ->
          if sysv_abi && instr_has_call instr then function_has_call := true;
          (match instr_def instr with Some d -> defs.(i) <- intern d | None -> ());
          iter_instr_uses stamp instr)
        instrs;
      iter_term_uses stamp b.term;
      block_instrs.(block_index) <- instrs;
      block_defs.(block_index) <- defs)
    blocks;
  let ntemps = !next_id in
  fn.names <- Array.of_list (List.rev !rev_names);
  fn.slots <- Array.make ntemps (-1);
  fn.allocas <- Array.make ntemps (-1);
  fn.alloca_cache <- Array.make ntemps None;
  fn.falloca_cache <- Array.make ntemps (-1);
  fn.param_cache <- Array.make ntemps None;
  fn.temp_tys <- Array.make ntemps Void;
  fn.cache <- Array.make ntemps None;
  fn.xcache <- Array.make ntemps (-1);
  let alloca_ty = Array.make ntemps Void in
  let is_scalar_alloca = Array.make ntemps false in
  let def_block = Array.make ntemps (-1) in
  let is_checked_result = Array.make ntemps false in
  let is_param = Array.make ntemps false in
  let param_uses = Array.make ntemps 0 in
  let temp_uses = Array.make ntemps 0 in
  let sole_use_block = Array.make ntemps (-1) in
  let param_store = Array.make ntemps (-1) in
  List.iter
    (fun (ty, name) ->
      let id = intern name in
      is_param.(id) <- true;
      fn.temp_tys.(id) <- ty)
    f.params;
  Array.iteri
    (fun block_index instrs ->
      let defs = block_defs.(block_index) in
      Array.iteri
        (fun i instr ->
          let d = defs.(i) in
          if d >= 0 then begin
            (match instr_dst_ty instr with
             | Some ty ->
                 fn.temp_tys.(d) <- ty;
                 def_block.(d) <- block_index
             | None -> ());
            match instr with
            | Alloca { ty; _ } when not (is_wide ty) ->
                alloca_ty.(d) <- ty;
                is_scalar_alloca.(d) <- true
            | Checked_arith _ -> is_checked_result.(d) <- true
            | _ -> ()
          end)
        instrs)
    block_instrs;
  let slot_size id =
    let ty = fn.temp_tys.(id) in
    if is_wide ty then mem_bytes ty else 8
  in
  let cursor = ref 0 in
  let bump size align =
    let align = max 1 align in
    cursor := !cursor + size;
    if !cursor mod align <> 0 then cursor := !cursor + (align - (!cursor mod align));
    !cursor
  in
  let dedicate id =
    if fn.slots.(id) < 0 then fn.slots.(id) <- bump (slot_size id) 8
  in

  let alloca_accesses = Array.make ntemps 0 in
  let invalid_allocas = Array.make ntemps false in
  let note_alloca ~allowed value =
    let id = value_id value in
    if id >= 0 && is_scalar_alloca.(id) then
      if allowed then alloca_accesses.(id) <- alloca_accesses.(id) + 1
      else invalid_allocas.(id) <- true
  in
  let note_param value =
    let id = value_id value in
    if id >= 0 && is_param.(id) then param_uses.(id) <- param_uses.(id) + 1
  in
  let escapes = Array.make ntemps false in
  let note_escape block_index value =
    let id = value_id value in
    if id >= 0 then begin
      let defining_block = def_block.(id) in
      if defining_block >= 0 && defining_block <> block_index then
        escapes.(id) <- true
    end
  in
  Array.iteri
    (fun block_index instrs ->
      let note_use value =
        note_param value;
        let id = value_id value in
        if id >= 0 then begin
          temp_uses.(id) <- temp_uses.(id) + 1;
          let prior = sole_use_block.(id) in
          if prior = -1 then sole_use_block.(id) <- block_index
          else if prior <> block_index then sole_use_block.(id) <- -2
        end;
        note_escape block_index value
      in
      Array.iter
        (fun instr ->
          iter_instr_uses note_use instr;
          (match instr with
           | Store { src = Temp param; dst = Temp alloca; _ }
             when is_param.(param.tid) ->
               param_store.(param.tid) <- alloca.tid
           | _ -> ());
          match instr with
          | Alloca _ -> ()
          | Load { src; _ } -> note_alloca ~allowed:true src
          | Store { src; dst; _ } ->
              note_alloca ~allowed:false src;
              note_alloca ~allowed:true dst
          | _ -> iter_instr_uses (note_alloca ~allowed:false) instr)
        instrs;
      iter_term_uses
        (fun value ->
          note_use value;
          note_alloca ~allowed:false value)
        blocks.(block_index).term)
    block_instrs;
  let promotable = ref [] in
  for id = ntemps - 1 downto 0 do
    if is_scalar_alloca.(id) && alloca_accesses.(id) >= 2 && not invalid_allocas.(id)
    then promotable := (id, alloca_accesses.(id)) :: !promotable
  done;
  let promotable =
    List.sort
      (fun (a, na) (b, nb) ->
        let by_hotness = Int.compare nb na in
        if by_hotness <> 0 then by_hotness
        else String.compare fn.names.(a) fn.names.(b))
      !promotable
  in
  let int_promotable =
    List.filter (fun (id, _) -> not (is_float alloca_ty.(id))) promotable
  in
  let float_promotable =
    if windows_abi || not !function_has_call then
      List.filter
        (fun (id, accesses) ->
          is_xmm_ty alloca_ty.(id) && ((not windows_abi) || accesses >= 3))
        promotable
    else []
  in
  let promoted_int = ref [] in
  let promoted_float = ref [] in
  let rec assign_allocas regs candidates count =
    match (regs, candidates) with
    | r :: regs, (id, _) :: rest when count < 3 ->
        fn.alloca_cache.(id) <- Some r;
        promoted_int := (id, r) :: !promoted_int;
        assign_allocas regs rest (count + 1)
    | _ -> ()
  in
  assign_allocas cache_gregs int_promotable 0;
  let rec assign_fallocas regs candidates count =
    match (regs, candidates) with
    | reg :: regs, (id, _) :: rest when count < 3 ->
        fn.falloca_cache.(id) <- reg;
        promoted_float := (id, reg) :: !promoted_float;
        assign_fallocas regs rest (count + 1)
    | _ -> ()
  in
  assign_fallocas cache_xmms float_promotable 0;

  let assigned_alloca_gregs = List.map snd !promoted_int in
  let checked_regs =
    List.filter (fun reg -> not (List.mem reg assigned_alloca_gregs)) cache_gregs
  in
  let checked_candidates = ref [] in
  for id = 0 to ntemps - 1 do
    if escapes.(id) && is_checked_result.(id) && temp_uses.(id) > 0 then
      checked_candidates := (id, temp_uses.(id)) :: !checked_candidates
  done;
  let checked_candidates =
    List.sort
      (fun (a, na) (b, nb) ->
        let by_hotness = Int.compare nb na in
        if by_hotness <> 0 then by_hotness
        else String.compare fn.names.(a) fn.names.(b))
      !checked_candidates
  in
  let checked_cached = ref [] in
  let rec assign_checked regs candidates =
    match (regs, candidates) with
    | reg :: regs, (id, _) :: rest ->
        fn.cache.(id) <- Some reg;
        checked_cached := (id, reg) :: !checked_cached;
        assign_checked regs rest
    | _ -> ()
  in
  assign_checked checked_regs checked_candidates;

  let label_indices = StrTbl.create nblocks in
  Array.iteri (fun i block -> StrTbl.add label_indices block.label i) blocks;
  let stores_alloca alloca = function
    | Store { dst = Temp target; _ } -> target.tid = alloca
    | _ -> false
  in
  Array.iteri
    (fun block_index instrs ->
      let defs = block_defs.(block_index) in
      Array.iteri
        (fun instr_index instr ->
          match instr with
          | Load { src = Temp alloca; _ } ->
              let d = defs.(instr_index) in
              let use_block = if d >= 0 then sole_use_block.(d) else -1 in
              let target_reg = fn.alloca_cache.(alloca.tid) in
              if d >= 0 && escapes.(d) && target_reg <> None && use_block >= 0 then begin
                let reaches_use =
                  match blocks.(block_index).term with
                  | Br label -> StrTbl.find_opt label_indices label = Some use_block
                  | Br_cond { then_label; else_label; _ } ->
                      StrTbl.find_opt label_indices then_label = Some use_block
                      || StrTbl.find_opt label_indices else_label = Some use_block
                  | _ -> false
                in
                let defining_tail_clean = ref true in
                for i = instr_index + 1 to Array.length instrs - 1 do
                  if stores_alloca alloca.tid instrs.(i) then
                    defining_tail_clean := false
                done;
                let successor = block_instrs.(use_block) in
                let last_use = ref (-1) and first_store = ref (Array.length successor) in
                Array.iteri
                  (fun i successor_instr ->
                    iter_instr_uses
                      (fun value -> if value_id value = d then last_use := i)
                      successor_instr;
                    if stores_alloca alloca.tid successor_instr && !first_store > i then
                      first_store := i)
                  successor;
                iter_term_uses
                  (fun value ->
                    if value_id value = d then last_use := Array.length successor)
                  blocks.(use_block).term;
                if reaches_use
                   && !defining_tail_clean
                   && !last_use >= 0
                   && !first_store > !last_use
                then fn.cache.(d) <- target_reg
              end
          | _ -> ())
        instrs)
    block_instrs;

  List.iter
    (fun (_, name) ->
      let id = temp_id fn name in
      let alloca = param_store.(id) in
      if param_uses.(id) = 1 && alloca >= 0 && fn.alloca_cache.(alloca) <> None then
        fn.param_cache.(id) <- fn.alloca_cache.(alloca))
    f.params;

  let is_wide_temp id = is_wide fn.temp_tys.(id) in

  if is_wide f.ret then fn.sret <- Some (bump 8 8);
  List.iter
    (fun (_, name) ->
      let id = temp_id fn name in
      if fn.param_cache.(id) = None then dedicate id)
    f.params;
  let outgoing = ref 0 in
  Array.iteri
    (fun block_index instrs ->
      let defs = block_defs.(block_index) in
      Array.iteri
        (fun i instr ->
          let d = defs.(i) in
          (match instr with
           | Alloca { ty; align; _ } ->
               if fn.alloca_cache.(d) = None && fn.falloca_cache.(d) < 0 then begin
                 let size = max 1 (size_bytes ty) in
                 let want = min 16 (max align (min 16 size)) in
                 fn.allocas.(d) <- bump size want
               end
           | Bin { op = Mul; ty; _ } when is_wide ty ->
               if fn.mul_scratch = 0 then fn.mul_scratch <- bump 8 8;
               dedicate d
           | _ ->
               if d >= 0
                  && (escapes.(d) || is_wide_temp d)
                  && fn.cache.(d) = None
               then dedicate d);
          match instr with
          | Call { ret; args; _ } ->
              let tys = List.map fst args in
              let tys = if is_wide ret then Ptr :: tys else tys in
              outgoing := max !outgoing (outgoing_bytes tys)
          | instr -> outgoing := max !outgoing (implicit_call_outgoing instr))
        instrs)
    block_instrs;

  let pool = ref [] in

  let assigned_gregs =
    List.map snd !promoted_int @ List.map snd !checked_cached
  in
  let reserved_cache = List.filter (fun r -> List.mem r assigned_gregs) cache_gregs in
  let available_cache = List.filter (fun r -> not (List.mem r reserved_cache)) cache_gregs in
  let used_cache = ref reserved_cache in
  let assigned_xmms = List.map snd !promoted_float in
  let reserved_xcache =
    List.filter (fun reg -> List.mem reg assigned_xmms) cache_xmms
  in
  let available_xcache =
    List.filter (fun reg -> not (List.mem reg reserved_xcache)) cache_xmms
  in
  let used_xcache = ref reserved_xcache in
  let xcache_score = Array.make 16 0 in
  if windows_abi then
    List.iter
      (fun (id, reg) -> xcache_score.(reg) <- alloca_accesses.(id))
      !promoted_float;

  let last_use = Array.make ntemps (-1) in
  let last_use_block = Array.make ntemps (-1) in
  let use_count = Array.make ntemps 0 in
  let use_count_block = Array.make ntemps (-1) in

  Array.iteri
    (fun block_index instrs ->
      let defs = block_defs.(block_index) in
      let n = Array.length instrs in
      let note_use id index =
        last_use_block.(id) <- block_index;
        last_use.(id) <- index;
        if windows_abi then
          if use_count_block.(id) = block_index then
            use_count.(id) <- use_count.(id) + 1
          else begin
            use_count_block.(id) <- block_index;
            use_count.(id) <- 1
          end
      in
      let note_use_value index value =
        let id = value_id value in
        if id >= 0 then note_use id index
      in
      Array.iteri
        (fun i instr -> iter_instr_uses (note_use_value i) instr)
        instrs;
      iter_term_uses (note_use_value n) blocks.(block_index).term;
      let dies_of id =
        if last_use_block.(id) = block_index then last_use.(id) else -1
      in
      let reads_of id =
        if use_count_block.(id) = block_index then use_count.(id) else 1
      in

      let calls_before = Array.make (n + 1) 0 in
      if sysv_abi then
        for i = 0 to n - 1 do
          calls_before.(i + 1) <-
            calls_before.(i) + if instr_has_call instrs.(i) then 1 else 0
        done;

      let next_stores = ref [] in
      let record_next_stores alloca =
        let next = Array.make (n + 1) n in
        let upcoming = ref n in
        for i = n - 1 downto 0 do
          next.(i) <- !upcoming;
          (match instrs.(i) with
           | Store { dst = Temp target; _ } when target.tid = alloca ->
               upcoming := i
           | _ -> ())
        done;
        next_stores := (alloca, next) :: !next_stores
      in
      List.iter (fun (id, _) -> record_next_stores id) !promoted_int;
      List.iter (fun (id, _) -> record_next_stores id) !promoted_float;

      Array.iteri
        (fun i instr ->
          match instr with
          | Load { src = Temp alloca; _ }
            when defs.(i) >= 0 && not escapes.(defs.(i)) -> (
              let dst = defs.(i) in
              match (fn.alloca_cache.(alloca.tid), dies_of dst) with
              | Some r, dies when dies >= 0 ->
                  let next = List.assoc alloca.tid !next_stores in
                  if next.(i) >= dies then fn.cache.(dst) <- Some r
              | _ -> ())
          | _ -> ())
        instrs;
      Array.iteri
        (fun i instr ->
          match instr with
          | Load { src = Temp alloca; _ }
            when defs.(i) >= 0 && not escapes.(defs.(i)) ->
              let dst = defs.(i) in
              let reg = fn.falloca_cache.(alloca.tid) in
              let dies = dies_of dst in
              if reg >= 0 && dies >= 0 then begin
                let next = List.assoc alloca.tid !next_stores in
                if next.(i) >= dies then fn.xcache.(dst) <- reg
              end
          | _ -> ())
        instrs;

      (* Update a promoted integer local in its own register. The canonical IR
         shape is `%old = load %slot; %new = bin %old, x; store %new, %slot`.
         When the result has no other reader, assigning it the slot's register
         turns the final store into a no-op. Restrict this to a reusable lhs:
         the integer emitter consumes lhs before overwriting the destination,
         whereas reusing a rhs register for subtraction would change meaning. *)
      Array.iteri
        (fun i instr ->
          if i + 1 < n then
            match (instr, instrs.(i + 1)) with
            | Bin { dst; lhs = Temp lhs; ty; _ },
              Store { src = Temp stored; dst = Temp alloca; _ }
              when String.equal dst stored.tname
                   && not (is_wide ty)
                   && not (is_float ty) -> (
                let d = defs.(i) in
                let target = fn.alloca_cache.(alloca.tid) in
                if d >= 0
                   && not escapes.(d)
                   && dies_of d = i + 1
                   && dies_of lhs.tid = i
                   && fn.cache.(lhs.tid) = target
                then fn.cache.(d) <- target)
            | _ -> ())
        instrs;

      let slot_free = ref !pool in
      let slot_releases = Array.make (n + 1) [] in
      let reg_free = ref available_cache in
      let active = ref [] in
      let reg_releases = Array.make (n + 1) [] in
      let xmm_free = ref available_xcache in
      let xmm_active = ref [] in
      let xmm_releases = Array.make (n + 1) [] in
      Array.iteri
        (fun i instr ->
          let d = defs.(i) in
          (if d >= 0 && (not (is_alloca instr))
              && (not escapes.(d))
              && (not (is_wide_temp d))
              && fn.slots.(d) < 0
           then begin
             let off =
               match !slot_free with
               | off :: rest -> slot_free := rest; off
               | [] ->
                   let off = bump 8 8 in
                   pool := off :: !pool;
                   off
             in
             fn.slots.(d) <- off;
             let dies = let dies = dies_of d in if dies < 0 then i else dies in
             slot_releases.(dies) <- off :: slot_releases.(dies)
           end);
          (match instr_dst_ty instr with
           | Some ty
             when d >= 0
                  && (not escapes.(d))
                  && (not (is_wide ty))
                  && fn.cache.(d) = None
                  && (not (is_float ty))
                  && dies_of d > i -> (
               (* Prefer the register of an input whose final read is this
                  instruction, but only when the emitter consumes that input
                  before writing the destination. This is the small coalescing
                  step that turns `%a = op ...; %b = op %a, ...` into in-place
                  arithmetic rather than a chain of register moves. *)
               let reusable_value value =
                 let id = value_id value in
                 if id >= 0 && dies_of id = i then
                   match fn.cache.(id) with
                   | Some r when List.mem r !active -> Some r
                   | _ -> None
                 else None
               in
               let or_else found value =
                 match found with Some _ -> found | None -> reusable_value value
               in
               let reusable =
                 match instr with
                 | Load { src; _ } | Conv { src; _ } -> reusable_value src
                 | Bin { lhs; _ } -> reusable_value lhs
                 | Icmp { lhs; rhs; _ } | Fcmp { lhs; rhs; _ } ->
                     or_else (reusable_value lhs) rhs
                 | Gep_byte { base; _ } -> reusable_value base
                 | Call { args; callee; _ } ->
                     let from_callee =
                       match callee with Direct _ -> None | Indirect v -> reusable_value v
                     in
                     List.fold_left
                       (fun found (_, value) -> or_else found value)
                       from_callee args
                 | _ -> None
               in
               let dies = dies_of d in
               match (reusable, !reg_free) with
               | Some r, _ ->
                   (* Its old value was due to release after this instruction;
                      transfer ownership directly to the result instead. *)
                   reg_releases.(i) <-
                     List.filter (fun released -> released <> r) reg_releases.(i);
                   fn.cache.(d) <- Some r;
                   if not (List.mem r !used_cache) then used_cache := r :: !used_cache;
                   reg_releases.(dies) <- r :: reg_releases.(dies)
               | None, r :: rest ->
                   reg_free := rest;
                   active := r :: !active;
                   fn.cache.(d) <- Some r;
                   if not (List.mem r !used_cache) then used_cache := r :: !used_cache;
                   reg_releases.(dies) <- r :: reg_releases.(dies)
               | None, [] -> () )
           | _ -> ());
          (match instr_dst_ty instr with
           | Some ty
             when d >= 0
                  && is_xmm_ty ty
                  && (not escapes.(d))
                  && fn.xcache.(d) < 0
                  && (let dies = dies_of d in
                      dies > i
                      && (windows_abi || calls_before.(dies) = calls_before.(i + 1))) -> (
               let reusable =
                 match instr with
                 | Fbin { lhs = Temp name; _ } when dies_of name.tid = i ->
                     let reg = fn.xcache.(name.tid) in
                     if reg >= 0 && List.mem reg !xmm_active then Some reg else None
                 | _ -> None
               in
               match (reusable, !xmm_free) with
               | Some reg, _ ->
                   xmm_releases.(i) <-
                     List.filter (fun released -> released <> reg) xmm_releases.(i);
                   fn.xcache.(d) <- reg;
                   if not (List.mem reg !used_xcache) then
                     used_xcache := reg :: !used_xcache;
                   if windows_abi then
                     xcache_score.(reg) <- xcache_score.(reg) + reads_of d;
                   let dies = dies_of d in
                   xmm_releases.(dies) <- reg :: xmm_releases.(dies)
               | None, reg :: rest ->
                   xmm_free := rest;
                   xmm_active := reg :: !xmm_active;
                   fn.xcache.(d) <- reg;
                   if not (List.mem reg !used_xcache) then
                     used_xcache := reg :: !used_xcache;
                   if windows_abi then
                     xcache_score.(reg) <- xcache_score.(reg) + reads_of d;
                   let dies = dies_of d in
                   xmm_releases.(dies) <- reg :: xmm_releases.(dies)
               | None, [] -> ())
           | _ -> ());
          slot_free := slot_releases.(i) @ !slot_free;
          let dead = reg_releases.(i) in
          if dead <> [] then begin
            active := List.filter (fun r -> not (List.mem r dead)) !active;
            reg_free := dead @ !reg_free
          end;
          let xmm_dead = xmm_releases.(i) in
          if xmm_dead <> [] then begin
            xmm_active :=
              List.filter (fun reg -> not (List.mem reg xmm_dead)) !xmm_active;
            xmm_free := xmm_dead @ !xmm_free
          end)
        instrs)
    block_instrs;
  fn.saved <- List.map (fun r -> (r, bump 8 8)) (List.rev !used_cache);
  if windows_abi then begin
    let profitable = List.filter (fun reg -> xcache_score.(reg) >= 2) !used_xcache in
    let rejected =
      List.filter (fun reg -> not (List.mem reg profitable)) !used_xcache
    in
    if rejected <> [] then
      for id = 0 to ntemps - 1 do
        let reg = fn.xcache.(id) in
        if reg >= 0 && List.mem reg rejected then fn.xcache.(id) <- -1
      done;
    fn.saved_xmms <-
      List.map (fun reg -> (reg, bump 16 16)) (List.rev profitable)
  end;

  let locals = !cursor in
  let total = locals + !outgoing in
  fn.frame <- (total + 15) / 16 * 16;
  fn

let imm_int64 st text =
  match StrTbl.find_opt st.int_imms text with
  | Some v -> v
  | None ->
      let v =
        match Int64.of_string_opt text with
        | Some v -> v
        | None -> (
            match Int64.of_string_opt ("0u" ^ text) with
            | Some v -> v
            | None -> fail "integer literal out of range: %s" text)
      in
      StrTbl.add st.int_imms text v;
      v

let mask_to_bits bits v =
  if bits >= 64 then v else Int64.logand v (Int64.sub (Int64.shift_left 1L bits) 1L)

let imm_zx_hex st bits text =
  let key = (false, bits, text) in
  match ImmTbl.find_opt st.imm_text key with
  | Some rendered -> rendered
  | None ->
      let v = mask_to_bits bits (imm_int64 st text) in
      let rendered =
        { text = Printf.sprintf "$0x%Lx" v; op = Asm_x64_obj.imm_operand v }
      in
      ImmTbl.add st.imm_text key rendered;
      rendered

let imm_sx_hex st bits text =
  let key = (true, bits, text) in
  match ImmTbl.find_opt st.imm_text key with
  | Some rendered -> rendered
  | None ->
      let v = mask_to_bits bits (imm_int64 st text) in
      let v =
        if bits >= 64 then v
        else
          let shift = 64 - bits in
          Int64.shift_right (Int64.shift_left v shift) shift
      in
      let rendered =
        { text = Printf.sprintf "$0x%Lx" v; op = Asm_x64_obj.imm_operand v }
      in
      ImmTbl.add st.imm_text key rendered;
      rendered

let fits_int32 v = Int64.compare v (-2147483648L) >= 0 && Int64.compare v 2147483647L <= 0

let alu_immediate st ty text =
  let exact = exact_bits ty in
  let work = work_bits ty in
  let value = mask_to_bits exact (imm_int64 st text) in
  if work = 32 then
    let v = Int64.logand value 0xFFFF_FFFFL in
    Some { text = Printf.sprintf "$0x%Lx" v; op = Asm_x64_obj.imm_operand v }
  else
    let low32 = Int64.logand value 0xFFFF_FFFFL in
    let encoded =
      if Int64.logand low32 0x8000_0000L <> 0L
      then Int64.logor low32 0xFFFF_FFFF_0000_0000L
      else low32
    in
    let mask =
      if exact >= 64 then Int64.minus_one
      else Int64.sub (Int64.shift_left 1L exact) 1L
    in
    if Int64.logand encoded mask = value then
      Some
        { text = Printf.sprintf "$%Ld" encoded
        ; op = Asm_x64_obj.imm_operand encoded
        }
    else None

let is_all_ones st ty text =
  let bits = exact_bits ty in
  let mask =
    if bits >= 64 then Int64.minus_one
    else Int64.sub (Int64.shift_left 1L bits) 1L
  in
  mask_to_bits bits (imm_int64 st text) = mask

type mem = int -> opd

let mem_via_reg reg : mem =
  let r = gr reg 64 in
  let code = greg_code reg in
  fun k ->
    { text =
        (if k = 0 then "(" ^ r ^ ")" else string_of_int k ^ "(" ^ r ^ ")")
    ; op = Asm_x64_obj.mem_reg_disp code k
    }

let mem_via_rbp off : mem = fun k -> mem_rbp_o (-off + k)

let load_bytes_to st reg (at : mem) n =
  let r32 = gro reg 32 and r64 = gro reg 64 in
  match n with
  | 1 -> insn2 st "movzbl" (at 0) r32
  | 2 -> insn2 st "movzwl" (at 0) r32
  | 4 -> insn2 st "movl" (at 0) r32
  | 8 -> insn2 st "movq" (at 0) r64
  | 3 ->
      insn2 st "movzwl" (at 0) r32;
      insn2 st "movzbl" (at 2) (gro Rdx 32);
      insn2 st "shll" (imm_int_o 16) (gro Rdx 32);
      insn2 st "orl" (gro Rdx 32) r32
  | 5 | 6 | 7 ->
      insn2 st "movl" (at 0) r32;
      for i = n - 1 downto 4 do
        insn2 st "movzbl" (at i) (gro Rdx 32);
        insn2 st "shlq" (imm_int_o (i * 8)) (gro Rdx 64);
        insn2 st "orq" (gro Rdx 64) r64
      done
  | _ -> fail "unsupported load of %d bytes" n

(* Write the low [n] bytes of [reg] to [at]. For n < 8 this shifts [reg], so the
   caller must not still need its value -- only the full-width case is safe to
   use with a register whose contents have to survive. *)
let store_bytes_from st reg (at : mem) n =
  let r8 = gro reg 8 and r16 = gro reg 16 and r32 = gro reg 32 and r64 = gro reg 64 in
  match n with
  | 1 -> insn2 st "movb" r8 (at 0)
  | 2 -> insn2 st "movw" r16 (at 0)
  | 4 -> insn2 st "movl" r32 (at 0)
  | 8 -> insn2 st "movq" r64 (at 0)
  | 3 ->
      insn2 st "movw" r16 (at 0);
      insn2 st "shrl" (imm_int_o 16) r32;
      insn2 st "movb" r8 (at 2)
  | 5 | 6 | 7 ->
      insn2 st "movl" r32 (at 0);
      insn2 st "shrq" (imm_int_o 32) r64;
      if n = 5 then insn2 st "movb" r8 (at 4)
      else begin
        insn2 st "movw" r16 (at 4);
        if n = 7 then begin
          insn2 st "shrl" (imm_int_o 16) r32;
          insn2 st "movb" r8 (at 6)
        end
      end
  | _ -> fail "unsupported store of %d bytes" n

let direct_mem fn value =
  match value with
  | Temp t ->
      let off = fn.allocas.(t.tid) in
      if off >= 0 then Some (mem_via_rbp off) else None
  | _ -> None

let cached fn id = fn.cache.(id)
let cached_alloca fn id = fn.alloca_cache.(id)
let cached_xmm fn id = let n = fn.xcache.(id) in if n >= 0 then Some n else None
let cached_falloca fn id =
  let n = fn.falloca_cache.(id) in
  if n >= 0 then Some n else None

let addr_to_reg st fn reg value =
  let r = gro reg 64 in
  match value with
  | Temp t ->
      let off = fn.allocas.(t.tid) in
      if off >= 0 then insn2 st "leaq" (mem_slot fn off) r
      else (
        match cached fn t.tid with
        | Some c -> if c <> reg then insn2 st "movq" (gro c 64) r
        | None -> insn2 st "movq" (mem_slot fn (slot_of fn t.tid)) r)
  | Global sym -> insn2 st "leaq" (rip_o sym) r
  | Null -> insn2 st "xorl" (gro reg 32) (gro reg 32)
  | Int_imm text -> insn2 st "movabsq" (imm_zx_hex st 64 text) r
  | Bool_imm _ | Float_bits _ -> fail "%s: not a pointer value" fn.fname

let int_to_reg st fn reg ty value =
  match value with
  | Temp t ->
      let off = fn.allocas.(t.tid) in
      if off >= 0 then insn2 st "leaq" (mem_slot fn off) (gro reg 64)
      else (
        match cached fn t.tid with
        | Some c -> if c <> reg then insn2 st "movq" (gro c 64) (gro reg 64)
        | None -> insn2 st "movq" (mem_slot fn (slot_of fn t.tid)) (gro reg 64))
  | Global sym -> insn2 st "leaq" (rip_o sym) (gro reg 64)
  | Null -> insn2 st "xorl" (gro reg 32) (gro reg 32)
  | Bool_imm b -> insn2 st "movl" (if b then o_imm1 else o_imm0) (gro reg 32)
  | Int_imm text ->
      let bits = exact_bits ty in
      if bits_of_ty ty <= 32 then insn2 st "movl" (imm_zx_hex st bits text) (gro reg 32)
      else insn2 st "movabsq" (imm_zx_hex st bits text) (gro reg 64)
  | Float_bits bits ->
      insn2 st "movl"
        { text = "$0x" ^ bits
        ; op = Asm_x64_obj.imm_operand (Int64.of_string ("0x" ^ bits))
        }
        (gro reg 32)

let extend_reg_in_place st reg bits ~signed =
  if bits < 64 then
    if is_machine_width bits then begin
      let src = gro reg bits and dst = gro reg 64 in
      match (signed, bits) with
      | true, 8 -> insn2 st "movsbq" src dst
      | true, 16 -> insn2 st "movswq" src dst
      | true, _ -> insn2 st "movslq" src dst
      | false, 8 -> insn2 st "movzbq" src dst
      | false, 16 -> insn2 st "movzwq" src dst
      | false, _ -> insn2 st "movl" src (gro reg 32)
    end
    else begin
      let shift = 64 - bits in
      insn2 st "shlq" (imm_int_o shift) (gro reg 64);
      insn2 st (if signed then "sarq" else "shrq") (imm_int_o shift) (gro reg 64)
    end

let int_to_reg_ext st fn reg ty value ~signed =
  let bits = exact_bits ty in
  match value with
  | Int_imm text ->
      insn2 st "movabsq"
        ((if signed then imm_sx_hex st else imm_zx_hex st) bits text)
        (gro reg 64)
  | Temp t
    when fn.allocas.(t.tid) < 0
         && cached fn t.tid = None
         && is_machine_width bits ->
      let off = mem_slot fn (slot_of fn t.tid) in
      if signed then
        match bits with
        | 8 -> insn2 st "movsbq" off (gro reg 64)
        | 16 -> insn2 st "movswq" off (gro reg 64)
        | 32 -> insn2 st "movslq" off (gro reg 64)
        | _ -> insn2 st "movq" off (gro reg 64)
      else (
        match bits with
        | 8 -> insn2 st "movzbl" off (gro reg 32)
        | 16 -> insn2 st "movzwl" off (gro reg 32)
        | 32 -> insn2 st "movl" off (gro reg 32)
        | _ -> insn2 st "movq" off (gro reg 64))
  | Temp t when cached fn t.tid <> None && is_machine_width bits && bits < 64 ->
      let c = gro (Option.get (cached fn t.tid)) bits in
      if signed then
        match bits with
        | 8 -> insn2 st "movsbq" c (gro reg 64)
        | 16 -> insn2 st "movswq" c (gro reg 64)
        | _ -> insn2 st "movslq" c (gro reg 64)
      else (
        match bits with
        | 8 -> insn2 st "movzbl" c (gro reg 32)
        | 16 -> insn2 st "movzwl" c (gro reg 32)
        | _ -> insn2 st "movl" c (gro reg 32))
  | _ ->
      int_to_reg st fn reg ty value;
      extend_reg_in_place st reg bits ~signed

let int_to_reg_zx st fn reg ty value = int_to_reg_ext st fn reg ty value ~signed:false
let int_to_reg_sx st fn reg ty value = int_to_reg_ext st fn reg ty value ~signed:true

let int_use st fn reg ty value ~signed =
  match value with
  | Temp t when exact_bits ty = 64 -> (
      match cached fn t.tid with
      | Some c -> gro c 64
      | None ->
          int_to_reg_ext st fn reg ty value ~signed;
          gro reg 64)
  | _ ->
      int_to_reg_ext st fn reg ty value ~signed;
      gro reg 64

let dst_reg fn dst = match cached fn (temp_id fn dst) with Some c -> c | None -> Rax

(* Finish a result already computed in [reg]. A cached result is done; anything
   else has to reach its slot. *)
let commit_from st fn dst reg =
  let id = temp_id fn dst in
  match cached fn id with
  | Some c -> if c <> reg then insn2 st "movq" (gro reg 64) (gro c 64)
  | None -> insn2 st "movq" (gro reg 64) (mem_slot fn (slot_of fn id))

let fmov_sfx = function F 32 -> "ss" | F 64 -> "sd" | ty -> fail "not an xmm type: %s" (ty_name ty)

let xmm_names =
  [| "%xmm0"; "%xmm1"; "%xmm2"; "%xmm3"; "%xmm4"; "%xmm5"; "%xmm6"; "%xmm7"
   ; "%xmm8"; "%xmm9"; "%xmm10"; "%xmm11"; "%xmm12"; "%xmm13"; "%xmm14"; "%xmm15"
  |]

let xmm n =
  if n >= 0 && n < Array.length xmm_names then xmm_names.(n)
  else fail "xmm register index out of range: %d" n

let xmmo_table =
  Array.init 16 (fun n ->
    { text = xmm_names.(n); op = Asm_x64_obj.xmm_operand n })

let xmmo n =
  if n >= 0 && n < 16 then xmmo_table.(n)
  else fail "xmm register index out of range: %d" n

let float_to_xmm st fn n ty value =
  let sfx = fmov_sfx ty in
  match value with
  | Temp t -> (
      match cached_xmm fn t.tid with
      | Some source ->
          if source <> n then insn2_suffix st "mov" sfx (xmmo source) (xmmo n)
      | None ->
          insn2_suffix st "mov" sfx (mem_slot fn (slot_of fn t.tid)) (xmmo n))
  | Float_bits bits ->
      let label = pool_label st (bits_of_ty ty) bits in
      insn2_suffix st "mov" sfx (rip_o label) (xmmo n)
  | Null | Int_imm "0" -> insn2 st "xorps" (xmmo n) (xmmo n)
  | _ -> fail "%s: unsupported float operand" fn.fname

let commit_int st fn dst =
  let id = temp_id fn dst in
  match cached fn id with
  | Some c -> insn2 st "movq" (gro Rax 64) (gro c 64)
  | None -> insn2 st "movq" (gro Rax 64) (mem_slot fn (slot_of fn id))
let commit_xmm st fn dst ty n =
  let id = temp_id fn dst in
  match cached_xmm fn id with
  | Some target ->
      if target <> n then
        insn2_suffix st "mov" (fmov_sfx ty) (xmmo n) (xmmo target)
  | None ->
      insn2_suffix st "mov" (fmov_sfx ty) (xmmo n) (mem_slot fn (slot_of fn id))

let xmm_dst fn dst fallback =
  Option.value ~default:fallback (cached_xmm fn (temp_id fn dst))

let cc_of_icmp = function
  | Eq -> "e" | Ne -> "ne"
  | Slt -> "l" | Sle -> "le" | Sgt -> "g" | Sge -> "ge"
  | Ult -> "b" | Ule -> "be" | Ugt -> "a" | Uge -> "ae"

let invert_cc = function
  | "e" -> "ne" | "ne" -> "e"
  | "l" -> "ge" | "ge" -> "l"
  | "le" -> "g" | "g" -> "le"
  | "b" -> "ae" | "ae" -> "b"
  | "be" -> "a" | "a" -> "be"
  | cc -> fail "no inverse for condition code %s" cc

let icmp_is_signed = function
  | Slt | Sle | Sgt | Sge -> true
  | Eq | Ne | Ult | Ule | Ugt | Uge -> false

(* Integers wider than a register are byte strings in memory, little-endian, and
   every operation below is memory-to-memory over a byte-at-a-time loop.
   Byte-at-a-time rather than 64-bit limbs because a legal width need not be a
   multiple of 64 -- i1000 is 125 bytes, so a limb loop would need a ragged final
   limb in every routine. These are rare and already O(n); uniformity is worth
   more here than a constant factor.

   Register roles, fixed across all of them:
     r9  = &dst      r10 = &lhs      r11 = &rhs
     rcx = index     rax, rdx = scratch      r8 = carry/limit
   None of these is callee-saved, so nothing needs preserving. *)

let wide_addr st fn reg value =
  match value with
  | Temp t -> insn2 st "leaq" (mem_slot fn (slot_of fn t.tid)) (gro reg 64)
  | Int_imm _ -> fail "internal: wide immediate needs its byte length"
  | _ -> fail "%s: unsupported wide operand" fn.fname

let wide_operand st fn reg nbytes value =
  match value with
  | Int_imm text ->
      insn2 st "leaq" (rip_o (wide_pool_label st nbytes text)) (gro reg 64)
  | _ -> wide_addr st fn reg value

(* dst[0..n) = src[0..n) *)
let wide_copy st n =
  let top = new_loop_label st "wcpy" in
  line st "xorl\t%%ecx, %%ecx";
  raw st "%s:" top;
  line st "movzbl\t(%%r10,%%rcx), %%eax";
  line st "movb\t%%al, (%%r9,%%rcx)";
  line st "incq\t%%rcx";
  line st "cmpq\t$%d, %%rcx" n;
  line st "jb\t%s" top

(* dst[from..to) = al *)
let wide_fill st ~from_ ~to_ =
  if to_ > from_ then begin
    let top = new_loop_label st "wfil" in
    line st "movl\t$%d, %%ecx" from_;
    raw st "%s:" top;
    line st "movb\t%%al, (%%r9,%%rcx)";
    line st "incq\t%%rcx";
    line st "cmpq\t$%d, %%rcx" to_;
    line st "jb\t%s" top
  end

(* dst = lhs OP rhs, bitwise, no carry between bytes. *)
let wide_bitwise st op n =
  let mnemonic = match op with And -> "and" | Or -> "or" | Xor -> "xor" | _ -> assert false in
  let top = new_loop_label st "wbit" in
  line st "xorl\t%%ecx, %%ecx";
  raw st "%s:" top;
  line st "movzbl\t(%%r10,%%rcx), %%eax";
  line st "movzbl\t(%%r11,%%rcx), %%edx";
  line st "%sl\t%%edx, %%eax" mnemonic;
  line st "movb\t%%al, (%%r9,%%rcx)";
  line st "incq\t%%rcx";
  line st "cmpq\t$%d, %%rcx" n;
  line st "jb\t%s" top

(* dst = lhs +/- rhs. The carry (or borrow) is kept in r8d as a 0/1 value rather
   than in the flags, because the loop's own index arithmetic clobbers them. *)
let wide_addsub st ~sub n =
  let top = new_loop_label st "wadd" in
  line st "xorl\t%%ecx, %%ecx";
  line st "xorl\t%%r8d, %%r8d";
  raw st "%s:" top;
  line st "movzbl\t(%%r10,%%rcx), %%eax";
  line st "movzbl\t(%%r11,%%rcx), %%edx";
  if sub then begin
    line st "subl\t%%edx, %%eax";
    line st "subl\t%%r8d, %%eax";
    line st "movb\t%%al, (%%r9,%%rcx)";
    line st "shrl\t$31, %%eax"
  end
  else begin
    line st "addl\t%%edx, %%eax";
    line st "addl\t%%r8d, %%eax";
    line st "movb\t%%al, (%%r9,%%rcx)";
    line st "shrl\t$8, %%eax"
  end;
  line st "movl\t%%eax, %%r8d";
  line st "incq\t%%rcx";
  line st "cmpq\t$%d, %%rcx" n;
  line st "jb\t%s" top

(* dst = lhs * rhs, truncated to n bytes.

   Schoolbook long multiplication in base 256. dst is zeroed, then for each byte
   i of lhs the whole of rhs is multiplied in and accumulated at offset i,
   stopping at n bytes since anything above that is discarded anyway -- which is
   exactly the wrapping LLVM's `mul` specifies.

   dst cannot alias either operand: wide temps always get dedicated slots. *)
let wide_mul st fn n =
  let scratch = (mem_slot fn fn.mul_scratch).text in
  let outer = new_loop_label st "wmul" in
  let inner = new_loop_label st "wmuli" in
  (* dst = 0 *)
  line st "xorl\t%%eax, %%eax";
  wide_fill st ~from_:0 ~to_:n;
  line st "movl\t$%d, %%r8d" n;
  raw st "%s:" outer;
  line st "movzbl\t(%%r10), %%eax";
  line st "movl\t%%eax, %s" scratch;
  line st "xorl\t%%eax, %%eax" ;
  line st "xorl\t%%ecx, %%ecx";
  raw st "%s:" inner;
  line st "movzbl\t(%%r11,%%rcx), %%edx";
  line st "imull\t%s, %%edx" scratch;
  line st "addl\t%%edx, %%eax";
  line st "movzbl\t(%%r9,%%rcx), %%edx";
  line st "addl\t%%edx, %%eax";
  line st "movb\t%%al, (%%r9,%%rcx)";
  line st "shrl\t$8, %%eax";
  line st "incq\t%%rcx";
  line st "cmpq\t%%r8, %%rcx";
  line st "jb\t%s" inner;
  line st "incq\t%%r9";
  line st "incq\t%%r10";
  line st "decq\t%%r8";
  line st "jnz\t%s" outer

(* eax = -1, 0 or 1 according to lhs <=> rhs.

   Reduced to a three-way result so the caller can apply any predicate with one
   signed setcc. Scanning runs from the most significant byte down; only that top
   byte's comparison is signed, and only when the type is. *)
let wide_cmp3 st n ~signed =
  let loop = new_loop_label st "wcmp" in
  let test = new_loop_label st "wcmpt" in
  let less = new_loop_label st "wcmpl" in
  let greater = new_loop_label st "wcmpg" in
  let fin = new_loop_label st "wcmpe" in
  if signed then begin
    line st "movsbl\t%d(%%r10), %%eax" (n - 1);
    line st "movsbl\t%d(%%r11), %%edx" (n - 1);
    line st "cmpl\t%%edx, %%eax";
    line st "jl\t%s" less;
    line st "jg\t%s" greater
  end
  else begin
    line st "movzbl\t%d(%%r10), %%eax" (n - 1);
    line st "movzbl\t%d(%%r11), %%edx" (n - 1);
    line st "cmpl\t%%edx, %%eax";
    line st "jb\t%s" less;
    line st "ja\t%s" greater
  end;
  line st "movl\t$%d, %%ecx" (n - 2);
  line st "jmp\t%s" test;
  raw st "%s:" loop;
  line st "movzbl\t(%%r10,%%rcx), %%eax";
  line st "movzbl\t(%%r11,%%rcx), %%edx";
  line st "cmpl\t%%edx, %%eax";
  line st "jb\t%s" less;
  line st "ja\t%s" greater;
  line st "decq\t%%rcx";
  raw st "%s:" test;
  line st "cmpq\t$0, %%rcx";
  line st "jge\t%s" loop;
  line st "xorl\t%%eax, %%eax";
  line st "jmp\t%s" fin;
  raw st "%s:" less;
  line st "movl\t$-1, %%eax";
  line st "jmp\t%s" fin;
  raw st "%s:" greater;
  line st "movl\t$1, %%eax";
  raw st "%s:" fin

let emit_call st fn dst ret _ret_ext callee args arg_exts =
  if List.length args <> List.length arg_exts then
    fail "%s: call ABI metadata has %d entries for %d arguments"
      fn.fname (List.length arg_exts) (List.length args);
  let sret = is_wide ret in
  let tys = List.map fst args in
  let homes = classify_args (if sret then Ptr :: tys else tys) in
  let homes =
    if sret then
      match (dst, homes) with
      | Some d, Gpr r :: rest ->
          insn2 st "leaq" (mem_slot fn (slot_of fn (temp_id fn d))) (gro r 64);
          rest
      | None, _ ->
          fail "%s: call returning i%d with no destination" fn.fname (exact_bits ret)
      | Some _, _ -> fail "%s: hidden result pointer has no register home" fn.fname
    else homes
  in
  List.iter2
    (fun ((ty, value), ext) home ->
      match home with
      | Gpr r ->
          if is_wide ty then wide_operand st fn r (mem_bytes ty) value
          else if ext = Sign_ext then int_to_reg_sx st fn r ty value
          else int_to_reg_zx st fn r ty value
      | Xmm n ->
          if is_xmm_ty ty then float_to_xmm st fn n ty value
          else if ty = F 16 then begin
            int_to_reg_zx st fn Rax (I 16) value;
            insn2 st "movd" (gro Rax 32) (xmmo n)
          end
          else fail "%s: integer argument assigned an xmm home" fn.fname
      | Stack off ->
          if is_wide ty then begin
            wide_operand st fn R10 (mem_bytes ty) value;
            insn2 st "movq" (gro R10 64) (mem_rsp_o off)
          end
          else if is_xmm_ty ty then begin
            let scratch = if windows_abi then 4 else 8 in
            float_to_xmm st fn scratch ty value;
            insn2_suffix st "mov" (fmov_sfx ty) (xmmo scratch) (mem_rsp_o off)
          end
          else begin
            if ext = Sign_ext then int_to_reg_sx st fn R10 ty value
            else int_to_reg_zx st fn R10 ty value;
            insn2 st "movq" (gro R10 64) (mem_rsp_o off)
          end)
    (List.combine args arg_exts) homes;
  (match callee with
   | Direct sym -> insn1 st "callq" (name_o sym)
   | Indirect v ->
       int_to_reg_zx st fn R11 Ptr v;
       insn1 st "callq" o_star_r11);
  match dst with
  | None -> ()
  | Some dst ->
      if ret = Void || sret then ()
      else if is_xmm_ty ret then commit_xmm st fn dst ret 0
      else if ret = F 16 then begin
        insn2 st "movd" (xmmo 0) (gro Rax 32);
        commit_int st fn dst
      end
      else commit_int st fn dst

let half_to_float_xmm st fn n value =
  int_to_reg_zx st fn Rax (I 16) value;
  insn2 st "movd" (gro Rax 32) (xmmo 0);
  insn1 st "callq" (name_o "__extendhfsf2");
  if n <> 0 then insn2 st "movaps" (xmmo 0) (xmmo n)

let float_xmm_to_half st n =
  if n <> 0 then insn2 st "movaps" (xmmo n) (xmmo 0);
  insn1 st "callq" (name_o "__truncsfhf2");
  insn2 st "movd" (xmmo 0) (gro Rax 32)

let emit_float_libcall st fn name ~arg_ty ~arg ~ret_ty ~dst =
  (if arg_ty = F 16 then begin
     int_to_reg_zx st fn Rax (I 16) arg;
     insn2 st "movd" (gro Rax 32) (xmmo 0)
   end
   else float_to_xmm st fn 0 arg_ty arg);
  insn1 st "callq" (name_o name);
  if ret_ty = F 16 then begin
    insn2 st "movd" (xmmo 0) (gro Rax 32);
    commit_int st fn dst
  end
  else commit_xmm st fn dst ret_ty 0

(* Bring both operands of a half-typed binary operation up to float, leaving lhs
   in xmm1 and rhs in xmm0 — the order `subss`/`divss` want, since AT&T computes
   dst = dst OP src.

   The promoted lhs is parked in [dst]'s own slot across the second libcall
   because that slot is the one piece of frame guaranteed free here: the slot
   allocator claims it before releasing anything that dies at this instruction,
   so it cannot alias lhs or rhs. *)
let promote_half_pair st fn dst lhs rhs =
  half_to_float_xmm st fn 0 lhs;
  insn2 st "movss" (xmmo 0) (mem_slot fn (slot_of fn (temp_id fn dst)));
  half_to_float_xmm st fn 0 rhs;
  insn2 st "movss" (mem_slot fn (slot_of fn (temp_id fn dst))) (xmmo 1)

let emit_icmp_flags st fn pred ty lhs rhs =
  if is_wide ty then begin
    let n = mem_bytes ty in
    wide_operand st fn R10 n lhs;
    wide_operand st fn R11 n rhs;
    wide_cmp3 st n ~signed:(icmp_is_signed pred);
    insn2 st "cmpl" o_imm0 (gro Rax 32);
    match pred with
    | Eq -> "e" | Ne -> "ne"
    | Slt | Ult -> "l" | Sle | Ule -> "le"
    | Sgt | Ugt -> "g" | Sge | Uge -> "ge"
  end
  else begin
    let signed = icmp_is_signed pred in
    let a = int_use st fn Rax ty lhs ~signed in
    let b = int_use st fn R10 ty rhs ~signed in
    insn2 st "cmpq" b a;
    cc_of_icmp pred
  end

let fcmp_fusable pred ty =
  ty <> F 16 && match pred with Fogt | Folt | Foge | Fole -> true | _ -> false

let emit_fcmp_flags st fn ~dst pred ty lhs rhs =
  let single_cc = match pred with Fogt | Folt | Foge | Fole -> true | _ -> false in
  let can_emit = (ty <> F 16 || dst <> None) in
  if not can_emit then None
  else begin
    let swapped = match pred with Folt | Fole -> true | _ -> false in
    let a = if swapped then rhs else lhs and b = if swapped then lhs else rhs in
    (if ty = F 16 then begin
       promote_half_pair st fn (Option.get dst) a b;
       insn2 st "ucomiss" (xmmo 0) (xmmo 1)
     end
     else begin
       let sfx = fmov_sfx ty in
       float_to_xmm st fn 4 ty a;
       float_to_xmm st fn 5 ty b;
       insn2 st ("ucomi" ^ sfx) (xmmo 5) (xmmo 4)
     end);
    if single_cc then
      Some (match pred with Fogt | Folt -> "a" | _ -> "ae")
    else None
  end

let emit_checked_arith_flags st fn dst op lhs rhs =
  let out = dst_reg fn dst in
  int_to_reg st fn out (I 64) lhs;
  let rhs_op =
    match rhs with
    | Int_imm text -> (
        match alu_immediate st (I 64) text with
        | Some imm -> imm
        | None ->
            int_to_reg st fn R10 (I 64) rhs;
            gro R10 64)
    | _ ->
        int_to_reg st fn R10 (I 64) rhs;
        gro R10 64
  in
  let mnemonic = match op with Add -> "addq" | Sub -> "subq" | Mul -> "imulq" | _ -> assert false in
  insn2 st mnemonic rhs_op (gro out 64);
  commit_from st fn dst out

let emit_instr st fn instr =
  match instr with
  | Alloca _ -> ()
  | Load { dst; ty; src = Temp alloca; align = _ }
    when cached_falloca fn alloca.tid <> None ->
      let source = Option.get (cached_falloca fn alloca.tid) in
      let out = xmm_dst fn dst source in
      if source <> out then
        insn2_suffix st "mov" (fmov_sfx ty) (xmmo source) (xmmo out);
      commit_xmm st fn dst ty out
  | Store { ty; src; dst = Temp alloca; align = _ }
    when cached_falloca fn alloca.tid <> None ->
      float_to_xmm st fn (Option.get (cached_falloca fn alloca.tid)) ty src
  | Load { dst; ty = _; src = Temp alloca; align = _ }
    when cached_alloca fn alloca.tid <> None ->
      let source = Option.get (cached_alloca fn alloca.tid) in
      let out = dst_reg fn dst in
      if source <> out then insn2 st "movq" (gro source 64) (gro out 64);
      commit_from st fn dst out
  | Store { src = Temp param; dst = Temp alloca; _ }
    when cached_alloca fn alloca.tid <> None
         && fn.param_cache.(param.tid) = cached_alloca fn alloca.tid ->
      ()
  | Store { ty; src; dst = Temp alloca; align = _ }
    when cached_alloca fn alloca.tid <> None ->
      int_to_reg st fn (Option.get (cached_alloca fn alloca.tid)) ty src
  | Load { dst; ty; src; align = _ } when is_wide ty ->
      addr_to_reg st fn R10 src;
      insn2 st "leaq" (mem_slot fn (slot_of fn (temp_id fn dst))) (gro R9 64);
      wide_copy st (mem_bytes ty)
  | Store { ty; src; dst; align = _ } when is_wide ty ->
      wide_operand st fn R10 (mem_bytes ty) src;
      addr_to_reg st fn R9 dst;
      wide_copy st (mem_bytes ty)
  | Load { dst; ty; src; align = _ } ->
      let at =
        match direct_mem fn src with
        | Some at -> at
        | None ->
            addr_to_reg st fn R10 src;
            mem_via_reg R10
      in
      if is_xmm_ty ty then begin
        let out = xmm_dst fn dst 4 in
        insn2_suffix st "mov" (fmov_sfx ty) (at 0) (xmmo out);
        commit_xmm st fn dst ty out
      end
      else begin
        let r = dst_reg fn dst in
        load_bytes_to st r at (mem_bytes ty);
        commit_from st fn dst r
      end
  | Store { ty; src; dst; align = _ } ->
      let at =
        match direct_mem fn dst with
        | Some at -> at
        | None ->
            addr_to_reg st fn R10 dst;
            mem_via_reg R10
      in
      if is_xmm_ty ty then begin
        float_to_xmm st fn 4 ty src;
        insn2_suffix st "mov" (fmov_sfx ty) (xmmo 4) (at 0)
      end
      else begin
        let n = mem_bytes ty in
        let r =
          match src with
          | Temp t when n = 8 && cached fn t.tid <> None -> Option.get (cached fn t.tid)
          | _ ->
              int_to_reg st fn Rax ty src;
              Rax
        in
        store_bytes_from st r at n
      end
  | Bin { dst; op; ty; lhs; rhs } when is_wide ty ->
      let n = mem_bytes ty in
      wide_operand st fn R10 n lhs;
      wide_operand st fn R11 n rhs;
      insn2 st "leaq" (mem_slot fn (slot_of fn (temp_id fn dst))) (gro R9 64);
      (match op with
       | Add -> wide_addsub st ~sub:false n
       | Sub -> wide_addsub st ~sub:true n
       | Mul -> wide_mul st fn n
       | And | Or | Xor -> wide_bitwise st op n
       | Sdiv | Srem | Udiv | Urem ->
           let signed = op = Sdiv || op = Srem in
           let want_quotient = op = Sdiv || op = Udiv in
           insn2 st "leaq" (mem_slot fn (slot_of fn (temp_id fn dst))) (gro Rax 64);
           if windows_abi then begin
             if want_quotient then begin
               line st "movq\t%%rax, %%rcx";
               line st "xorl\t%%edx, %%edx"
             end
             else begin
               line st "xorl\t%%ecx, %%ecx";
               line st "movq\t%%rax, %%rdx"
             end;
             line st "movq\t%%r10, %%r8";
             line st "movq\t%%r11, %%r9";
             line st "movq\t$%d, 32(%%rsp)" n;
             line st "movq\t$%d, 40(%%rsp)" (if signed then 1 else 0)
           end
           else begin
             if want_quotient then begin
               line st "movq\t%%rax, %%rdi";
               line st "xorl\t%%esi, %%esi"
             end
             else begin
               line st "xorl\t%%edi, %%edi";
               line st "movq\t%%rax, %%rsi"
             end;
             line st "movq\t%%r10, %%rdx";
             line st "movq\t%%r11, %%rcx";
             line st "movl\t$%d, %%r8d" n;
             line st "movl\t$%d, %%r9d" (if signed then 1 else 0)
           end;
           line st "callq\txi_wide_divrem"
       | Shl | Lshr | Ashr ->
           fail "shifting integers wider than 64 bits is not supported yet (i%d)"
             (exact_bits ty))
  | Conv { dst; op; from_ty; src; to_ty } when is_wide from_ty || is_wide to_ty -> (
      match op with
      | Sext | Zext when is_wide to_ty ->
          let nd = mem_bytes to_ty in
          let ns = if is_wide from_ty then mem_bytes from_ty else 8 in
          insn2 st "leaq" (mem_slot fn (slot_of fn (temp_id fn dst))) (gro R9 64);
          if is_wide from_ty then begin
            wide_addr st fn R10 src;
            wide_copy st ns
          end
          else begin
            int_to_reg_ext st fn Rax from_ty src ~signed:(op = Sext);
            line st "movq\t%%rax, (%%r9)"
          end;
          if op = Zext then line st "xorl\t%%eax, %%eax"
          else begin
            line st "movsbl\t%d(%%r9), %%eax" (ns - 1);
            line st "sarl\t$31, %%eax"
          end;
          wide_fill st ~from_:ns ~to_:nd
      | Trunc when is_wide from_ty && is_wide to_ty ->
          wide_addr st fn R10 src;
          insn2 st "leaq" (mem_slot fn (slot_of fn (temp_id fn dst))) (gro R9 64);
          wide_copy st (mem_bytes to_ty)
      | Trunc when is_wide from_ty ->
          wide_addr st fn R10 src;
          line st "movq\t(%%r10), %%rax";
          commit_int st fn dst
      | _ ->
          fail "unsupported conversion %s -> %s at more than 64 bits"
            (ty_name from_ty) (ty_name to_ty))
  | Bin { dst; op; ty; lhs; rhs } ->
      let w = work_bits ty in
      let out = dst_reg fn dst in
      let a = gro out w and b = gro R10 w in
      let sfx = sfx_of_bits w in
      (match op with
       | Add | Sub | And | Or | Xor | Mul ->
           let mnemonic =
             match op with
             | Add -> "add" | Sub -> "sub" | And -> "and" | Or -> "or"
             | Mul -> "imul" | _ -> "xor"
           in
           int_to_reg st fn out ty lhs;
           let rhs_op =
             match rhs with
             | Int_imm text -> (
                 match alu_immediate st ty text with
                 | Some imm -> imm
                 | None ->
                     int_to_reg st fn R10 ty rhs;
                     b)
             | Temp t when w = 64 && exact_bits ty = 64 && cached fn t.tid <> None ->
                 gro (Option.get (cached fn t.tid)) 64
             | _ ->
                 int_to_reg st fn R10 ty rhs;
                 b
           in
           if op = Xor
              && (match rhs with Int_imm text -> is_all_ones st ty text | _ -> false)
           then insn1 st ("not" ^ sfx) a
           else insn2_suffix st mnemonic sfx rhs_op a;
           commit_from st fn dst out
       | Sdiv | Srem | Udiv | Urem ->
           (* Division is the one place a narrow type cannot be faked with a
              wider operation on dirty high bits: the quotient depends on the
              operands' true values. Both are extended to a clean 64 bits from the
              type's real width, then divided there whatever the type -- correct
              for every legal width, and it avoids needing a 24- or 40-bit
              division the machine does not have. *)
           let signed = op = Sdiv || op = Srem in
           if signed then begin
             int_to_reg_sx st fn Rax ty lhs;
             int_to_reg_sx st fn R10 ty rhs;
             insn0 st "cqto";
             insn1 st "idivq" (gro R10 64)
           end
           else begin
             int_to_reg_zx st fn Rax ty lhs;
             int_to_reg_zx st fn R10 ty rhs;
             insn2 st "xorl" (gro Rdx 32) (gro Rdx 32);
             insn1 st "divq" (gro R10 64)
           end;
           if op = Srem || op = Urem then insn2 st "movq" (gro Rdx 64) (gro Rax 64);
           commit_int st fn dst
       | Shl | Lshr | Ashr ->
           (match op with
            | Lshr -> int_to_reg_zx st fn out ty lhs
            | Ashr -> int_to_reg_sx st fn out ty lhs
            | _ -> int_to_reg st fn out ty lhs);
           let count =
             match rhs with
             | Int_imm text ->
                 let v = Int64.logand (imm_int64 st text) 0xFFL in
                 { text = Printf.sprintf "$%Ld" v
                 ; op = Asm_x64_obj.imm_operand v
                 }
             | _ ->
                 int_to_reg st fn Rcx ty rhs;
                 gro Rcx 8
           in
           let mnemonic = match op with Shl -> "shl" | Lshr -> "shr" | _ -> "sar" in
           insn2_suffix st mnemonic sfx count a;
           commit_from st fn dst out)
  | Checked_arith { dst; overflow; op; lhs; rhs } ->
      emit_checked_arith_flags st fn dst op lhs rhs;
      insn1 st "seto" (gro R10 8);
      let at =
        match direct_mem fn overflow with
        | Some at -> at
        | None ->
            addr_to_reg st fn R9 overflow;
            mem_via_reg R9
      in
      store_bytes_from st R10 at 1
  | Fbin { dst; op; ty; lhs; rhs } ->
      let mnemonic =
        match op with Fadd -> "add" | Fsub -> "sub" | Fmul -> "mul" | Fdiv -> "div"
      in
      if ty = F 16 then begin
        promote_half_pair st fn dst lhs rhs;
        insn2 st (mnemonic ^ "ss") (xmmo 0) (xmmo 1);
        float_xmm_to_half st 1;
        commit_int st fn dst
      end
      else begin
        let sfx = fmov_sfx ty in
        let out = xmm_dst fn dst 4 in
        float_to_xmm st fn out ty lhs;
        float_to_xmm st fn 5 ty rhs;
        insn2_suffix st mnemonic sfx (xmmo 5) (xmmo out);
        commit_xmm st fn dst ty out
      end
  | Icmp { dst; pred; ty; lhs; rhs } ->
      let cc = emit_icmp_flags st fn pred ty lhs rhs in
      insn1 st ("set" ^ cc) (gro Rax 8);
      insn2 st "movzbl" (gro Rax 8) (gro Rax 32);
      commit_int st fn dst
  | Fcmp { dst; pred; ty; lhs; rhs } -> (
      match emit_fcmp_flags st fn ~dst:(Some dst) pred ty lhs rhs with
      | Some cc ->
          insn1 st ("set" ^ cc) (gro Rax 8);
          insn2 st "movzbl" (gro Rax 8) (gro Rax 32);
          commit_int st fn dst
      | None ->
          (* `ucomis*` sets ZF=PF=CF=1 when either operand is NaN, and ZF=1 with
             PF clear for equality. Reading the four equality predicates off that:

               oeq  ordered   and equal      ZF && !PF
               one  ordered   and not equal  !ZF          (NaN already clears it)
               ueq  unordered or  equal      ZF           (NaN already sets it)
               une  unordered or  not equal  !ZF || PF

             Only oeq and une need the parity flag folded in, which is why those
             two cannot be a single setcc -- nor a single jcc, hence they are not
             fusable into a branch. *)
          (match pred with
           | Foeq ->
               insn1 st "sete" (gro Rax 8);
               insn1 st "setnp" (gro R10 8);
               insn2 st "andb" (gro R10 8) (gro Rax 8)
           | Fone -> insn1 st "setne" (gro Rax 8)
           | Fueq -> insn1 st "sete" (gro Rax 8)
           | Fune ->
               insn1 st "setne" (gro Rax 8);
               insn1 st "setp" (gro R10 8);
               insn2 st "orb" (gro R10 8) (gro Rax 8)
           | _ -> assert false);
          insn2 st "movzbl" (gro Rax 8) (gro Rax 32);
          commit_int st fn dst)
  | Conv { dst; op; from_ty; src; to_ty } -> (
      match op with
      | Trunc | Bitcast | Ptrtoint | Inttoptr ->
          if is_xmm_ty from_ty || is_xmm_ty to_ty then
            fail "%s: bitcast between integer and float is not supported" fn.fname;
          let out = dst_reg fn dst in
          int_to_reg st fn out from_ty src;
          commit_from st fn dst out
      | Zext ->
          let out = dst_reg fn dst in
          int_to_reg_zx st fn out from_ty src;
          commit_from st fn dst out
      | Sext ->
          let out = dst_reg fn dst in
          int_to_reg_sx st fn out from_ty src;
          commit_from st fn dst out
      | Fptrunc -> (
          match (from_ty, to_ty) with
          | F 64, F 32 ->
              float_to_xmm st fn 4 from_ty src;
              insn2 st "cvtsd2ss" (xmmo 4) (xmmo 4);
              commit_xmm st fn dst to_ty 4
          | F 64, F 16 ->
              emit_float_libcall st fn "__truncdfhf2" ~arg_ty:from_ty ~arg:src ~ret_ty:to_ty ~dst
          | F 32, F 16 ->
              emit_float_libcall st fn "__truncsfhf2" ~arg_ty:from_ty ~arg:src ~ret_ty:to_ty ~dst
          | _ -> fail "unsupported fptrunc %s -> %s" (ty_name from_ty) (ty_name to_ty))
      | Fpext -> (
          match (from_ty, to_ty) with
          | F 32, F 64 ->
              float_to_xmm st fn 4 from_ty src;
              insn2 st "cvtss2sd" (xmmo 4) (xmmo 4);
              commit_xmm st fn dst to_ty 4
          | F 16, F 32 ->
              emit_float_libcall st fn "__extendhfsf2" ~arg_ty:from_ty ~arg:src ~ret_ty:to_ty ~dst
          | F 16, F 64 ->
              half_to_float_xmm st fn 0 src;
              insn2 st "cvtss2sd" (xmmo 0) (xmmo 0);
              commit_xmm st fn dst to_ty 0
          | _ -> fail "unsupported fpext %s -> %s" (ty_name from_ty) (ty_name to_ty))
      | Sitofp ->
          if not (is_xmm_ty to_ty) then fail "unsupported sitofp to %s" (ty_name to_ty);
          int_to_reg_sx st fn Rax from_ty src;
          insn2 st ("cvtsi2" ^ fmov_sfx to_ty ^ "q") (gro Rax 64) (xmmo 4);
          commit_xmm st fn dst to_ty 4
      | Uitofp ->
          if not (is_xmm_ty to_ty) then fail "unsupported uitofp to %s" (ty_name to_ty);
          if bits_of_ty from_ty < 64 then begin
            int_to_reg_zx st fn Rax from_ty src;
            insn2 st ("cvtsi2" ^ fmov_sfx to_ty ^ "q") (gro Rax 64) (xmmo 4);
            commit_xmm st fn dst to_ty 4
          end
          else begin
            let done_label = Printf.sprintf ".Lu2f%d.%s" (Hashtbl.hash dst) fn.fname in
            let sfx = fmov_sfx to_ty in
            int_to_reg st fn Rax from_ty src;
            insn2 st "testq" (gro Rax 64) (gro Rax 64);
            insn1 st "js" (name_o (done_label ^ ".neg"));
            insn2 st ("cvtsi2" ^ sfx ^ "q") (gro Rax 64) (xmmo 4);
            insn1 st "jmp" (name_o (done_label ^ ".end"));
            deflabel st (done_label ^ ".neg");
            insn2 st "movq" (gro Rax 64) (gro R10 64);
            insn2 st "shrq" o_imm1 (gro R10 64);
            insn2 st "andl" o_imm1 (gro Rax 32);
            insn2 st "orq" (gro Rax 64) (gro R10 64);
            insn2 st ("cvtsi2" ^ sfx ^ "q") (gro R10 64) (xmmo 4);
            insn2_suffix st "add" sfx (xmmo 4) (xmmo 4);
            deflabel st (done_label ^ ".end");
            commit_xmm st fn dst to_ty 4
          end
      | Fptosi ->
          if not (is_xmm_ty from_ty) then fail "unsupported fptosi from %s" (ty_name from_ty);
          float_to_xmm st fn 4 from_ty src;
          insn2 st ("cvtt" ^ fmov_sfx from_ty ^ "2siq") (xmmo 4) (gro Rax 64);
          commit_int st fn dst
      | Fptoui ->
          if not (is_xmm_ty from_ty) then fail "unsupported fptoui from %s" (ty_name from_ty);
          if bits_of_ty to_ty >= 64 then
            fail "%s: fptoui to i64 is not supported yet" fn.fname;
          float_to_xmm st fn 4 from_ty src;
          insn2 st ("cvtt" ^ fmov_sfx from_ty ^ "2siq") (xmmo 4) (gro Rax 64);
          commit_int st fn dst)
  | Gep_global { dst; sym } ->
      let out = dst_reg fn dst in
      insn2 st "leaq" (rip_o sym) (gro out 64);
      commit_from st fn dst out
  | Gep_byte { dst; base; index } ->
      let out = dst_reg fn dst in
      let index_ty, index_value = index in
      (match index_value with
       | Int_imm text when fits_int32 (imm_int64 st text) ->
           (* A literal displacement folds into the addressing mode, but only
              while it fits the 32-bit signed disp field. *)
           addr_to_reg st fn out base;
           let v = imm_int64 st text in
           if v <> 0L then insn2 st "leaq" (mem_via_reg out (Int64.to_int v)) (gro out 64)
       | _ ->
           addr_to_reg st fn out base;
           int_to_reg_sx st fn R10 index_ty index_value;
           insn2 st "addq" (gro R10 64) (gro out 64));
      commit_from st fn dst out
  | Call { dst; ret; ret_ext; callee; args; arg_exts } ->
      emit_call st fn dst ret ret_ext callee args arg_exts

let emit_epilogue st fn =
  List.iter
    (fun (reg, off) ->
      insn2 st "movaps" (mem_slot fn off) (xmmo reg))
    fn.saved_xmms;
  List.iter
    (fun (r, off) -> insn2 st "movq" (mem_slot fn off) (gro r 64))
    fn.saved;
  insn2 st "movq" o_rbp o_rsp;
  insn1 st "popq" o_rbp;
  insn0 st "retq"

let emit_cond_branch st fn ~cc ~then_label ~else_label ~next =
  if next = Some else_label then
    insn1 st ("j" ^ cc) (name_o (block_label fn then_label))
  else if next = Some then_label then
    insn1 st ("j" ^ invert_cc cc) (name_o (block_label fn else_label))
  else begin
    insn1 st ("j" ^ cc) (name_o (block_label fn then_label));
    insn1 st "jmp" (name_o (block_label fn else_label))
  end

let emit_term st fn ~next term =
  match term with
  | Ret None -> emit_epilogue st fn
  | Ret (Some (ty, value)) when is_wide ty ->
      let dest =
        match fn.sret with
        | Some off -> off
        | None -> fail "%s: wide return without a destination slot" fn.fname
      in
      wide_operand st fn R10 (mem_bytes ty) value;
      insn2 st "movq" (mem_slot fn dest) (gro R9 64);
      wide_copy st (mem_bytes ty);
      emit_epilogue st fn
  | Ret (Some (ty, value)) ->
      if ty = I 1 then begin
        match value with
        | Bool_imm b ->
            insn2 st "movl" (imm_int_o (if b then 1 else 0)) (gro Rax 32)
        | _ -> int_to_reg_zx st fn Rax ty value
      end
      else if is_xmm_ty ty then float_to_xmm st fn 0 ty value
      else if ty = F 16 then begin
        int_to_reg_zx st fn Rax (I 16) value;
        insn2 st "movd" (gro Rax 32) (xmmo 0)
      end
      else if fn.ret_ext = Sign_ext then int_to_reg_sx st fn Rax ty value
      else int_to_reg_zx st fn Rax ty value;
      emit_epilogue st fn
  | Br label ->
      if next <> Some label then insn1 st "jmp" (name_o (block_label fn label))
  | Br_cond { cond; then_label; else_label } -> (
      let goto label =
        if next <> Some label then insn1 st "jmp" (name_o (block_label fn label))
      in
      match cond with
      | Bool_imm true -> goto then_label
      | Bool_imm false -> goto else_label
      | _ ->
          int_to_reg st fn Rax (I 1) cond;
          insn2 st "testb" o_imm1 (gro Rax 8);
          emit_cond_branch st fn ~cc:"ne" ~then_label ~else_label ~next)
  | Switch { ty; selector; default_label; cases } ->
      let bits = bits_of_ty ty in
      int_to_reg st fn Rax ty selector;
      let default = block_label fn default_label in
      let parsed =
        List.map
          (fun (key, target) -> imm_int64 st key, key, block_label fn target)
          cases
        |> List.sort (fun (left, _, _) (right, _, _) -> Int64.compare left right)
      in
      let compare_key (_, key, _) =
        let value = imm_int64 st key in
        if bits = 64 && not (fits_int32 value) then begin
          insn2 st "movabsq" (imm_sx_hex st bits key) (gro R10 64);
          insn2 st "cmpq" (gro R10 64) (gro Rax 64)
        end
        else
          insn2_suffix st "cmp" (sfx_of_bits bits) (imm_sx_hex st bits key)
            (gro Rax bits)
      in
      let emit_linear entries =
        List.iter
          (fun ((_, _, target) as entry) ->
            compare_key entry;
            insn1 st "je" (name_o target))
          entries;
        if next <> Some default_label then insn1 st "jmp" (name_o default)
      in
      let count = List.length parsed in
      let dense_span =
        match parsed with
        | [] -> None
        | (minimum, _, _) :: _ ->
            let maximum, _, _ = List.hd (List.rev parsed) in
            let distance = Int64.sub maximum minimum in
            if Int64.compare distance 0L >= 0
               && Int64.compare distance 4095L <= 0
               && Int64.compare (Int64.add distance 1L)
                    (Int64.of_int (2 * count))
                  <= 0
            then Some (minimum, Int64.to_int (Int64.add distance 1L))
            else None
      in
      if count <= 3 then emit_linear parsed
      else
        (match dense_span with
        | Some (minimum, span) ->
            let table =
              Printf.sprintf ".L%s.switch.table.%d" fn.fname st.switch_next
            in
            st.switch_next <- st.switch_next + 1;
            let targets = Array.make span default in
            List.iter
              (fun (value, _, target) ->
                targets.(Int64.to_int (Int64.sub value minimum)) <- target)
              parsed;
            st.switch_pool_order <-
              (table, Array.to_list targets) :: st.switch_pool_order;
            if minimum <> 0L then
              if fits_int32 minimum then
                insn2 st "subq"
                  { text = Printf.sprintf "$%Ld" minimum
                  ; op = Asm_x64_obj.imm_operand minimum
                  }
                  (gro Rax 64)
              else begin
                insn2 st "movabsq"
                  { text = Printf.sprintf "$0x%Lx" minimum
                  ; op = Asm_x64_obj.imm_operand minimum
                  }
                  (gro R10 64);
                insn2 st "subq" (gro R10 64) (gro Rax 64)
              end;
            insn2 st "cmpq" (imm_int_o (span - 1)) (gro Rax 64);
            insn1 st "ja" (name_o default);
            insn2 st "leaq" (rip_o table) (gro R11 64);
            insn2 st "movslq" (mem_index_o R11 Rax 4) (gro Rax 64);
            insn2 st "addq" (gro R11 64) (gro Rax 64);
            insn1 st "jmp" o_star_rax
        | None ->
            let entries = Array.of_list parsed in
            let node = ref 0 in
            let new_node_label () =
              let label =
                Printf.sprintf ".L%s.switch.node.%d.%d" fn.fname st.switch_next !node
              in
              incr node;
              label
            in
            st.switch_next <- st.switch_next + 1;
            let rec emit_tree low high =
              let middle = low + ((high - low) / 2) in
              let ((_, _, target) as entry) = entries.(middle) in
              compare_key entry;
              insn1 st "je" (name_o target);
              if low = high then insn1 st "jmp" (name_o default)
              else if middle = low then emit_tree (middle + 1) high
              else if middle = high then emit_tree low (middle - 1)
              else begin
                let left = new_node_label () in
                insn1 st "jl" (name_o left);
                emit_tree (middle + 1) high;
                deflabel st left;
                emit_tree low (middle - 1)
              end
            in
            emit_tree 0 (count - 1))
  | Unreachable -> insn0 st "ud2"

let uses_in_block b name =
  let count = ref 0 in
  let tally v = match v with Temp t when t.tname = name -> incr count | _ -> () in
  List.iter (iter_instr_uses tally) b.instrs;
  iter_term_uses tally b.term;
  !count

let emit_blocks st fn (blocks : block list) =
  let arr = Array.of_list blocks in
  Array.iteri
    (fun i b ->
      let next = if i + 1 < Array.length arr then Some arr.(i + 1).label else None in
      deflabel st (block_label fn b.label);
      let fused =
        match (List.rev b.instrs, b.term) with
        | Load { dst = bool_dst; ty = I 1; src = overflow; _ }
          :: Checked_arith { dst; overflow = checked_overflow; op; lhs; rhs }
          :: rest_rev,
          Br_cond { cond = Temp c; then_label; else_label }
          when c.tname = bool_dst
               && checked_overflow = overflow
               && uses_in_block b bool_dst = 1 ->
            List.iter (emit_instr st fn) (List.rev rest_rev);
            emit_checked_arith_flags st fn dst op lhs rhs;
            emit_cond_branch st fn ~cc:"o" ~then_label ~else_label ~next;
            true
        | last :: rest_rev, Br_cond { cond = Temp c; then_label; else_label } -> (
            let fuse dst = c.tname = dst && uses_in_block b dst = 1 in
            let before () = List.iter (emit_instr st fn) (List.rev rest_rev) in
            match last with
            | Call { dst = Some dst; ret = I 1; ret_ext; callee; args; arg_exts }
              when fuse dst ->
                before ();
                emit_call st fn None (I 1) ret_ext callee args arg_exts;
                insn2 st "testb" o_imm1 (gro Rax 8);
                emit_cond_branch st fn ~cc:"ne" ~then_label ~else_label ~next;
                true
            | Icmp { dst; pred; ty; lhs; rhs } when fuse dst ->
                before ();
                let cc = emit_icmp_flags st fn pred ty lhs rhs in
                emit_cond_branch st fn ~cc ~then_label ~else_label ~next;
                true
            | Fcmp { dst; pred; ty; lhs; rhs } when fuse dst && fcmp_fusable pred ty ->
                before ();
                (match emit_fcmp_flags st fn ~dst:None pred ty lhs rhs with
                 | Some cc -> emit_cond_branch st fn ~cc ~then_label ~else_label ~next
                 | None -> assert false);
                true
            | _ -> false)
        | _ -> false
      in
      if not fused then begin
        List.iter (emit_instr st fn) b.instrs;
        emit_term st fn ~next b.term
      end)
    arr

let emit_func st (f : func) fn =
  dir_text st;
  dir_globl st f.name;
  if windows_abi then
    line st ".def\t%s;\t.scl\t2;\t.type\t32;\t.endef" f.name
  else
    dir_type_function st f.name;
  dir_p2align16 st;
  if windows_abi then line st ".seh_proc\t%s" f.name
  else dir_cfi_startproc st;
  deflabel st f.name;
  insn1 st "pushq" o_rbp;
  if windows_abi then line st ".seh_pushreg\t%%rbp"
  else dir_cfi_prologue_rbp st;
  insn2 st "movq" o_rsp o_rbp;
  if windows_abi then line st ".seh_setframe\t%%rbp, 0"
  else dir_cfi_def_cfa_register_rbp st;
  if fn.frame > 0 then begin
    if windows_abi && fn.frame >= 4096 then begin
      insn2 st "movl" (imm_int_o fn.frame) (gro Rax 32);
      insn1 st "callq" (name_o "___chkstk_ms");
      insn2 st "subq" (gro Rax 64) o_rsp
    end
    else insn2 st "subq" (imm_int_o fn.frame) o_rsp;
    if windows_abi then line st ".seh_stackalloc\t%d" fn.frame
  end;
  List.iter
    (fun (r, off) ->
      insn2 st "movq" (gro r 64) (mem_slot fn off);
      if windows_abi then
        line st ".seh_savereg\t%s, %d" (gr r 64) (fn.frame - off)
      else
        dir_cfi_offset st (gr r 64) (greg_code r) (-(off + 16)))
    fn.saved;
  List.iter
    (fun (reg, off) ->
      insn2 st "movaps" (xmmo reg) (mem_slot fn off);
      if windows_abi then
        line st ".seh_savexmm\t%s, %d" (xmm reg) (fn.frame - off))
    fn.saved_xmms;
  if windows_abi then line st ".seh_endprologue";
  let all_tys = List.map fst f.params in
  let all_homes = classify_args (if fn.sret <> None then Ptr :: all_tys else all_tys) in
  let param_homes =
    match (fn.sret, all_homes) with
    | Some off, Gpr r :: rest ->
        insn2 st "movq" (gro r 64) (mem_slot fn off);
        rest
    | Some _, _ -> fail "%s: hidden result pointer has no register home" fn.fname
    | None, homes -> homes
  in
  List.iter2
    (fun (ty, name) home ->
      match fn.param_cache.(temp_id fn name) with
      | Some r ->
          (match home with
           | Gpr incoming -> insn2 st "movq" (gro incoming 64) (gro r 64)
           | Stack _ ->
               insn2 st "movq" (mem_rbp_o (incoming_stack_offset home)) (gro r 64)
           | Xmm _ -> fail "%s: cached integer parameter has an xmm home" fn.fname)
      | None ->
          let off = mem_slot fn (slot_of fn (temp_id fn name)) in
          match home with
          | Gpr incoming -> insn2 st "movq" (gro incoming 64) off
          | Xmm n ->
            if is_xmm_ty ty then insn2_suffix st "mov" (fmov_sfx ty) (xmmo n) off
            else if ty = F 16 then begin
              insn2 st "movd" (xmmo n) (gro Rax 32);
              insn2 st "movq" (gro Rax 64) off
            end
            else fail "%s: integer parameter has an xmm home" fn.fname
          | Stack _ ->
            insn2 st "movq" (mem_rbp_o (incoming_stack_offset home)) (gro Rax 64);
            insn2 st "movq" (gro Rax 64) off
    )
    f.params param_homes;
  List.iter
    (fun (ty, name) ->
      if is_wide ty then begin
        let off = mem_slot fn (slot_of fn (temp_id fn name)) in
        insn2 st "movq" off (gro R10 64);
        insn2 st "leaq" off (gro R9 64);
        wide_copy st (mem_bytes ty)
      end)
    f.params;
  emit_blocks st fn f.blocks;
  if windows_abi then line st ".seh_endproc"
  else begin
    dir_cfi_endproc st;
    dir_size st f.name
  end;
  blank st

let escape_ascii bytes =
  let out = Buffer.create (String.length bytes + 16) in
  String.iter
    (fun ch ->
      let code = Char.code ch in
      if ch = '"' then Buffer.add_string out "\\\""
      else if ch = '\\' then Buffer.add_string out "\\\\"
      else if code >= 32 && code <= 126 then Buffer.add_char out ch
      else Buffer.add_string out (Printf.sprintf "\\%03o" code))
    bytes;
  Buffer.contents out

let emit_globals st (globals : global list) =
  if globals <> [] then begin
    if windows_abi then raw st "\t.section\t.rdata,\"dr\""
    else raw st "\t.section\t.rodata";
    List.iter
      (fun g ->
        line st ".p2align\t%d" (if g.g_align >= 8 then 3 else if g.g_align >= 4 then 2 else 0);
        raw st "%s:" g.g_name;
        line st ".ascii\t\"%s\"" (escape_ascii g.g_bytes))
      globals;
    blank st
  end

let emit_wide_pool st =
  match st.wide_pool_order with
  | [] -> ()
  | entries ->
      if windows_abi then raw st "\t.section\t.rdata,\"dr\""
      else raw st "\t.section\t.rodata";
      List.rev entries
      |> List.iter (fun (label, bytes) ->
             line st ".p2align\t3";
             raw st "%s:" label;
             line st ".ascii\t\"%s\"" (escape_ascii bytes))

let emit_switch_pool st =
  match st.switch_pool_order with
  | [] -> ()
  | tables ->
      if windows_abi then raw st "\t.section\t.rdata,\"dr\""
      else raw st "\t.section\t.rodata";
      List.rev tables
      |> List.iter (fun (label, targets) ->
             line st ".p2align\t2";
             deflabel st label;
             List.iter
               (fun target ->
                 match st.output with
                 | Text_output _ -> line st ".long\t%s - %s" target label
                 | Machine_output builder ->
                     Asm_x64_obj.emit_pc32_delta builder ~target ~base:label)
               targets)

let emit_pool st =
  match st.pool_order with
  | [] -> ()
  | entries ->
      if windows_abi then raw st "\t.section\t.rdata,\"dr\""
      else raw st "\t.section\t.rodata";
      List.rev entries
      |> List.iter (fun (label, width, bits) ->
             line st ".p2align\t%d" (if width = 64 then 3 else 2);
             raw st "%s:" label;
             (* Lir guarantees the pattern is already in the value's own width
                (see Lir.Float_bits), so it goes out as a raw integer and no
                float parsing happens at assembly time either. *)
             if width = 64 then line st ".quad\t0x%s" bits else line st ".long\t0x%s" bits)

let parallel_threshold = 32

let render_domain_cap = 8

let render_functions_with funcs render_one =
  let count = Array.length funcs in
  let domains = min render_domain_cap (Domain.recommended_domain_count ()) in
  if count < parallel_threshold || domains < 2 then Array.init count render_one
  else begin
    let rendered = Array.make count None in
    let next = Atomic.make 0 in
    let first_error = Atomic.make None in
    let worker () =
      let rec loop () =
        if Atomic.get first_error = None then begin
          let i = Atomic.fetch_and_add next 1 in
          if i < count then begin
            (try rendered.(i) <- Some (render_one i)
             with exn ->
               ignore (Atomic.compare_and_set first_error None (Some exn)));
            loop ()
          end
        end
      in
      loop ()
    in
    let workers = Array.init (domains - 1) (fun _ -> Domain.spawn worker) in
    worker ();
    Array.iter (fun domain -> Domain.join domain) workers;
    match Atomic.get first_error with
    | Some exn -> raise exn
    | None -> Array.map Option.get rendered
  end

let render_text_functions funcs =
  render_functions_with funcs (fun index ->
    let st, buffer = new_text_state ~scope:index ~capacity:4096 in
    let fn = plan_frame funcs.(index) in
    emit_func st funcs.(index) fn;
    emit_switch_pool st;
    emit_pool st;
    emit_wide_pool st;
    Buffer.contents buffer)

let render_text_one_profiled index func =
  let started = Clock.now () in
  let st, buffer = new_text_state ~scope:index ~capacity:4096 in
  let planning_started = Clock.now () in
  let fn = plan_frame func in
  let planning = Clock.now () -. planning_started in
  emit_func st func fn;
  emit_switch_pool st;
  emit_pool st;
  emit_wide_pool st;
  (Buffer.contents buffer, planning, Clock.now () -. started)

let render_machine_one index func =
  let st, builder = new_machine_state ~scope:index in
  let fn = plan_frame func in
  emit_func st func fn;
  emit_switch_pool st;
  emit_pool st;
  emit_wide_pool st;
  Asm_x64_obj.finish_builder builder

let render_machine_one_profiled index func =
  let started = Clock.now () in
  let st, builder = new_machine_state ~scope:index in
  let planning_started = Clock.now () in
  let fn = plan_frame func in
  let planning = Clock.now () -. planning_started in
  emit_func st func fn;
  emit_switch_pool st;
  emit_pool st;
  emit_wide_pool st;
  ( Asm_x64_obj.finish_builder builder
  , planning
  , Clock.now () -. started )

let render_machine_functions funcs =
  render_functions_with funcs (fun index -> render_machine_one index funcs.(index))

let profiled_results results =
  let planning = ref 0. in
  let total = ref 0. in
  let rendered =
    Array.map
      (fun (output, planning_time, total_time) ->
        planning := !planning +. planning_time;
        total := !total +. total_time;
        output)
      results
  in
  let planning_share =
    if !total <= 0. then 0. else min 1. (!planning /. !total)
  in
  (rendered, planning_share)

let generate_fragments (m : modul) : string array =
  let funcs = Array.of_list m.funcs in
  let rendered = render_text_functions funcs in
  let st, buffer = new_text_state ~scope:(-1) ~capacity:65536 in
  emit_globals st m.globals;
  if sysv_abi then raw st "\t.section\t.note.GNU-stack,\"\",@progbits";
  Array.append rendered [| Buffer.contents buffer |]

let generate_assembled (m : modul) : Asm_x64_obj.assembled =
  let funcs = Array.of_list m.funcs in
  let rendered = render_machine_functions funcs in
  let st, builder = new_machine_state ~scope:(-1) in
  emit_globals st m.globals;
  let globals = Asm_x64_obj.finish_builder builder in
  Array.append rendered [| globals |] |> Asm_x64_obj.merge_fragments

let generate_assembled_with_optimization_share
    (m : modul) : Asm_x64_obj.assembled * float =
  let funcs = Array.of_list m.funcs in
  let profiled =
    render_functions_with funcs
      (fun index -> render_machine_one_profiled index funcs.(index))
  in
  let rendered, planning_share = profiled_results profiled in
  let st, builder = new_machine_state ~scope:(-1) in
  emit_globals st m.globals;
  let globals = Asm_x64_obj.finish_builder builder in
  ( Array.append rendered [| globals |] |> Asm_x64_obj.merge_fragments
  , planning_share )

let stream_is_profitable ~functions =
  functions >= parallel_threshold && functions <= 4096

let stream_spin_limit = 200

type stream =
  { lock : Mutex.t
  ; cond : Condition.t
  ; stream_funcs : func array
  ; stream_rendered : Asm_x64_obj.assembled option array
  ; published : int Atomic.t
  ; claimed : int Atomic.t
  ; waiters : int Atomic.t
  ; closed : bool Atomic.t
  ; stream_error : exn option Atomic.t
  ; mutable overflow : func list
  ; mutable stream_workers : unit Domain.t array
  }

let stream_note_error t exn =
  ignore (Atomic.compare_and_set t.stream_error None (Some exn));
  Mutex.lock t.lock;
  Condition.broadcast t.cond;
  Mutex.unlock t.lock

let rec stream_claim t =
  let c = Atomic.get t.claimed in
  if c >= Atomic.get t.published then -1
  else if Atomic.compare_and_set t.claimed c (c + 1) then c
  else stream_claim t

let stream_worker t =
  let rec loop spins =
    let i = stream_claim t in
    if i >= 0 then begin
      (match render_machine_one i t.stream_funcs.(i) with
       | fragment -> t.stream_rendered.(i) <- Some fragment
       | exception exn -> stream_note_error t exn);
      if Atomic.get t.stream_error = None then loop 0
    end
    else if Atomic.get t.stream_error <> None || Atomic.get t.closed then ()
    else if spins < stream_spin_limit then begin
      Domain.cpu_relax ();
      loop (spins + 1)
    end
    else begin
      Mutex.lock t.lock;
      if Atomic.get t.claimed >= Atomic.get t.published
         && (not (Atomic.get t.closed))
         && Atomic.get t.stream_error = None
      then begin
        Atomic.incr t.waiters;
        Condition.wait t.cond t.lock;
        Atomic.decr t.waiters
      end;
      Mutex.unlock t.lock;
      loop 0
    end
  in
  loop 0

let stream_placeholder_func : func =
  { name = ""; ret = Void; ret_ext = No_ext; params = []; param_exts = []; blocks = [] }

let start_assembly_stream ~functions () =
  let capacity = (functions * 2) + 1024 in
  let t =
    { lock = Mutex.create ()
    ; cond = Condition.create ()
    ; stream_funcs = Array.make capacity stream_placeholder_func
    ; stream_rendered = Array.make capacity None
    ; published = Atomic.make 0
    ; claimed = Atomic.make 0
    ; waiters = Atomic.make 0
    ; closed = Atomic.make false
    ; stream_error = Atomic.make None
    ; overflow = []
    ; stream_workers = [||]
    }
  in
  let domains = min render_domain_cap (Domain.recommended_domain_count ()) in
  t.stream_workers <-
    Array.init (max 0 (domains - 1)) (fun _ -> Domain.spawn (fun () -> stream_worker t));
  t

let stream_func t func =
  let i = Atomic.get t.published in
  if i >= Array.length t.stream_funcs then t.overflow <- func :: t.overflow
  else begin
    t.stream_funcs.(i) <- func;
    Atomic.set t.published (i + 1);
    if Atomic.get t.waiters > 0 then begin
      Mutex.lock t.lock;
      Condition.signal t.cond;
      Mutex.unlock t.lock
    end
  end

let finish_assembly_stream t (globals : global list) : Asm_x64_obj.assembled =
  Atomic.set t.closed true;
  Mutex.lock t.lock;
  Condition.broadcast t.cond;
  Mutex.unlock t.lock;
  stream_worker t;
  Array.iter Domain.join t.stream_workers;
  (match Atomic.get t.stream_error with Some exn -> raise exn | None -> ());
  let count = Atomic.get t.published in
  let streamed = Array.init count (fun i -> Option.get t.stream_rendered.(i)) in
  let overflow =
    match t.overflow with
    | [] -> [||]
    | pending ->
        let funcs = Array.of_list (List.rev pending) in
        render_functions_with funcs (fun k -> render_machine_one (count + k) funcs.(k))
  in
  let st, builder = new_machine_state ~scope:(-1) in
  emit_globals st globals;
  let globals_fragment = Asm_x64_obj.finish_builder builder in
  Array.concat [ streamed; overflow; [| globals_fragment |] ]
  |> Asm_x64_obj.merge_fragments

let generate (m : modul) : string =
  let st = Buffer.create 65536 in
  Buffer.add_string st
    (if windows_abi then
       "# xi generated x86_64 assembly (windows, MS x64 ABI)\n\n"
     else
       "# xi generated x86_64 assembly (linux, System V ABI)\n\n");
  Array.iter (Buffer.add_string st) (generate_fragments m);
  Buffer.contents st

let generate_with_optimization_share (m : modul) : string * float =
  let funcs = Array.of_list m.funcs in
  let profiled =
    render_functions_with funcs
      (fun index -> render_text_one_profiled index funcs.(index))
  in
  let rendered, planning_share = profiled_results profiled in
  let globals, buffer = new_text_state ~scope:(-1) ~capacity:65536 in
  emit_globals globals m.globals;
  if sysv_abi then raw globals "\t.section\t.note.GNU-stack,\"\",@progbits";
  let output = Buffer.create 65536 in
  Buffer.add_string output
    (if windows_abi then
       "# xi generated x86_64 assembly (windows, MS x64 ABI)\n\n"
     else
       "# xi generated x86_64 assembly (linux, System V ABI)\n\n");
  Array.iter (Buffer.add_string output) rendered;
  Buffer.add_string output (Buffer.contents buffer);
  (Buffer.contents output, planning_share)
