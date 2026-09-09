(* Typed x86-64 machine sink and relocatable-object input.

   Asm_x64 owns instruction selection, ABI lowering, and register allocation.
   Its production byte backend sends operands/instructions directly into a
   [builder] here; no translation-unit assembly text is constructed. Hot
   instruction helpers provide typed operands directly, while remaining
   formatted forms are normalized one line at a time by a bounded reader. It is
   not an external or general-purpose assembler. The distinct
   --emit=asm backend bypasses this module and hands real text to Zig. *)

exception Unsupported of string

let fail fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

type section = Text | Rodata
type reloc_kind = Pc32 | Plt32

type relocation =
  { section : section
  ; offset : int
  ; symbol : string
  ; kind : reloc_kind
  ; addend : int
  }

type symbol =
  { name : string
  ; section : section option
  ; value : int
  ; size : int
  ; global : bool
  ; function_ : bool
  }

type unwind_operation =
  | Push_nonvolatile of { code_offset : int; register : int }
  | Set_frame of { code_offset : int; register : int; offset : int }
  | Allocate_stack of { code_offset : int; size : int }
  | Save_nonvolatile of { code_offset : int; register : int; offset : int }
  | Save_xmm of { code_offset : int; register : int; offset : int }

type unwind =
  { function_name : string
  ; start_offset : int
  ; end_offset : int
  ; prologue_size : int
  ; operations : unwind_operation list
  }

type cfi_operation =
  | Def_cfa_offset of { code_offset : int; offset : int }
  | Def_cfa_register of { code_offset : int; register : int }
  | Cfi_offset of { code_offset : int; register : int; offset : int }

type cfi =
  { function_name : string
  ; start_offset : int
  ; end_offset : int
  ; operations : cfi_operation list
  }

type assembled =
  { text : bytes
  ; rodata : bytes
  ; symbols : symbol list
  ; relocations : relocation list
  ; unwinds : unwind list
  ; cfis : cfi list
  }

module Code = struct
  type t = { mutable data : bytes; mutable len : int }

  let create capacity = { data = Bytes.create capacity; len = 0 }

  let ensure t extra =
    let needed = t.len + extra in
    if needed > Bytes.length t.data then begin
      let size = ref (max 16 (Bytes.length t.data * 2)) in
      while !size < needed do size := !size * 2 done;
      let data = Bytes.create !size in
      Bytes.blit t.data 0 data 0 t.len;
      t.data <- data
    end

  let u8 t n =
    ensure t 1;
    Bytes.set t.data t.len (Char.chr (n land 0xff));
    t.len <- t.len + 1

  let u16 t n =
    u8 t n;
    u8 t (n lsr 8)

  let i32 t n =
    let open Int64 in
    let v = of_int n in
    for shift = 0 to 3 do
      u8 t (to_int (logand (shift_right_logical v (shift * 8)) 0xffL))
    done

  let i32_64 t v =
    let open Int64 in
    for shift = 0 to 3 do
      u8 t (to_int (logand (shift_right_logical v (shift * 8)) 0xffL))
    done

  let i64 t v =
    let open Int64 in
    for shift = 0 to 7 do
      u8 t (to_int (logand (shift_right_logical v (shift * 8)) 0xffL))
    done

  let patch_i32 t off n =
    if off < 0 || off + 4 > t.len then invalid_arg "Code.patch_i32";
    let v = Int64.of_int n in
    for shift = 0 to 3 do
      Bytes.set t.data (off + shift)
        (Char.chr
           (Int64.to_int
              (Int64.logand (Int64.shift_right_logical v (shift * 8)) 0xffL)))
    done

  let add_string t s =
    let n = String.length s in
    ensure t n;
    Bytes.blit_string s 0 t.data t.len n;
    t.len <- t.len + n

  let add_subbytes t source offset length =
    ensure t length;
    Bytes.blit source offset t.data t.len length;
    t.len <- t.len + length

  let align t alignment fill =
    while t.len mod alignment <> 0 do u8 t fill done

  let contents t = Bytes.sub t.data 0 t.len
end

type reg_class = Gpr | Xmm
type reg = { code : int; width : int; class_ : reg_class; needs_rex : bool }

type mem =
  { disp : int
  ; base : reg option
  ; index : reg option
  ; scale : int
  ; symbol : string option
  ; rip : bool
  }

type operand =
  | Reg of reg
  | Imm of int64
  | Mem of mem
  | Name of string
  | Indirect of operand

let trim = String.trim

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let parse_int64 text =
  match Int64.of_string_opt text with
  | Some n -> n
  | None ->
      (match Int64.of_string_opt ("0u" ^ text) with
       | Some n -> n
       | None -> fail "invalid integer %S" text)

let gpr name =
  let mk code width ?(needs_rex = false) () =
    Reg { code; width; class_ = Gpr; needs_rex }
  in
  match name with
  | "rax" -> mk 0 64 () | "rcx" -> mk 1 64 () | "rdx" -> mk 2 64 ()
  | "rbx" -> mk 3 64 () | "rsp" -> mk 4 64 () | "rbp" -> mk 5 64 ()
  | "rsi" -> mk 6 64 () | "rdi" -> mk 7 64 ()
  | "eax" -> mk 0 32 () | "ecx" -> mk 1 32 () | "edx" -> mk 2 32 ()
  | "ebx" -> mk 3 32 () | "esp" -> mk 4 32 () | "ebp" -> mk 5 32 ()
  | "esi" -> mk 6 32 () | "edi" -> mk 7 32 ()
  | "ax" -> mk 0 16 () | "cx" -> mk 1 16 () | "dx" -> mk 2 16 ()
  | "bx" -> mk 3 16 () | "sp" -> mk 4 16 () | "bp" -> mk 5 16 ()
  | "si" -> mk 6 16 () | "di" -> mk 7 16 ()
  | "rip" -> mk 16 64 ()
  | "al" -> mk 0 8 () | "cl" -> mk 1 8 () | "dl" -> mk 2 8 ()
  | "bl" -> mk 3 8 ()
  | "spl" -> mk 4 8 ~needs_rex:true () | "bpl" -> mk 5 8 ~needs_rex:true ()
  | "sil" -> mk 6 8 ~needs_rex:true () | "dil" -> mk 7 8 ~needs_rex:true ()
  | _ when String.length name >= 2 && name.[0] = 'r' ->
      let suffix, width =
        let n = String.length name in
        if name.[n - 1] = 'd' then String.sub name 1 (n - 2), 32
        else if name.[n - 1] = 'w' then String.sub name 1 (n - 2), 16
        else if name.[n - 1] = 'b' then String.sub name 1 (n - 2), 8
        else String.sub name 1 (n - 1), 64
      in
      let code =
        match int_of_string_opt suffix with
        | Some n when n >= 8 && n <= 15 -> n
        | _ -> fail "unknown register %%%s" name
      in
      mk code width ~needs_rex:(width = 8) ()
  | _ -> fail "unknown register %%%s" name

let parse_reg text =
  if not (starts_with ~prefix:"%" text) then None
  else
    let name = String.sub text 1 (String.length text - 1) in
    if starts_with ~prefix:"xmm" name then
      match int_of_string_opt (String.sub name 3 (String.length name - 3)) with
      | Some code when code >= 0 && code <= 15 ->
          Some (Reg { code; width = 128; class_ = Xmm; needs_rex = false })
      | _ -> fail "unknown XMM register %S" text
    else Some (gpr name)

let split_commas text =
  let out = ref [] in
  let start = ref 0 in
  let depth = ref 0 in
  String.iteri
    (fun i c ->
      if c = '(' then incr depth
      else if c = ')' then decr depth
      else if c = ',' && !depth = 0 then begin
        out := trim (String.sub text !start (i - !start)) :: !out;
        start := i + 1
      end)
    text;
  let tail = trim (String.sub text !start (String.length text - !start)) in
  List.rev (if tail = "" then !out else tail :: !out)

let parse_mem text =
  match String.index_opt text '(' with
  | None -> None
  | Some open_ ->
      let close =
        match String.rindex_opt text ')' with
        | Some i when i = String.length text - 1 -> i
        | _ -> fail "malformed memory operand %S" text
      in
      let head = trim (String.sub text 0 open_) in
      let inside = String.sub text (open_ + 1) (close - open_ - 1) in
      let parts = String.split_on_char ',' inside |> List.map trim in
      let reg_part s =
        if s = "" then None
        else match parse_reg s with Some (Reg r) -> Some r | _ -> fail "bad address register %S" s
      in
      let base, index, scale =
        match parts with
        | [ base ] -> reg_part base, None, 1
        | [ base; index ] -> reg_part base, reg_part index, 1
        | [ base; index; scale ] ->
            let scale =
              match int_of_string_opt scale with
              | Some (1 | 2 | 4 | 8 as n) -> n
              | _ -> fail "bad address scale in %S" text
            in
            reg_part base, reg_part index, scale
        | _ -> fail "malformed address %S" text
      in
      let rip =
        match base with
        | Some { code = 16; _ } -> true
        | _ -> false
      in
      let base =
        if inside = "%rip" then None else base
      in
      let symbol, disp =
        if head = "" then None, 0
        else
          match int_of_string_opt head with
          | Some n -> None, n
          | None -> Some head, 0
      in
      Some (Mem { disp; base; index; scale; symbol; rip = inside = "%rip" || rip })

let rec parse_operand text =
  let text = trim text in
  if text = "" then fail "empty operand";
  if text.[0] = '*' then
    Indirect (parse_operand (String.sub text 1 (String.length text - 1)))
  else if text.[0] = '$' then
    Imm (parse_int64 (String.sub text 1 (String.length text - 1)))
  else
    match parse_reg text with
    | Some r -> r
    | None ->
        (match parse_mem text with Some m -> m | None -> Name text)

let gpr_operand ~code ~width =
  Reg { code; width; class_ = Gpr; needs_rex = width = 8 && code >= 4 }

let xmm_operand code = Reg { code; width = 128; class_ = Xmm; needs_rex = false }

let rbp_reg = { code = 5; width = 64; class_ = Gpr; needs_rex = false }

let mem_rbp disp =
  Mem { disp; base = Some rbp_reg; index = None; scale = 1; symbol = None; rip = false }

let mem_reg_disp code disp =
  Mem
    { disp
    ; base = Some { code; width = 64; class_ = Gpr; needs_rex = false }
    ; index = None
    ; scale = 1
    ; symbol = None
    ; rip = false
    }

let mem_reg_index ~base ~index ~scale =
  Mem
    { disp = 0
    ; base = Some { code = base; width = 64; class_ = Gpr; needs_rex = false }
    ; index = Some { code = index; width = 64; class_ = Gpr; needs_rex = false }
    ; scale
    ; symbol = None
    ; rip = false
    }

let sym_rip symbol =
  Mem { disp = 0; base = None; index = None; scale = 1; symbol = Some symbol; rip = true }

let imm_operand v = Imm v
let name_operand s = Name s
let indirect_operand op = Indirect op

let operand_cache : (string, operand) Hashtbl.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> Hashtbl.create 1024)

let parse_operand_cached text =
  let cache = Domain.DLS.get operand_cache in
  match Hashtbl.find_opt cache text with
  | Some op -> op
  | None ->
      let op = parse_operand text in
      Hashtbl.add cache text op;
      op

type branch_kind = Jump | Conditional of int

type fixup =
  { section : section
  ; instruction_offset : int
  ; offset : int
  ; label : string
  ; kind : branch_kind
  ; mutable short : bool
  }

type alignment =
  { section : section
  ; start_offset : int
  ; old_size : int
  ; alignment : int
  ; fill : int
  ; mutable new_size : int
  }

type unwind_draft =
  { function_name : string
  ; start_offset : int
  ; mutable prologue_size : int option
  ; mutable operations : unwind_operation list
  }

type cfi_draft =
  { mutable cfi_function_name : string option
  ; cfi_start_offset : int
  ; mutable cfi_operations : cfi_operation list
  }

type state =
  { text : Code.t
  ; rodata : Code.t
  ; mutable current : section
  ; labels : (string, section * int) Hashtbl.t
  ; globals : (string, unit) Hashtbl.t
  ; functions : (string, unit) Hashtbl.t
  ; sizes : (string, int) Hashtbl.t
  ; mutable fixups : fixup list
  ; mutable alignments : alignment list
  ; mutable relocs : relocation list
  ; mutable current_unwind : unwind_draft option
  ; mutable unwinds : unwind list
  ; mutable current_cfi : cfi_draft option
  ; mutable cfis : cfi list
  }

let code st = match st.current with Text -> st.text | Rodata -> st.rodata
let pos st = (code st).Code.len
let byte st n = Code.u8 (code st) n
let i32 st n = Code.i32 (code st) n
let i32_64 st n = Code.i32_64 (code st) n

let add_reloc st symbol kind addend =
  let offset = pos st in
  st.relocs <- { section = st.current; offset; symbol; kind; addend } :: st.relocs;
  (* COFF REL32 stores its addend in the relocated field and subtracts the end
     of that four-byte field. ELF RELA ignores these bytes and uses [addend]
     from its relocation record. *)
  i32 st (addend + 4)

let rex st ~w ~r ~x ~b ~force =
  let value =
    0x40 lor (if w then 8 else 0) lor
    (if r >= 8 then 4 else 0) lor
    (if x >= 8 then 2 else 0) lor
    (if b >= 8 then 1 else 0)
  in
  if force || value <> 0x40 then byte st value

let modrm st md reg rm = byte st ((md lsl 6) lor ((reg land 7) lsl 3) lor (rm land 7))

let scale_bits = function 1 -> 0 | 2 -> 1 | 4 -> 2 | 8 -> 3 | _ -> assert false

let rm_rex_bits = function
  | Reg r -> r.code, 0, r.needs_rex
  | Mem m ->
      let b = match m.base with Some r -> r.code | None -> 0 in
      let x = match m.index with Some r -> r.code | None -> 0 in
      b, x, false
  | _ -> fail "expected register or memory operand"

let emit_rm_after_opcode st reg_field operand =
  match operand with
  | Reg r -> modrm st 3 reg_field r.code
  | Mem { rip = true; symbol = Some symbol; _ } ->
      modrm st 0 reg_field 5;
      add_reloc st symbol Pc32 (-4)
  | Mem { symbol = Some symbol; _ } ->
      fail "non-PC-relative symbol address %S is unsupported" symbol
  | Mem m ->
      let base_code = Option.map (fun r -> r.code) m.base in
      let index_code = Option.map (fun r -> r.code) m.index in
      let need_sib =
        m.index <> None ||
        match base_code with Some code -> code land 7 = 4 | None -> true
      in
      let md =
        match base_code with
        | None -> 0
        | Some code when m.disp = 0 && code land 7 <> 5 -> 0
        | Some _ when m.disp >= -128 && m.disp <= 127 -> 1
        | Some _ -> 2
      in
      modrm st md reg_field (if need_sib then 4 else Option.get base_code);
      if need_sib then begin
        let idx = Option.value index_code ~default:4 in
        let base = Option.value base_code ~default:5 in
        byte st ((scale_bits m.scale lsl 6) lor ((idx land 7) lsl 3) lor (base land 7))
      end;
      if md = 1 then byte st m.disp
      else if md = 2 || base_code = None then i32 st m.disp
  | _ -> fail "expected r/m operand"

let emit_rm st ?(prefix = []) ~w ~opcode ~reg_field operand =
  List.iter (byte st) prefix;
  let b, x, force = rm_rex_bits operand in
  rex st ~w ~r:reg_field ~x ~b ~force;
  List.iter (byte st) opcode;
  emit_rm_after_opcode st reg_field operand

let width_of_suffix mnemonic =
  match mnemonic.[String.length mnemonic - 1] with
  | 'b' -> 8 | 'w' -> 16 | 'l' -> 32 | 'q' -> 64
  | _ -> fail "instruction has no size suffix: %s" mnemonic

let fits_i8 n = Int64.compare n (-128L) >= 0 && Int64.compare n 127L <= 0

let emit_binary st mnemonic group opcode_rm_reg opcode_reg_rm operands =
  let width = width_of_suffix mnemonic in
  let prefix = if width = 16 then [ 0x66 ] else [] in
  match operands with
  | [ Imm imm; (Reg _ | Mem _ as dst) ] ->
      let b, x, force = rm_rex_bits dst in
      List.iter (byte st) prefix;
      rex st ~w:(width = 64) ~r:0 ~x ~b ~force;
      if width = 8 then begin
        byte st 0x80;
        emit_rm_after_opcode st group dst;
        byte st (Int64.to_int imm)
      end
      else if fits_i8 imm then begin
        byte st 0x83;
        emit_rm_after_opcode st group dst;
        byte st (Int64.to_int imm)
      end
      else begin
        byte st 0x81;
        emit_rm_after_opcode st group dst;
        i32_64 st imm
      end
  | [ Reg src; ((Reg _ | Mem _) as dst_op) ] ->
      emit_rm st ~prefix ~w:(width = 64)
        ~opcode:[ if width = 8 then opcode_reg_rm - 1 else opcode_reg_rm ]
        ~reg_field:src.code dst_op
  | [ (Mem _ as src); Reg dst ] ->
      emit_rm st ~prefix ~w:(width = 64)
        ~opcode:[ if width = 8 then opcode_rm_reg - 1 else opcode_rm_reg ]
        ~reg_field:dst.code src
  | _ -> fail "unsupported %s operand form" mnemonic

let emit_mov st mnemonic operands =
  let width = width_of_suffix mnemonic in
  let prefix = if width = 16 then [ 0x66 ] else [] in
  match operands with
  | [ Imm imm; Reg dst ]
    when width = 64 && mnemonic <> "movabsq"
         && Int64.compare imm (-2_147_483_648L) >= 0
         && Int64.compare imm 2_147_483_647L <= 0 ->
      emit_rm st ~w:true ~opcode:[ 0xc7 ] ~reg_field:0 (Reg dst);
      i32_64 st imm
  | [ Imm imm; Reg dst ] ->
      List.iter (byte st) prefix;
      rex st ~w:(width = 64) ~r:0 ~x:0 ~b:dst.code
        ~force:(width = 8 && dst.needs_rex);
      byte st ((if width = 8 then 0xb0 else 0xb8) + (dst.code land 7));
      if width = 8 then byte st (Int64.to_int imm)
      else if width = 16 then Code.u16 (code st) (Int64.to_int imm)
      else if width = 32 then i32_64 st imm
      else Code.i64 (code st) imm
  | [ Imm imm; (Mem _ as dst) ] ->
      let b, x, force = rm_rex_bits dst in
      List.iter (byte st) prefix;
      rex st ~w:(width = 64) ~r:0 ~x ~b ~force;
      byte st (if width = 8 then 0xc6 else 0xc7);
      emit_rm_after_opcode st 0 dst;
      if width = 8 then byte st (Int64.to_int imm)
      else if width = 16 then Code.u16 (code st) (Int64.to_int imm)
      else i32_64 st imm
  | [ Reg src; (Reg _ | Mem _ as dst) ] ->
      emit_rm st ~prefix ~w:(width = 64)
        ~opcode:[ if width = 8 then 0x88 else 0x89 ]
        ~reg_field:src.code dst
  | [ (Mem _ as src); Reg dst ] ->
      emit_rm st ~prefix ~w:(width = 64)
        ~opcode:[ if width = 8 then 0x8a else 0x8b ]
        ~reg_field:dst.code src
  | _ -> fail "unsupported %s operand form" mnemonic

let cc_code = function
  | "o" -> 0 | "no" -> 1
  | "e" | "z" -> 4 | "ne" | "nz" -> 5
  | "b" -> 2 | "be" -> 6 | "a" -> 7 | "ae" -> 3
  | "l" -> 12 | "le" -> 14 | "g" -> 15 | "ge" -> 13
  | "s" -> 8 | "p" -> 10 | "np" -> 11
  | cc -> fail "unsupported condition code %s" cc

let branch_fixup st kind label =
  let off = pos st in
  let opcode_size = match kind with Jump -> 1 | Conditional _ -> 2 in
  st.fixups <-
    { section = st.current
    ; instruction_offset = off - opcode_size
    ; offset = off
    ; label
    ; kind
    ; short = false
    }
    :: st.fixups;
  i32 st 0

let emit_sse_rm st ~prefix ~opcode operands =
  match operands with
  | [ Reg src; ((Reg _ | Mem _) as dst_op) ] when src.class_ = Xmm ->
      if match dst_op with Reg r -> r.class_ = Xmm | Mem _ -> true | _ -> false then
        emit_rm st ~prefix ~w:false ~opcode:[ 0x0f; opcode + 1 ]
          ~reg_field:src.code dst_op
      else fail "bad SSE destination"
  | [ (Mem _ as src); Reg dst ] when dst.class_ = Xmm ->
      emit_rm st ~prefix ~w:false ~opcode:[ 0x0f; opcode ]
        ~reg_field:dst.code src
  | [ Reg src; Reg dst ] when src.class_ = Xmm && dst.class_ = Xmm ->
      emit_rm st ~prefix ~w:false ~opcode:[ 0x0f; opcode ]
        ~reg_field:dst.code (Reg src)
  | _ -> fail "unsupported SSE move operand form"

let emit_sse_bin st prefix opcode operands =
  match operands with
  | [ (Reg _ | Mem _ as src); Reg dst ] when dst.class_ = Xmm ->
      emit_rm st ~prefix:[ prefix ] ~w:false ~opcode:[ 0x0f; opcode ]
        ~reg_field:dst.code src
  | _ -> fail "unsupported SSE arithmetic operand form"

let emit_ext_move st mnemonic operands =
  let opcode, w =
    match mnemonic with
    | "movsbl" -> 0xbe, false | "movsbq" -> 0xbe, true
    | "movswl" -> 0xbf, false | "movswq" -> 0xbf, true
    | "movslq" -> 0x63, true
    | "movzbl" -> 0xb6, false | "movzbq" -> 0xb6, true
    | "movzwl" -> 0xb7, false | "movzwq" -> 0xb7, true
    | _ -> assert false
  in
  match operands with
  | [ (Reg _ | Mem _ as src); Reg dst ] ->
      let bytes = if mnemonic = "movslq" then [ 0x63 ] else [ 0x0f; opcode ] in
      emit_rm st ~w ~opcode:bytes ~reg_field:dst.code src
  | _ -> fail "unsupported %s operands" mnemonic

let emit_instruction st mnemonic operands =
  match mnemonic with
  | "movb" | "movw" | "movl" | "movq" | "movabsq" ->
      emit_mov st mnemonic operands
  | "movsbl" | "movsbq" | "movswl" | "movswq" | "movslq"
  | "movzbl" | "movzbq" | "movzwl" | "movzwq" ->
      emit_ext_move st mnemonic operands
  | "leaq" ->
      (match operands with
       | [ (Mem _ as src); Reg dst ] ->
           emit_rm st ~w:true ~opcode:[ 0x8d ] ~reg_field:dst.code src
       | _ -> fail "unsupported leaq operands")
  | "addb" | "addw" | "addl" | "addq" ->
      emit_binary st mnemonic 0 0x03 0x01 operands
  | "orb" | "orw" | "orl" | "orq" ->
      emit_binary st mnemonic 1 0x0b 0x09 operands
  | "andb" | "andw" | "andl" | "andq" ->
      emit_binary st mnemonic 4 0x23 0x21 operands
  | "subb" | "subw" | "subl" | "subq" ->
      emit_binary st mnemonic 5 0x2b 0x29 operands
  | "xorb" | "xorw" | "xorl" | "xorq" ->
      emit_binary st mnemonic 6 0x33 0x31 operands
  | "cmpb" | "cmpw" | "cmpl" | "cmpq" ->
      emit_binary st mnemonic 7 0x3b 0x39 operands
  | "testb" | "testw" | "testl" | "testq" ->
      let width = width_of_suffix mnemonic in
      (match operands with
       | [ Imm imm; Reg ({ code = 0; _ } as dst) ] ->
           if width = 16 then byte st 0x66;
           rex st ~w:(width = 64) ~r:0 ~x:0 ~b:dst.code
             ~force:(width = 8 && dst.needs_rex);
           byte st (if width = 8 then 0xa8 else 0xa9);
           if width = 8 then byte st (Int64.to_int imm)
           else if width = 16 then Code.u16 (code st) (Int64.to_int imm)
           else i32_64 st imm
       | [ Imm imm; (Reg _ | Mem _ as dst) ] ->
           let b, x, force = rm_rex_bits dst in
           if width = 16 then byte st 0x66;
           rex st ~w:(width = 64) ~r:0 ~x ~b ~force;
           byte st (if width = 8 then 0xf6 else 0xf7);
           emit_rm_after_opcode st 0 dst;
           if width = 8 then byte st (Int64.to_int imm) else i32_64 st imm
       | [ Reg src; (Reg _ | Mem _ as dst) ] ->
           emit_rm st ~prefix:(if width = 16 then [ 0x66 ] else [])
             ~w:(width = 64) ~opcode:[ if width = 8 then 0x84 else 0x85 ]
             ~reg_field:src.code dst
       | _ -> fail "unsupported %s operands" mnemonic)
  | "imulq" | "imull" ->
      let w = mnemonic = "imulq" in
      (match operands with
       | [ Imm imm; Reg dst ] ->
           emit_rm st ~w
             ~opcode:[ if fits_i8 imm then 0x6b else 0x69 ]
             ~reg_field:dst.code (Reg dst);
           if fits_i8 imm then byte st (Int64.to_int imm) else i32_64 st imm
       | [ (Reg _ | Mem _ as src); Reg dst ] ->
           emit_rm st ~w ~opcode:[ 0x0f; 0xaf ] ~reg_field:dst.code src
       | _ -> fail "unsupported %s operands" mnemonic)
  | "divq" | "idivq" ->
      (match operands with
       | [ (Reg _ | Mem _ as src) ] ->
           emit_rm st ~w:true ~opcode:[ 0xf7 ]
             ~reg_field:(if mnemonic = "divq" then 6 else 7) src
       | _ -> fail "unsupported %s operands" mnemonic)
  | "cqto" -> byte st 0x48; byte st 0x99
  | "incq" | "decq" ->
      (match operands with
       | [ (Reg _ | Mem _ as dst) ] ->
           emit_rm st ~w:true ~opcode:[ 0xff ]
             ~reg_field:(if mnemonic = "incq" then 0 else 1) dst
       | _ -> fail "unsupported %s operands" mnemonic)
  | "shlb" | "shll" | "shlq" | "shrb" | "shrl" | "shrq"
  | "sarb" | "sarl" | "sarq" ->
      let width = width_of_suffix mnemonic in
      let group =
        if starts_with ~prefix:"shl" mnemonic then 4
        else if starts_with ~prefix:"shr" mnemonic then 5 else 7
      in
      (match operands with
       | [ Imm imm; (Reg _ | Mem _ as dst) ] ->
           let b, x, force = rm_rex_bits dst in
           rex st ~w:(width = 64) ~r:0 ~x ~b ~force;
           byte st (if imm = 1L then 0xd1 else 0xc1);
           emit_rm_after_opcode st group dst;
           if imm <> 1L then byte st (Int64.to_int imm)
       | [ Reg { code = 1; width = 8; _ }; (Reg _ | Mem _ as dst) ] ->
           emit_rm st ~w:(width = 64) ~opcode:[ 0xd3 ] ~reg_field:group dst
       | _ -> fail "unsupported %s operands" mnemonic)
  | "notq" | "notl" ->
      let width = width_of_suffix mnemonic in
      (match operands with
       | [ (Reg _ | Mem _ as dst) ] ->
           emit_rm st ~w:(width = 64) ~opcode:[ 0xf7 ] ~reg_field:2 dst
       | _ -> fail "unsupported %s operands" mnemonic)
  | "pushq" | "popq" ->
      (match operands with
       | [ Reg r ] ->
           rex st ~w:false ~r:0 ~x:0 ~b:r.code ~force:false;
           byte st ((if mnemonic = "pushq" then 0x50 else 0x58) + (r.code land 7))
       | _ -> fail "unsupported %s operands" mnemonic)
  | "retq" -> byte st 0xc3
  | "ud2" -> byte st 0x0f; byte st 0x0b
  | "callq" ->
      (match operands with
       | [ Name symbol ] -> byte st 0xe8; add_reloc st symbol Plt32 (-4)
       | [ Indirect (Reg _ | Mem _ as target) ] ->
           emit_rm st ~w:false ~opcode:[ 0xff ] ~reg_field:2 target
       | _ -> fail "unsupported callq operands")
  | "jmp" ->
      (match operands with
       | [ Name label ] -> byte st 0xe9; branch_fixup st Jump label
       | [ Indirect (Reg _ | Mem _ as target) ] ->
           emit_rm st ~w:false ~opcode:[ 0xff ] ~reg_field:4 target
       | _ -> fail "unsupported jmp operands")
  | m when String.length m >= 2 && m.[0] = 'j' ->
      (match operands with
       | [ Name label ] ->
           let cc = cc_code (String.sub m 1 (String.length m - 1)) in
           byte st 0x0f;
           byte st (0x80 + cc);
           branch_fixup st (Conditional cc) label
       | _ -> fail "unsupported %s operands" m)
  | m when starts_with ~prefix:"set" m ->
      let cc = String.sub m 3 (String.length m - 3) in
      (match operands with
       | [ (Reg _ | Mem _ as dst) ] ->
           emit_rm st ~w:false ~opcode:[ 0x0f; 0x90 + cc_code cc ]
             ~reg_field:0 dst
       | _ -> fail "unsupported %s operands" m)
  | "movss" -> emit_sse_rm st ~prefix:[ 0xf3 ] ~opcode:0x10 operands
  | "movsd" -> emit_sse_rm st ~prefix:[ 0xf2 ] ~opcode:0x10 operands
  | "movaps" -> emit_sse_rm st ~prefix:[] ~opcode:0x28 operands
  | "addss" -> emit_sse_bin st 0xf3 0x58 operands
  | "subss" -> emit_sse_bin st 0xf3 0x5c operands
  | "mulss" -> emit_sse_bin st 0xf3 0x59 operands
  | "divss" -> emit_sse_bin st 0xf3 0x5e operands
  | "addsd" -> emit_sse_bin st 0xf2 0x58 operands
  | "subsd" -> emit_sse_bin st 0xf2 0x5c operands
  | "mulsd" -> emit_sse_bin st 0xf2 0x59 operands
  | "divsd" -> emit_sse_bin st 0xf2 0x5e operands
  | "ucomiss" ->
      (match operands with
       | [ (Reg _ | Mem _ as src); Reg dst ] ->
           emit_rm st ~w:false ~opcode:[ 0x0f; 0x2e ]
             ~reg_field:dst.code src
       | _ -> fail "unsupported ucomiss operands")
  | "ucomisd" ->
      (match operands with
       | [ (Reg _ | Mem _ as src); Reg dst ] ->
           emit_rm st ~prefix:[ 0x66 ] ~w:false ~opcode:[ 0x0f; 0x2e ]
             ~reg_field:dst.code src
       | _ -> fail "unsupported ucomisd operands")
  | "xorps" ->
      (match operands with
       | [ (Reg _ | Mem _ as src); Reg dst ] ->
           emit_rm st ~w:false ~opcode:[ 0x0f; 0x57 ] ~reg_field:dst.code src
       | _ -> fail "unsupported xorps operands")
  | "cvtsd2ss" -> emit_sse_bin st 0xf2 0x5a operands
  | "cvtss2sd" -> emit_sse_bin st 0xf3 0x5a operands
  | "cvtsi2ssq" | "cvtsi2sdq" ->
      let prefix = if mnemonic = "cvtsi2ssq" then 0xf3 else 0xf2 in
      (match operands with
       | [ (Reg _ | Mem _ as src); Reg dst ] ->
           emit_rm st ~prefix:[ prefix ] ~w:true ~opcode:[ 0x0f; 0x2a ]
             ~reg_field:dst.code src
       | _ -> fail "unsupported %s operands" mnemonic)
  | "cvttss2siq" | "cvttsd2siq" ->
      let prefix = if mnemonic = "cvttss2siq" then 0xf3 else 0xf2 in
      (match operands with
       | [ (Reg _ | Mem _ as src); Reg dst ] ->
           emit_rm st ~prefix:[ prefix ] ~w:true ~opcode:[ 0x0f; 0x2c ]
             ~reg_field:dst.code src
       | _ -> fail "unsupported %s operands" mnemonic)
  | "movd" ->
      (match operands with
       | [ Reg src; Reg dst ] when src.class_ = Gpr && dst.class_ = Xmm ->
           emit_rm st ~prefix:[ 0x66 ] ~w:false ~opcode:[ 0x0f; 0x6e ]
             ~reg_field:dst.code (Reg src)
       | [ Reg src; Reg dst ] when src.class_ = Xmm && dst.class_ = Gpr ->
           emit_rm st ~prefix:[ 0x66 ] ~w:false ~opcode:[ 0x0f; 0x7e ]
             ~reg_field:src.code (Reg dst)
       | _ -> fail "unsupported movd operands")
  | _ -> fail "unsupported machine instruction %s" mnemonic

let decode_ascii text =
  let n = String.length text in
  if n < 2 || text.[0] <> '"' || text.[n - 1] <> '"' then
    fail "malformed .ascii argument %S" text;
  let out = Buffer.create n in
  let i = ref 1 in
  while !i < n - 1 do
    if text.[!i] <> '\\' then begin
      Buffer.add_char out text.[!i];
      incr i
    end
    else begin
      incr i;
      if !i >= n - 1 then fail "unterminated .ascii escape";
      match text.[!i] with
      | '\\' | '"' as c -> Buffer.add_char out c; incr i
      | c when c >= '0' && c <= '7' ->
          if !i + 2 >= n then fail "short octal escape";
          let digit k = Char.code text.[!i + k] - Char.code '0' in
          Buffer.add_char out (Char.chr ((digit 0 lsl 6) lor (digit 1 lsl 3) lor digit 2));
          i := !i + 3
      | c -> fail "unsupported .ascii escape \\%c" c
    end
  done;
  Buffer.contents out

let define_label st name =
  if Hashtbl.mem st.labels name then fail "duplicate assembly label %s" name;
  Hashtbl.add st.labels name (st.current, pos st);
  match st.current_cfi with
  | Some draft when draft.cfi_function_name = None ->
      draft.cfi_function_name <- Some name
  | _ -> ()

let gpr_number text =
  match parse_reg (trim text) with
  | Some (Reg { code; class_ = Gpr; _ }) when code < 16 -> code
  | _ -> fail "invalid SEH general-purpose register %S" text

let xmm_number text =
  match parse_reg (trim text) with
  | Some (Reg { code; class_ = Xmm; _ }) -> code
  | _ -> fail "invalid SEH XMM register %S" text

let seh_position st draft =
  if st.current <> Text then fail "SEH directive outside .text";
  pos st - draft.start_offset

let with_unwind st directive f =
  match st.current_unwind with
  | Some draft -> f draft
  | None -> fail "%s outside .seh_proc" directive

let with_cfi st directive f =
  match st.current_cfi with
  | Some draft -> f draft
  | None -> fail "%s outside .cfi_startproc" directive

let parse_seh_register_offset args =
  match split_commas args with
  | [ register; offset ] ->
      register,
      (match int_of_string_opt offset with
       | Some n -> n
       | None -> fail "invalid SEH offset %S" offset)
  | _ -> fail "malformed SEH directive arguments %S" args

let set_section_text st = st.current <- Text
let set_global st name = Hashtbl.replace st.globals name ()
let mark_function st name = Hashtbl.replace st.functions name ()

let set_size_here st name =
  match Hashtbl.find_opt st.labels name with
  | Some (section, start) when section = st.current ->
      Hashtbl.replace st.sizes name (pos st - start)
  | _ -> ()

let p2align st power =
  let alignment = 1 lsl power in
  let fill = if st.current = Text then 0x90 else 0 in
  let start_offset = pos st in
  Code.align (code st) alignment fill;
  let old_size = pos st - start_offset in
  st.alignments <-
    { section = st.current
    ; start_offset
    ; old_size
    ; alignment
    ; fill
    ; new_size = old_size
    }
    :: st.alignments

let cfi_startproc st =
  if st.current_cfi <> None then fail "nested .cfi_startproc";
  st.current_cfi <-
    Some
      { cfi_function_name = None
      ; cfi_start_offset = pos st
      ; cfi_operations = []
      }

let cfi_def_cfa_offset st offset =
  with_cfi st ".cfi_def_cfa_offset" (fun draft ->
    draft.cfi_operations <-
      Def_cfa_offset { code_offset = pos st - draft.cfi_start_offset; offset }
      :: draft.cfi_operations)

let cfi_def_cfa_register st register =
  with_cfi st ".cfi_def_cfa_register" (fun draft ->
    draft.cfi_operations <-
      Def_cfa_register
        { code_offset = pos st - draft.cfi_start_offset; register }
      :: draft.cfi_operations)

let cfi_offset st register offset =
  with_cfi st ".cfi_offset" (fun draft ->
    draft.cfi_operations <-
      Cfi_offset
        { code_offset = pos st - draft.cfi_start_offset; register; offset }
      :: draft.cfi_operations)

let cfi_endproc st =
  with_cfi st ".cfi_endproc" (fun draft ->
    let function_name =
      match draft.cfi_function_name with
      | Some name -> name
      | None -> fail ".cfi_startproc has no function label"
    in
    st.cfis <-
      { function_name
      ; start_offset = draft.cfi_start_offset
      ; end_offset = pos st
      ; operations = List.rev draft.cfi_operations
      }
      :: st.cfis;
    st.current_cfi <- None)

let parse_directive st directive args =
  match directive with
  | ".text" -> set_section_text st
  | ".section" ->
      if starts_with ~prefix:".rodata" args || starts_with ~prefix:".rdata" args
      then st.current <- Rodata
      else if starts_with ~prefix:".note.GNU-stack" args then ()
      else fail "unsupported section directive %S" args
  | ".globl" -> set_global st (trim args)
  | ".type" ->
      (match String.index_opt args ',' with
       | Some comma ->
           let name = trim (String.sub args 0 comma) in
           if String.contains args 'f' then mark_function st name
       | None -> ())
  | ".size" ->
      (match String.index_opt args ',' with
       | Some comma -> set_size_here st (trim (String.sub args 0 comma))
       | None -> ())
  | ".p2align" ->
      let first =
        match String.split_on_char ',' args with
        | value :: _ -> trim value
        | [] -> "0"
      in
      p2align st (Option.value (int_of_string_opt first) ~default:0)
  | ".ascii" -> Code.add_string (code st) (decode_ascii (trim args))
  | ".long" -> Code.i32_64 (code st) (parse_int64 (trim args))
  | ".quad" -> Code.i64 (code st) (parse_int64 (trim args))
  | ".def" -> ()
  | ".cfi_startproc" -> cfi_startproc st
  | ".cfi_def_cfa_offset" ->
      (match int_of_string_opt (trim args) with
       | Some offset -> cfi_def_cfa_offset st offset
       | None -> fail "invalid CFA offset %S" args)
  | ".cfi_def_cfa_register" -> cfi_def_cfa_register st (gpr_number args)
  | ".cfi_offset" ->
      let register, offset = parse_seh_register_offset args in
      cfi_offset st (gpr_number register) offset
  | ".cfi_endproc" -> cfi_endproc st
  | ".seh_proc" ->
      if st.current_unwind <> None then fail "nested .seh_proc";
      Hashtbl.replace st.functions (trim args) ();
      st.current_unwind <-
        Some
          { function_name = trim args
          ; start_offset = pos st
          ; prologue_size = None
          ; operations = []
          }
  | ".seh_pushreg" ->
      with_unwind st directive (fun draft ->
        draft.operations <-
          Push_nonvolatile
            { code_offset = seh_position st draft; register = gpr_number args }
          :: draft.operations)
  | ".seh_setframe" ->
      let register, offset = parse_seh_register_offset args in
      with_unwind st directive (fun draft ->
        draft.operations <-
          Set_frame
            { code_offset = seh_position st draft
            ; register = gpr_number register
            ; offset
            }
          :: draft.operations)
  | ".seh_stackalloc" ->
      let size =
        match int_of_string_opt (trim args) with
        | Some n -> n
        | None -> fail "invalid .seh_stackalloc size %S" args
      in
      with_unwind st directive (fun draft ->
        draft.operations <-
          Allocate_stack { code_offset = seh_position st draft; size }
          :: draft.operations)
  | ".seh_savereg" ->
      let register, offset = parse_seh_register_offset args in
      with_unwind st directive (fun draft ->
        draft.operations <-
          Save_nonvolatile
            { code_offset = seh_position st draft
            ; register = gpr_number register
            ; offset
            }
          :: draft.operations)
  | ".seh_savexmm" ->
      let register, offset = parse_seh_register_offset args in
      with_unwind st directive (fun draft ->
        draft.operations <-
          Save_xmm
            { code_offset = seh_position st draft
            ; register = xmm_number register
            ; offset
            }
          :: draft.operations)
  | ".seh_endprologue" ->
      with_unwind st directive (fun draft ->
        draft.prologue_size <- Some (seh_position st draft))
  | ".seh_endproc" ->
      with_unwind st directive (fun draft ->
        let prologue_size =
          match draft.prologue_size with
          | Some n -> n
          | None -> fail "%s has no .seh_endprologue" draft.function_name
        in
        st.unwinds <-
          { function_name = draft.function_name
          ; start_offset = draft.start_offset
          ; end_offset = pos st
          ; prologue_size
          ; operations = draft.operations
          }
          :: st.unwinds;
        st.current_unwind <- None)
  | _ -> fail "unsupported assembler directive %s" directive

let branch_saving fixup =
  match fixup.kind with Jump -> 3 | Conditional _ -> 4

let make_text_position_mapper fixups alignments =
  let events =
    List.filter_map
      (fun (fixup : fixup) ->
        if fixup.short then
          Some (fixup.instruction_offset + 1, -branch_saving fixup)
        else None)
      fixups
    @ List.map
        (fun (alignment : alignment) ->
          let threshold =
            alignment.start_offset + if alignment.old_size = 0 then 0 else 1
          in
          threshold, alignment.new_size - alignment.old_size)
        alignments
    |> List.sort (fun (left, _) (right, _) -> Int.compare left right)
  in
  let positions = Array.make (List.length events) 0 in
  let deltas = Array.make (List.length events) 0 in
  let cumulative = ref 0 in
  List.iteri
    (fun index (position, delta) ->
      cumulative := !cumulative + delta;
      positions.(index) <- position;
      deltas.(index) <- !cumulative)
    events;
  fun position ->
    let low = ref 0 in
    let high = ref (Array.length positions) in
    while !low < !high do
      let middle = (!low + !high) / 2 in
      if positions.(middle) <= position then low := middle + 1
      else high := middle
    done;
    position + if !low = 0 then 0 else deltas.(!low - 1)

let unwind_operation_at map_offset = function
  | Push_nonvolatile operation ->
      Push_nonvolatile
        { operation with code_offset = map_offset operation.code_offset }
  | Set_frame operation ->
      Set_frame { operation with code_offset = map_offset operation.code_offset }
  | Allocate_stack operation ->
      Allocate_stack
        { operation with code_offset = map_offset operation.code_offset }
  | Save_nonvolatile operation ->
      Save_nonvolatile
        { operation with code_offset = map_offset operation.code_offset }
  | Save_xmm operation ->
      Save_xmm { operation with code_offset = map_offset operation.code_offset }

let cfi_operation_at map_offset = function
  | Def_cfa_offset operation ->
      Def_cfa_offset
        { operation with code_offset = map_offset operation.code_offset }
  | Def_cfa_register operation ->
      Def_cfa_register
        { operation with code_offset = map_offset operation.code_offset }
  | Cfi_offset operation ->
      Cfi_offset { operation with code_offset = map_offset operation.code_offset }

type text_edit = Branch of fixup | Align of alignment

let relax_text_branches st =
  let fixups =
    List.filter (fun (fixup : fixup) -> fixup.section = Text) st.fixups
    |> List.sort (fun (left : fixup) (right : fixup) ->
         Int.compare left.instruction_offset right.instruction_offset)
  in
  let alignments =
    List.filter (fun (alignment : alignment) -> alignment.section = Text) st.alignments
    |> List.sort (fun (left : alignment) (right : alignment) ->
         Int.compare left.start_offset right.start_offset)
  in
  let target fixup =
    match Hashtbl.find_opt st.labels fixup.label with
    | Some (Text, offset) -> offset
    | Some _ -> fail "cross-section branch to %s" fixup.label
    | None -> fail "undefined local branch label %s" fixup.label
  in
  let changed = ref true in
  let rounds = ref 0 in
  while !changed && !rounds < 100 do
    changed := false;
    incr rounds;
    List.iter
      (fun (alignment : alignment) ->
        let map = make_text_position_mapper fixups alignments in
        let position = map alignment.start_offset in
        let position =
          if alignment.old_size = 0 then
            position - alignment.new_size + alignment.old_size
          else position
        in
        let desired =
          (alignment.alignment - (position mod alignment.alignment))
          mod alignment.alignment
        in
        if desired <> alignment.new_size then begin
          alignment.new_size <- desired;
          changed := true
        end)
      alignments;
    let map = make_text_position_mapper fixups alignments in
    List.iter
      (fun (fixup : fixup) ->
        let previous = fixup.short in
        let instruction = map fixup.instruction_offset in
        let target_offset = target fixup in
        let hypothetical_delta =
          if target_offset > fixup.instruction_offset && not fixup.short then
            -branch_saving fixup
          else 0
        in
        let displacement =
          map target_offset + hypothetical_delta - (instruction + 2)
        in
        fixup.short <- displacement >= -128 && displacement <= 127;
        if fixup.short <> previous then changed := true)
      fixups
  done;
  if !changed then begin
    List.iter (fun (fixup : fixup) -> fixup.short <- false) fixups;
    List.iter
      (fun (alignment : alignment) ->
        let map = make_text_position_mapper fixups alignments in
        let position = map alignment.start_offset in
        let position =
          if alignment.old_size = 0 then
            position - alignment.new_size + alignment.old_size
          else position
        in
        alignment.new_size <-
          (alignment.alignment - (position mod alignment.alignment))
          mod alignment.alignment)
      alignments
  end;
  let map = make_text_position_mapper fixups alignments in
  let edits =
    List.map (fun fixup -> Branch fixup) fixups
    @ List.map (fun alignment -> Align alignment) alignments
    |> List.sort (fun left right ->
         let position = function
           | Branch fixup -> fixup.instruction_offset
           | Align alignment -> alignment.start_offset
         in
         let result = Int.compare (position left) (position right) in
         if result <> 0 then result
         else match left, right with Align _, Branch _ -> -1 | Branch _, Align _ -> 1 | _ -> 0)
  in
  let old_text = Code.contents st.text in
  let output = Code.create (Bytes.length old_text) in
  let cursor = ref 0 in
  let copy_until position =
    if position < !cursor then fail "overlapping text layout edits";
    Code.add_subbytes output old_text !cursor (position - !cursor)
  in
  List.iter
    (function
      | Align alignment ->
          copy_until alignment.start_offset;
          for _ = 1 to alignment.new_size do Code.u8 output alignment.fill done;
          cursor := alignment.start_offset + alignment.old_size
      | Branch fixup ->
          copy_until fixup.instruction_offset;
          let instruction = output.Code.len in
          if fixup.short then begin
            (match fixup.kind with
             | Jump -> Code.u8 output 0xeb
             | Conditional cc -> Code.u8 output (0x70 + cc));
            let displacement = map (target fixup) - (instruction + 2) in
            if displacement < -128 || displacement > 127 then
              fail "branch relaxation became unstable";
            Code.u8 output displacement
          end
          else begin
            (match fixup.kind with
             | Jump -> Code.u8 output 0xe9
             | Conditional cc ->
                 Code.u8 output 0x0f;
                 Code.u8 output (0x80 + cc));
            let end_offset =
              instruction + (match fixup.kind with Jump -> 5 | Conditional _ -> 6)
            in
            Code.i32 output (map (target fixup) - end_offset)
          end;
          cursor := fixup.offset + 4)
    edits;
  Code.add_subbytes output old_text !cursor (Bytes.length old_text - !cursor);
  let old_labels =
    Hashtbl.fold (fun name location entries -> (name, location) :: entries)
      st.labels []
  in
  List.iter
    (fun (name, (section, offset)) ->
      if section = Text then
        Hashtbl.replace st.labels name (Text, map offset))
    old_labels;
  let old_sizes =
    Hashtbl.fold (fun name size entries -> (name, size) :: entries) st.sizes []
  in
  List.iter
    (fun (name, size) ->
      match List.assoc_opt name old_labels with
      | Some (Text, start) ->
          Hashtbl.replace st.sizes name (map (start + size) - map start)
      | _ -> ())
    old_sizes;
  st.relocs <-
    List.map
      (fun (relocation : relocation) ->
        if relocation.section = Text then
          { relocation with offset = map relocation.offset }
        else relocation)
      st.relocs;
  st.unwinds <-
    List.map
      (fun (unwind : unwind) ->
        let old_start = unwind.start_offset in
        let new_start = map old_start in
        let relative offset = map (old_start + offset) - new_start in
        { unwind with
          start_offset = new_start
        ; end_offset = map unwind.end_offset
        ; prologue_size = relative unwind.prologue_size
        ; operations = List.map (unwind_operation_at relative) unwind.operations
        })
      st.unwinds;
  st.cfis <-
    List.map
      (fun (cfi : cfi) ->
        let old_start = cfi.start_offset in
        let new_start = map old_start in
        let relative offset = map (old_start + offset) - new_start in
        { cfi with
          start_offset = new_start
        ; end_offset = map cfi.end_offset
        ; operations = List.map (cfi_operation_at relative) cfi.operations
        })
      st.cfis;
  st.text.data <- output.data;
  st.text.len <- output.len

type builder = state

let create_builder () =
  { text = Code.create 65536
  ; rodata = Code.create 4096
  ; current = Text
  ; labels = Hashtbl.create 256
  ; globals = Hashtbl.create 64
  ; functions = Hashtbl.create 64
  ; sizes = Hashtbl.create 64
  ; fixups = []
  ; alignments = []
  ; relocs = []
  ; current_unwind = None
  ; unwinds = []
  ; current_cfi = None
  ; cfis = []
  }

let insn_line_cache : (string, string * operand list) Hashtbl.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> Hashtbl.create 1024)

let emit_line st original =
  let line_cache = Domain.DLS.get insn_line_cache in
  match Hashtbl.find_opt line_cache original with
  | Some (head, operands) -> emit_instruction st head operands
  | None ->
      let line = trim original in
      if line = "" || line.[0] = '#' then ()
      else if line.[String.length line - 1] = ':' then
        define_label st (String.sub line 0 (String.length line - 1))
      else
        let split =
          match String.index_opt line '\t' with
          | Some i -> i
          | None ->
              (match String.index_opt line ' ' with
               | Some i -> i
               | None -> String.length line)
        in
        let head = String.sub line 0 split in
        let args =
          if split = String.length line then ""
          else trim (String.sub line (split + 1) (String.length line - split - 1))
        in
        if head.[0] = '.' then parse_directive st head args
        else begin
          let operands = split_commas args |> List.map parse_operand_cached in
          Hashtbl.add line_cache original (head, operands);
          emit_instruction st head operands
        end

let emit_typed_instruction st mnemonic operands =
  emit_instruction st mnemonic operands

let emit_pc32_delta st ~target ~base =
  let base_offset =
    match Hashtbl.find_opt st.labels base with
    | Some (section, offset) when section = st.current -> offset
    | Some _ -> fail "jump-table base %s is in another section" base
    | None -> fail "undefined jump-table base %s" base
  in
  let slot = pos st - base_offset in
  add_reloc st target Pc32 slot

let operand text = parse_operand_cached text

let finish_builder st =
  relax_text_branches st;
  (match st.current_unwind with
   | Some draft -> fail "unterminated .seh_proc for %s" draft.function_name
   | None -> ());
  (match st.current_cfi with
   | Some _ -> fail "unterminated .cfi_startproc"
   | None -> ());
  let symbols =
    Hashtbl.fold
      (fun name (section, value) acc ->
        { name
        ; section = Some section
        ; value
        ; size = Option.value (Hashtbl.find_opt st.sizes name) ~default:0
        ; global = Hashtbl.mem st.globals name
        ; function_ = Hashtbl.mem st.functions name
        }
        :: acc)
      st.labels []
  in
  let defined = Hashtbl.create (List.length symbols) in
  List.iter (fun symbol -> Hashtbl.replace defined symbol.name ()) symbols;
  let symbols =
    List.fold_left
      (fun acc (relocation : relocation) ->
        if Hashtbl.mem defined relocation.symbol then acc
        else begin
          Hashtbl.add defined relocation.symbol ();
          { name = relocation.symbol
          ; section = None
          ; value = 0
          ; size = 0
          ; global = true
          ; function_ = true
          }
          :: acc
        end)
      symbols st.relocs
  in
  { text = Code.contents st.text
  ; rodata = Code.contents st.rodata
  ; symbols
  ; relocations = List.rev st.relocs
  ; unwinds = List.rev st.unwinds
  ; cfis = List.rev st.cfis
  }

let assemble source =
  let builder = create_builder () in
  String.split_on_char '\n' source |> List.iter (emit_line builder);
  finish_builder builder

let merge_fragments assembled =
  let text = Code.create 65536 in
  let rodata = Code.create 4096 in
  let relocations = ref [] in
  let unwinds = ref [] in
  let cfis = ref [] in
  let symbols_by_name = Hashtbl.create 256 in
  let symbol_order = ref [] in
  let merge_symbol (symbol : symbol) =
    match Hashtbl.find_opt symbols_by_name symbol.name with
    | None ->
        Hashtbl.add symbols_by_name symbol.name symbol;
        symbol_order := symbol.name :: !symbol_order
    | Some previous ->
        let chosen =
          match previous.section, symbol.section with
          | None, Some _ -> symbol
          | Some _, None -> previous
          | None, None ->
              { previous with
                global = previous.global || symbol.global
              ; function_ = previous.function_ || symbol.function_
              }
          | Some _, Some _ ->
              fail "duplicate definition of assembly symbol %s" symbol.name
        in
        Hashtbl.replace symbols_by_name symbol.name chosen
  in
  Array.iter
    (fun (fragment : assembled) ->
      if Bytes.length fragment.text > 0 then Code.align text 16 0x90;
      let text_shift = text.len in
      Code.add_subbytes text fragment.text 0 (Bytes.length fragment.text);
      if Bytes.length fragment.rodata > 0 then Code.align rodata 8 0;
      let rodata_shift = rodata.len in
      Code.add_subbytes rodata fragment.rodata 0 (Bytes.length fragment.rodata);
      let shift section value =
        value + match section with Text -> text_shift | Rodata -> rodata_shift
      in
      List.iter
        (fun (symbol : symbol) ->
          merge_symbol
            { symbol with
              value =
                (match symbol.section with
                 | None -> symbol.value
                 | Some section -> shift section symbol.value)
            })
        fragment.symbols;
      relocations :=
        List.rev_append
          (List.map
             (fun (relocation : relocation) ->
               { relocation with
                 offset = shift relocation.section relocation.offset
               })
             fragment.relocations)
          !relocations;
      unwinds :=
        List.rev_append
          (List.map
             (fun (unwind : unwind) ->
               { unwind with
                 start_offset = unwind.start_offset + text_shift
               ; end_offset = unwind.end_offset + text_shift
               })
             fragment.unwinds)
          !unwinds;
      cfis :=
        List.rev_append
          (List.map
             (fun (cfi : cfi) ->
               { cfi with
                 start_offset = cfi.start_offset + text_shift
               ; end_offset = cfi.end_offset + text_shift
               })
             fragment.cfis)
          !cfis)
    assembled;
  { text = Code.contents text
  ; rodata = Code.contents rodata
  ; symbols =
      List.rev_map
        (fun name -> Hashtbl.find symbols_by_name name)
        !symbol_order
  ; relocations = List.rev !relocations
  ; unwinds = List.rev !unwinds
  ; cfis = List.rev !cfis
  }

let assemble_fragments fragments =
  let count = Array.length fragments in
  let assembled = Array.make count None in
  let next = Atomic.make 0 in
  let first_error = Atomic.make None in
  let worker () =
    let rec loop () =
      if Atomic.get first_error = None then begin
        let index = Atomic.fetch_and_add next 1 in
        if index < count then begin
          (try assembled.(index) <- Some (assemble fragments.(index))
           with exn ->
             ignore (Atomic.compare_and_set first_error None (Some exn)));
          loop ()
        end
      end
    in
    loop ()
  in
  let domains = min 6 (Domain.recommended_domain_count ()) in
  if count >= 16 && domains >= 2 then begin
    let workers = Array.init (domains - 1) (fun _ -> Domain.spawn worker) in
    worker ();
    Array.iter Domain.join workers
  end
  else worker ();
  (match Atomic.get first_error with Some exn -> raise exn | None -> ());
  Array.map Option.get assembled |> merge_fragments
