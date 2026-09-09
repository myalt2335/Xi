open Asm_x64_obj

let align_up n alignment = (n + alignment - 1) / alignment * alignment

let set_u8 out off n = Bytes.set out off (Char.chr (n land 0xff))

let set_u16 out off n =
  set_u8 out off n;
  set_u8 out (off + 1) (n lsr 8)

let set_u32_i64 out off n =
  for i = 0 to 3 do
    set_u8 out (off + i)
      (Int64.to_int
         (Int64.logand (Int64.shift_right_logical n (i * 8)) 0xffL))
  done

let set_u32 out off n = set_u32_i64 out off (Int64.of_int n)

let set_u64 out off n =
  for i = 0 to 7 do
    set_u8 out (off + i)
      (Int64.to_int
         (Int64.logand (Int64.shift_right_logical n (i * 8)) 0xffL))
  done

let set_i64 = set_u64

let add_u8 buffer value =
  Buffer.add_char buffer (Char.chr (value land 0xff))

let add_u32 buffer value =
  let value = Int64.of_int value in
  for index = 0 to 3 do
    add_u8 buffer
      (Int64.to_int
         (Int64.logand
            (Int64.shift_right_logical value (index * 8))
            0xffL))
  done

let rec add_uleb128 buffer value =
  let byte = value land 0x7f in
  let rest = value lsr 7 in
  add_u8 buffer (if rest = 0 then byte else byte lor 0x80);
  if rest <> 0 then add_uleb128 buffer rest

let dwarf_register = function
  | 0 -> 0
  | 1 -> 2
  | 2 -> 1
  | 3 -> 3
  | 4 -> 7
  | 5 -> 6
  | 6 -> 4
  | 7 -> 5
  | register when register >= 8 && register <= 15 -> register
  | register ->
      raise
        (Unsupported
           (Printf.sprintf "register %d has no AMD64 DWARF mapping" register))

let add_advance buffer delta =
  if delta < 0 then raise (Unsupported "CFI operations are out of order")
  else if delta = 0 then ()
  else if delta <= 63 then add_u8 buffer (0x40 + delta)
  else if delta <= 255 then begin
    add_u8 buffer 0x02;
    add_u8 buffer delta
  end
  else if delta <= 65_535 then begin
    add_u8 buffer 0x03;
    add_u8 buffer delta;
    add_u8 buffer (delta lsr 8)
  end
  else begin
    add_u8 buffer 0x04;
    add_u32 buffer delta
  end

let build_eh_frame cfis =
  let output = Buffer.create (24 + (List.length cfis * 32)) in
  Buffer.add_string output
    "\x14\x00\x00\x00\x00\x00\x00\x00\x01zR\x00\x01\x78\x10\x01\x1b\
     \x0c\x07\x08\x90\x01\x00\x00";
  let relocations = ref [] in
  List.iter
    (fun (cfi : cfi) ->
      let fde_start = Buffer.length output in
      let body = Buffer.create 40 in
      add_u32 body (fde_start + 4);
      add_u32 body 0;
      add_u32 body (cfi.end_offset - cfi.start_offset);
      add_u8 body 0;
      let previous = ref 0 in
      List.iter
        (fun operation ->
          let code_offset =
            match operation with
            | Def_cfa_offset operation -> operation.code_offset
            | Def_cfa_register operation -> operation.code_offset
            | Cfi_offset operation -> operation.code_offset
          in
          add_advance body (code_offset - !previous);
          previous := code_offset;
          match operation with
          | Def_cfa_offset { offset; _ } ->
              add_u8 body 0x0e;
              add_uleb128 body offset
          | Def_cfa_register { register; _ } ->
              add_u8 body 0x0d;
              add_uleb128 body (dwarf_register register)
          | Cfi_offset { register; offset; _ } ->
              if offset >= 0 || offset mod 8 <> 0 then
                raise
                  (Unsupported
                     (Printf.sprintf "invalid CFA-relative save offset %d" offset));
              let register = dwarf_register register in
              if register >= 64 then
                raise (Unsupported "extended DWARF register saves are unsupported");
              add_u8 body (0x80 + register);
              add_uleb128 body (-offset / 8))
        cfi.operations;
      while (4 + Buffer.length body) mod 8 <> 0 do add_u8 body 0 done;
      add_u32 output (Buffer.length body);
      Buffer.add_buffer output body;
      relocations := (fde_start + 8, cfi.function_name) :: !relocations)
    cfis;
  Bytes.of_string (Buffer.contents output), List.rev !relocations

let string_table names =
  let buf = Buffer.create 256 in
  Buffer.add_char buf '\000';
  let offsets = Hashtbl.create (List.length names) in
  List.iter
    (fun name ->
      if not (Hashtbl.mem offsets name) then begin
        Hashtbl.add offsets name (Buffer.length buf);
        Buffer.add_string buf name;
        Buffer.add_char buf '\000'
      end)
    names;
  (Bytes.of_string (Buffer.contents buf), offsets)

type section_desc =
  { name : string
  ; typ : int
  ; flags : int64
  ; offset : int
  ; size : int
  ; link : int
  ; info : int
  ; align : int
  ; entsize : int
  }

let write_section_header out base shstr desc =
  set_u32 out base (Hashtbl.find shstr desc.name);
  set_u32 out (base + 4) desc.typ;
  set_u64 out (base + 8) desc.flags;
  set_u64 out (base + 16) 0L;
  set_u64 out (base + 24) (Int64.of_int desc.offset);
  set_u64 out (base + 32) (Int64.of_int desc.size);
  set_u32 out (base + 40) desc.link;
  set_u32 out (base + 44) desc.info;
  set_u64 out (base + 48) (Int64.of_int desc.align);
  set_u64 out (base + 56) (Int64.of_int desc.entsize)

let generate (input : assembled) =
  let eh_frame, eh_relocations = build_eh_frame input.cfis in
  let locals, globals =
    List.partition (fun symbol -> not symbol.global) input.symbols
  in
  let by_name (a : symbol) (b : symbol) = String.compare a.name b.name in
  let locals = List.sort by_name locals in
  let globals = List.sort by_name globals in
  let symbols = locals @ globals in
  let strtab, str_offsets =
    string_table (List.map (fun (symbol : symbol) -> symbol.name) symbols)
  in
  let symbol_indices = Hashtbl.create (List.length symbols) in
  List.iteri
    (fun i (symbol : symbol) -> Hashtbl.add symbol_indices symbol.name (i + 1))
    symbols;
  let symtab_size = (List.length symbols + 1) * 24 in
  let text_relocs =
    List.filter (fun (reloc : relocation) -> reloc.section = Text) input.relocations
  in
  let rodata_relocs =
    List.filter (fun (reloc : relocation) -> reloc.section = Rodata) input.relocations
  in
  let rela_size = List.length text_relocs * 24 in
  let rela_rodata_size = List.length rodata_relocs * 24 in
  let rela_eh_size = List.length eh_relocations * 24 in
  let sh_names =
    [ ""; ".text"; ".rela.text"; ".rodata"; ".rela.rodata"; ".eh_frame"
    ; ".rela.eh_frame"
    ; ".symtab"; ".strtab"; ".shstrtab"; ".note.GNU-stack"
    ]
  in
  let shstrtab, sh_offsets = string_table sh_names in
  let text_off = align_up 64 16 in
  let rela_off = align_up (text_off + Bytes.length input.text) 8 in
  let rodata_off = align_up (rela_off + rela_size) 8 in
  let rela_rodata_off = align_up (rodata_off + Bytes.length input.rodata) 8 in
  let eh_frame_off = align_up (rela_rodata_off + rela_rodata_size) 8 in
  let rela_eh_off = align_up (eh_frame_off + Bytes.length eh_frame) 8 in
  let symtab_off = align_up (rela_eh_off + rela_eh_size) 8 in
  let strtab_off = symtab_off + symtab_size in
  let shstrtab_off = strtab_off + Bytes.length strtab in
  let shoff = align_up (shstrtab_off + Bytes.length shstrtab) 8 in
  let section_count = 11 in
  let out = Bytes.make (shoff + section_count * 64) '\000' in
  Bytes.blit_string "\x7fELF" 0 out 0 4;
  set_u8 out 4 2;
  set_u8 out 5 1;
  set_u8 out 6 1;
  set_u16 out 16 1;
  set_u16 out 18 62;
  set_u32 out 20 1;
  set_u64 out 40 (Int64.of_int shoff);
  set_u16 out 52 64;
  set_u16 out 58 64;
  set_u16 out 60 section_count;
  set_u16 out 62 9;
  Bytes.blit input.text 0 out text_off (Bytes.length input.text);
  Bytes.blit input.rodata 0 out rodata_off (Bytes.length input.rodata);
  Bytes.blit eh_frame 0 out eh_frame_off (Bytes.length eh_frame);
  Bytes.blit strtab 0 out strtab_off (Bytes.length strtab);
  Bytes.blit shstrtab 0 out shstrtab_off (Bytes.length shstrtab);
  List.iteri
    (fun i (relocation : relocation) ->
      let off = rela_off + i * 24 in
      let symbol =
        match Hashtbl.find_opt symbol_indices relocation.symbol with
        | Some index -> index
        | None -> raise (Unsupported ("missing relocation symbol " ^ relocation.symbol))
      in
      let typ = match relocation.kind with Pc32 -> 2 | Plt32 -> 4 in
      set_u64 out off (Int64.of_int relocation.offset);
      set_u64 out (off + 8)
        (Int64.logor (Int64.shift_left (Int64.of_int symbol) 32)
           (Int64.of_int typ));
      set_i64 out (off + 16) (Int64.of_int relocation.addend))
    text_relocs;
  List.iteri
    (fun i (relocation : relocation) ->
      let off = rela_rodata_off + i * 24 in
      let symbol =
        match Hashtbl.find_opt symbol_indices relocation.symbol with
        | Some index -> index
        | None -> raise (Unsupported ("missing relocation symbol " ^ relocation.symbol))
      in
      let typ = match relocation.kind with Pc32 -> 2 | Plt32 -> 4 in
      set_u64 out off (Int64.of_int relocation.offset);
      set_u64 out (off + 8)
        (Int64.logor (Int64.shift_left (Int64.of_int symbol) 32)
           (Int64.of_int typ));
      set_i64 out (off + 16) (Int64.of_int relocation.addend))
    rodata_relocs;
  List.iteri
    (fun index (offset, symbol_name) ->
      let off = rela_eh_off + (index * 24) in
      let symbol =
        match Hashtbl.find_opt symbol_indices symbol_name with
        | Some value -> value
        | None ->
            raise
              (Unsupported ("missing CFI function symbol " ^ symbol_name))
      in
      set_u64 out off (Int64.of_int offset);
      set_u64 out (off + 8)
        (Int64.logor (Int64.shift_left (Int64.of_int symbol) 32) 2L);
      set_i64 out (off + 16) 0L)
    eh_relocations;
  List.iteri
    (fun i (symbol : symbol) ->
      let off = symtab_off + (i + 1) * 24 in
      set_u32 out off (Hashtbl.find str_offsets symbol.name);
      let bind = if symbol.global then 1 else 0 in
      let typ = if symbol.function_ then 2 else 0 in
      set_u8 out (off + 4) ((bind lsl 4) lor typ);
      let shndx =
        match symbol.section with None -> 0 | Some Text -> 1 | Some Rodata -> 3
      in
      set_u16 out (off + 6) shndx;
      set_u64 out (off + 8) (Int64.of_int symbol.value);
      set_u64 out (off + 16) (Int64.of_int symbol.size))
    symbols;
  let sections =
    [ { name = ""; typ = 0; flags = 0L; offset = 0; size = 0
      ; link = 0; info = 0; align = 0; entsize = 0 }
    ; { name = ".text"; typ = 1; flags = 0x6L; offset = text_off
      ; size = Bytes.length input.text; link = 0; info = 0; align = 16; entsize = 0 }
    ; { name = ".rela.text"; typ = 4; flags = 0L; offset = rela_off
      ; size = rela_size; link = 7; info = 1; align = 8; entsize = 24 }
    ; { name = ".rodata"; typ = 1; flags = 0x2L; offset = rodata_off
      ; size = Bytes.length input.rodata; link = 0; info = 0; align = 8; entsize = 0 }
    ; { name = ".rela.rodata"; typ = 4; flags = 0L; offset = rela_rodata_off
      ; size = rela_rodata_size; link = 7; info = 3; align = 8; entsize = 24 }
    ; { name = ".eh_frame"; typ = 0x70000001; flags = 0x2L; offset = eh_frame_off
      ; size = Bytes.length eh_frame; link = 0; info = 0; align = 8; entsize = 0 }
    ; { name = ".rela.eh_frame"; typ = 4; flags = 0L; offset = rela_eh_off
      ; size = rela_eh_size; link = 7; info = 5; align = 8; entsize = 24 }
    ; { name = ".symtab"; typ = 2; flags = 0L; offset = symtab_off
      ; size = symtab_size; link = 8; info = List.length locals + 1
      ; align = 8; entsize = 24 }
    ; { name = ".strtab"; typ = 3; flags = 0L; offset = strtab_off
      ; size = Bytes.length strtab; link = 0; info = 0; align = 1; entsize = 0 }
    ; { name = ".shstrtab"; typ = 3; flags = 0L; offset = shstrtab_off
      ; size = Bytes.length shstrtab; link = 0; info = 0; align = 1; entsize = 0 }
    ; { name = ".note.GNU-stack"; typ = 1; flags = 0L; offset = shoff
      ; size = 0; link = 0; info = 0; align = 1; entsize = 0 }
    ]
  in
  List.iteri
    (fun i desc -> write_section_header out (shoff + i * 64) sh_offsets desc)
    sections;
  out
