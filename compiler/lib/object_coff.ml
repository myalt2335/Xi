open Asm_x64_obj

let set_u8 out off n = Bytes.set out off (Char.chr (n land 0xff))

let set_u16 out off n =
  set_u8 out off n;
  set_u8 out (off + 1) (n lsr 8)

let set_u32 out off n =
  let value = Int64.of_int n in
  for i = 0 to 3 do
    set_u8 out (off + i)
      (Int64.to_int
         (Int64.logand (Int64.shift_right_logical value (i * 8)) 0xffL))
  done

let add_u8 buffer n = Buffer.add_char buffer (Char.chr (n land 0xff))

let add_u16 buffer n =
  add_u8 buffer n;
  add_u8 buffer (n lsr 8)

let add_u32 buffer n =
  let value = Int64.of_int n in
  for i = 0 to 3 do
    add_u8 buffer
      (Int64.to_int
         (Int64.logand (Int64.shift_right_logical value (i * 8)) 0xffL))
  done

let align_buffer buffer alignment =
  while Buffer.length buffer mod alignment <> 0 do add_u8 buffer 0 done

let checked_div name value alignment =
  if value < 0 || value mod alignment <> 0 then
    raise
      (Unsupported
         (Printf.sprintf "%s offset %d is not a nonnegative multiple of %d"
            name value alignment));
  value / alignment

let checked_u8 name value =
  if value < 0 || value > 255 then
    raise
      (Unsupported
         (Printf.sprintf "%s value %d does not fit in one byte" name value));
  value

let encode_unwind_operations operations =
  let out = Buffer.create 32 in
  let slots = ref 0 in
  let primary code_offset opcode opinfo =
    add_u8 out (checked_u8 "unwind code offset" code_offset);
    add_u8 out ((opinfo lsl 4) lor opcode);
    incr slots
  in
  List.iter
    (function
      | Push_nonvolatile { code_offset; register } ->
          primary code_offset 0 register
      | Set_frame { code_offset; _ } ->
          primary code_offset 3 0
      | Allocate_stack { code_offset; size } ->
          if size >= 8 && size <= 128 && size mod 8 = 0 then
            primary code_offset 2 ((size / 8) - 1)
          else if size <= 524_280 then begin
            primary code_offset 1 0;
            add_u16 out (checked_div "stack allocation" size 8);
            incr slots
          end
          else begin
            primary code_offset 1 1;
            add_u32 out size;
            slots := !slots + 2
          end
      | Save_nonvolatile { code_offset; register; offset } ->
          primary code_offset 4 register;
          add_u16 out (checked_div "saved-register" offset 8);
          incr slots
      | Save_xmm { code_offset; register; offset } ->
          primary code_offset 8 register;
          add_u16 out (checked_div "saved-XMM" offset 16);
          incr slots)
    operations;
  Bytes.of_string (Buffer.contents out), !slots

type unwind_location =
  { unwind : unwind
  ; xdata_offset : int
  ; end_symbol : string
  ; unwind_symbol : string
  }

let synthetic_names index (unwind : unwind) =
  ( Printf.sprintf ".Lxic$end$%d$%s" index unwind.function_name
  , Printf.sprintf ".Lxic$unwind$%d$%s" index unwind.function_name )

let build_unwind_data unwinds =
  let out = Buffer.create (List.length unwinds * 24) in
  let locations =
    List.mapi
      (fun index (unwind : unwind) ->
        align_buffer out 4;
        let xdata_offset = Buffer.length out in
        let operation_bytes, slot_count =
          encode_unwind_operations unwind.operations
        in
        let frame_register, frame_offset =
          List.fold_left
            (fun current -> function
              | Set_frame { register; offset; _ } ->
                  if current <> None then
                    raise
                      (Unsupported
                         ("multiple .seh_setframe directives in "
                          ^ unwind.function_name));
                  Some (register, checked_div "frame" offset 16)
              | _ -> current)
            None unwind.operations
          |> Option.value ~default:(0, 0)
        in
        if frame_offset > 15 then
          raise
            (Unsupported
               (Printf.sprintf "%s frame offset %d exceeds the Windows encoding"
                  unwind.function_name (frame_offset * 16)));
        add_u8 out 1;
        add_u8 out (checked_u8 "prologue size" unwind.prologue_size);
        add_u8 out (checked_u8 "unwind code count" slot_count);
        add_u8 out ((frame_offset lsl 4) lor frame_register);
        Buffer.add_bytes out operation_bytes;
        if slot_count mod 2 <> 0 then begin
          add_u16 out 0
        end;
        let end_symbol, unwind_symbol = synthetic_names index unwind in
        { unwind; xdata_offset; end_symbol; unwind_symbol })
      unwinds
  in
  Bytes.of_string (Buffer.contents out), locations

type coff_symbol =
  { name : string
  ; value : int
  ; section_number : int
  ; typ : int
  ; storage_class : int
  }

type coff_relocation =
  { virtual_address : int
  ; symbol : string
  ; typ : int
  }

type section_desc =
  { name : string
  ; data : bytes
  ; relocations : coff_relocation list
  ; characteristics : int
  ; mutable raw_offset : int
  ; mutable reloc_offset : int
  }

let write_name out off string_offsets name =
  if String.length name <= 8 then
    Bytes.blit_string name 0 out off (String.length name)
  else begin
    set_u32 out off 0;
    set_u32 out (off + 4) (Hashtbl.find string_offsets name)
  end

let generate (input : assembled) =
  let xdata, unwind_locations = build_unwind_data input.unwinds in
  let pdata = Bytes.make (List.length unwind_locations * 12) '\000' in
  let text_relocations =
    List.filter_map
      (fun (relocation : relocation) ->
        if relocation.section = Text then
          Some
            { virtual_address = relocation.offset
            ; symbol = relocation.symbol
            ; typ = 0x0004
            }
        else None)
      input.relocations
  in
  let rodata_relocations =
    List.filter_map
      (fun (relocation : relocation) ->
        if relocation.section = Rodata then
          Some
            { virtual_address = relocation.offset
            ; symbol = relocation.symbol
            ; typ = 0x0004
            }
        else None)
      input.relocations
  in
  let pdata_relocations =
    List.mapi
      (fun index location ->
        let base = index * 12 in
        [ { virtual_address = base
          ; symbol = location.unwind.function_name
          ; typ = 0x0003
          }
        ; { virtual_address = base + 4
          ; symbol = location.end_symbol
          ; typ = 0x0003
          }
        ; { virtual_address = base + 8
          ; symbol = location.unwind_symbol
          ; typ = 0x0003
          }
        ])
      unwind_locations
    |> List.flatten
  in
  let sections =
    [ { name = ".text"; data = input.text; relocations = text_relocations
      ; characteristics = 0x60500020; raw_offset = 0; reloc_offset = 0 }
    ; { name = ".rdata"; data = input.rodata; relocations = rodata_relocations
      ; characteristics = 0x40400040; raw_offset = 0; reloc_offset = 0 }
    ; { name = ".xdata"; data = xdata; relocations = []
      ; characteristics = 0x40300040; raw_offset = 0; reloc_offset = 0 }
    ; { name = ".pdata"; data = pdata; relocations = pdata_relocations
      ; characteristics = 0x40300040; raw_offset = 0; reloc_offset = 0 }
    ]
  in
  List.iter
    (fun section ->
      if List.length section.relocations > 65_535 then
        raise
          (Unsupported
             (Printf.sprintf "%s has too many COFF relocations" section.name)))
    sections;
  let source_symbols =
    List.map
      (fun (symbol : symbol) ->
        let section_number =
          match symbol.section with None -> 0 | Some Text -> 1 | Some Rodata -> 2
        in
        { name = symbol.name
        ; value = symbol.value
        ; section_number
        ; typ = if symbol.function_ then 0x20 else 0
        ; storage_class = if symbol.global || symbol.section = None then 2 else 3
        })
      input.symbols
  in
  let synthetic_symbols =
    List.concat_map
      (fun location ->
        [ { name = location.end_symbol
          ; value = location.unwind.end_offset
          ; section_number = 1; typ = 0; storage_class = 3
          }
        ; { name = location.unwind_symbol
          ; value = location.xdata_offset
          ; section_number = 3; typ = 0; storage_class = 3
          }
        ])
      unwind_locations
  in
  let symbols = source_symbols @ synthetic_symbols in
  let symbol_indices = Hashtbl.create (List.length symbols) in
  List.iteri
    (fun index (symbol : coff_symbol) ->
      if Hashtbl.mem symbol_indices symbol.name then
        raise (Unsupported ("duplicate COFF symbol " ^ symbol.name));
      Hashtbl.add symbol_indices symbol.name index)
    symbols;
  let long_names =
    List.filter_map
      (fun (symbol : coff_symbol) ->
        if String.length symbol.name > 8 then Some symbol.name else None)
      symbols
  in
  let string_buffer = Buffer.create 256 in
  add_u32 string_buffer 0;
  let string_offsets = Hashtbl.create (List.length long_names) in
  List.iter
    (fun name ->
      if not (Hashtbl.mem string_offsets name) then begin
        Hashtbl.add string_offsets name (Buffer.length string_buffer);
        Buffer.add_string string_buffer name;
        Buffer.add_char string_buffer '\000'
      end)
    long_names;
  let string_table = Bytes.of_string (Buffer.contents string_buffer) in
  set_u32 string_table 0 (Bytes.length string_table);
  let cursor = ref (20 + (List.length sections * 40)) in
  List.iter
    (fun section ->
      if Bytes.length section.data > 0 then begin
        section.raw_offset <- !cursor;
        cursor := !cursor + Bytes.length section.data
      end;
      if section.relocations <> [] then begin
        section.reloc_offset <- !cursor;
        cursor := !cursor + (List.length section.relocations * 10)
      end)
    sections;
  let symbol_table_offset = !cursor in
  let symbol_table_size = List.length symbols * 18 in
  let out =
    Bytes.make
      (symbol_table_offset + symbol_table_size + Bytes.length string_table)
      '\000'
  in
  set_u16 out 0 0x8664;
  set_u16 out 2 (List.length sections);
  set_u32 out 4 0;
  set_u32 out 8 symbol_table_offset;
  set_u32 out 12 (List.length symbols);
  List.iteri
    (fun index section ->
      let off = 20 + (index * 40) in
      Bytes.blit_string section.name 0 out off (String.length section.name);
      set_u32 out (off + 16) (Bytes.length section.data);
      set_u32 out (off + 20) section.raw_offset;
      set_u32 out (off + 24) section.reloc_offset;
      set_u16 out (off + 32) (List.length section.relocations);
      set_u32 out (off + 36) section.characteristics;
      if section.raw_offset <> 0 then
        Bytes.blit section.data 0 out section.raw_offset (Bytes.length section.data);
      List.iteri
        (fun relocation_index relocation ->
          let reloc_off = section.reloc_offset + (relocation_index * 10) in
          let symbol_index =
            match Hashtbl.find_opt symbol_indices relocation.symbol with
            | Some value -> value
            | None ->
                raise
                  (Unsupported
                     ("missing COFF relocation symbol " ^ relocation.symbol))
          in
          set_u32 out reloc_off relocation.virtual_address;
          set_u32 out (reloc_off + 4) symbol_index;
          set_u16 out (reloc_off + 8) relocation.typ)
        section.relocations)
    sections;
  List.iteri
    (fun index (symbol : coff_symbol) ->
      let off = symbol_table_offset + (index * 18) in
      write_name out off string_offsets symbol.name;
      set_u32 out (off + 8) symbol.value;
      set_u16 out (off + 12) symbol.section_number;
      set_u16 out (off + 14) symbol.typ;
      set_u8 out (off + 16) symbol.storage_class)
    symbols;
  Bytes.blit string_table 0 out
    (symbol_table_offset + symbol_table_size)
    (Bytes.length string_table);
  out
