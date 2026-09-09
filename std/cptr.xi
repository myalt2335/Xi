// Xi standard library: cptr - raw, GC-excluded C memory (Feature B).
//
// `cptr` is an opaque, pointer-sized handle to memory that lives OUTSIDE the Xi
// garbage collector. It is the general "talk to C memory" primitive: it builds
// a `WNDCLASSW`, a `MSG`, a wide string, an `RECT`, and it unlocks binary
// formats, mmap, and syscalls. Because the backing store is a plain `malloc`
// block (never a GC object), the collector never moves it, scans it, or frees
// it — lifetime is fully manual, so every `cptr::alloc` must be paired with a
// `cptr::free`.
//
//   cptr p = cptr::alloc(64);   // 64 zeroed bytes, never GC-managed
//   ... use p ...
//   cptr::free(p);              // release it yourself
//
// Reads/writes are typed and take a BYTE offset into the block. Offsets are not
// bounds-checked (this is raw memory); staying inside the allocation is your
// responsibility, exactly as in C.

intrinsic alloc;      // alloc(int nbytes) -> cptr   (zeroed; null if nbytes <= 0)
intrinsic free;       // free(cptr p)                (no-op on null)
intrinsic null;       // null() -> cptr              (the null pointer)
intrinsic is_null;    // is_null(cptr p) -> bool
intrinsic addr;       // addr(cptr p) -> uint64      (numeric address)
intrinsic from_addr;  // from_addr(int a) -> cptr    (numeric address -> pointer)
intrinsic offset;     // offset(cptr p, int bytes) -> cptr

// Typed reads: (cptr p, int byte_offset) -> value. Access is unaligned-safe.
intrinsic read_i8;    // -> int8
intrinsic read_i16;   // -> int16
intrinsic read_i32;   // -> int32
intrinsic read_i64;   // -> int64
intrinsic read_u8;    // -> uint8
intrinsic read_u16;   // -> uint16
intrinsic read_u32;   // -> uint32
intrinsic read_u64;   // -> uint64
intrinsic read_f32;   // -> dec32
intrinsic read_f64;   // -> dec64
intrinsic read_ptr;   // -> cptr
intrinsic read_cstr;  // read_cstr(cptr p, int off) -> string  (NUL-terminated)

// Typed writes: (cptr p, int byte_offset, value). Access is unaligned-safe.
intrinsic write_i8;   // write_i8(cptr p, int off, int v)
intrinsic write_i16;
intrinsic write_i32;
intrinsic write_i64;
intrinsic write_u8;
intrinsic write_u16;
intrinsic write_u32;
intrinsic write_u64;
intrinsic write_f32;  // write_f32(cptr p, int off, dec v)
intrinsic write_f64;
intrinsic write_ptr;  // write_ptr(cptr p, int off, cptr v)
intrinsic write_cstr; // write_cstr(cptr p, int off, string s) -> int  (bytes written, excl. NUL)
