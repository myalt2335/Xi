// Xi standard library: mem - element-level buffer operations.
//
// Xi has no raw pointers, so these are the "memcpy / memset / memcmp" of the
// language, operating on the ELEMENTS of a numeric array (`array<uint8>`,
// `array<uint32>`, `array<int>`, ...). On an `array<uint8>` that is byte-level,
// which is exactly what hashing and message padding need. Offsets and `count`
// are element indices / counts.
//
//   mem::copy(dst, dstOff, src, srcOff, count)   copy `count` elements
//   mem::set(buf, off, value, count)             fill `count` elements with `value`
//   mem::cmp(a, aOff, b, bOff, count) -> int      -1 / 0 / 1 (first difference)
//
// `mem::copy` is overlap-safe within a single array (memmove semantics). Writing
// past the current length grows the array, so `mem::set(block, len, 0, 64 - len)`
// zero-pads a block. Like `arr::`, these are element-type-polymorphic compiler
// intrinsics rather than a single fixed runtime signature.

intrinsic copy;
intrinsic set;
intrinsic cmp;

// Endian pack/unpack between a byte buffer and an integer. Byte order is defined
// by array position (not the host CPU), so these behave identically on little-
// and big-endian machines:
//
//   mem::read_be(buf, off, nbytes) -> int    // MSB is at the lowest index
//   mem::read_le(buf, off, nbytes) -> int    // LSB is at the lowest index
//   mem::write_be(buf, off, value, nbytes)   // writes `nbytes`, MSB first
//   mem::write_le(buf, off, value, nbytes)   // writes `nbytes`, LSB first
//
// SHA-256 is big-endian: read a 32-bit word with mem::read_be(block, i*4, 4),
// and emit the digest / message length with mem::write_be(..., 4 or 8).
intrinsic read_be;
intrinsic read_le;
intrinsic write_be;
intrinsic write_le;
