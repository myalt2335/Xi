// Xi standard library: bit
//
// Bit shifts and rotations. The bitwise LOGIC operators live in the language
// itself (`&` AND, `|` OR, `^^` XOR, `~` NOT), this module supplies the
// operations that either collide with existing syntax (`<<` is the io chain)
// or have no natural operator (rotations). Without `#include bit`, none of
// these names resolve.
//
//   bit::shl(x, n)          x shifted left by n bits   (grows; sized truncates)
//   bit::shr(x, n)          x shifted right by n bits  (floor, non-negative)
//   bit::rotl(x, n, width)  rotate left within `width` bits  (width <= 64)
//   bit::rotr(x, n, width)  rotate right within `width` bits (width <= 64)
//
// Rotations take an explicit width because a value carries no width of its own
// on this path: `bit::rotr(x, 7, 32)` rotates a 32-bit quantity.

extern f shl(int x, int n) -> int = "xi_bit_shl";
extern f shr(int x, int n) -> int = "xi_bit_shr";
extern f rotl(int x, int n, int width) -> int = "xi_bit_rotl";
extern f rotr(int x, int n, int width) -> int = "xi_bit_rotr";
