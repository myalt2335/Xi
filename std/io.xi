// Xi standard library: io
//
// `ask` reads a line from stdin. The `wrt` / `wrtl` / `wrtr` streaming
// anchors are language-level syntax (`io::wrt << x << y;`) implemented by
// compiler intrinsics; they are declared here so the module file is the
// complete registry and `#include io` is required to use them.

extern f ask() -> string = "xi_io_ask";
extern f flush() -> void = "xi_io_flush";
// Clear the current terminal line and return to column 0; no-op if stdout is not a TTY.
extern f clrl() -> void = "xi_io_clrl";
extern f read_file(string path) -> string = "xi_io_read_file";
extern f write_file(string path, string contents) -> int = "xi_io_write_file";
extern f append_file(string path, string contents) -> int = "xi_io_append_file";
extern f file_exists(string path) -> int = "xi_io_file_exists";
// Raw-byte file IO: `read_bytes` returns an `array<uint8>` of the file's exact
// bytes (unlike `read_file`, which routes through a string and stops at a NUL),
// and `write_bytes` writes an `array<uint8>` back out. 1/0 success like write_file.
extern f read_bytes(string path) -> array<uint8> = "xi_io_read_bytes";
extern f write_bytes(string path, array<uint8> data) -> int = "xi_io_write_bytes";

intrinsic wrt;
intrinsic wrtl;
// Buffer the complete chain, emit it to stdout in one write, return to column 0,
// then flush. Intended for flicker-free progress/status updates on one line.
intrinsic wrtr;
