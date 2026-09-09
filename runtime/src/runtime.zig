const std = @import("std");
const builtin = @import("builtin");

const FILE = opaque {};

extern "c" fn malloc(usize) ?*anyopaque;
extern "c" fn calloc(usize, usize) ?*anyopaque;
extern "c" fn realloc(?*anyopaque, usize) ?*anyopaque;
extern "c" fn free(?*anyopaque) void;
extern "c" fn abort() noreturn;
extern "c" fn exit(c_int) noreturn;
extern "c" fn atexit(*const fn () callconv(.c) void) c_int;

extern "c" fn memcpy(noalias ?*anyopaque, noalias ?*const anyopaque, usize) ?*anyopaque;
extern "c" fn memmove(?*anyopaque, ?*const anyopaque, usize) ?*anyopaque;
extern "c" fn memset(?*anyopaque, c_int, usize) ?*anyopaque;
extern "c" fn memcmp(?*const anyopaque, ?*const anyopaque, usize) c_int;
extern "c" fn strlen([*:0]const u8) usize;
extern "c" fn strcmp([*:0]const u8, [*:0]const u8) c_int;
extern "c" fn strcspn([*:0]const u8, [*:0]const u8) usize;
extern "c" fn strstr([*:0]const u8, [*:0]const u8) ?[*:0]const u8;
extern "c" fn strchr([*:0]const u8, c_int) ?[*:0]u8;
extern "c" fn strtod(noalias [*:0]const u8, noalias ?*?[*:0]u8) f64;
extern "c" fn strtol(noalias [*:0]const u8, noalias ?*?[*:0]u8, c_int) c_long;
extern "c" fn snprintf(noalias [*]u8, usize, noalias [*:0]const u8, ...) c_int;
extern "c" fn sscanf(noalias [*:0]const u8, noalias [*:0]const u8, ...) c_int;
extern "c" fn qsort(?*anyopaque, usize, usize, *const fn (?*const anyopaque, ?*const anyopaque) callconv(.c) c_int) void;
extern "c" fn time(?*XiTimeT) XiTimeT;
extern "c" fn clock() c_long;
extern "c" fn strftime(noalias [*]u8, usize, noalias [*:0]const u8, noalias *const XiTm) usize;
extern "c" fn localtime(?*const XiTimeT) ?*XiTm;
extern "c" fn gmtime(?*const XiTimeT) ?*XiTm;
extern "c" fn mktime(*XiTm) XiTimeT;
extern "c" fn system([*:0]const u8) c_int;
extern "c" fn getenv([*:0]const u8) ?[*:0]const u8;

extern "c" fn fopen([*:0]const u8, [*:0]const u8) ?*FILE;
extern "c" fn fclose(*FILE) c_int;
extern "c" fn fseek(*FILE, c_long, c_int) c_int;
extern "c" fn ftell(*FILE) c_long;
extern "c" fn fread(?*anyopaque, usize, usize, *FILE) usize;
extern "c" fn fwrite(?*const anyopaque, usize, usize, *FILE) usize;
extern "c" fn fgets([*]u8, c_int, *FILE) ?[*]u8;
extern "c" fn fgetc(*FILE) c_int;
extern "c" fn fputc(c_int, *FILE) c_int;
extern "c" fn fputs([*:0]const u8, *FILE) c_int;
extern "c" fn fflush(?*FILE) c_int;
extern "c" fn ferror(*FILE) c_int;
extern "c" fn printf(noalias [*:0]const u8, ...) c_int;
extern "c" fn fprintf(noalias *FILE, noalias [*:0]const u8, ...) c_int;
extern "c" fn remove([*:0]const u8) c_int;
extern "c" fn rename([*:0]const u8, [*:0]const u8) c_int;
extern "c" fn signal(c_int, ?*const fn (c_int) callconv(.c) void) ?*const fn (c_int) callconv(.c) void;

const XiTimeT = if (builtin.os.tag == .windows) i64 else c_long;
const XiTm = extern struct {
    tm_sec: c_int,
    tm_min: c_int,
    tm_hour: c_int,
    tm_mday: c_int,
    tm_mon: c_int,
    tm_year: c_int,
    tm_wday: c_int,
    tm_yday: c_int,
    tm_isdst: c_int,
};

const XiTimespec = extern struct {
    tv_sec: XiTimeT,
    tv_nsec: c_long,
};

const XiFileTime = extern struct {
    dwLowDateTime: u32,
    dwHighDateTime: u32,
};

const XiStat64 = extern struct {
    st_dev: c_uint,
    st_ino: c_ushort,
    st_mode: c_ushort,
    st_nlink: c_short,
    st_uid: c_short,
    st_gid: c_short,
    st_rdev: c_uint,
    st_size: i64,
    st_atime: i64,
    st_mtime: i64,
    st_ctime: i64,
};

const SIG_DFL: ?*const fn (c_int) callconv(.c) void = @ptrFromInt(0);
const SIG_IGN: ?*const fn (c_int) callconv(.c) void = @ptrFromInt(1);
const SIGINT: c_int = 2;
const SIGTERM: c_int = 15;
const SIGABRT: c_int = 22;

const SEEK_SET: c_int = 0;
const SEEK_END: c_int = 2;
const EOF: c_int = -1;

extern "kernel32" fn GetSystemTimeAsFileTime(*XiFileTime) callconv(.winapi) void;
extern "kernel32" fn QueryPerformanceCounter(*i64) callconv(.winapi) c_int;
extern "kernel32" fn QueryPerformanceFrequency(*i64) callconv(.winapi) c_int;
extern "kernel32" fn Sleep(c_ulong) callconv(.winapi) void;
extern "c" fn clock_gettime(c_int, *XiTimespec) c_int;
extern "c" fn nanosleep(*const XiTimespec, ?*XiTimespec) c_int;
extern "c" fn getcwd([*]u8, usize) ?[*]u8;
extern "c" fn chdir([*:0]const u8) c_int;
extern "c" fn mkdir([*:0]const u8, c_int) c_int;
extern "c" fn rmdir([*:0]const u8) c_int;
extern "c" fn getpid() c_int;
extern "c" fn getppid() c_int;
extern "c" fn setenv([*:0]const u8, [*:0]const u8, c_int) c_int;
extern "c" fn unsetenv([*:0]const u8) c_int;
extern "c" var environ: ?[*]?[*:0]u8;
extern "c" fn __p__environ() *?[*]?[*:0]u8;
extern "c" fn _getcwd([*]u8, c_int) ?[*]u8;
extern "c" fn _chdir([*:0]const u8) c_int;
extern "c" fn _mkdir([*:0]const u8) c_int;
extern "c" fn _rmdir([*:0]const u8) c_int;
extern "c" fn _getpid() c_int;
extern "c" fn _putenv_s([*:0]const u8, [*:0]const u8) c_int;
extern "c" fn _popen([*:0]const u8, [*:0]const u8) ?*FILE;
extern "c" fn _pclose(*FILE) c_int;
extern "c" fn popen([*:0]const u8, [*:0]const u8) ?*FILE;
extern "c" fn pclose(*FILE) c_int;
extern "c" fn _fileno(*FILE) c_int;
extern "c" fn _isatty(c_int) c_int;
extern "c" fn fileno(*FILE) c_int;
extern "c" fn isatty(c_int) c_int;
extern "c" fn _stat64([*:0]const u8, *XiStat64) c_int;
extern "c" fn stat64([*:0]const u8, *XiStat64) c_int;

extern "c" fn sin(f64) f64;
extern "c" fn cos(f64) f64;
extern "c" fn tan(f64) f64;
extern "c" fn sqrt(f64) f64;
extern "c" fn fabs(f64) f64;
extern "c" fn floor(f64) f64;
extern "c" fn ceil(f64) f64;
extern "c" fn round(f64) f64;
extern "c" fn trunc(f64) f64;
extern "c" fn log(f64) f64;
extern "c" fn log10(f64) f64;
extern "c" fn log2(f64) f64;
const c_exp = @extern(*const fn (f64) callconv(.c) f64, .{ .name = "exp" });
extern "c" fn pow(f64, f64) f64;
extern "c" fn fmin(f64, f64) f64;
extern "c" fn fmax(f64, f64) f64;
extern "c" fn atan2(f64, f64) f64;
extern "c" fn ldexp(f64, c_int) f64;
extern "c" fn frexp(f64, *c_int) f64;
extern "c" fn lround(f64) c_long;
extern "c" fn llround(f64) c_longlong;

export fn stdinFile() callconv(.c) *FILE {
    if (builtin.os.tag == .windows) return __acrt_iob_func(0) else return stdin;
}
export fn stdoutFile() callconv(.c) *FILE {
    if (builtin.os.tag == .windows) return __acrt_iob_func(1) else return stdout;
}
export fn stderrFile() callconv(.c) *FILE {
    if (builtin.os.tag == .windows) return __acrt_iob_func(2) else return stderr;
}
extern "c" fn __acrt_iob_func(c_uint) *FILE;
extern "c" var stdin: *FILE;
extern "c" var stdout: *FILE;
extern "c" var stderr: *FILE;

export fn isalpha(c: u8) callconv(.c) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z');
}
fn cIsNan(x: f64) bool {
    return x != x;
}
fn cIsInf(x: f64) bool {
    return x == std.math.inf(f64) or x == -std.math.inf(f64);
}
fn cSignbit(x: f64) bool {
    return (@as(u64, @bitCast(x)) >> 63) != 0;
}
const INFINITY: f64 = std.math.inf(f64);
const NAN: f64 = std.math.nan(f64);

const INT64_MAX: i64 = std.math.maxInt(i64);
const INT64_MIN: i64 = std.math.minInt(i64);
const UINT64_MAX: u64 = std.math.maxInt(u64);
const SIZE_MAX: usize = std.math.maxInt(usize);
const UINT16_MAX: usize = 65535;

const XI_ERR_NONE: c_int = 0;
const XI_ERR_INVALID_ARGUMENT: c_int = 1;
const XI_ERR_OUT_OF_RANGE: c_int = 2;
const XI_ERR_NOT_FOUND: c_int = 3;
const XI_ERR_OVERFLOW: c_int = 4;
const XI_ERR_OUT_OF_MEMORY: c_int = 5;
const XI_ERR_INTERNAL: c_int = 6;

const XI_NUM_INT: u8 = 0;
const XI_NUM_DEC: u8 = 1;
const XI_NUM_WIDE: u8 = 2;
const XI_NUM_BIG: u8 = 3;
const XI_NUM_BIGDEC: u8 = 4;
const XI_NUM_SOFTFLOAT: u8 = 5;

const XI_SF_NORMAL: u8 = 0;
const XI_SF_ZERO: u8 = 1;
const XI_SF_INF: u8 = 2;
const XI_SF_NAN: u8 = 3;

const XI_VALUE_NULL: u8 = 0;
const XI_VALUE_NUM: u8 = 1;
const XI_VALUE_STR: u8 = 2;
const XI_VALUE_ARR: u8 = 3;
const XI_VALUE_MAP: u8 = 4;
const XI_VALUE_STRUCT: u8 = 5;
const XI_VALUE_BOOL: u8 = 6;
const XI_VALUE_OPAQUE: u8 = 7;

const XI_STR_KIND_INTERNED: u8 = 1;
const XI_STR_KIND_VIEW: u8 = 2;

const XiBig = struct {
    sign: i32,
    len: usize,
    cap: usize,
    limbs: [*]u32,
};

const XiNum = struct {
    tag: u8,
    i: i64,
    d: f64,
    wide_bits: u16,
    wide: [32]u8,
    big: ?*XiBig,
    dec_exp: i32,
    soft_kind: u8,
    soft_sign: i8,
    gc_mark: u8,
    gc_next: ?*XiNum,
};

const XiValue = struct {
    tag: u8 = XI_VALUE_NULL,
    ptr: ?*anyopaque = null,
    boolean: bool = false,
};

const XiStrInternEntry = struct {
    value: [*]u8,
    len: usize,
    hash: u64,
    kind: u8,
    owner: ?[*:0]const u8,
    gc_mark: u8,
    bucket_next: ?*XiStrInternEntry,
    gc_next: ?*XiStrInternEntry,
};

const XiStrArray = extern struct {
    len: usize,
    cap: usize,
    items: ?[*]XiValue,
    packed_nums: ?[*]u8,
    packed_bits: u16,
    packed_unsigned: u8,
    i64_cache: ?[*]i64,
    gc_mark: u8,
    gc_next: ?*XiStrArray,
};

comptime {
    if (@offsetOf(XiStrArray, "len") != 0 or
        @offsetOf(XiStrArray, "packed_nums") != 24 or
        @offsetOf(XiStrArray, "packed_bits") != 32 or
        @offsetOf(XiStrArray, "packed_unsigned") != 34)
        @compileError("XiStrArray guarded-access ABI layout changed");
}

const XiMap = struct {
    len: usize,
    used: usize,
    cap: usize,
    keys: ?[*]?[*:0]const u8,
    values: ?[*]XiValue,
    hashes: ?[*]u64,
    states: ?[*]u8,
    gc_mark: u8,
    gc_next: ?*XiMap,
};

const XiStruct = struct {
    len: usize,
    cap: usize,
    fields: ?[*]?[*:0]const u8,
    values: ?[*]XiValue,
    gc_mark: u8,
    gc_next: ?*XiStruct,
};

const XiClosure = struct {
    code: ?*anyopaque,
    env: ?*anyopaque,
    env_bytes: usize,
    gc_mark: u8,
    gc_next: ?*XiClosure,
};

const Str = [*:0]const u8;
const OptStr = ?[*:0]const u8;

fn ptrEq(a: ?*const anyopaque, b: ?*const anyopaque) bool {
    return @intFromPtr(a) == @intFromPtr(b);
}

fn strAddr(s: OptStr) usize {
    return @intFromPtr(s);
}

fn cnew(comptime T: type) *T {
    return @ptrCast(@alignCast(malloc(@sizeOf(T)) orelse xi_oom()));
}

fn cfree(p: anytype) void {
    free(@ptrCast(p));
}

export fn xi_oom() callconv(.c) noreturn {
    _ = fputs("xi_runtime: out of memory\n", stderrFile());
    abort();
}

fn fatal(comptime msg: [:0]const u8) noreturn {
    _ = fputs(msg.ptr, stderrFile());
    abort();
}

var xi_gc_head: ?*XiNum = null;
var xi_gc_num_lookup: ?[*]?*XiNum = null;
var xi_gc_num_lookup_cap: usize = 0;
const XI_GC_LOOKUP_STR: u8 = 2;
const XI_GC_LOOKUP_ARR: u8 = 3;
const XI_GC_LOOKUP_MAP: u8 = 4;
const XI_GC_LOOKUP_STRUCT: u8 = 5;
const XI_GC_LOOKUP_CLOSURE: u8 = 6;

const XiGcLookupEntry = struct {
    key: usize,
    target: ?*anyopaque,
    kind: u8,
};

var xi_gc_lookup: ?[*]XiGcLookupEntry = null;
var xi_gc_lookup_cap: usize = 0;
var xi_gc_str_head: ?*XiStrInternEntry = null;
var xi_str_intern_buckets: ?[*]?*XiStrInternEntry = null;
var xi_str_intern_cap: usize = 0;
var xi_str_intern_used: usize = 0;
var xi_str_lookup_cache_value: OptStr = null;
var xi_str_lookup_cache_entry: ?*XiStrInternEntry = null;
var xi_gc_arr_head: ?*XiStrArray = null;
var xi_gc_map_head: ?*XiMap = null;
var xi_gc_struct_head: ?*XiStruct = null;
var xi_gc_closure_head: ?*XiClosure = null;

var xi_num_freelist: ?*XiNum = null;
var xi_num_freelist_len: usize = 0;
const XI_NUM_FREELIST_CAP: usize = 8192;

var xi_num_int_cache_initialized: bool = false;
var xi_gc_initialized: bool = false;
var xi_gc_collecting: bool = false;
var xi_gc_pause_depth: usize = 0;
// 0 = automatic (the compatibility default), 1 = explicit collections only,
// 2 = collection disabled.  Even off-mode allocations remain linked so the
var xi_gc_mode: u8 = 0;
var xi_gc_collections: usize = 0;
var xi_gc_allocs_since_collect: usize = 0;
const XI_GC_MIN_THRESHOLD: usize = 4096;
var xi_gc_collect_threshold: usize = XI_GC_MIN_THRESHOLD;

var xi_cli_args_loaded: bool = false;
var xi_cli_argc: i64 = 0;
var xi_cli_argv: ?[*]OptStr = null;
export var xi_cli_argv_storage: ?[*]OptStr = null;

var xi_last_err_code: c_int = XI_ERR_NONE;
var xi_last_err_message: [256]u8 = std.mem.zeroes([256]u8);
const XI_NUM_INT_CACHE_MIN: i64 = -32;
const XI_NUM_INT_CACHE_MAX: i64 = 4096;
const XI_NUM_INT_CACHE_SIZE: usize = @intCast((XI_NUM_INT_CACHE_MAX - XI_NUM_INT_CACHE_MIN) + 1);
var xi_num_int_cache: [XI_NUM_INT_CACHE_SIZE]XiNum = undefined;

export fn xi_err_set_message(code: c_int, message: OptStr) callconv(.c) void {
    xi_last_err_code = code;
    _ = snprintf(&xi_last_err_message, xi_last_err_message.len, "%s", if (message) |m| m else @as(Str, ""));
}

fn xi_gc_pause_begin() void {
    xi_gc_pause_depth += 1;
}

fn xi_gc_pause_end() void {
    if (xi_gc_pause_depth > 0) xi_gc_pause_depth -= 1;
}

export fn xi_err_code() callconv(.c) c_int {
    return xi_last_err_code;
}

export fn xi_err_message() callconv(.c) Str {
    if (xi_last_err_code == XI_ERR_NONE) return "";
    return @ptrCast(&xi_last_err_message);
}

export fn xi_err_clear() callconv(.c) void {
    xi_last_err_code = XI_ERR_NONE;
    xi_last_err_message[0] = 0;
}

export fn xi_err_set(code: c_int, message: OptStr) callconv(.c) void {
    xi_err_set_message(code, message);
}

fn xi_runtime_gc_cleanup() callconv(.c) void {
    var current = xi_gc_head;
    while (current) |cur| {
        const next = cur.gc_next;
        if ((cur.tag == XI_NUM_BIG or cur.tag == XI_NUM_BIGDEC or cur.tag == XI_NUM_SOFTFLOAT) and cur.big != null) {
            xi_big_free(cur.big);
        }
        free(cur);
        current = next;
    }
    xi_gc_head = null;

    var pooled = xi_num_freelist;
    while (pooled) |p| {
        const next = p.gc_next;
        free(p);
        pooled = next;
    }
    xi_num_freelist = null;
    xi_num_freelist_len = 0;

    var str_node = xi_gc_str_head;
    while (str_node) |sn| {
        const next = sn.gc_next;
        if (sn.kind == XI_STR_KIND_INTERNED) cfree(sn.value);
        free(sn);
        str_node = next;
    }
    xi_gc_str_head = null;
    xi_str_lookup_cache_value = null;
    xi_str_lookup_cache_entry = null;
    cfree(xi_str_intern_buckets);
    xi_str_intern_buckets = null;
    xi_str_intern_cap = 0;
    xi_str_intern_used = 0;

    var arr = xi_gc_arr_head;
    while (arr) |a| {
        const next = a.gc_next;
        cfree(a.items);
        cfree(a.packed_nums);
        cfree(a.i64_cache);
        free(a);
        arr = next;
    }
    xi_gc_arr_head = null;

    var map = xi_gc_map_head;
    while (map) |m| {
        const next = m.gc_next;
        cfree(m.keys);
        cfree(m.values);
        cfree(m.hashes);
        cfree(m.states);
        free(m);
        map = next;
    }
    xi_gc_map_head = null;

    var st = xi_gc_struct_head;
    while (st) |s| {
        const next = s.gc_next;
        cfree(s.fields);
        cfree(s.values);
        free(s);
        st = next;
    }
    xi_gc_struct_head = null;

    var closure = xi_gc_closure_head;
    while (closure) |cl| {
        const next = cl.gc_next;
        free(cl.env);
        free(cl);
        closure = next;
    }
    xi_gc_closure_head = null;

    cfree(xi_cli_argv_storage);
    xi_cli_argv_storage = null;
    xi_cli_argv = null;
    xi_cli_argc = 0;
}

fn xi_runtime_gc_ensure_init() void {
    if (xi_gc_initialized) return;
    xi_gc_initialized = true;
    _ = atexit(&xi_runtime_gc_cleanup);
}

const WINAPI: std.builtin.CallingConvention = .c;

const MEMORY_BASIC_INFORMATION = extern struct {
    BaseAddress: usize,
    AllocationBase: usize,
    AllocationProtect: u32,
    __pad0: u32,
    RegionSize: usize,
    State: u32,
    Protect: u32,
    Type: u32,
    __pad1: u32,
};
extern "kernel32" fn GetCurrentThreadStackLimits(LowLimit: *usize, HighLimit: *usize) callconv(WINAPI) void;
extern "kernel32" fn VirtualQuery(lpAddress: usize, lpBuffer: *MEMORY_BASIC_INFORMATION, dwLength: usize) callconv(WINAPI) usize;
const MEM_COMMIT: u32 = 0x1000;
const PAGE_GUARD: u32 = 0x100;
const PAGE_NOACCESS: u32 = 0x01;
const PAGE_EXECUTE: u32 = 0x10;

fn xi_gc_get_stack_bounds(low: *usize, high: *usize) bool {
    if (builtin.os.tag == .windows) {
        var stack_low: usize = 0;
        var stack_high: usize = 0;
        GetCurrentThreadStackLimits(&stack_low, &stack_high);
        if (stack_low == 0 or stack_high <= stack_low) return false;
        low.* = stack_low;
        high.* = stack_high;
        return true;
    } else {
        const f = fopen("/proc/self/maps", "r") orelse return false;
        defer _ = fclose(f);
        var line: [512]u8 = undefined;
        while (fgets(&line, line.len, f)) |_| {
            const lp: [*:0]const u8 = @ptrCast(&line);
            if (strstr(lp, "[stack") == null) continue;
            var start: c_ulonglong = 0;
            var end: c_ulonglong = 0;
            if (sscanf(lp, "%llx-%llx", &start, &end) == 2 and end > start) {
                low.* = @intCast(start);
                high.* = @intCast(end);
                return true;
            }
        }
        return false;
    }
}

fn xi_gc_align_up(value: usize) usize {
    const align_v = @sizeOf(usize);
    const rem = value % align_v;
    if (rem == 0) return value;
    return value + (align_v - rem);
}

fn xi_gc_align_down(value: usize) usize {
    const align_v = @sizeOf(usize);
    return value - (value % align_v);
}

fn xi_gc_lookup_index(addr: usize, cap: usize) usize {
    return ((addr >> 4) *% @as(usize, 0x9E3779B185EBCA87)) & (cap - 1);
}

fn xi_gc_build_num_lookup() void {
    cfree(xi_gc_num_lookup);
    xi_gc_num_lookup = null;
    xi_gc_num_lookup_cap = 0;
    var count: usize = 0;
    var cur = xi_gc_head;
    while (cur) |n| : (cur = n.gc_next) count += 1;
    if (count == 0) return;
    if (count > SIZE_MAX / 2) fatal("xi_runtime: GC numeric lookup overflow\n");
    var cap: usize = 16;
    while (cap < count * 2) {
        if (cap > SIZE_MAX / 2) fatal("xi_runtime: GC numeric lookup overflow\n");
        cap *= 2;
    }
    const table: [*]?*XiNum = @ptrCast(@alignCast(calloc(cap, @sizeOf(?*XiNum)) orelse xi_oom()));
    cur = xi_gc_head;
    while (cur) |n| : (cur = n.gc_next) {
        var idx = xi_gc_lookup_index(@intFromPtr(n), cap);
        while (table[idx] != null) idx = (idx + 1) & (cap - 1);
        table[idx] = n;
    }
    xi_gc_num_lookup = table;
    xi_gc_num_lookup_cap = cap;
}

fn xi_gc_drop_num_lookup() void {
    cfree(xi_gc_num_lookup);
    xi_gc_num_lookup = null;
    xi_gc_num_lookup_cap = 0;
}

fn xi_gc_lookup_insert(table: [*]XiGcLookupEntry, cap: usize, key: usize, target: *anyopaque, kind: u8) void {
    var idx = xi_gc_lookup_index(key, cap);
    while (table[idx].key != 0) idx = (idx + 1) & (cap - 1);
    table[idx].key = key;
    table[idx].target = target;
    table[idx].kind = kind;
}

fn xi_gc_build_lookup() void {
    cfree(xi_gc_lookup);
    xi_gc_lookup = null;
    xi_gc_lookup_cap = 0;
    var count: usize = 0;
    var str = xi_gc_str_head;
    while (str) |s| : (str = s.gc_next) count += 1;
    var arr = xi_gc_arr_head;
    while (arr) |a| : (arr = a.gc_next) count += 1;
    var map = xi_gc_map_head;
    while (map) |m| : (map = m.gc_next) count += 1;
    var structure = xi_gc_struct_head;
    while (structure) |s| : (structure = s.gc_next) count += 1;
    var closure = xi_gc_closure_head;
    while (closure) |c| : (closure = c.gc_next) count += 1;
    if (count == 0) return;
    if (count > SIZE_MAX / 2) fatal("xi_runtime: GC lookup overflow\n");
    var cap: usize = 16;
    while (cap < count * 2) {
        if (cap > SIZE_MAX / 2) fatal("xi_runtime: GC lookup overflow\n");
        cap *= 2;
    }
    const table: [*]XiGcLookupEntry = @ptrCast(@alignCast(calloc(cap, @sizeOf(XiGcLookupEntry)) orelse xi_oom()));
    str = xi_gc_str_head;
    while (str) |s| : (str = s.gc_next)
        xi_gc_lookup_insert(table, cap, @intFromPtr(s.value), @ptrCast(s), XI_GC_LOOKUP_STR);
    arr = xi_gc_arr_head;
    while (arr) |a| : (arr = a.gc_next)
        xi_gc_lookup_insert(table, cap, @intFromPtr(a), @ptrCast(a), XI_GC_LOOKUP_ARR);
    map = xi_gc_map_head;
    while (map) |m| : (map = m.gc_next)
        xi_gc_lookup_insert(table, cap, @intFromPtr(m), @ptrCast(m), XI_GC_LOOKUP_MAP);
    structure = xi_gc_struct_head;
    while (structure) |s| : (structure = s.gc_next)
        xi_gc_lookup_insert(table, cap, @intFromPtr(s), @ptrCast(s), XI_GC_LOOKUP_STRUCT);
    closure = xi_gc_closure_head;
    while (closure) |c| : (closure = c.gc_next)
        xi_gc_lookup_insert(table, cap, @intFromPtr(c), @ptrCast(c), XI_GC_LOOKUP_CLOSURE);
    xi_gc_lookup = table;
    xi_gc_lookup_cap = cap;
}

fn xi_gc_lookup_target(addr: usize, kind: u8) ?*anyopaque {
    const table = xi_gc_lookup orelse return null;
    var idx = xi_gc_lookup_index(addr, xi_gc_lookup_cap);
    while (table[idx].key != 0) {
        if (table[idx].key == addr and table[idx].kind == kind) return table[idx].target;
        idx = (idx + 1) & (xi_gc_lookup_cap - 1);
    }
    return null;
}

fn xi_gc_drop_lookup() void {
    cfree(xi_gc_lookup);
    xi_gc_lookup = null;
    xi_gc_lookup_cap = 0;
}

fn xi_gc_mark_num_addr(addr: usize) void {
    if (addr == 0) return;
    if (xi_gc_num_lookup) |table| {
        var idx = xi_gc_lookup_index(addr, xi_gc_num_lookup_cap);
        while (table[idx]) |candidate| {
            if (@intFromPtr(candidate) == addr) {
                if (candidate.gc_mark == 0) candidate.gc_mark = 1;
                return;
            }
            idx = (idx + 1) & (xi_gc_num_lookup_cap - 1);
        }
        return;
    }
    var cur = xi_gc_head;
    while (cur) |c| {
        if (@intFromPtr(c) == addr) {
            if (c.gc_mark == 0) c.gc_mark = 1;
            return;
        }
        cur = c.gc_next;
    }
}

fn xi_gc_mark_str_addr(addr: usize) void {
    if (addr == 0) return;
    var cur = xi_gc_str_head;
    if (xi_gc_lookup != null) {
        const target = xi_gc_lookup_target(addr, XI_GC_LOOKUP_STR) orelse return;
        cur = @ptrCast(@alignCast(target));
    }
    while (cur) |c| {
        if (@intFromPtr(c.value) == addr) {
            if (c.gc_mark == 0) {
                c.gc_mark = 1;
                if (c.kind == XI_STR_KIND_VIEW) {
                    if (c.owner) |ow| xi_gc_mark_str_addr(@intFromPtr(ow));
                }
            }
            return;
        }
        cur = c.gc_next;
    }
}

fn xi_gc_mark_arr_addr(addr: usize) void {
    if (addr == 0) return;
    var cur = xi_gc_arr_head;
    if (xi_gc_lookup != null) {
        const target = xi_gc_lookup_target(addr, XI_GC_LOOKUP_ARR) orelse return;
        cur = @ptrCast(@alignCast(target));
    }
    while (cur) |c| {
        if (@intFromPtr(c) == addr) {
            if (c.gc_mark != 0) return;
            c.gc_mark = 1;
            if (c.packed_nums == null) {
                if (c.items) |items| {
                    var i: usize = 0;
                    while (i < c.len) : (i += 1) xi_gc_mark_value(items[i]);
                }
            }
            return;
        }
        cur = c.gc_next;
    }
}

fn xi_gc_mark_map_addr(addr: usize) void {
    if (addr == 0) return;
    var cur = xi_gc_map_head;
    if (xi_gc_lookup != null) {
        const target = xi_gc_lookup_target(addr, XI_GC_LOOKUP_MAP) orelse return;
        cur = @ptrCast(@alignCast(target));
    }
    while (cur) |c| {
        if (@intFromPtr(c) == addr) {
            if (c.gc_mark != 0) return;
            c.gc_mark = 1;
            var i: usize = 0;
            while (i < c.cap) : (i += 1) {
                const states = c.states orelse continue;
                if (states[i] != 1) continue;
                xi_gc_mark_str_addr(strAddr(c.keys.?[i]));
                xi_gc_mark_value(c.values.?[i]);
            }
            return;
        }
        cur = c.gc_next;
    }
}

fn xi_gc_mark_struct_addr(addr: usize) void {
    if (addr == 0) return;
    var cur = xi_gc_struct_head;
    if (xi_gc_lookup != null) {
        const target = xi_gc_lookup_target(addr, XI_GC_LOOKUP_STRUCT) orelse return;
        cur = @ptrCast(@alignCast(target));
    }
    while (cur) |c| {
        if (@intFromPtr(c) == addr) {
            if (c.gc_mark != 0) return;
            c.gc_mark = 1;
            var i: usize = 0;
            while (i < c.len) : (i += 1) {
                xi_gc_mark_str_addr(strAddr(c.fields.?[i]));
                xi_gc_mark_value(c.values.?[i]);
            }
            return;
        }
        cur = c.gc_next;
    }
}

fn xi_gc_mark_closure_addr(addr: usize) void {
    if (addr == 0) return;
    var cur = xi_gc_closure_head;
    if (xi_gc_lookup != null) {
        const target = xi_gc_lookup_target(addr, XI_GC_LOOKUP_CLOSURE) orelse return;
        cur = @ptrCast(@alignCast(target));
    }
    while (cur) |c| {
        if (@intFromPtr(c) == addr) {
            if (c.gc_mark != 0) return;
            c.gc_mark = 1;
            if (c.env != null and c.env_bytes >= @sizeOf(usize)) {
                const words: [*]usize = @ptrCast(@alignCast(c.env.?));
                const nwords = c.env_bytes / @sizeOf(usize);
                var i: usize = 0;
                while (i < nwords) : (i += 1) xi_gc_mark_ptr(words[i]);
            }
            return;
        }
        cur = c.gc_next;
    }
}

fn xi_gc_mark_ptr(addr: usize) void {
    if (addr == 0) return;
    xi_gc_mark_num_addr(addr);
    xi_gc_mark_str_addr(addr);
    xi_gc_mark_arr_addr(addr);
    xi_gc_mark_map_addr(addr);
    xi_gc_mark_struct_addr(addr);
    xi_gc_mark_closure_addr(addr);
}

fn xi_gc_mark_value(value: XiValue) void {
    switch (value.tag) {
        XI_VALUE_NUM => xi_gc_mark_num_addr(@intFromPtr(value.ptr)),
        XI_VALUE_STR => xi_gc_mark_str_addr(@intFromPtr(value.ptr)),
        XI_VALUE_ARR => xi_gc_mark_arr_addr(@intFromPtr(value.ptr)),
        XI_VALUE_MAP => xi_gc_mark_map_addr(@intFromPtr(value.ptr)),
        XI_VALUE_STRUCT => xi_gc_mark_struct_addr(@intFromPtr(value.ptr)),
        XI_VALUE_OPAQUE => xi_gc_mark_ptr(@intFromPtr(value.ptr)),
        else => {},
    }
}

fn xi_gc_mark_stack_region(low: usize, high: usize) void {
    if (high <= low) return;
    const start = xi_gc_align_up(low);
    const end = xi_gc_align_down(high);
    if (end <= start) return;
    var addr = start;
    while (addr < end) : (addr += @sizeOf(usize)) {
        const word = @as(*const usize, @ptrFromInt(addr)).*;
        xi_gc_mark_ptr(word);
    }
}

fn xi_gc_windows_protect_readable(protect: u32) bool {
    const base = protect & 0xff;
    if (base == PAGE_NOACCESS) return false;
    if (base == PAGE_EXECUTE) return false;
    return true;
}

fn xi_gc_mark_windows_stack(low: usize, high: usize) void {
    var cursor = low;
    while (cursor < high) {
        var mbi: MEMORY_BASIC_INFORMATION = undefined;
        const queried = VirtualQuery(cursor, &mbi, @sizeOf(MEMORY_BASIC_INFORMATION));
        if (queried == 0) break;
        const region_start = mbi.BaseAddress;
        const region_end = region_start + mbi.RegionSize;
        if (region_end <= cursor) break;
        const scan_start = if (cursor > region_start) cursor else region_start;
        const scan_end = if (high < region_end) high else region_end;
        if (scan_start < scan_end and mbi.State == MEM_COMMIT and
            (mbi.Protect & PAGE_GUARD) == 0 and xi_gc_windows_protect_readable(mbi.Protect))
        {
            xi_gc_mark_stack_region(scan_start, scan_end);
        }
        cursor = region_end;
    }
}

inline fn captureRegisters(buf: *[8]usize) void {
    if (builtin.cpu.arch == .x86_64) {
        buf[0] = asm volatile ("" : [value] "={rbx}" (-> usize));
        buf[1] = asm volatile ("" : [value] "={rbp}" (-> usize));
        buf[2] = asm volatile ("" : [value] "={r12}" (-> usize));
        buf[3] = asm volatile ("" : [value] "={r13}" (-> usize));
        buf[4] = asm volatile ("" : [value] "={r14}" (-> usize));
        buf[5] = asm volatile ("" : [value] "={r15}" (-> usize));
        buf[6] = asm volatile ("" : [value] "={rdi}" (-> usize));
        buf[7] = asm volatile ("" : [value] "={rsi}" (-> usize));
    }
}

fn xi_gc_rebuild_string_buckets() void {
    if (xi_gc_str_head == null) {
        cfree(xi_str_intern_buckets);
        xi_str_intern_buckets = null;
        xi_str_intern_cap = 0;
        xi_str_intern_used = 0;
        return;
    }
    var live_entries: usize = 0;
    var e = xi_gc_str_head;
    while (e) |entry| : (e = entry.gc_next) {
        if (entry.kind == XI_STR_KIND_INTERNED) live_entries += 1;
    }
    if (live_entries == 0) {
        cfree(xi_str_intern_buckets);
        xi_str_intern_buckets = null;
        xi_str_intern_cap = 0;
        xi_str_intern_used = 0;
        return;
    }
    const target_cap: usize = if (live_entries >= (SIZE_MAX - 1) / 2) SIZE_MAX else (live_entries * 2 + 1);
    var new_cap: usize = 64;
    while (new_cap < target_cap) {
        if (new_cap > SIZE_MAX / 2) {
            new_cap = target_cap;
            break;
        }
        new_cap *= 2;
    }
    const new_buckets: [*]?*XiStrInternEntry = @ptrCast(@alignCast(calloc(new_cap, @sizeOf(?*XiStrInternEntry)) orelse xi_oom()));
    var used: usize = 0;
    e = xi_gc_str_head;
    while (e) |entry| : (e = entry.gc_next) {
        if (entry.kind != XI_STR_KIND_INTERNED) {
            entry.bucket_next = null;
            continue;
        }
        const idx: usize = @intCast(entry.hash & @as(u64, new_cap - 1));
        entry.bucket_next = new_buckets[idx];
        new_buckets[idx] = entry;
        used += 1;
    }
    cfree(xi_str_intern_buckets);
    xi_str_intern_buckets = new_buckets;
    xi_str_intern_cap = new_cap;
    xi_str_intern_used = used;
}

fn xi_gc_clear_marks() void {
    var n = xi_gc_head;
    while (n) |c| : (n = c.gc_next) c.gc_mark = 0;
    var s = xi_gc_str_head;
    while (s) |c| : (s = c.gc_next) c.gc_mark = 0;
    var a = xi_gc_arr_head;
    while (a) |c| : (a = c.gc_next) c.gc_mark = 0;
    var m = xi_gc_map_head;
    while (m) |c| : (m = c.gc_next) c.gc_mark = 0;
    var st = xi_gc_struct_head;
    while (st) |c| : (st = c.gc_next) c.gc_mark = 0;
    var cl = xi_gc_closure_head;
    while (cl) |c| : (cl = c.gc_next) c.gc_mark = 0;
}

fn xi_gc_mark_cli_args() void {
    const argv = xi_cli_argv orelse return;
    if (xi_cli_argc <= 0) return;
    var i: i64 = 0;
    while (i < xi_cli_argc) : (i += 1) {
        xi_gc_mark_ptr(strAddr(argv[@intCast(i)]));
    }
}

fn xi_gc_sweep_nums() usize {
    var live: usize = 0;
    var prev: ?*XiNum = null;
    var current = xi_gc_head;
    while (current) |cur| {
        const next = cur.gc_next;
        if (cur.gc_mark == 0) {
            if ((cur.tag == XI_NUM_BIG or cur.tag == XI_NUM_BIGDEC or cur.tag == XI_NUM_SOFTFLOAT) and cur.big != null) {
                xi_big_free(cur.big);
            }
            cur.big = null;
            if (prev == null) {
                xi_gc_head = next;
            } else {
                prev.?.gc_next = next;
            }
            if (xi_num_freelist_len < XI_NUM_FREELIST_CAP) {
                cur.gc_next = xi_num_freelist;
                xi_num_freelist = cur;
                xi_num_freelist_len += 1;
            } else {
                free(cur);
            }
        } else {
            cur.gc_mark = 0;
            prev = cur;
            live += 1;
        }
        current = next;
    }
    return live;
}

fn xi_gc_sweep_strings() usize {
    var live: usize = 0;
    xi_str_lookup_cache_value = null;
    xi_str_lookup_cache_entry = null;
    var prev: ?*XiStrInternEntry = null;
    var current = xi_gc_str_head;
    while (current) |cur| {
        const next = cur.gc_next;
        if (cur.gc_mark == 0) {
            if (cur.kind == XI_STR_KIND_INTERNED) cfree(cur.value);
            free(cur);
            if (prev == null) {
                xi_gc_str_head = next;
            } else {
                prev.?.gc_next = next;
            }
        } else {
            cur.gc_mark = 0;
            prev = cur;
            live += 1;
        }
        current = next;
    }
    xi_gc_rebuild_string_buckets();
    return live;
}

fn xi_gc_sweep_arrays() usize {
    var live: usize = 0;
    var prev: ?*XiStrArray = null;
    var current = xi_gc_arr_head;
    while (current) |cur| {
        const next = cur.gc_next;
        if (cur.gc_mark == 0) {
            cfree(cur.items);
            cfree(cur.packed_nums);
            cfree(cur.i64_cache);
            free(cur);
            if (prev == null) {
                xi_gc_arr_head = next;
            } else {
                prev.?.gc_next = next;
            }
        } else {
            cur.gc_mark = 0;
            prev = cur;
            live += 1;
        }
        current = next;
    }
    return live;
}

fn xi_gc_sweep_maps() usize {
    var live: usize = 0;
    var prev: ?*XiMap = null;
    var current = xi_gc_map_head;
    while (current) |cur| {
        const next = cur.gc_next;
        if (cur.gc_mark == 0) {
            cfree(cur.keys);
            cfree(cur.values);
            cfree(cur.hashes);
            cfree(cur.states);
            free(cur);
            if (prev == null) {
                xi_gc_map_head = next;
            } else {
                prev.?.gc_next = next;
            }
        } else {
            cur.gc_mark = 0;
            prev = cur;
            live += 1;
        }
        current = next;
    }
    return live;
}

fn xi_gc_sweep_structs() usize {
    var live: usize = 0;
    var prev: ?*XiStruct = null;
    var current = xi_gc_struct_head;
    while (current) |cur| {
        const next = cur.gc_next;
        if (cur.gc_mark == 0) {
            cfree(cur.fields);
            cfree(cur.values);
            free(cur);
            if (prev == null) {
                xi_gc_struct_head = next;
            } else {
                prev.?.gc_next = next;
            }
        } else {
            cur.gc_mark = 0;
            prev = cur;
            live += 1;
        }
        current = next;
    }
    return live;
}

fn xi_gc_sweep_closures() usize {
    var live: usize = 0;
    var prev: ?*XiClosure = null;
    var current = xi_gc_closure_head;
    while (current) |cur| {
        const next = cur.gc_next;
        if (cur.gc_mark == 0) {
            free(cur.env);
            free(cur);
            if (prev == null) {
                xi_gc_closure_head = next;
            } else {
                prev.?.gc_next = next;
            }
        } else {
            cur.gc_mark = 0;
            prev = cur;
            live += 1;
        }
        current = next;
    }
    return live;
}

fn xi_gc_mark_roots(stack_low: usize, stack_high: usize) void {
    var regs: [8]usize = std.mem.zeroes([8]usize);
    captureRegisters(&regs);
    const regs_lo = @intFromPtr(&regs);
    const regs_hi = regs_lo + @sizeOf(@TypeOf(regs));
    var stack_cursor = regs_lo;
    if (stack_cursor < stack_low) stack_cursor = stack_low;
    if (stack_cursor > stack_high) stack_cursor = stack_high;
    if (builtin.os.tag == .windows) {
        xi_gc_mark_windows_stack(stack_cursor, stack_high);
    } else {
        xi_gc_mark_stack_region(stack_cursor, stack_high);
    }
    xi_gc_mark_stack_region(regs_lo, regs_hi);
    xi_gc_mark_cli_args();
}

fn xi_runtime_gc_collect() void {
    if (!xi_gc_initialized or xi_gc_collecting or xi_gc_pause_depth > 0 or xi_gc_mode == 2) return;
    var stack_low: usize = 0;
    var stack_high: usize = 0;
    if (!xi_gc_get_stack_bounds(&stack_low, &stack_high)) return;

    xi_gc_collecting = true;
    xi_gc_clear_marks();
    xi_gc_build_num_lookup();
    xi_gc_build_lookup();
    xi_gc_mark_roots(stack_low, stack_high);
    xi_gc_drop_lookup();
    xi_gc_drop_num_lookup();

    var live_total: usize = 0;
    live_total += xi_gc_sweep_nums();
    live_total += xi_gc_sweep_strings();
    live_total += xi_gc_sweep_arrays();
    live_total += xi_gc_sweep_maps();
    live_total += xi_gc_sweep_structs();
    live_total += xi_gc_sweep_closures();

    xi_gc_allocs_since_collect = 0;
    var next_threshold = live_total / 2 + 64;
    if (next_threshold < XI_GC_MIN_THRESHOLD) next_threshold = XI_GC_MIN_THRESHOLD;
    xi_gc_collect_threshold = next_threshold;
    xi_gc_collections += 1;
    xi_gc_collecting = false;
}

fn xi_gc_note_allocation() void {
    if (!xi_gc_initialized) return;
    xi_gc_allocs_since_collect += 1;
    if (xi_gc_mode == 0 and xi_gc_pause_depth == 0 and !xi_gc_collecting and xi_gc_allocs_since_collect >= xi_gc_collect_threshold) {
        xi_runtime_gc_collect();
    }
}

export fn xi_gc_set_mode(mode: i32) callconv(.c) void {
    xi_gc_mode = if (mode >= 0 and mode <= 2) @intCast(mode) else 0;
}

export fn xi_gc_collect() callconv(.c) void {
    xi_runtime_gc_ensure_init();
    xi_runtime_gc_collect();
}

export fn xi_gc_collection_count() callconv(.c) i64 {
    return @intCast(xi_gc_collections);
}

export fn xi_gc_live_count() callconv(.c) i64 {
    var total: usize = 0;
    var n = xi_gc_head;
    while (n) |p| : (n = p.gc_next) total += 1;
    var s = xi_gc_str_head;
    while (s) |p| : (s = p.gc_next) total += 1;
    var a = xi_gc_arr_head;
    while (a) |p| : (a = p.gc_next) total += 1;
    var m = xi_gc_map_head;
    while (m) |p| : (m = p.gc_next) total += 1;
    var st = xi_gc_struct_head;
    while (st) |p| : (st = p.gc_next) total += 1;
    var cl = xi_gc_closure_head;
    while (cl) |p| : (cl = p.gc_next) total += 1;
    return @intCast(total);
}

fn cmemcpy(dst: anytype, src: anytype, n: usize) void {
    _ = memcpy(@ptrCast(dst), @ptrCast(src), n);
}
fn cmemmove(dst: anytype, src: anytype, n: usize) void {
    _ = memmove(@ptrCast(dst), @ptrCast(src), n);
}
fn cmemset(dst: anytype, val: c_int, n: usize) void {
    _ = memset(@ptrCast(dst), val, n);
}
fn cmemcmp(a: anytype, b: anytype, n: usize) c_int {
    return memcmp(@ptrCast(a), @ptrCast(b), n);
}
const empty_cstr: [*:0]const u8 = "";

export fn xi_closure_env_alloc(bytes: i64) callconv(.c) ?*anyopaque {
    if (bytes <= 0) return null;
    const env = calloc(1, @intCast(bytes));
    if (env == null) xi_err_set(XI_ERR_OUT_OF_MEMORY, "closure environment allocation failed");
    return env;
}

export fn xi_closure_new(code: ?*anyopaque, env: ?*anyopaque, env_bytes: i64) callconv(.c) ?*anyopaque {
    xi_runtime_gc_ensure_init();
    const closure: *XiClosure = @ptrCast(@alignCast(malloc(@sizeOf(XiClosure)) orelse {
        xi_err_set(XI_ERR_OUT_OF_MEMORY, "closure allocation failed");
        return null;
    }));
    closure.code = code;
    closure.env = env;
    closure.env_bytes = if (env_bytes > 0) @intCast(env_bytes) else 0;
    closure.gc_mark = 0;
    closure.gc_next = xi_gc_closure_head;
    xi_gc_closure_head = closure;
    xi_gc_note_allocation();
    return @ptrCast(closure);
}

export fn xi_closure_code(closure: ?*anyopaque) callconv(.c) ?*anyopaque {
    if (closure) |c| {
        const cl: *XiClosure = @ptrCast(@alignCast(c));
        return cl.code;
    }
    return null;
}

export fn xi_closure_env(closure: ?*anyopaque) callconv(.c) ?*anyopaque {
    if (closure) |c| {
        const cl: *XiClosure = @ptrCast(@alignCast(c));
        return cl.env;
    }
    return null;
}

fn xi_num_new() *XiNum {
    xi_runtime_gc_ensure_init();
    var n: *XiNum = undefined;
    if (xi_num_freelist) |fl| {
        n = fl;
        xi_num_freelist = fl.gc_next;
        xi_num_freelist_len -= 1;
    } else {
        n = @ptrCast(@alignCast(malloc(@sizeOf(XiNum)) orelse xi_oom()));
    }
    @memset(std.mem.asBytes(n), 0);
    n.gc_next = xi_gc_head;
    xi_gc_head = n;
    xi_gc_note_allocation();
    return n;
}

fn xi_num_int_cache_init() void {
    if (xi_num_int_cache_initialized) return;
    xi_num_int_cache_initialized = true;
    @memset(std.mem.asBytes(&xi_num_int_cache), 0);
    var i: usize = 0;
    while (i < XI_NUM_INT_CACHE_SIZE) : (i += 1) {
        const v: i64 = XI_NUM_INT_CACHE_MIN + @as(i64, @intCast(i));
        xi_num_int_cache[i].tag = XI_NUM_INT;
        xi_num_int_cache[i].i = v;
        xi_num_int_cache[i].d = @floatFromInt(v);
        xi_num_int_cache[i].gc_mark = 0;
        xi_num_int_cache[i].gc_next = null;
    }
}

fn xi_num_int_cached(v: i64) ?*XiNum {
    if (v < XI_NUM_INT_CACHE_MIN or v > XI_NUM_INT_CACHE_MAX) return null;
    xi_num_int_cache_init();
    const idx: usize = @intCast(v - XI_NUM_INT_CACHE_MIN);
    return &xi_num_int_cache[idx];
}

fn xi_num_make_int(v: i64) *XiNum {
    if (xi_num_int_cached(v)) |cached| return cached;
    xi_runtime_gc_ensure_init();
    var n: *XiNum = undefined;
    if (xi_num_freelist) |fl| {
        n = fl;
        xi_num_freelist = fl.gc_next;
        xi_num_freelist_len -= 1;
    } else {
        n = @ptrCast(@alignCast(malloc(@sizeOf(XiNum)) orelse xi_oom()));
    }
    n.tag = XI_NUM_INT;
    n.i = v;
    n.big = null;
    n.gc_mark = 0;
    n.gc_next = xi_gc_head;
    xi_gc_head = n;
    xi_gc_note_allocation();
    return n;
}

export fn xi_num_make_dec(v: f64) callconv(.c) *XiNum {
    const n = xi_num_new();
    n.tag = XI_NUM_DEC;
    n.d = v;
    n.i = f64ToI64Trunc(v);
    return n;
}

fn f64ToI64Trunc(v: f64) i64 {
    if (cIsNan(v)) return 0;
    if (v >= 9223372036854775808.0) return INT64_MAX;
    if (v < -9223372036854775808.0) return INT64_MIN;
    return @intFromFloat(trunc(v));
}

fn xi_str_hash_update(hash0: u64, bytes: [*]const u8, len: usize) u64 {
    var hash = hash0;
    var i: usize = 0;
    while (i < len) : (i += 1) {
        hash ^= @as(u64, bytes[i]);
        hash *%= 1099511628211;
    }
    return hash;
}

fn xi_str_hash_bytes(s: ?[*]const u8, len_in: usize) u64 {
    var len = len_in;
    if (s == null) len = 0;
    const src: [*]const u8 = if (s) |ss| ss else @ptrCast(empty_cstr);
    var hash: u64 = 1469598103934665603;
    hash = xi_str_hash_update(hash, src, len);
    if (hash == 0) return 1;
    return hash;
}

fn xi_str_entry_lookup(s: OptStr) ?*XiStrInternEntry {
    const ss = s orelse return null;
    if (xi_str_lookup_cache_value != null and strAddr(xi_str_lookup_cache_value) == strAddr(ss)) {
        return xi_str_lookup_cache_entry;
    }
    var cur = xi_gc_str_head;
    while (cur) |c| : (cur = c.gc_next) {
        if (@intFromPtr(c.value) == strAddr(ss)) {
            xi_str_lookup_cache_value = ss;
            xi_str_lookup_cache_entry = c;
            return c;
        }
    }
    xi_str_lookup_cache_value = null;
    xi_str_lookup_cache_entry = null;
    return null;
}

export fn xi_str_len_known(s: OptStr) callconv(.c) usize {
    const ss = s orelse return 0;
    if (xi_str_entry_lookup(ss)) |entry| return entry.len;
    return strlen(ss);
}

fn xi_str_equal_known(a: OptStr, b: OptStr) c_int {
    if (strAddr(a) == strAddr(b)) return 1;
    if (a == null or b == null) return 0;
    const al = xi_str_len_known(a);
    const bl = xi_str_len_known(b);
    if (al != bl) return 0;
    return if (cmemcmp(a.?, b.?, al) == 0) 1 else 0;
}

fn xi_str_copy_range(base: OptStr, begin: usize, len: usize) Str {
    if (base == null or len == 0) return xi_str_intern_bytes(empty_cstr, 0);
    return xi_str_intern_bytes(base.? + begin, len);
}

fn xi_str_intern_rehash(min_cap: usize) void {
    var new_cap: usize = if (xi_str_intern_cap == 0) 64 else xi_str_intern_cap;
    while (new_cap < min_cap) {
        if (new_cap > SIZE_MAX / 2) fatal("xi_runtime: string table overflow\n");
        new_cap *= 2;
    }
    const new_buckets: [*]?*XiStrInternEntry = @ptrCast(@alignCast(calloc(new_cap, @sizeOf(?*XiStrInternEntry)) orelse xi_oom()));
    if (xi_str_intern_buckets) |old| {
        var i: usize = 0;
        while (i < xi_str_intern_cap) : (i += 1) {
            var entry = old[i];
            while (entry) |en| {
                const next = en.bucket_next;
                const idx: usize = @intCast(en.hash & @as(u64, new_cap - 1));
                en.bucket_next = new_buckets[idx];
                new_buckets[idx] = en;
                entry = next;
            }
        }
        cfree(old);
    }
    xi_str_intern_buckets = new_buckets;
    xi_str_intern_cap = new_cap;
}

fn xi_str_intern_ensure_capacity(min_entries: usize) void {
    if (xi_str_intern_cap == 0) {
        xi_str_intern_rehash(64);
        return;
    }
    const grow_threshold = xi_str_intern_cap - (xi_str_intern_cap / 4);
    if (min_entries >= grow_threshold) xi_str_intern_rehash(xi_str_intern_cap * 2);
}

export fn xi_str_intern_bytes(s: ?[*]const u8, len_in: usize) callconv(.c) Str {
    var len = len_in;
    const src: [*]const u8 = if (s) |ss| ss else @ptrCast(empty_cstr);
    if (s == null) len = 0;

    xi_runtime_gc_ensure_init();
    xi_str_intern_ensure_capacity(xi_str_intern_used + 1);

    const hash = xi_str_hash_bytes(src, len);
    const bucket: usize = @intCast(hash & @as(u64, xi_str_intern_cap - 1));
    var entry = xi_str_intern_buckets.?[bucket];
    while (entry) |en| : (entry = en.bucket_next) {
        if (en.hash == hash and en.len == len and cmemcmp(en.value, src, len) == 0) {
            return @ptrCast(en.value);
        }
    }

    const out: [*]u8 = @ptrCast(malloc(len + 1) orelse xi_oom());
    cmemcpy(out, src, len);
    out[len] = 0;

    const node = cnew(XiStrInternEntry);
    node.value = out;
    node.len = len;
    node.hash = hash;
    node.kind = XI_STR_KIND_INTERNED;
    node.owner = null;
    node.gc_mark = 0;
    node.bucket_next = xi_str_intern_buckets.?[bucket];
    node.gc_next = xi_gc_str_head;
    xi_gc_str_head = node;
    xi_str_intern_buckets.?[bucket] = node;
    xi_str_intern_used += 1;
    xi_str_lookup_cache_value = @ptrCast(out);
    xi_str_lookup_cache_entry = node;
    xi_gc_note_allocation();
    return @ptrCast(out);
}

export fn xi_str_intern_cstr(s: OptStr) callconv(.c) Str {
    const ss = s orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return xi_str_intern_bytes(empty_cstr, 0);
    };
    return xi_str_intern_bytes(ss, xi_str_len_known(ss));
}

export fn xi_str_dup(s: OptStr) callconv(.c) Str {
    return xi_str_intern_cstr(s);
}

fn xi_runtime_is_num_ptr(ptr: ?*const anyopaque) bool {
    const p = @intFromPtr(ptr);
    if (p == 0) return false;
    const lo = @intFromPtr(&xi_num_int_cache[0]);
    const hi = lo + XI_NUM_INT_CACHE_SIZE * @sizeOf(XiNum);
    if (p >= lo and p < hi and (p - lo) % @sizeOf(XiNum) == 0) return true;
    var cur = xi_gc_head;
    while (cur) |c| : (cur = c.gc_next) {
        if (@intFromPtr(c) == p) return true;
    }
    return false;
}

fn xi_runtime_is_str_ptr(ptr: ?*const anyopaque) bool {
    const p = @intFromPtr(ptr);
    var cur = xi_gc_str_head;
    while (cur) |c| : (cur = c.gc_next) {
        if (@intFromPtr(c.value) == p) return true;
    }
    return false;
}

fn xi_runtime_is_arr_ptr(ptr: ?*const anyopaque) bool {
    const p = @intFromPtr(ptr);
    var cur = xi_gc_arr_head;
    while (cur) |c| : (cur = c.gc_next) {
        if (@intFromPtr(c) == p) return true;
    }
    return false;
}

fn xi_runtime_is_map_ptr(ptr: ?*const anyopaque) bool {
    const p = @intFromPtr(ptr);
    var cur = xi_gc_map_head;
    while (cur) |c| : (cur = c.gc_next) {
        if (@intFromPtr(c) == p) return true;
    }
    return false;
}

fn xi_runtime_is_struct_ptr(ptr: ?*const anyopaque) bool {
    const p = @intFromPtr(ptr);
    var cur = xi_gc_struct_head;
    while (cur) |c| : (cur = c.gc_next) {
        if (@intFromPtr(c) == p) return true;
    }
    return false;
}

fn valNull() XiValue {
    return .{ .tag = XI_VALUE_NULL, .ptr = null, .boolean = false };
}
fn valNum(v: ?*XiNum) XiValue {
    if (v == null) return valNull();
    return .{ .tag = XI_VALUE_NUM, .ptr = @ptrCast(v), .boolean = false };
}
fn valStr(v: OptStr) XiValue {
    if (v == null) return valNull();
    return .{ .tag = XI_VALUE_STR, .ptr = @constCast(@ptrCast(v)), .boolean = false };
}
fn valArr(v: ?*XiStrArray) XiValue {
    if (v == null) return valNull();
    return .{ .tag = XI_VALUE_ARR, .ptr = @ptrCast(v), .boolean = false };
}
fn valMap(v: ?*XiMap) XiValue {
    if (v == null) return valNull();
    return .{ .tag = XI_VALUE_MAP, .ptr = @ptrCast(v), .boolean = false };
}
fn valStruct(v: ?*XiStruct) XiValue {
    if (v == null) return valNull();
    return .{ .tag = XI_VALUE_STRUCT, .ptr = @ptrCast(v), .boolean = false };
}
fn valBool(v: bool) XiValue {
    return .{ .tag = XI_VALUE_BOOL, .ptr = null, .boolean = v };
}
fn valRaw(v: ?*anyopaque) XiValue {
    if (v == null) return valNull();
    if (xi_runtime_is_num_ptr(v)) return valNum(@ptrCast(@alignCast(v)));
    if (xi_runtime_is_str_ptr(v)) return valStr(@ptrCast(v));
    if (xi_runtime_is_arr_ptr(v)) return valArr(@ptrCast(@alignCast(v)));
    if (xi_runtime_is_map_ptr(v)) return valMap(@ptrCast(@alignCast(v)));
    if (xi_runtime_is_struct_ptr(v)) return valStruct(@ptrCast(@alignCast(v)));
    return .{ .tag = XI_VALUE_OPAQUE, .ptr = v, .boolean = false };
}

fn valAsNum(v: XiValue) ?*XiNum {
    if (v.ptr == null) return null;
    return @ptrCast(@alignCast(v.ptr));
}
fn valAsStr(v: XiValue) OptStr {
    if (v.ptr == null) return null;
    return @ptrCast(v.ptr);
}
fn valAsArr(v: XiValue) ?*XiStrArray {
    if (v.ptr == null) return null;
    return @ptrCast(@alignCast(v.ptr));
}
fn valAsMap(v: XiValue) ?*XiMap {
    if (v.ptr == null) return null;
    return @ptrCast(@alignCast(v.ptr));
}
fn valAsStruct(v: XiValue) ?*XiStruct {
    if (v.ptr == null) return null;
    return @ptrCast(@alignCast(v.ptr));
}

fn xi_value_default_for_tag(tag: u8) XiValue {
    return switch (tag) {
        XI_VALUE_NUM => valNum(xi_num_make_int(0)),
        XI_VALUE_STR => valStr(xi_str_intern_cstr("")),
        XI_VALUE_BOOL => valBool(false),
        else => valNull(),
    };
}

fn xi_value_as_raw_or_null(value: *const XiValue) ?*anyopaque {
    switch (value.tag) {
        XI_VALUE_NUM, XI_VALUE_STR, XI_VALUE_ARR, XI_VALUE_MAP, XI_VALUE_STRUCT => return value.ptr,
        XI_VALUE_NULL => return null,
        else => {
            xi_err_set(XI_ERR_INVALID_ARGUMENT, "runtime value type mismatch");
            return null;
        },
    }
}

fn shlU32(x: u32, n: usize) u32 {
    return x << @as(u5, @intCast(n));
}
fn shrU32(x: u32, n: usize) u32 {
    return x >> @as(u5, @intCast(n));
}
fn shlU64(x: u64, n: usize) u64 {
    return x << @as(u6, @intCast(n));
}

fn xi_big_new(cap_in: usize) *XiBig {
    var cap = cap_in;
    if (cap < 1) cap = 1;
    const b = cnew(XiBig);
    b.sign = 0;
    b.len = 0;
    b.cap = cap;
    b.limbs = @ptrCast(@alignCast(calloc(cap, @sizeOf(u32)) orelse xi_oom()));
    return b;
}

fn xi_big_free(b: ?*XiBig) void {
    const bb = b orelse return;
    cfree(bb.limbs);
    free(bb);
}

fn xi_big_reserve(b: *XiBig, need: usize) void {
    if (b.cap >= need) return;
    var nc: usize = if (b.cap != 0) b.cap else 1;
    while (nc < need) nc *= 2;
    const nl: [*]u32 = @ptrCast(@alignCast(realloc(@ptrCast(b.limbs), nc * @sizeOf(u32)) orelse xi_oom()));
    var i = b.cap;
    while (i < nc) : (i += 1) nl[i] = 0;
    b.limbs = nl;
    b.cap = nc;
}

fn xi_big_trim(b: *XiBig) void {
    while (b.len > 0 and b.limbs[b.len - 1] == 0) b.len -= 1;
    if (b.len == 0) b.sign = 0;
}

fn xi_big_clone(a: *const XiBig) *XiBig {
    const r = xi_big_new(if (a.len != 0) a.len else 1);
    var i: usize = 0;
    while (i < a.len) : (i += 1) r.limbs[i] = a.limbs[i];
    r.len = a.len;
    r.sign = a.sign;
    return r;
}

fn xi_big_from_i64(v: i64) *XiBig {
    if (v == 0) return xi_big_new(1);
    const mag: u64 = if (v < 0) (~@as(u64, @bitCast(v)) +% 1) else @as(u64, @bitCast(v));
    const b = xi_big_new(2);
    b.limbs[0] = @truncate(mag & 0xffffffff);
    b.limbs[1] = @truncate(mag >> 32);
    b.len = 2;
    xi_big_trim(b);
    b.sign = if (v < 0) -1 else 1;
    return b;
}

fn xi_big_from_wide(bytes: ?[*]const u8, n_in: usize, is_signed: c_int) *XiBig {
    if (bytes == null or n_in == 0) return xi_big_new(1);
    const bs = bytes.?;
    const n = n_in;
    var stackbuf: [64]u8 = undefined;
    var heapbuf: ?[*]u8 = null;
    var mag: [*]u8 = &stackbuf;
    if (n > stackbuf.len) {
        heapbuf = @ptrCast(malloc(n) orelse xi_oom());
        mag = heapbuf.?;
    }
    var neg: c_int = 0;
    if (is_signed != 0 and (bs[n - 1] & 0x80) != 0) {
        neg = 1;
        var carry: c_int = 1;
        var i: usize = 0;
        while (i < n) : (i += 1) {
            const s: c_int = @as(c_int, ~bs[i]) + carry;
            mag[i] = @truncate(@as(u32, @bitCast(s)) & 0xff);
            carry = s >> 8;
        }
    } else {
        var i: usize = 0;
        while (i < n) : (i += 1) mag[i] = bs[i];
    }
    const nlimbs = (n + 3) / 4;
    const b = xi_big_new(nlimbs);
    var i: usize = 0;
    while (i < n) : (i += 1) {
        b.limbs[i / 4] |= shlU32(@as(u32, mag[i]), (i % 4) * 8);
    }
    b.len = nlimbs;
    xi_big_trim(b);
    b.sign = if (b.len != 0) (if (neg != 0) @as(i32, -1) else 1) else 0;
    if (heapbuf) |hb| cfree(hb);
    return b;
}

fn xi_big_from_decimal(s: [*]const u8, len: usize) *XiBig {
    var neg = false;
    var i: usize = 0;
    if (len > 0 and (s[0] == '-' or s[0] == '+')) {
        neg = (s[0] == '-');
        i = 1;
    }
    const digits = if (len > i) len - i else 0;
    const b = xi_big_new(digits / 9 + 2);
    while (i < len) : (i += 1) {
        const c = s[i];
        if (c < '0' or c > '9') continue;
        var carry: u64 = @as(u64, c - '0');
        var j: usize = 0;
        while (j < b.len) : (j += 1) {
            const cur = @as(u64, b.limbs[j]) * 10 + carry;
            b.limbs[j] = @truncate(cur & 0xffffffff);
            carry = cur >> 32;
        }
        while (carry != 0) {
            xi_big_reserve(b, b.len + 1);
            b.limbs[b.len] = @truncate(carry & 0xffffffff);
            b.len += 1;
            carry >>= 32;
        }
    }
    xi_big_trim(b);
    b.sign = if (b.len != 0) (if (neg) @as(i32, -1) else 1) else 0;
    return b;
}

fn xi_big_scale10(a: *XiBig, k: u32) void {
    if (a.sign == 0) return;
    var step: u32 = 0;
    while (step < k) : (step += 1) {
        var carry: u64 = 0;
        var i: usize = 0;
        while (i < a.len) : (i += 1) {
            const cur = @as(u64, a.limbs[i]) * 10 + carry;
            a.limbs[i] = @truncate(cur & 0xffffffff);
            carry = cur >> 32;
        }
        while (carry != 0) {
            xi_big_reserve(a, a.len + 1);
            a.limbs[a.len] = @truncate(carry & 0xffffffff);
            a.len += 1;
            carry >>= 32;
        }
    }
}

fn xi_big_ucmp(a: *const XiBig, b: *const XiBig) c_int {
    if (a.len != b.len) return if (a.len < b.len) -1 else 1;
    var i = a.len;
    while (i > 0) {
        i -= 1;
        if (a.limbs[i] != b.limbs[i]) return if (a.limbs[i] < b.limbs[i]) @as(c_int, -1) else 1;
    }
    return 0;
}

fn xi_big_cmp(a: *const XiBig, b: *const XiBig) c_int {
    if (a.sign != b.sign) return if (a.sign < b.sign) -1 else 1;
    if (a.sign == 0) return 0;
    const u = xi_big_ucmp(a, b);
    return if (a.sign > 0) u else -u;
}

fn xi_big_uadd(a: *const XiBig, b: *const XiBig) *XiBig {
    const n = (if (a.len > b.len) a.len else b.len) + 1;
    const r = xi_big_new(n);
    var carry: u64 = 0;
    var i: usize = 0;
    while (i < n) : (i += 1) {
        var s: u64 = carry;
        if (i < a.len) s += a.limbs[i];
        if (i < b.len) s += b.limbs[i];
        r.limbs[i] = @truncate(s & 0xffffffff);
        carry = s >> 32;
    }
    r.len = n;
    xi_big_trim(r);
    return r;
}

fn xi_big_usub(a: *const XiBig, b: *const XiBig) *XiBig {
    const r = xi_big_new(if (a.len != 0) a.len else 1);
    var borrow: i64 = 0;
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        var d: i64 = @as(i64, a.limbs[i]) - borrow - (if (i < b.len) @as(i64, b.limbs[i]) else 0);
        if (d < 0) {
            d += (@as(i64, 1) << 32);
            borrow = 1;
        } else {
            borrow = 0;
        }
        r.limbs[i] = @truncate(@as(u64, @bitCast(d)) & 0xffffffff);
    }
    r.len = a.len;
    xi_big_trim(r);
    return r;
}

fn xi_big_add(a: *const XiBig, b: *const XiBig) *XiBig {
    if (a.sign == 0) return xi_big_clone(b);
    if (b.sign == 0) return xi_big_clone(a);
    if (a.sign == b.sign) {
        const r = xi_big_uadd(a, b);
        r.sign = if (r.len != 0) a.sign else 0;
        return r;
    }
    const c = xi_big_ucmp(a, b);
    if (c == 0) return xi_big_new(1);
    if (c > 0) {
        const r = xi_big_usub(a, b);
        r.sign = if (r.len != 0) a.sign else 0;
        return r;
    }
    const r = xi_big_usub(b, a);
    r.sign = if (r.len != 0) b.sign else 0;
    return r;
}

fn xi_big_sub(a: *const XiBig, b: *const XiBig) *XiBig {
    const nb = xi_big_clone(b);
    nb.sign = -nb.sign;
    const r = xi_big_add(a, nb);
    xi_big_free(nb);
    return r;
}

fn xi_big_mul(a: *const XiBig, b: *const XiBig) *XiBig {
    if (a.sign == 0 or b.sign == 0) return xi_big_new(1);
    const r = xi_big_new(a.len + b.len);
    r.len = a.len + b.len;
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        var carry: u64 = 0;
        const av: u64 = a.limbs[i];
        var j: usize = 0;
        while (j < b.len) : (j += 1) {
            const cur = @as(u64, r.limbs[i + j]) + av * @as(u64, b.limbs[j]) + carry;
            r.limbs[i + j] = @truncate(cur & 0xffffffff);
            carry = cur >> 32;
        }
        r.limbs[i + b.len] = r.limbs[i + b.len] +% @as(u32, @truncate(carry));
    }
    xi_big_trim(r);
    r.sign = if (r.len != 0) a.sign * b.sign else 0;
    return r;
}

fn xi_big_pow_u64(base: *const XiBig, exp_in: u64) *XiBig {
    var e = exp_in;
    var result = xi_big_from_i64(1);
    var factor = xi_big_clone(base);
    while (e != 0) {
        if (e & 1 != 0) {
            const next = xi_big_mul(result, factor);
            xi_big_free(result);
            result = next;
        }
        e >>= 1;
        if (e != 0) {
            const next = xi_big_mul(factor, factor);
            xi_big_free(factor);
            factor = next;
        }
    }
    xi_big_free(factor);
    return result;
}

fn xi_big_divmod_small(a: *XiBig, d: u32) u32 {
    var rem: u64 = 0;
    var i = a.len;
    while (i > 0) {
        i -= 1;
        const cur = (rem << 32) | a.limbs[i];
        a.limbs[i] = @truncate(cur / d);
        rem = cur % d;
    }
    xi_big_trim(a);
    return @truncate(rem);
}

fn xi_big_bitlen(a: *const XiBig) usize {
    if (a.len == 0) return 0;
    const top = a.len - 1;
    var v = a.limbs[top];
    var bits = top * 32;
    while (v != 0) {
        bits += 1;
        v >>= 1;
    }
    return bits;
}

fn xi_big_shl1(a: *XiBig) void {
    if (a.len == 0) return;
    var carry: u32 = 0;
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        const nv = (a.limbs[i] << 1) | carry;
        carry = a.limbs[i] >> 31;
        a.limbs[i] = nv;
    }
    if (carry != 0) {
        xi_big_reserve(a, a.len + 1);
        a.limbs[a.len] = carry;
        a.len += 1;
    }
}

fn xi_big_divmod(a: *const XiBig, b: *const XiBig, q_out: **XiBig, r_out: **XiBig) void {
    const q = xi_big_new(if (a.len != 0) a.len else 1);
    q.len = a.len;
    var k: usize = 0;
    while (k < q.len) : (k += 1) q.limbs[k] = 0;
    var r = xi_big_new(1);
    r.len = 0;
    r.sign = 0;
    const nbits = xi_big_bitlen(a);
    var bi = nbits;
    while (bi > 0) {
        bi -= 1;
        xi_big_shl1(r);
        if ((shrU32(a.limbs[bi >> 5], bi & 31) & 1) != 0) {
            if (r.len == 0) {
                r.len = 1;
                r.limbs[0] = 0;
            }
            r.limbs[0] |= 1;
            r.sign = 1;
        }
        if (r.sign != 0 and xi_big_ucmp(r, b) >= 0) {
            const t = xi_big_usub(r, b);
            xi_big_free(r);
            r = t;
            r.sign = if (r.len != 0) 1 else 0;
            q.limbs[bi >> 5] |= shlU32(1, bi & 31);
        }
    }
    xi_big_trim(q);
    q.sign = if (q.len != 0) 1 else 0;
    q_out.* = q;
    r_out.* = r;
}

fn xi_big_is_zero(a: *const XiBig) bool {
    return a.sign == 0 or a.len == 0;
}

fn xi_big_shl(a: *const XiBig, bits: usize) *XiBig {
    if (xi_big_is_zero(a)) return xi_big_new(1);
    const limb_sh = bits / 32;
    const bit_sh = bits % 32;
    const r = xi_big_new(a.len + limb_sh + 2);
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        const v = shlU64(@as(u64, a.limbs[i]), bit_sh);
        r.limbs[i + limb_sh] |= @as(u32, @truncate(v & 0xffffffff));
        r.limbs[i + limb_sh + 1] |= @as(u32, @truncate(v >> 32));
    }
    r.len = a.len + limb_sh + 2;
    xi_big_trim(r);
    r.sign = if (r.len != 0) 1 else 0;
    return r;
}

fn xi_big_shr(a: *const XiBig, bits: usize) *XiBig {
    const limb_sh = bits / 32;
    const bit_sh = bits % 32;
    if (limb_sh >= a.len) return xi_big_new(1);
    const r = xi_big_new(a.len - limb_sh);
    var i = limb_sh;
    while (i < a.len) : (i += 1) {
        const lo = shrU32(a.limbs[i], bit_sh);
        var hi: u32 = 0;
        if (bit_sh != 0 and i + 1 < a.len) {
            hi = shlU32(a.limbs[i + 1], 32 - bit_sh);
        }
        r.limbs[i - limb_sh] = lo | hi;
    }
    r.len = a.len - limb_sh;
    xi_big_trim(r);
    r.sign = if (r.len != 0) 1 else 0;
    return r;
}

fn xi_big_test_bit(a: *const XiBig, idx: usize) c_int {
    const limb = idx / 32;
    if (limb >= a.len) return 0;
    return @intCast(shrU32(a.limbs[limb], idx % 32) & 1);
}

fn xi_big_low_bits_nonzero(a: *const XiBig, k: usize) c_int {
    const full = k / 32;
    const rem = k % 32;
    var i: usize = 0;
    while (i < full and i < a.len) : (i += 1) {
        if (a.limbs[i] != 0) return 1;
    }
    if (rem != 0 and full < a.len) {
        if ((a.limbs[full] & (shlU32(1, rem) - 1)) != 0) return 1;
    }
    return 0;
}

fn xi_big_fits_i64(a: *const XiBig, out: *i64) bool {
    if (a.sign == 0) {
        out.* = 0;
        return true;
    }
    if (a.len > 2) return false;
    var mag: u64 = a.limbs[0];
    if (a.len == 2) mag |= @as(u64, a.limbs[1]) << 32;
    if (a.sign > 0) {
        if (mag > @as(u64, @bitCast(INT64_MAX))) return false;
        out.* = @bitCast(mag);
    } else {
        if (mag > @as(u64, @bitCast(INT64_MAX)) + 1) return false;
        out.* = if (mag == @as(u64, @bitCast(INT64_MAX)) + 1) INT64_MIN else -@as(i64, @bitCast(mag));
    }
    return true;
}

fn xi_big_to_i64_trunc(a: *const XiBig) i64 {
    var mag: u64 = if (a.len > 0) a.limbs[0] else 0;
    if (a.len > 1) mag |= @as(u64, a.limbs[1]) << 32;
    if (a.sign < 0) mag = ~mag +% 1;
    return @bitCast(mag);
}

fn xi_big_to_double(a: *const XiBig) f64 {
    var r: f64 = 0.0;
    var i = a.len;
    while (i > 0) {
        i -= 1;
        r = r * 4294967296.0 + @as(f64, @floatFromInt(a.limbs[i]));
    }
    return if (a.sign < 0) -r else r;
}

fn xi_big_to_wide(a: *const XiBig, out: [*]u8, n: usize) void {
    var i: usize = 0;
    while (i < n) : (i += 1) {
        const limb = i / 4;
        const sh = (i % 4) * 8;
        out[i] = if (limb < a.len) @truncate(shrU32(a.limbs[limb], sh)) else 0;
    }
    if (a.sign < 0) {
        var carry: c_int = 1;
        i = 0;
        while (i < n) : (i += 1) {
            const s: c_int = @as(c_int, ~out[i]) + carry;
            out[i] = @truncate(@as(u32, @bitCast(s)) & 0xff);
            carry = s >> 8;
        }
    }
}

fn xi_big_to_decimal(a: *const XiBig) [*:0]u8 {
    if (a.sign == 0) {
        const s: [*]u8 = @ptrCast(malloc(2) orelse xi_oom());
        s[0] = '0';
        s[1] = 0;
        return @ptrCast(s);
    }
    const t = xi_big_clone(a);
    const cap = t.len * 10 + 3;
    const tmp: [*]u8 = @ptrCast(malloc(cap) orelse xi_oom());
    var dlen: usize = 0;
    while (t.sign != 0) {
        var r = xi_big_divmod_small(t, 1000000000);
        var kk: usize = 0;
        while (kk < 9) : (kk += 1) {
            tmp[dlen] = @intCast('0' + (r % 10));
            dlen += 1;
            r /= 10;
            if (t.sign == 0 and r == 0) break;
        }
    }
    xi_big_free(t);

    const out_len = dlen + (if (a.sign < 0) @as(usize, 1) else 0);
    const out: [*]u8 = @ptrCast(malloc(out_len + 1) orelse xi_oom());
    var pos: usize = 0;
    if (a.sign < 0) {
        out[pos] = '-';
        pos += 1;
    }
    while (dlen > 0) {
        dlen -= 1;
        out[pos] = tmp[dlen];
        pos += 1;
    }
    out[pos] = 0;
    cfree(tmp);
    return @ptrCast(out);
}

fn xi_big_decimal_digits(a: *const XiBig) c_int {
    const s = xi_big_to_decimal(a);
    const n: c_int = @intCast(strlen(s));
    cfree(s);
    return n;
}

fn xi_big_ten_pow(k: u32) *XiBig {
    const p = xi_big_from_i64(1);
    xi_big_scale10(p, k);
    return p;
}

fn xi_big_is_pow2(b: ?*const XiBig) c_int {
    const bb = b orelse return 0;
    if (bb.len == 0) return 0;
    var i: usize = 0;
    while (i + 1 < bb.len) : (i += 1) {
        if (bb.limbs[i] != 0) return 0;
    }
    const top = bb.limbs[bb.len - 1];
    return if (top != 0 and (top & (top - 1)) == 0) 1 else 0;
}

fn xi_num_to_big(v: *const XiNum) *XiBig {
    switch (v.tag) {
        XI_NUM_BIG => return xi_big_clone(v.big.?),
        XI_NUM_WIDE => return xi_big_from_wide(&v.wide, v.wide_bits / 8, 1),
        XI_NUM_DEC => return xi_big_from_i64(f64ToI64Trunc(v.d)),
        XI_NUM_SOFTFLOAT => {
            if (v.soft_kind != XI_SF_NORMAL or v.big == null) return xi_big_from_i64(0);
            const m = if (v.i >= 0) xi_big_shl(v.big.?, @intCast(v.i)) else xi_big_shr(v.big.?, @intCast(-v.i));
            m.sign = if (m.len != 0) (if (v.soft_sign < 0) @as(i32, -1) else 1) else 0;
            return m;
        },
        else => return xi_big_from_i64(v.i),
    }
}

fn xi_num_make_big(b: *XiBig) *XiNum {
    var fit: i64 = 0;
    if (xi_big_fits_i64(b, &fit)) {
        xi_big_free(b);
        return xi_num_make_int(fit);
    }
    const n = xi_num_new();
    n.tag = XI_NUM_BIG;
    n.big = b;
    n.i = xi_big_to_i64_trunc(b);
    n.d = xi_big_to_double(b);
    return n;
}

export fn xi_as_f64(n: *const XiNum) callconv(.c) f64 {
    if (n.tag == XI_NUM_DEC or n.tag == XI_NUM_BIGDEC or n.tag == XI_NUM_SOFTFLOAT) return n.d;
    if (n.tag == XI_NUM_BIG) return xi_big_to_double(n.big.?);
    if (n.tag == XI_NUM_WIDE) {
        const t = xi_big_from_wide(&n.wide, n.wide_bits / 8, 1);
        const r = xi_big_to_double(t);
        xi_big_free(t);
        return r;
    }
    return @floatFromInt(n.i);
}

export fn xi_num_to_i64(n: ?*XiNum) callconv(.c) i64 {
    const nn = n orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "numeric value was null");
        return 0;
    };
    if (nn.tag == XI_NUM_DEC or nn.tag == XI_NUM_BIGDEC) {
        if (cIsNan(nn.d) or cIsInf(nn.d)) return 0;
        if (nn.d > @as(f64, @floatFromInt(INT64_MAX))) return INT64_MAX;
        if (nn.d < @as(f64, @floatFromInt(INT64_MIN))) return INT64_MIN;
        return @intFromFloat(nn.d);
    }
    if (nn.tag == XI_NUM_BIG) return xi_big_to_i64_trunc(nn.big.?);
    return nn.i;
}

export fn xi_num_from_i64(v: i64) callconv(.c) *XiNum {
    return xi_num_make_int(v);
}

export fn xi_num_from_f64(v: f64) callconv(.c) *XiNum {
    return xi_num_make_dec(v);
}

export fn xi_num_as_f64(n: *const XiNum) callconv(.c) f64 {
    return xi_as_f64(n);
}

export fn xi_num_as_i64(v: ?*XiNum) callconv(.c) i64 {
    return xi_num_to_i64(v);
}

export fn xi_num_try_i64(v: ?*XiNum, out: ?*i64) callconv(.c) bool {
    const n = v orelse return false;
    const dst = out orelse return false;
    switch (n.tag) {
        XI_NUM_INT => {
            dst.* = n.i;
            return true;
        },
        XI_NUM_BIG => return xi_big_fits_i64(n.big.?, dst),
        XI_NUM_WIDE => {
            const b = xi_big_from_wide(&n.wide, n.wide_bits / 8, 1);
            defer xi_big_free(b);
            return xi_big_fits_i64(b, dst);
        },
        else => return false,
    }
}

export fn xi_num_from_decimal(s: OptStr) callconv(.c) *XiNum {
    const ss = s orelse return xi_num_make_int(0);
    return xi_num_make_big(xi_big_from_decimal(ss, strlen(ss)));
}

fn xi_bigdec_to_double(coeff: *const XiBig, exp: i32) f64 {
    return xi_big_to_double(coeff) * pow(10.0, @floatFromInt(exp));
}

fn xi_bigdec_to_decimal(coeff: *const XiBig, exp: i32) [*:0]u8 {
    if (coeff.sign == 0) {
        const z: [*]u8 = @ptrCast(malloc(2) orelse xi_oom());
        z[0] = '0';
        z[1] = 0;
        return @ptrCast(z);
    }
    const mag = xi_big_clone(coeff);
    mag.sign = 1;
    const digits = xi_big_to_decimal(mag);
    xi_big_free(mag);
    const nd = strlen(digits);
    const neg = coeff.sign < 0;

    const extra: usize = if (exp > 0) @intCast(exp) else @intCast(-@as(i64, exp));
    const buf: [*]u8 = @ptrCast(malloc(nd + extra + 8) orelse xi_oom());
    var p: usize = 0;
    if (neg) {
        buf[p] = '-';
        p += 1;
    }

    if (exp >= 0) {
        var i: usize = 0;
        while (i < nd) : (i += 1) {
            buf[p] = digits[i];
            p += 1;
        }
        var k: i32 = 0;
        while (k < exp) : (k += 1) {
            buf[p] = '0';
            p += 1;
        }
        buf[p] = 0;
    } else {
        const f: i32 = -exp;
        var frac_start: usize = 0;
        if (@as(i64, @intCast(nd)) > @as(i64, f)) {
            const intlen = nd - @as(usize, @intCast(f));
            var i: usize = 0;
            while (i < intlen) : (i += 1) {
                buf[p] = digits[i];
                p += 1;
            }
            frac_start = p;
            buf[p] = '.';
            p += 1;
            i = intlen;
            while (i < nd) : (i += 1) {
                buf[p] = digits[i];
                p += 1;
            }
        } else {
            buf[p] = '0';
            p += 1;
            frac_start = p;
            buf[p] = '.';
            p += 1;
            var k: i32 = 0;
            while (k < f - @as(i32, @intCast(nd))) : (k += 1) {
                buf[p] = '0';
                p += 1;
            }
            var i: usize = 0;
            while (i < nd) : (i += 1) {
                buf[p] = digits[i];
                p += 1;
            }
        }
        buf[p] = 0;
        while (p > frac_start + 1 and buf[p - 1] == '0') p -= 1;
        if (buf[p - 1] == '.') p -= 1;
        buf[p] = 0;
    }
    cfree(digits);
    return @ptrCast(buf);
}

fn xi_num_make_bigdec(coeff: *XiBig, exp: i32) *XiNum {
    const n = xi_num_new();
    n.tag = XI_NUM_BIGDEC;
    n.big = coeff;
    n.dec_exp = exp;
    n.d = xi_bigdec_to_double(coeff, exp);
    n.i = f64ToI64Trunc(n.d);
    return n;
}

export fn xi_num_trunc_to_int(v: ?*const XiNum) callconv(.c) *XiNum {
    const vv = v orelse return xi_num_make_int(0);
    if (vv.tag == XI_NUM_BIGDEC) {
        const s = xi_bigdec_to_decimal(vv.big.?, vv.dec_exp);
        if (strchr(@ptrCast(s), '.')) |dot| dot[0] = 0;
        const out = xi_num_from_decimal(@ptrCast(s));
        cfree(s);
        return out;
    }
    return xi_num_make_big(xi_num_to_big(vv));
}

export fn xi_num_clamp_nonneg(v: ?*XiNum) callconv(.c) *XiNum {
    const vv = v orelse return xi_num_make_int(0);
    var neg = false;
    switch (vv.tag) {
        XI_NUM_INT => neg = vv.i < 0,
        XI_NUM_BIG, XI_NUM_BIGDEC => neg = (vv.big != null and vv.big.?.sign < 0),
        XI_NUM_DEC => neg = vv.d < 0,
        XI_NUM_SOFTFLOAT => neg = (vv.soft_kind == XI_SF_NORMAL or vv.soft_kind == XI_SF_INF) and vv.soft_sign < 0,
        XI_NUM_WIDE => {
            const b = xi_big_from_wide(&vv.wide, vv.wide_bits / 8, 1);
            neg = b.sign < 0;
            xi_big_free(b);
        },
        else => neg = false,
    }
    return if (neg) xi_num_make_int(0) else vv;
}

export fn xi_num_dec_from_decimal(s: OptStr) callconv(.c) *XiNum {
    const ss = s orelse return xi_num_make_bigdec(xi_big_new(1), 0);
    const len = strlen(ss);
    var i: usize = 0;
    var neg = false;
    if (i < len and (ss[i] == '+' or ss[i] == '-')) {
        neg = (ss[i] == '-');
        i += 1;
    }
    const coeff = xi_big_new(len / 9 + 2);
    var fracdigits: i32 = 0;
    var seen_point = false;
    while (i < len) : (i += 1) {
        const c = ss[i];
        if (c == '.') {
            seen_point = true;
            continue;
        }
        if (c == 'e' or c == 'E') break;
        if (c < '0' or c > '9') continue;
        var carry: u64 = @as(u64, c - '0');
        var j: usize = 0;
        while (j < coeff.len) : (j += 1) {
            const cur = @as(u64, coeff.limbs[j]) * 10 + carry;
            coeff.limbs[j] = @truncate(cur & 0xffffffff);
            carry = cur >> 32;
        }
        while (carry != 0) {
            xi_big_reserve(coeff, coeff.len + 1);
            coeff.limbs[coeff.len] = @truncate(carry & 0xffffffff);
            coeff.len += 1;
            carry >>= 32;
        }
        if (seen_point) fracdigits += 1;
    }
    var exp10: i32 = -fracdigits;
    if (i < len and (ss[i] == 'e' or ss[i] == 'E')) {
        i += 1;
        var esign: i32 = 1;
        if (i < len and (ss[i] == '+' or ss[i] == '-')) {
            if (ss[i] == '-') esign = -1;
            i += 1;
        }
        var e: i32 = 0;
        while (i < len) : (i += 1) {
            const c = ss[i];
            if (c < '0' or c > '9') break;
            e = e * 10 + @as(i32, c - '0');
        }
        exp10 += esign * e;
    }
    xi_big_trim(coeff);
    coeff.sign = if (coeff.len != 0) (if (neg) @as(i32, -1) else 1) else 0;
    return xi_num_make_bigdec(coeff, exp10);
}

fn xi_num_to_bigdec_parts(v: *const XiNum, coeff_out: **XiBig, exp_out: *i32) void {
    if (v.tag == XI_NUM_BIGDEC) {
        coeff_out.* = xi_big_clone(v.big.?);
        exp_out.* = v.dec_exp;
    } else {
        coeff_out.* = xi_num_to_big(v);
        exp_out.* = 0;
    }
}

fn xi_bigdec_addsub(a: *const XiNum, b: *const XiNum, subtract: bool) *XiNum {
    var ca: *XiBig = undefined;
    var cb: *XiBig = undefined;
    var ea: i32 = undefined;
    var eb: i32 = undefined;
    xi_num_to_bigdec_parts(a, &ca, &ea);
    xi_num_to_bigdec_parts(b, &cb, &eb);
    const e = if (ea < eb) ea else eb;
    if (ea > e) xi_big_scale10(ca, @intCast(ea - e));
    if (eb > e) xi_big_scale10(cb, @intCast(eb - e));
    const rc = if (subtract) xi_big_sub(ca, cb) else xi_big_add(ca, cb);
    xi_big_free(ca);
    xi_big_free(cb);
    return xi_num_make_bigdec(rc, e);
}

fn xi_bigdec_mul(a: *const XiNum, b: *const XiNum) *XiNum {
    var ca: *XiBig = undefined;
    var cb: *XiBig = undefined;
    var ea: i32 = undefined;
    var eb: i32 = undefined;
    xi_num_to_bigdec_parts(a, &ca, &ea);
    xi_num_to_bigdec_parts(b, &cb, &eb);
    const rc = xi_big_mul(ca, cb);
    xi_big_free(ca);
    xi_big_free(cb);
    return xi_num_make_bigdec(rc, ea + eb);
}

fn xi_bigdec_cmp(a: *const XiNum, b: *const XiNum) c_int {
    var ca: *XiBig = undefined;
    var cb: *XiBig = undefined;
    var ea: i32 = undefined;
    var eb: i32 = undefined;
    xi_num_to_bigdec_parts(a, &ca, &ea);
    xi_num_to_bigdec_parts(b, &cb, &eb);
    const e = if (ea < eb) ea else eb;
    if (ea > e) xi_big_scale10(ca, @intCast(ea - e));
    if (eb > e) xi_big_scale10(cb, @intCast(eb - e));
    const c = xi_big_cmp(ca, cb);
    xi_big_free(ca);
    xi_big_free(cb);
    return c;
}

fn xi_bigdec_normalize(coeff: **XiBig, exp: *i32) void {
    var c = coeff.*;
    while (c.sign != 0) {
        const t = xi_big_clone(c);
        const r = xi_big_divmod_small(t, 10);
        if (r != 0) {
            xi_big_free(t);
            break;
        }
        xi_big_free(c);
        c = t;
        exp.* += 1;
    }
    coeff.* = c;
}

const XI_DEC_DIV_PRECISION: c_int = 50;

fn xi_bigdec_div(a: *XiNum, b: *XiNum) *XiNum {
    var ca: *XiBig = undefined;
    var cb: *XiBig = undefined;
    var ea: i32 = undefined;
    var eb: i32 = undefined;
    xi_num_to_bigdec_parts(a, &ca, &ea);
    xi_num_to_bigdec_parts(b, &cb, &eb);

    if (cb.sign == 0) {
        xi_big_free(ca);
        xi_big_free(cb);
        return xi_num_make_dec(xi_as_f64(a) / xi_as_f64(b));
    }
    if (ca.sign == 0) {
        xi_big_free(ca);
        xi_big_free(cb);
        return xi_num_make_bigdec(xi_big_new(1), 0);
    }

    const result_sign = ca.sign * cb.sign;
    ca.sign = 1;
    cb.sign = 1;

    const P = XI_DEC_DIV_PRECISION;
    const digA = xi_big_decimal_digits(ca);
    const digB = xi_big_decimal_digits(cb);
    var scale: i32 = @intCast(P + 2 - digA + digB);
    if (scale < 0) scale = 0;
    const num = xi_big_clone(ca);
    if (scale > 0) xi_big_scale10(num, @intCast(scale));
    var q: *XiBig = undefined;
    var rem: *XiBig = undefined;
    xi_big_divmod(num, cb, &q, &rem);
    xi_big_free(num);
    var exp: i32 = ea - eb - scale;
    const nonzero_tail = (rem.sign != 0);
    xi_big_free(rem);

    if (nonzero_tail) {
        const dq = xi_big_decimal_digits(q);
        if (dq > P) {
            const drop: u32 = @intCast(dq - P);
            const tenp = xi_big_ten_pow(drop);
            var qh: *XiBig = undefined;
            var rd: *XiBig = undefined;
            xi_big_divmod(q, tenp, &qh, &rd);
            const half = xi_big_ten_pow(drop);
            _ = xi_big_divmod_small(half, 2);
            const cmp = xi_big_ucmp(rd, half);
            const roundup = cmp >= 0;
            if (roundup) {
                const one = xi_big_from_i64(1);
                const inc = xi_big_add(qh, one);
                xi_big_free(qh);
                xi_big_free(one);
                qh = inc;
            }
            xi_big_free(q);
            xi_big_free(tenp);
            xi_big_free(rd);
            xi_big_free(half);
            q = qh;
            exp += @intCast(drop);
        }
    }

    q.sign = if (q.len != 0) result_sign else 0;
    xi_bigdec_normalize(&q, &exp);
    xi_big_free(ca);
    xi_big_free(cb);
    return xi_num_make_bigdec(q, exp);
}

fn xi_num_int_cmp(a: *XiNum, b: *XiNum) c_int {
    if (a.tag == XI_NUM_INT and b.tag == XI_NUM_INT) {
        return if (a.i < b.i) @as(c_int, -1) else (if (a.i > b.i) @as(c_int, 1) else 0);
    }
    const ba = xi_num_to_big(a);
    const bb = xi_num_to_big(b);
    const c = xi_big_cmp(ba, bb);
    xi_big_free(ba);
    xi_big_free(bb);
    return c;
}

fn xi_big_fits_nonneg_i64(b: *const XiBig, out: *i64) bool {
    if (b.sign < 0) return false;
    const max = xi_big_from_i64(INT64_MAX);
    const fits = xi_big_ucmp(b, max) <= 0;
    xi_big_free(max);
    if (!fits) return false;
    out.* = xi_big_to_i64_trunc(b);
    return true;
}

fn xi_num_nonneg_i64_exact(n: ?*XiNum, out: *i64) bool {
    const nn = n orelse return false;
    if (nn.tag == XI_NUM_INT) {
        if (nn.i < 0) return false;
        out.* = nn.i;
        return true;
    }
    if (nn.tag == XI_NUM_BIG) return xi_big_fits_nonneg_i64(nn.big.?, out);
    if (nn.tag == XI_NUM_WIDE) {
        const b = xi_big_from_wide(&nn.wide, nn.wide_bits / 8, 1);
        const ok = xi_big_fits_nonneg_i64(b, out);
        xi_big_free(b);
        return ok;
    }
    return false;
}

fn xi_add_ovf_i64(a: i64, b: i64, out: *i64) bool {
    const r = @addWithOverflow(a, b);
    out.* = r[0];
    return r[1] != 0;
}
fn xi_sub_ovf_i64(a: i64, b: i64, out: *i64) bool {
    const r = @subWithOverflow(a, b);
    out.* = r[0];
    return r[1] != 0;
}
fn xi_mul_ovf_i64(a: i64, b: i64, out: *i64) bool {
    const r = @mulWithOverflow(a, b);
    out.* = r[0];
    return r[1] != 0;
}

fn xi_soft_format(width: u32, exp_bits: *u32, prec: *u32) void {
    var w: u32 = undefined;
    if (width == 16) {
        w = 5;
    } else if (width == 32) {
        w = 8;
    } else if (width == 64) {
        w = 11;
    } else {
        const lw = 4.0 * (log(@floatFromInt(width)) / log(2.0)) - 13.0;
        var wr: c_long = lround(lw);
        if (wr < 2) wr = 2;
        if (wr > 60) wr = 60;
        if (@as(u32, @intCast(wr)) > width - 2) wr = @intCast(width - 2);
        w = @intCast(wr);
    }
    exp_bits.* = w;
    prec.* = width - w;
}

fn xi_soft_alloc(width: u32) *XiNum {
    const n = xi_num_new();
    n.tag = XI_NUM_SOFTFLOAT;
    n.big = null;
    n.wide_bits = @intCast(width);
    n.soft_kind = XI_SF_NORMAL;
    n.soft_sign = 1;
    n.i = 0;
    n.d = 0.0;
    return n;
}

fn xi_soft_zero(width: u32, sign: c_int) *XiNum {
    const n = xi_soft_alloc(width);
    n.soft_kind = XI_SF_ZERO;
    n.soft_sign = if (sign < 0) -1 else 1;
    n.d = if (sign < 0) -0.0 else 0.0;
    return n;
}

fn xi_soft_inf(width: u32, sign: c_int) *XiNum {
    const n = xi_soft_alloc(width);
    n.soft_kind = XI_SF_INF;
    n.soft_sign = if (sign < 0) -1 else 1;
    n.d = if (sign < 0) -INFINITY else INFINITY;
    return n;
}

fn xi_soft_nan(width: u32) *XiNum {
    const n = xi_soft_alloc(width);
    n.soft_kind = XI_SF_NAN;
    n.d = NAN;
    return n;
}

fn xi_soft_to_double(n: *const XiNum) f64 {
    if (n.soft_kind == XI_SF_NAN) return NAN;
    if (n.soft_kind == XI_SF_INF) return if (n.soft_sign < 0) -INFINITY else INFINITY;
    if (n.soft_kind == XI_SF_ZERO or n.big == null or n.big.?.len == 0) {
        return if (n.soft_sign < 0) -0.0 else 0.0;
    }
    const b = xi_big_bitlen(n.big.?);
    var mant: i64 = undefined;
    var e2: i64 = undefined;
    if (b > 53) {
        const top = xi_big_shr(n.big.?, b - 53);
        mant = xi_big_to_i64_trunc(top);
        xi_big_free(top);
        e2 = n.i + @as(i64, @intCast(b - 53));
    } else {
        mant = xi_big_to_i64_trunc(n.big.?);
        e2 = n.i;
    }
    if (e2 > 100000) return if (n.soft_sign < 0) -INFINITY else INFINITY;
    if (e2 < -100000) return if (n.soft_sign < 0) -0.0 else 0.0;
    const r = ldexp(@floatFromInt(mant), @intCast(e2));
    return if (n.soft_sign < 0) -r else r;
}

fn xi_soft_from_parts(sign: c_int, mag_in: ?*XiBig, E_in: i64, width: u32) *XiNum {
    var mag = mag_in;
    var E = E_in;
    var w: u32 = undefined;
    var prec: u32 = undefined;
    xi_soft_format(width, &w, &prec);
    if (mag) |m| m.sign = if (m.len != 0) 1 else 0;
    if (mag == null or xi_big_is_zero(mag.?)) {
        if (mag) |m| xi_big_free(m);
        return xi_soft_zero(width, sign);
    }
    var b = xi_big_bitlen(mag.?);
    if (b > prec) {
        const drop = b - prec;
        const round_bit = xi_big_test_bit(mag.?, drop - 1);
        const sticky = if (drop >= 1) xi_big_low_bits_nonzero(mag.?, drop - 1) else 0;
        const qh = xi_big_shr(mag.?, drop);
        xi_big_free(mag.?);
        mag = qh;
        E += @as(i64, @intCast(drop));
        if (round_bit != 0 and (sticky != 0 or xi_big_test_bit(mag.?, 0) != 0)) {
            const one = xi_big_from_i64(1);
            const s = xi_big_add(mag.?, one);
            xi_big_free(one);
            xi_big_free(mag.?);
            mag = s;
            if (xi_big_bitlen(mag.?) > prec) {
                const q2 = xi_big_shr(mag.?, 1);
                xi_big_free(mag.?);
                mag = q2;
                E += 1;
            }
        }
    } else if (b < prec) {
        const s = xi_big_shl(mag.?, prec - b);
        xi_big_free(mag.?);
        mag = s;
        E -= @as(i64, @intCast(prec - b));
    }
    b = xi_big_bitlen(mag.?);
    const e = E + @as(i64, @intCast(prec)) - 1;
    const emax: i64 = (@as(i64, 1) << @as(u6, @intCast(w - 1))) - 1;
    const emin: i64 = 2 - (@as(i64, 1) << @as(u6, @intCast(w - 1)));
    if (e > emax) {
        xi_big_free(mag.?);
        return xi_soft_inf(width, sign);
    }
    if (e < emin) {
        xi_big_free(mag.?);
        return xi_soft_zero(width, sign);
    }
    const n = xi_soft_alloc(width);
    n.soft_kind = XI_SF_NORMAL;
    n.soft_sign = if (sign < 0) -1 else 1;
    mag.?.sign = if (mag.?.len != 0) 1 else 0;
    n.big = mag;
    n.i = E;
    n.d = xi_soft_to_double(n);
    return n;
}

fn xi_soft_from_double(d: f64, width: u32) *XiNum {
    if (cIsNan(d)) return xi_soft_nan(width);
    const sign: c_int = if (cSignbit(d)) -1 else 1;
    if (cIsInf(d)) return xi_soft_inf(width, sign);
    if (d == 0.0) return xi_soft_zero(width, sign);
    var e2: c_int = 0;
    const m = frexp(fabs(d), &e2);
    const mant: i64 = @intFromFloat(ldexp(m, 53));
    const mag = xi_big_from_i64(mant);
    return xi_soft_from_parts(sign, mag, @as(i64, e2) - 53, width);
}

fn xi_soft_from_bigint_owned(sign: c_int, mag: *XiBig, width: u32) *XiNum {
    mag.sign = if (mag.len != 0) 1 else 0;
    return xi_soft_from_parts(sign, mag, 0, width);
}

fn xi_soft_from_fraction(sign: c_int, num_in: *XiBig, den_in: *XiBig, width: u32) *XiNum {
    const num = num_in;
    const den = den_in;
    if (xi_big_is_zero(num)) {
        xi_big_free(num);
        xi_big_free(den);
        return xi_soft_zero(width, sign);
    }
    var w: u32 = undefined;
    var prec: u32 = undefined;
    xi_soft_format(width, &w, &prec);
    const bn: i64 = @intCast(xi_big_bitlen(num));
    const bd: i64 = @intCast(xi_big_bitlen(den));
    const k: i64 = (@as(i64, prec) + 4) - (bn - bd);
    var A: *XiBig = undefined;
    var B: *XiBig = undefined;
    if (k >= 0) {
        A = xi_big_shl(num, @intCast(k));
        B = xi_big_clone(den);
    } else {
        A = xi_big_clone(num);
        B = xi_big_shl(den, @intCast(-k));
    }
    xi_big_free(num);
    xi_big_free(den);
    var Q: *XiBig = undefined;
    var R: *XiBig = undefined;
    xi_big_divmod(A, B, &Q, &R);
    const sticky = !xi_big_is_zero(R);
    xi_big_free(A);
    xi_big_free(B);
    xi_big_free(R);
    if (sticky) {
        if (Q.len == 0) {
            xi_big_reserve(Q, 1);
            Q.len = 1;
        }
        Q.limbs[0] |= 1;
        Q.sign = 1;
    }
    return xi_soft_from_parts(sign, Q, -k, width);
}

fn xi_soft_from_bigdec_owned(sign: c_int, coeff_in: *XiBig, dexp: i32, width: u32) *XiNum {
    const coeff = coeff_in;
    if (xi_big_is_zero(coeff)) {
        xi_big_free(coeff);
        return xi_soft_zero(width, sign);
    }
    var num: *XiBig = undefined;
    var den: *XiBig = undefined;
    if (dexp >= 0) {
        const p = xi_big_ten_pow(@intCast(dexp));
        num = xi_big_mul(coeff, p);
        xi_big_free(p);
        xi_big_free(coeff);
        den = xi_big_from_i64(1);
    } else {
        num = coeff;
        den = xi_big_ten_pow(@intCast(-dexp));
    }
    return xi_soft_from_fraction(sign, num, den, width);
}

fn xi_soft_recoerce(sf: *const XiNum, width: u32) *XiNum {
    switch (sf.soft_kind) {
        XI_SF_ZERO => return xi_soft_zero(width, sf.soft_sign),
        XI_SF_INF => return xi_soft_inf(width, sf.soft_sign),
        XI_SF_NAN => return xi_soft_nan(width),
        else => {},
    }
    const mag = xi_big_clone(sf.big.?);
    return xi_soft_from_parts(sf.soft_sign, mag, sf.i, width);
}

fn xi_soft_coerce_value(v: *XiNum, width: u32) *XiNum {
    switch (v.tag) {
        XI_NUM_SOFTFLOAT => return if (v.wide_bits == width) v else xi_soft_recoerce(v, width),
        XI_NUM_DEC => return xi_soft_from_double(v.d, width),
        XI_NUM_BIGDEC => {
            const c = xi_big_clone(v.big.?);
            const sign: c_int = if (c.sign < 0) -1 else 1;
            c.sign = if (c.len != 0) 1 else 0;
            return xi_soft_from_bigdec_owned(sign, c, v.dec_exp, width);
        },
        else => {
            const b = xi_num_to_big(v);
            const sign: c_int = if (b.sign < 0) -1 else 1;
            b.sign = if (b.len != 0) 1 else 0;
            return xi_soft_from_bigint_owned(sign, b, width);
        },
    }
}

export fn xi_softfloat_coerce(v: *XiNum, width: i32) callconv(.c) *XiNum {
    return xi_soft_coerce_value(v, @intCast(width));
}

export fn xi_softfloat_from_decimal(s: OptStr, width: i32) callconv(.c) *XiNum {
    const bd = xi_num_dec_from_decimal(s);
    const c = xi_big_clone(bd.big.?);
    const sign: c_int = if (c.sign < 0) -1 else 1;
    c.sign = if (c.len != 0) 1 else 0;
    return xi_soft_from_bigdec_owned(sign, c, bd.dec_exp, @intCast(width));
}

export fn xi_softfloat_zero(width: i32) callconv(.c) *XiNum {
    return xi_soft_zero(@intCast(width), 1);
}

fn xi_soft_result_width(a: *const XiNum, b: *const XiNum) u32 {
    const wa: u32 = if (a.tag == XI_NUM_SOFTFLOAT) a.wide_bits else 0;
    const wb: u32 = if (b.tag == XI_NUM_SOFTFLOAT) b.wide_bits else 0;
    const w = if (wa > wb) wa else wb;
    return if (w != 0) w else 64;
}

fn xi_soft_value_cmp(a: *const XiNum, b: *const XiNum) c_int {
    if (a.soft_kind == XI_SF_NAN or b.soft_kind == XI_SF_NAN) return 2;
    const za = (a.soft_kind == XI_SF_ZERO);
    const zb = (b.soft_kind == XI_SF_ZERO);
    if (za and zb) return 0;
    const sa: c_int = if (a.soft_sign < 0) -1 else 1;
    const sb: c_int = if (b.soft_sign < 0) -1 else 1;
    if (za) return if (sb < 0) 1 else -1;
    if (zb) return if (sa < 0) -1 else 1;
    if (sa != sb) return if (sa > sb) 1 else -1;
    var mag: c_int = undefined;
    if (a.soft_kind == XI_SF_INF or b.soft_kind == XI_SF_INF) {
        const ia = (a.soft_kind == XI_SF_INF);
        const ib = (b.soft_kind == XI_SF_INF);
        mag = if (ia and ib) 0 else (if (ia) @as(c_int, 1) else -1);
    } else {
        var wa: u32 = undefined;
        var pa: u32 = undefined;
        var wb: u32 = undefined;
        var pb: u32 = undefined;
        xi_soft_format(a.wide_bits, &wa, &pa);
        xi_soft_format(b.wide_bits, &wb, &pb);
        const ea = a.i + @as(i64, pa) - 1;
        const eb = b.i + @as(i64, pb) - 1;
        if (ea != eb) {
            mag = if (ea > eb) 1 else -1;
        } else {
            const la = xi_big_shl(a.big.?, pb - 1);
            const lb = xi_big_shl(b.big.?, pa - 1);
            mag = xi_big_ucmp(la, lb);
            xi_big_free(la);
            xi_big_free(lb);
        }
    }
    return if (sa < 0) -mag else mag;
}

fn xi_soft_add_same(a: *const XiNum, bin: *const XiNum, width: u32, subtract: bool) *XiNum {
    var nb = bin.*;
    if (subtract) nb.soft_sign = -bin.soft_sign;
    const b = &nb;
    if (a.soft_kind == XI_SF_NAN or b.soft_kind == XI_SF_NAN) return xi_soft_nan(width);
    if (a.soft_kind == XI_SF_INF or b.soft_kind == XI_SF_INF) {
        const ia = (a.soft_kind == XI_SF_INF);
        const ib = (b.soft_kind == XI_SF_INF);
        if (ia and ib) {
            return if (a.soft_sign == b.soft_sign) xi_soft_inf(width, a.soft_sign) else xi_soft_nan(width);
        }
        return xi_soft_inf(width, if (ia) a.soft_sign else b.soft_sign);
    }
    if (a.soft_kind == XI_SF_ZERO and b.soft_kind == XI_SF_ZERO) return xi_soft_zero(width, 1);
    if (a.soft_kind == XI_SF_ZERO) return xi_soft_from_parts(b.soft_sign, xi_big_clone(b.big.?), b.i, width);
    if (b.soft_kind == XI_SF_ZERO) return xi_soft_from_parts(a.soft_sign, xi_big_clone(a.big.?), a.i, width);
    var w: u32 = undefined;
    var prec: u32 = undefined;
    xi_soft_format(width, &w, &prec);
    const hi = if (a.i >= b.i) a else b;
    const lo = if (a.i >= b.i) b else a;
    const diff = hi.i - lo.i;
    const shi: c_int = if (hi.soft_sign < 0) -1 else 1;
    const slo: c_int = if (lo.soft_sign < 0) -1 else 1;
    var magR: *XiBig = undefined;
    var E0: i64 = undefined;
    var signR: c_int = undefined;
    if (diff > @as(i64, prec) + 4) {
        const shifted = xi_big_shl(hi.big.?, @as(usize, prec) + 4);
        E0 = hi.i - (@as(i64, prec) + 4);
        const one = xi_big_from_i64(1);
        if (shi == slo) {
            magR = xi_big_uadd(shifted, one);
            signR = shi;
        } else {
            magR = xi_big_usub(shifted, one);
            signR = shi;
        }
        xi_big_free(one);
        xi_big_free(shifted);
    } else {
        const shifted = xi_big_shl(hi.big.?, @intCast(diff));
        E0 = lo.i;
        if (shi == slo) {
            magR = xi_big_uadd(shifted, lo.big.?);
            signR = shi;
        } else {
            const c = xi_big_ucmp(shifted, lo.big.?);
            if (c == 0) {
                xi_big_free(shifted);
                return xi_soft_zero(width, 1);
            }
            if (c > 0) {
                magR = xi_big_usub(shifted, lo.big.?);
                signR = shi;
            } else {
                magR = xi_big_usub(lo.big.?, shifted);
                signR = slo;
            }
        }
        xi_big_free(shifted);
    }
    return xi_soft_from_parts(signR, magR, E0, width);
}

fn xi_soft_mul_same(a: *const XiNum, b: *const XiNum, width: u32) *XiNum {
    if (a.soft_kind == XI_SF_NAN or b.soft_kind == XI_SF_NAN) return xi_soft_nan(width);
    const sign: c_int = (if (a.soft_sign < 0) @as(c_int, -1) else 1) * (if (b.soft_sign < 0) @as(c_int, -1) else 1);
    const az = (a.soft_kind == XI_SF_ZERO);
    const bz = (b.soft_kind == XI_SF_ZERO);
    const ai = (a.soft_kind == XI_SF_INF);
    const bi = (b.soft_kind == XI_SF_INF);
    if ((ai and bz) or (bi and az)) return xi_soft_nan(width);
    if (ai or bi) return xi_soft_inf(width, sign);
    if (az or bz) return xi_soft_zero(width, sign);
    const magR = xi_big_mul(a.big.?, b.big.?);
    return xi_soft_from_parts(sign, magR, a.i + b.i, width);
}

fn xi_soft_div_same(a: *const XiNum, b: *const XiNum, width: u32) *XiNum {
    if (a.soft_kind == XI_SF_NAN or b.soft_kind == XI_SF_NAN) return xi_soft_nan(width);
    const sign: c_int = (if (a.soft_sign < 0) @as(c_int, -1) else 1) * (if (b.soft_sign < 0) @as(c_int, -1) else 1);
    const az = (a.soft_kind == XI_SF_ZERO);
    const bz = (b.soft_kind == XI_SF_ZERO);
    const ai = (a.soft_kind == XI_SF_INF);
    const bi = (b.soft_kind == XI_SF_INF);
    if ((ai and bi) or (az and bz)) return xi_soft_nan(width);
    if (ai or bz) return xi_soft_inf(width, sign);
    if (az or bi) return xi_soft_zero(width, sign);
    var w: u32 = undefined;
    var prec: u32 = undefined;
    xi_soft_format(width, &w, &prec);
    const shift: usize = @as(usize, prec) + 4;
    const A = xi_big_shl(a.big.?, shift);
    var Q: *XiBig = undefined;
    var R: *XiBig = undefined;
    xi_big_divmod(A, b.big.?, &Q, &R);
    const sticky = !xi_big_is_zero(R);
    xi_big_free(A);
    xi_big_free(R);
    if (sticky) {
        if (Q.len == 0) {
            xi_big_reserve(Q, 1);
            Q.len = 1;
        }
        Q.limbs[0] |= 1;
        Q.sign = 1;
    }
    return xi_soft_from_parts(sign, Q, a.i - b.i - @as(i64, @intCast(shift)), width);
}

fn xi_soft_binop(a: *XiNum, b: *XiNum, op: u8) *XiNum {
    const W = xi_soft_result_width(a, b);
    const ca = xi_soft_coerce_value(a, W);
    const cb = xi_soft_coerce_value(b, W);
    return switch (op) {
        '+' => xi_soft_add_same(ca, cb, W, false),
        '-' => xi_soft_add_same(ca, cb, W, true),
        '*' => xi_soft_mul_same(ca, cb, W),
        else => xi_soft_div_same(ca, cb, W),
    };
}

fn xi_soft_cmp(a: *XiNum, b: *XiNum) c_int {
    const W = xi_soft_result_width(a, b);
    const ca = xi_soft_coerce_value(a, W);
    const cb = xi_soft_coerce_value(b, W);
    return xi_soft_value_cmp(ca, cb);
}

fn xi_soft_sig_digits(n: *const XiNum, k: c_int, out: [*]u8, dexp_out: *i64) void {
    const M = n.big.?;
    const E = n.i;
    var num: *XiBig = undefined;
    var den: *XiBig = undefined;
    if (E >= 0) {
        num = xi_big_shl(M, @intCast(E));
        den = xi_big_from_i64(1);
    } else {
        num = xi_big_clone(M);
        num.sign = if (num.len != 0) 1 else 0;
        const one = xi_big_from_i64(1);
        den = xi_big_shl(one, @intCast(-E));
        xi_big_free(one);
    }
    const bn = xi_big_bitlen(num);
    const bd = xi_big_bitlen(den);
    const approx = @as(f64, @floatFromInt(@as(i64, @intCast(bn)) - @as(i64, @intCast(bd)))) * 0.301029995663981195;
    var dexp: i64 = @intFromFloat(floor(approx));
    var digits: ?[*:0]u8 = null;
    var iter: c_int = 0;
    while (iter < 6) : (iter += 1) {
        const s: i64 = @as(i64, k - 1) - dexp;
        var sn: *XiBig = undefined;
        var sd: *XiBig = undefined;
        if (s >= 0) {
            const p = xi_big_ten_pow(@intCast(s));
            sn = xi_big_mul(num, p);
            xi_big_free(p);
            sd = xi_big_clone(den);
        } else {
            sn = xi_big_clone(num);
            const p = xi_big_ten_pow(@intCast(-s));
            sd = xi_big_mul(den, p);
            xi_big_free(p);
        }
        var Q: *XiBig = undefined;
        var R: *XiBig = undefined;
        xi_big_divmod(sn, sd, &Q, &R);
        const r2 = xi_big_clone(R);
        xi_big_shl1(r2);
        r2.sign = if (r2.len != 0) 1 else 0;
        const c = xi_big_ucmp(r2, sd);
        xi_big_free(r2);
        const roundup = (c > 0) or (c == 0 and Q.len > 0 and (Q.limbs[0] & 1) != 0);
        if (roundup) {
            const one = xi_big_from_i64(1);
            const t = xi_big_add(Q, one);
            xi_big_free(one);
            xi_big_free(Q);
            Q = t;
        }
        xi_big_free(sn);
        xi_big_free(sd);
        xi_big_free(R);
        if (digits) |dg| cfree(dg);
        digits = xi_big_to_decimal(Q);
        xi_big_free(Q);
        const ndig: c_int = @intCast(strlen(digits.?));
        if (ndig == k) break;
        dexp += @as(i64, ndig - k);
    }
    const ndig: c_int = @intCast(strlen(digits.?));
    var j: c_int = 0;
    while (j < k) : (j += 1) {
        out[@intCast(j)] = if (j < ndig) digits.?[@intCast(j)] else '0';
    }
    out[@intCast(k)] = 0;
    cfree(digits.?);
    dexp_out.* = dexp;
    xi_big_free(num);
    xi_big_free(den);
}

fn xi_soft_format_decimal(sign: c_int, digits: [*]const u8, ndig_in: c_int, dexp: i64) [*:0]u8 {
    var ndig = ndig_in;
    while (ndig > 1 and digits[@intCast(ndig - 1)] == '0') ndig -= 1;
    var cap: usize = @as(usize, @intCast(ndig)) + 64;
    if (dexp > 0 and dexp < 64) cap += @intCast(dexp);
    const buf: [*]u8 = @ptrCast(malloc(cap) orelse xi_oom());
    var p: usize = 0;
    if (sign < 0) {
        buf[p] = '-';
        p += 1;
    }
    if (dexp >= -4 and dexp < 21) {
        if (dexp >= 0) {
            const intlen: c_int = @as(c_int, @intCast(dexp)) + 1;
            if (ndig <= intlen) {
                var j: c_int = 0;
                while (j < ndig) : (j += 1) {
                    buf[p] = digits[@intCast(j)];
                    p += 1;
                }
                j = ndig;
                while (j < intlen) : (j += 1) {
                    buf[p] = '0';
                    p += 1;
                }
            } else {
                var j: c_int = 0;
                while (j < intlen) : (j += 1) {
                    buf[p] = digits[@intCast(j)];
                    p += 1;
                }
                buf[p] = '.';
                p += 1;
                j = intlen;
                while (j < ndig) : (j += 1) {
                    buf[p] = digits[@intCast(j)];
                    p += 1;
                }
            }
        } else {
            buf[p] = '0';
            p += 1;
            buf[p] = '.';
            p += 1;
            var j: c_int = 0;
            while (j < @as(c_int, @intCast(-dexp - 1))) : (j += 1) {
                buf[p] = '0';
                p += 1;
            }
            j = 0;
            while (j < ndig) : (j += 1) {
                buf[p] = digits[@intCast(j)];
                p += 1;
            }
        }
    } else {
        buf[p] = digits[0];
        p += 1;
        if (ndig > 1) {
            buf[p] = '.';
            p += 1;
            var j: c_int = 1;
            while (j < ndig) : (j += 1) {
                buf[p] = digits[@intCast(j)];
                p += 1;
            }
        }
        const wrote = snprintf(buf + p, cap - p, "e%+lld", @as(c_longlong, dexp));
        p += @intCast(wrote);
    }
    buf[p] = 0;
    return @ptrCast(buf);
}

fn xi_soft_repr_eq(a: *const XiNum, b: *const XiNum) bool {
    if (a.soft_kind != b.soft_kind or a.wide_bits != b.wide_bits) return false;
    if (a.soft_kind == XI_SF_NAN) return true;
    if (a.soft_kind != XI_SF_NORMAL) return a.soft_sign == b.soft_sign;
    return a.soft_sign == b.soft_sign and a.i == b.i and xi_big_ucmp(a.big.?, b.big.?) == 0;
}

fn xi_dup_cstr(s: [*:0]const u8) [*:0]u8 {
    const n = strlen(s) + 1;
    const p: [*]u8 = @ptrCast(malloc(n) orelse xi_oom());
    cmemcpy(p, s, n);
    return @ptrCast(p);
}

fn xi_soft_to_string(n: *const XiNum) [*:0]u8 {
    if (n.soft_kind == XI_SF_NAN) return xi_dup_cstr("nan");
    if (n.soft_kind == XI_SF_INF) return xi_dup_cstr(if (n.soft_sign < 0) "-inf" else "inf");
    if (n.soft_kind == XI_SF_ZERO or n.big == null or n.big.?.len == 0) return xi_dup_cstr("0");
    var w: u32 = undefined;
    var prec: u32 = undefined;
    xi_soft_format(n.wide_bits, &w, &prec);
    var maxsig: c_int = @intFromFloat(@as(f64, @floatFromInt(prec)) * 0.30102999566 + 2.0);
    if (maxsig < 1) maxsig = 1;
    if (maxsig > 20000) maxsig = 20000;
    const digbuf: [*]u8 = @ptrCast(malloc(@as(usize, @intCast(maxsig)) + 2) orelse xi_oom());
    var dexp: i64 = 0;
    var k: c_int = 1;
    while (k <= maxsig) : (k += 1) {
        xi_soft_sig_digits(n, k, digbuf, &dexp);
        const cand = xi_soft_format_decimal(n.soft_sign, digbuf, k, dexp);
        const cs = xi_softfloat_from_decimal(@ptrCast(cand), @intCast(n.wide_bits));
        if (xi_soft_repr_eq(cs, n)) {
            cfree(digbuf);
            return cand;
        }
        cfree(cand);
    }
    xi_soft_sig_digits(n, maxsig, digbuf, &dexp);
    const cand = xi_soft_format_decimal(n.soft_sign, digbuf, maxsig, dexp);
    cfree(digbuf);
    return cand;
}

export fn xi_num_add(a: *XiNum, b: *XiNum) callconv(.c) *XiNum {
    if (a.tag == XI_NUM_SOFTFLOAT or b.tag == XI_NUM_SOFTFLOAT) return xi_soft_binop(a, b, '+');
    if (a.tag == XI_NUM_DEC or b.tag == XI_NUM_DEC) return xi_num_make_dec(xi_as_f64(a) + xi_as_f64(b));
    if (a.tag == XI_NUM_BIGDEC or b.tag == XI_NUM_BIGDEC) return xi_bigdec_addsub(a, b, false);
    if (a.tag == XI_NUM_INT and b.tag == XI_NUM_INT) {
        var r: i64 = 0;
        if (!xi_add_ovf_i64(a.i, b.i, &r)) return xi_num_make_int(r);
    }
    const ba = xi_num_to_big(a);
    const bb = xi_num_to_big(b);
    const br = xi_big_add(ba, bb);
    xi_big_free(ba);
    xi_big_free(bb);
    return xi_num_make_big(br);
}

export fn xi_num_sub(a: *XiNum, b: *XiNum) callconv(.c) *XiNum {
    if (a.tag == XI_NUM_SOFTFLOAT or b.tag == XI_NUM_SOFTFLOAT) return xi_soft_binop(a, b, '-');
    if (a.tag == XI_NUM_DEC or b.tag == XI_NUM_DEC) return xi_num_make_dec(xi_as_f64(a) - xi_as_f64(b));
    if (a.tag == XI_NUM_BIGDEC or b.tag == XI_NUM_BIGDEC) return xi_bigdec_addsub(a, b, true);
    if (a.tag == XI_NUM_INT and b.tag == XI_NUM_INT) {
        var r: i64 = 0;
        if (!xi_sub_ovf_i64(a.i, b.i, &r)) return xi_num_make_int(r);
    }
    const ba = xi_num_to_big(a);
    const bb = xi_num_to_big(b);
    const br = xi_big_sub(ba, bb);
    xi_big_free(ba);
    xi_big_free(bb);
    return xi_num_make_big(br);
}

export fn xi_num_mul(a: *XiNum, b: *XiNum) callconv(.c) *XiNum {
    if (a.tag == XI_NUM_SOFTFLOAT or b.tag == XI_NUM_SOFTFLOAT) return xi_soft_binop(a, b, '*');
    if (a.tag == XI_NUM_DEC or b.tag == XI_NUM_DEC) return xi_num_make_dec(xi_as_f64(a) * xi_as_f64(b));
    if (a.tag == XI_NUM_BIGDEC or b.tag == XI_NUM_BIGDEC) return xi_bigdec_mul(a, b);
    if (a.tag == XI_NUM_INT and b.tag == XI_NUM_INT) {
        var r: i64 = 0;
        if (!xi_mul_ovf_i64(a.i, b.i, &r)) return xi_num_make_int(r);
    }
    const ba = xi_num_to_big(a);
    const bb = xi_num_to_big(b);
    const br = xi_big_mul(ba, bb);
    xi_big_free(ba);
    xi_big_free(bb);
    return xi_num_make_big(br);
}

fn xi_num_i64_operand(value: i64) XiNum {
    var out: XiNum = undefined;
    out.tag = XI_NUM_INT;
    out.i = value;
    return out;
}

export fn xi_num_add_i64(a: *XiNum, b: i64) callconv(.c) *XiNum {
    var rhs = xi_num_i64_operand(b);
    return xi_num_add(a, &rhs);
}

export fn xi_num_sub_i64(a: *XiNum, b: i64) callconv(.c) *XiNum {
    var rhs = xi_num_i64_operand(b);
    return xi_num_sub(a, &rhs);
}

export fn xi_num_i64_sub(a: i64, b: *XiNum) callconv(.c) *XiNum {
    var lhs = xi_num_i64_operand(a);
    return xi_num_sub(&lhs, b);
}

export fn xi_num_mul_i64(a: *XiNum, b: i64) callconv(.c) *XiNum {
    var rhs = xi_num_i64_operand(b);
    return xi_num_mul(a, &rhs);
}

export fn xi_num_div(a: *XiNum, b: *XiNum) callconv(.c) *XiNum {
    if (a.tag == XI_NUM_SOFTFLOAT or b.tag == XI_NUM_SOFTFLOAT) return xi_soft_binop(a, b, '/');
    if (a.tag == XI_NUM_DEC or b.tag == XI_NUM_DEC) return xi_num_make_dec(xi_as_f64(a) / xi_as_f64(b));
    return xi_bigdec_div(a, b);
}

export fn xi_num_mod(a: *XiNum, b: *XiNum) callconv(.c) *XiNum {
    if (a.tag == XI_NUM_INT and b.tag == XI_NUM_INT) {
        if (b.i == 0 or b.i == -1) return xi_num_make_int(0);
        return xi_num_make_int(@rem(a.i, b.i));
    }
    const ba = xi_num_to_big(a);
    const bb = xi_num_to_big(b);
    if (xi_big_is_zero(bb)) {
        xi_big_free(ba);
        xi_big_free(bb);
        return xi_num_make_int(0);
    }
    const dividend_sign = ba.sign;
    var q: *XiBig = undefined;
    var r: *XiBig = undefined;
    xi_big_divmod(ba, bb, &q, &r);
    xi_big_free(q);
    xi_big_free(ba);
    xi_big_free(bb);
    r.sign = if (r.len != 0) dividend_sign else 0;
    return xi_num_make_big(r);
}

export fn xi_num_pow(a: *XiNum, b: *XiNum) callconv(.c) *XiNum {
    var e: i64 = 0;
    if ((a.tag == XI_NUM_INT or a.tag == XI_NUM_BIG or a.tag == XI_NUM_WIDE) and xi_num_nonneg_i64_exact(b, &e)) {
        const base = xi_num_to_big(a);
        const result = xi_big_pow_u64(base, @bitCast(e));
        xi_big_free(base);
        return xi_num_make_big(result);
    }
    return xi_num_make_dec(pow(xi_as_f64(a), xi_as_f64(b)));
}

export fn xi_num_band(a: *XiNum, b: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_int(xi_num_to_i64(a) & xi_num_to_i64(b));
}
export fn xi_num_bor(a: *XiNum, b: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_int(xi_num_to_i64(a) | xi_num_to_i64(b));
}
export fn xi_num_bxor(a: *XiNum, b: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_int(xi_num_to_i64(a) ^ xi_num_to_i64(b));
}
export fn xi_num_bnot(a: *XiNum) callconv(.c) *XiNum {
    const one = xi_num_make_int(1);
    const xp1 = xi_num_add(a, one);
    const zero = xi_num_make_int(0);
    return xi_num_sub(zero, xp1);
}

export fn xi_bit_shl(x: *XiNum, n: *XiNum) callconv(.c) *XiNum {
    const shift = xi_num_to_i64(n);
    if (shift <= 0) return x;
    const base = xi_big_from_i64(2);
    const factor = xi_big_pow_u64(base, @bitCast(shift));
    xi_big_free(base);
    const bx = xi_num_to_big(x);
    const fac = xi_num_make_big(factor);
    const bxn = xi_num_make_big(bx);
    return xi_num_mul(bxn, fac);
}

export fn xi_bit_shr(x: *XiNum, n: *XiNum) callconv(.c) *XiNum {
    const shift = xi_num_to_i64(n);
    if (shift <= 0) return x;
    if (x.tag == XI_NUM_INT and x.i >= 0) {
        if (shift >= 64) return xi_num_make_int(0);
        return xi_num_make_int(@bitCast(@as(u64, @bitCast(x.i)) >> @as(u6, @intCast(shift))));
    }
    const base = xi_big_from_i64(2);
    const divisor = xi_big_pow_u64(base, @bitCast(shift));
    xi_big_free(base);
    const bx = xi_num_to_big(x);
    const sign = bx.sign;
    var q: *XiBig = undefined;
    var r: *XiBig = undefined;
    xi_big_divmod(bx, divisor, &q, &r);
    xi_big_free(r);
    xi_big_free(bx);
    xi_big_free(divisor);
    q.sign = if (q.len != 0) sign else 0;
    return xi_num_make_big(q);
}

fn xi_num_make_u64(v: u64) *XiNum {
    if (v <= @as(u64, 0x7FFFFFFFFFFFFFFF)) return xi_num_make_int(@bitCast(v));
    const b = xi_big_new(2);
    b.limbs[0] = @truncate(v & 0xffffffff);
    b.limbs[1] = @truncate(v >> 32);
    b.len = 2;
    xi_big_trim(b);
    b.sign = 1;
    return xi_num_make_big(b);
}

fn xi_bit_width_mask(width: i64) u64 {
    if (width >= 64) return ~@as(u64, 0);
    if (width <= 0) return 0;
    return (@as(u64, 1) << @as(u6, @intCast(width))) - 1;
}

export fn xi_bit_rotl(x: *XiNum, n: *XiNum, width: *XiNum) callconv(.c) *XiNum {
    var w = xi_num_to_i64(width);
    if (w <= 0) return x;
    if (w > 64) w = 64;
    const mask = xi_bit_width_mask(w);
    const v = @as(u64, @bitCast(xi_num_to_i64(x))) & mask;
    var r = @rem(xi_num_to_i64(n), w);
    if (r < 0) r += w;
    const res: u64 = if (r == 0) v else ((v << @as(u6, @intCast(r))) | (v >> @as(u6, @intCast(w - r)))) & mask;
    return xi_num_make_u64(res);
}

export fn xi_bit_rotr(x: *XiNum, n: *XiNum, width: *XiNum) callconv(.c) *XiNum {
    var w = xi_num_to_i64(width);
    if (w <= 0) return x;
    if (w > 64) w = 64;
    const mask = xi_bit_width_mask(w);
    const v = @as(u64, @bitCast(xi_num_to_i64(x))) & mask;
    var r = @rem(xi_num_to_i64(n), w);
    if (r < 0) r += w;
    const res: u64 = if (r == 0) v else ((v >> @as(u6, @intCast(r))) | (v << @as(u6, @intCast(w - r)))) & mask;
    return xi_num_make_u64(res);
}

export fn xi_num_lt(a: *XiNum, b: *XiNum) callconv(.c) bool {
    if (a.tag == XI_NUM_SOFTFLOAT or b.tag == XI_NUM_SOFTFLOAT) return xi_soft_cmp(a, b) == -1;
    if (a.tag == XI_NUM_DEC or b.tag == XI_NUM_DEC) return xi_as_f64(a) < xi_as_f64(b);
    if (a.tag == XI_NUM_BIGDEC or b.tag == XI_NUM_BIGDEC) return xi_bigdec_cmp(a, b) < 0;
    return xi_num_int_cmp(a, b) < 0;
}

export fn xi_num_le(a: *XiNum, b: *XiNum) callconv(.c) bool {
    if (a.tag == XI_NUM_SOFTFLOAT or b.tag == XI_NUM_SOFTFLOAT) {
        const c = xi_soft_cmp(a, b);
        return c == -1 or c == 0;
    }
    if (a.tag == XI_NUM_DEC or b.tag == XI_NUM_DEC) return xi_as_f64(a) <= xi_as_f64(b);
    if (a.tag == XI_NUM_BIGDEC or b.tag == XI_NUM_BIGDEC) return xi_bigdec_cmp(a, b) <= 0;
    return xi_num_int_cmp(a, b) <= 0;
}

export fn xi_num_gt(a: *XiNum, b: *XiNum) callconv(.c) bool {
    if (a.tag == XI_NUM_SOFTFLOAT or b.tag == XI_NUM_SOFTFLOAT) return xi_soft_cmp(a, b) == 1;
    if (a.tag == XI_NUM_DEC or b.tag == XI_NUM_DEC) return xi_as_f64(a) > xi_as_f64(b);
    if (a.tag == XI_NUM_BIGDEC or b.tag == XI_NUM_BIGDEC) return xi_bigdec_cmp(a, b) > 0;
    return xi_num_int_cmp(a, b) > 0;
}

export fn xi_num_ge(a: *XiNum, b: *XiNum) callconv(.c) bool {
    if (a.tag == XI_NUM_SOFTFLOAT or b.tag == XI_NUM_SOFTFLOAT) {
        const c = xi_soft_cmp(a, b);
        return c == 1 or c == 0;
    }
    if (a.tag == XI_NUM_DEC or b.tag == XI_NUM_DEC) return xi_as_f64(a) >= xi_as_f64(b);
    if (a.tag == XI_NUM_BIGDEC or b.tag == XI_NUM_BIGDEC) return xi_bigdec_cmp(a, b) >= 0;
    return xi_num_int_cmp(a, b) >= 0;
}

export fn xi_num_eq(a: *XiNum, b: *XiNum) callconv(.c) bool {
    if (a.tag == XI_NUM_SOFTFLOAT or b.tag == XI_NUM_SOFTFLOAT) return xi_soft_cmp(a, b) == 0;
    if (a.tag == XI_NUM_DEC or b.tag == XI_NUM_DEC) return xi_as_f64(a) == xi_as_f64(b);
    if (a.tag == XI_NUM_BIGDEC or b.tag == XI_NUM_BIGDEC) return xi_bigdec_cmp(a, b) == 0;
    return xi_num_int_cmp(a, b) == 0;
}

export fn xi_num_ne(a: *XiNum, b: *XiNum) callconv(.c) bool {
    if (a.tag == XI_NUM_SOFTFLOAT or b.tag == XI_NUM_SOFTFLOAT) return xi_soft_cmp(a, b) != 0;
    if (a.tag == XI_NUM_DEC or b.tag == XI_NUM_DEC) return xi_as_f64(a) != xi_as_f64(b);
    if (a.tag == XI_NUM_BIGDEC or b.tag == XI_NUM_BIGDEC) return xi_bigdec_cmp(a, b) != 0;
    return xi_num_int_cmp(a, b) != 0;
}

export fn xi_num_truthy(a: *XiNum) callconv(.c) bool {
    if (a.tag == XI_NUM_DEC) return a.d != 0.0;
    if (a.tag == XI_NUM_SOFTFLOAT) return a.soft_kind != XI_SF_ZERO;
    if (a.tag == XI_NUM_BIG or a.tag == XI_NUM_BIGDEC) return a.big.?.sign != 0;
    if (a.tag == XI_NUM_WIDE) {
        const n = a.wide_bits / 8;
        var i: usize = 0;
        while (i < n and i < a.wide.len) : (i += 1) {
            if (a.wide[i] != 0) return true;
        }
        return false;
    }
    return a.i != 0;
}

fn xi_bitlen_u64(x_in: u64) usize {
    var x = x_in;
    var n: usize = 0;
    while (x != 0) {
        n += 1;
        x >>= 1;
    }
    return n;
}

fn xi_signed_bits_i64(x: i64) usize {
    if (x >= 0) return xi_bitlen_u64(@bitCast(x)) + 1;
    const mag: u64 = if (x == INT64_MIN) (@as(u64, @bitCast(INT64_MAX)) + 1) else @as(u64, @bitCast(-x));
    return xi_bitlen_u64(mag - 1) + 1;
}

fn xi_native_ladder_width(need: usize) usize {
    const ladder = [_]usize{ 8, 16, 32, 64, 128, 256 };
    for (ladder) |lw| {
        if (need <= lw) return lw;
    }
    return (need + 7) / 8 * 8;
}

export fn xi_num_size_bits(v: ?*XiNum, is_signed: c_int) callconv(.c) *XiNum {
    const vv = v orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "numeric value was null");
        return xi_num_make_int(0);
    };
    var bits: usize = 0;
    switch (vv.tag) {
        XI_NUM_INT => {
            const x = vv.i;
            const need = if (is_signed != 0) xi_signed_bits_i64(x) else xi_bitlen_u64(@bitCast(x));
            bits = xi_native_ladder_width(need);
        },
        XI_NUM_BIG => {
            const magbits = if (vv.big != null) xi_big_bitlen(vv.big.?) else 0;
            var need: usize = undefined;
            if (is_signed == 0) {
                need = magbits;
            } else if (vv.big != null and vv.big.?.sign < 0) {
                need = magbits + (if (xi_big_is_pow2(vv.big) != 0) @as(usize, 0) else 1);
            } else {
                need = magbits + 1;
            }
            bits = xi_native_ladder_width(need);
        },
        XI_NUM_DEC => bits = 64,
        XI_NUM_WIDE => bits = vv.wide_bits,
        XI_NUM_BIGDEC => bits = if (vv.big != null) xi_big_bitlen(vv.big.?) else 0,
        XI_NUM_SOFTFLOAT => bits = vv.wide_bits,
        else => bits = 0,
    }
    if (bits > @as(usize, @bitCast(INT64_MAX))) return xi_num_make_int(INT64_MAX);
    return xi_num_make_int(@intCast(bits));
}

fn xi_f64_to_half_bits(dv: f64) u16 {
    const f: f32 = @floatCast(dv);
    const x: u32 = @bitCast(f);
    const sign: u32 = (x >> 16) & 0x8000;
    const fexp: u32 = (x >> 23) & 0xff;
    const mant0: u32 = x & 0x7fffff;
    if (fexp == 0xff) {
        return @truncate(sign | 0x7c00 | (if (mant0 != 0) @as(u32, 0x200) else 0));
    }
    const e: i32 = @as(i32, @intCast(fexp)) - 127 + 15;
    if (e >= 0x1f) return @truncate(sign | 0x7c00);
    if (e <= 0) {
        if (e < -10) return @truncate(sign);
        const mant = mant0 | 0x800000;
        const shift: u5 = @intCast(14 - e);
        const hmant = mant >> shift;
        const rem = mant & ((@as(u32, 1) << shift) - 1);
        const half = @as(u32, 1) << (shift - 1);
        var hm = hmant;
        if (rem > half or (rem == half and (hmant & 1) != 0)) hm += 1;
        return @truncate(sign | hm);
    }
    var h: u16 = @truncate(sign | (@as(u32, @intCast(e)) << 10) | (mant0 >> 13));
    const rem = mant0 & 0x1fff;
    if (rem > 0x1000 or (rem == 0x1000 and (h & 1) != 0)) h +%= 1;
    return h;
}

fn xi_half_bits_to_float(h: u16) f32 {
    const sign: u32 = @as(u32, h & 0x8000) << 16;
    const hexp: u32 = (h >> 10) & 0x1f;
    var mant: u32 = h & 0x3ff;
    var bits: u32 = undefined;
    if (hexp == 0) {
        if (mant == 0) {
            bits = sign;
        } else {
            var e: i32 = 127 - 15 + 1;
            while ((mant & 0x400) == 0) {
                mant <<= 1;
                e -= 1;
            }
            mant &= 0x3ff;
            bits = sign | (@as(u32, @intCast(e)) << 23) | (mant << 13);
        }
    } else if (hexp == 0x1f) {
        bits = sign | 0x7f800000 | (mant << 13);
    } else {
        bits = sign | ((hexp - 15 + 127) << 23) | (mant << 13);
    }
    return @bitCast(bits);
}

export fn __extendhfsf2(a: f16) callconv(.c) f32 {
    return xi_half_bits_to_float(@bitCast(a));
}
export fn __extendhfdf2(a: f16) callconv(.c) f64 {
    return @floatCast(xi_half_bits_to_float(@bitCast(a)));
}
export fn __truncsfhf2(a: f32) callconv(.c) f16 {
    return @bitCast(xi_f64_to_half_bits(@floatCast(a)));
}
export fn __truncdfhf2(a: f64) callconv(.c) f16 {
    return @bitCast(xi_f64_to_half_bits(a));
}
export fn __gnu_h2f_ieee(a: u16) callconv(.c) f32 {
    return xi_half_bits_to_float(a);
}
export fn __gnu_f2h_ieee(a: f32) callconv(.c) u16 {
    return xi_f64_to_half_bits(@floatCast(a));
}

fn xi_fmt_float_shortest(buf: [*]u8, buflen: usize, value: f64, width: i32) void {
    if (cIsNan(value)) {
        _ = snprintf(buf, buflen, "nan");
        return;
    }
    if (cIsInf(value)) {
        _ = snprintf(buf, buflen, if (value < 0) "-inf" else "inf");
        return;
    }
    if (value == 0.0) {
        _ = snprintf(buf, buflen, "0");
        return;
    }
    const maxp: c_int = if (width == 64) 17 else (if (width == 32) 9 else 5);
    var sci: [64]u8 = undefined;
    var p: c_int = 1;
    while (p <= maxp) : (p += 1) {
        _ = snprintf(&sci, sci.len, "%.*e", @as(c_int, p - 1), value);
        const back = strtod(@ptrCast(&sci), null);
        var match: bool = undefined;
        if (width == 64) {
            match = (back == value);
        } else if (width == 32) {
            match = (@as(f32, @floatCast(back)) == @as(f32, @floatCast(value)));
        } else {
            match = (xi_f64_to_half_bits(back) == xi_f64_to_half_bits(value));
        }
        if (match) break;
    }
    var s: [*:0]const u8 = @ptrCast(&sci);
    var sign: c_int = 1;
    if (s[0] == '-') {
        sign = -1;
        s += 1;
    }
    var digits: [40]u8 = undefined;
    var ndig: c_int = 0;
    if (s[0] >= '0' and s[0] <= '9') {
        digits[@intCast(ndig)] = s[0];
        ndig += 1;
        s += 1;
    }
    if (s[0] == '.') {
        s += 1;
        while (s[0] >= '0' and s[0] <= '9' and ndig < @as(c_int, @intCast(digits.len))) {
            digits[@intCast(ndig)] = s[0];
            ndig += 1;
            s += 1;
        }
    }
    var dexp: i64 = 0;
    if (s[0] == 'e' or s[0] == 'E') {
        dexp = @intCast(strtol(s + 1, null, 10));
    }
    const out = xi_soft_format_decimal(sign, &digits, ndig, dexp);
    _ = snprintf(buf, buflen, "%s", @as([*:0]const u8, @ptrCast(out)));
    cfree(out);
}

var xi_io_wrtr_buf: ?[*]u8 = null;
var xi_io_wrtr_len: usize = 0;
var xi_io_wrtr_cap: usize = 0;
var xi_io_wrtr_depth: usize = 0;

fn xi_io_wrtr_reserve(extra: usize) void {
    if (extra == 0) return;
    if (extra > SIZE_MAX - xi_io_wrtr_len) xi_oom();
    const needed = xi_io_wrtr_len + extra;
    if (needed <= xi_io_wrtr_cap) return;

    var new_cap: usize = if (xi_io_wrtr_cap == 0) 128 else xi_io_wrtr_cap;
    while (new_cap < needed) {
        if (new_cap > SIZE_MAX / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    const old: ?*anyopaque = if (xi_io_wrtr_buf) |buf| @ptrCast(buf) else null;
    xi_io_wrtr_buf = @ptrCast(realloc(old, new_cap) orelse xi_oom());
    xi_io_wrtr_cap = new_cap;
}

fn xi_io_emit_bytes(bytes: [*]const u8, len: usize) void {
    if (len == 0) return;
    if (xi_io_wrtr_depth == 0) {
        _ = fwrite(bytes, 1, len, stdoutFile());
        return;
    }

    xi_io_wrtr_reserve(len);
    const out = xi_io_wrtr_buf.?;
    var i: usize = 0;
    while (i < len) : (i += 1) out[xi_io_wrtr_len + i] = bytes[i];
    xi_io_wrtr_len += len;
}

fn xi_io_emit_cstr(s: [*:0]const u8) void {
    xi_io_emit_bytes(s, strlen(s));
}

fn xi_io_emit_char(c: u8) void {
    if (xi_io_wrtr_depth == 0) {
        _ = fputc(c, stdoutFile());
        return;
    }
    xi_io_wrtr_reserve(1);
    xi_io_wrtr_buf.?[xi_io_wrtr_len] = c;
    xi_io_wrtr_len += 1;
}

export fn xi_io_wrtr_begin() callconv(.c) void {
    if (xi_io_wrtr_depth == 0) xi_io_wrtr_len = 0;
    xi_io_wrtr_depth += 1;
}

export fn xi_io_wrtr_end() callconv(.c) void {
    if (xi_io_wrtr_depth == 0) {
        _ = fputc('\r', stdoutFile());
        _ = fflush(stdoutFile());
        return;
    }

    xi_io_wrtr_depth -= 1;
    if (xi_io_wrtr_depth != 0) {
        xi_io_emit_char('\r');
        return;
    }

    xi_io_wrtr_reserve(1);
    xi_io_wrtr_buf.?[xi_io_wrtr_len] = '\r';
    xi_io_wrtr_len += 1;
    _ = fwrite(xi_io_wrtr_buf.?, 1, xi_io_wrtr_len, stdoutFile());
    _ = fflush(stdoutFile());
    xi_io_wrtr_len = 0;
}

export fn xi_io_write_f64w(value: f64, width: i32) callconv(.c) void {
    var buf: [64]u8 = undefined;
    xi_fmt_float_shortest(&buf, buf.len, value, width);
    xi_io_emit_cstr(@ptrCast(&buf));
}

export fn xi_io_write_num(v: ?*XiNum) callconv(.c) void {
    const vv = v orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "numeric value was null");
        return;
    };
    if (vv.tag == XI_NUM_WIDE) {
        xi_write_decimal_bytes(&vv.wide, vv.wide_bits / 8, 1);
        return;
    }
    if (vv.tag == XI_NUM_BIG) {
        const s = xi_big_to_decimal(vv.big.?);
        xi_io_emit_cstr(@ptrCast(s));
        cfree(s);
        return;
    }
    if (vv.tag == XI_NUM_BIGDEC) {
        const s = xi_bigdec_to_decimal(vv.big.?, vv.dec_exp);
        xi_io_emit_cstr(@ptrCast(s));
        cfree(s);
        return;
    }
    if (vv.tag == XI_NUM_SOFTFLOAT) {
        const s = xi_soft_to_string(vv);
        xi_io_emit_cstr(@ptrCast(s));
        cfree(s);
        return;
    }
    if (vv.tag == XI_NUM_DEC) {
        var buf: [64]u8 = undefined;
        xi_fmt_float_shortest(&buf, buf.len, vv.d, 64);
        xi_io_emit_cstr(@ptrCast(&buf));
        return;
    }
    var buf: [32]u8 = undefined;
    _ = snprintf(&buf, buf.len, "%lld", @as(c_longlong, vv.i));
    xi_io_emit_cstr(@ptrCast(&buf));
}

export fn xi_io_write_i64(v: i64) callconv(.c) void {
    var buf: [32]u8 = undefined;
    _ = snprintf(&buf, buf.len, "%lld", @as(c_longlong, v));
    xi_io_emit_cstr(@ptrCast(&buf));
}

export fn xi_io_write_u64(v: i64) callconv(.c) void {
    var buf: [32]u8 = undefined;
    _ = snprintf(&buf, buf.len, "%llu", @as(c_ulonglong, @bitCast(v)));
    xi_io_emit_cstr(@ptrCast(&buf));
}

fn xi_write_decimal_bytes(bytes: ?[*]const u8, n_in: usize, is_signed: c_int) void {
    const bs = bytes orelse return;
    if (n_in == 0) return;
    var n = n_in;
    var mag: [64]u8 = undefined;
    if (n > mag.len) n = mag.len;

    var negative = false;
    if (is_signed != 0 and (bs[n - 1] & 0x80) != 0) {
        negative = true;
        var carry: c_int = 1;
        var i: usize = 0;
        while (i < n) : (i += 1) {
            const sum: c_int = @as(c_int, ~bs[i]) + carry;
            mag[i] = @truncate(@as(u32, @bitCast(sum)) & 0xff);
            carry = sum >> 8;
        }
    } else {
        var i: usize = 0;
        while (i < n) : (i += 1) mag[i] = bs[i];
    }

    var is_zero = true;
    {
        var i: usize = 0;
        while (i < n) : (i += 1) {
            if (mag[i] != 0) {
                is_zero = false;
                break;
            }
        }
    }
    if (is_zero) {
        xi_io_emit_char('0');
        return;
    }

    var digits: [96]u8 = undefined;
    var dlen: usize = 0;
    while (!is_zero) {
        var rem: c_int = 0;
        var i = n;
        while (i > 0) {
            i -= 1;
            const cur: c_int = (rem << 8) | mag[i];
            mag[i] = @intCast(@divTrunc(cur, 10));
            rem = @rem(cur, 10);
        }
        digits[dlen] = @intCast('0' + rem);
        dlen += 1;
        is_zero = true;
        i = 0;
        while (i < n) : (i += 1) {
            if (mag[i] != 0) {
                is_zero = false;
                break;
            }
        }
    }

    if (negative) xi_io_emit_char('-');
    while (dlen > 0) {
        dlen -= 1;
        xi_io_emit_char(digits[dlen]);
    }
}

export fn xi_io_write_wide(bytes: ?[*]const u8, nbytes: i64, is_signed: c_int) callconv(.c) void {
    if (nbytes <= 0) return;
    xi_write_decimal_bytes(bytes, @intCast(nbytes), is_signed);
}

export fn xi_num_from_wide(bytes: ?[*]const u8, nbytes: i64, is_signed: c_int) callconv(.c) *XiNum {
    if (is_signed == 0) return xi_num_make_big(xi_big_from_wide(bytes, @intCast(nbytes), 0));
    if (nbytes > 32) {
        return xi_num_make_big(xi_big_from_wide(bytes, @intCast(nbytes), is_signed));
    }
    const n = xi_num_new();
    n.tag = XI_NUM_WIDE;
    var cnt: usize = @intCast(nbytes);
    if (cnt > n.wide.len) cnt = n.wide.len;
    @memset(&n.wide, 0);
    const bs = bytes.?;
    var i: usize = 0;
    while (i < cnt) : (i += 1) n.wide[i] = bs[i];
    n.wide_bits = @intCast(cnt * 8);
    var low: i64 = 0;
    i = 0;
    while (i < cnt and i < 8) : (i += 1) {
        low |= @as(i64, @bitCast(@as(u64, bs[i]) << @as(u6, @intCast(i * 8))));
    }
    n.i = low;
    n.d = @floatFromInt(low);
    return n;
}

export fn xi_wide_divrem(
    q: ?[*]u8,
    r: ?[*]u8,
    a: ?[*]const u8,
    b: ?[*]const u8,
    nbytes: i64,
    is_signed: c_int,
) callconv(.c) void {
    if (nbytes <= 0) return;
    const n: usize = @intCast(nbytes);
    const ap = a orelse return;
    const bp = b orelse return;

    const ba = xi_big_from_wide(ap, n, is_signed);
    const bb = xi_big_from_wide(bp, n, is_signed);

    if (xi_big_is_zero(bb)) {
        xi_big_free(ba);
        xi_big_free(bb);
        if (q) |qq| @memset(qq[0..n], 0);
        if (r) |rr| @memset(rr[0..n], 0);
        return;
    }

    const sa = ba.sign;
    const sb = bb.sign;
    if (ba.sign != 0) ba.sign = 1;
    if (bb.sign != 0) bb.sign = 1;

    var bq: *XiBig = undefined;
    var br: *XiBig = undefined;
    xi_big_divmod(ba, bb, &bq, &br);

    if (bq.len != 0 and bq.sign != 0) bq.sign = if (sa == sb) 1 else -1;
    if (br.len != 0 and br.sign != 0) br.sign = sa;

    if (q) |qq| xi_big_to_wide(bq, qq, n);
    if (r) |rr| xi_big_to_wide(br, rr, n);

    xi_big_free(ba);
    xi_big_free(bb);
    xi_big_free(bq);
    xi_big_free(br);
}

export fn xi_num_to_wide(v: ?*XiNum, out: ?[*]u8, nbytes: i64) callconv(.c) void {
    const o = out orelse return;
    if (nbytes <= 0) return;
    const n: usize = @intCast(nbytes);
    if (v) |vv| {
        if (vv.tag == XI_NUM_BIG) {
            xi_big_to_wide(vv.big.?, o, n);
            return;
        }
        if (vv.tag == XI_NUM_WIDE) {
            const have: usize = vv.wide_bits / 8;
            const neg = (have > 0) and (vv.wide[have - 1] & 0x80) != 0;
            var i: usize = 0;
            while (i < n) : (i += 1) {
                o[i] = if (i < have) vv.wide[i] else (if (neg) @as(u8, 0xff) else 0);
            }
            return;
        }
    }
    const val: i64 = if (v) |vv| xi_num_to_i64(vv) else 0;
    const neg = val < 0;
    var i: usize = 0;
    while (i < n) : (i += 1) {
        o[i] = if (i < 8) @truncate(@as(u64, @bitCast(val)) >> @as(u6, @intCast(i * 8))) else (if (neg) @as(u8, 0xff) else 0);
    }
}

export fn xi_io_write_cstr(s: OptStr) callconv(.c) void {
    const ss = s orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return;
    };
    const len = xi_str_len_known(ss);
    if (len > 0) xi_io_emit_bytes(ss, len);
}

export fn xi_io_write_bool(v: i32) callconv(.c) void {
    xi_io_emit_cstr(if (v != 0) "true" else "false");
}

export fn xi_io_newline() callconv(.c) void {
    xi_io_emit_char('\n');
}

export fn xi_io_flush() callconv(.c) void {
    _ = fflush(stdoutFile());
}

const XiConsoleCoord = extern struct {
    x: i16,
    y: i16,
};

const XiSmallRect = extern struct {
    left: i16,
    top: i16,
    right: i16,
    bottom: i16,
};

const XiConsoleScreenBufferInfo = extern struct {
    size: XiConsoleCoord,
    cursor_position: XiConsoleCoord,
    attributes: u16,
    window: XiSmallRect,
    maximum_window_size: XiConsoleCoord,
};

const XI_STD_OUTPUT_HANDLE: u32 = 0xfffffff5;

extern "kernel32" fn GetStdHandle(nStdHandle: u32) callconv(.winapi) ?*anyopaque;
extern "kernel32" fn GetConsoleScreenBufferInfo(
    hConsoleOutput: *anyopaque,
    lpConsoleScreenBufferInfo: *XiConsoleScreenBufferInfo,
) callconv(.winapi) c_int;
extern "kernel32" fn FillConsoleOutputCharacterA(
    hConsoleOutput: *anyopaque,
    cCharacter: u8,
    nLength: u32,
    dwWriteCoord: XiConsoleCoord,
    lpNumberOfCharsWritten: *u32,
) callconv(.winapi) c_int;
extern "kernel32" fn FillConsoleOutputAttribute(
    hConsoleOutput: *anyopaque,
    wAttribute: u16,
    nLength: u32,
    dwWriteCoord: XiConsoleCoord,
    lpNumberOfAttrsWritten: *u32,
) callconv(.winapi) c_int;
extern "kernel32" fn SetConsoleCursorPosition(
    hConsoleOutput: *anyopaque,
    dwCursorPosition: XiConsoleCoord,
) callconv(.winapi) c_int;

fn xi_io_stdout_is_tty() bool {
    if (builtin.os.tag == .windows) return _isatty(_fileno(stdoutFile())) != 0;
    return isatty(fileno(stdoutFile())) != 0;
}

fn xi_io_clrl_windows() void {
    _ = fflush(stdoutFile());

    const handle = GetStdHandle(XI_STD_OUTPUT_HANDLE) orelse return;
    if (@intFromPtr(handle) == std.math.maxInt(usize)) return;

    var info: XiConsoleScreenBufferInfo = undefined;
    if (GetConsoleScreenBufferInfo(handle, &info) == 0 or info.size.x <= 0) return;

    const start = XiConsoleCoord{ .x = 0, .y = info.cursor_position.y };
    const width: u32 = @intCast(info.size.x);
    var written: u32 = 0;
    _ = FillConsoleOutputCharacterA(handle, ' ', width, start, &written);
    _ = FillConsoleOutputAttribute(handle, info.attributes, width, start, &written);
    _ = SetConsoleCursorPosition(handle, start);
}

export fn xi_io_clrl() callconv(.c) void {
    if (!xi_io_stdout_is_tty()) return;

    if (builtin.os.tag == .windows) {
        xi_io_clrl_windows();
        return;
    }

    _ = fputs("\r\x1b[2K", stdoutFile());
    _ = fflush(stdoutFile());
}

export fn xi_io_ask() callconv(.c) Str {
    var line: [2048]u8 = undefined;
    _ = fflush(stdoutFile());
    if (fgets(&line, line.len, stdinFile()) == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "input stream reached EOF");
        return xi_str_intern_cstr("");
    }
    line[strcspn(@ptrCast(&line), "\r\n")] = 0;
    return xi_str_intern_cstr(@ptrCast(&line));
}

export fn xi_io_read_file(path: OptStr) callconv(.c) Str {
    const p = path orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "file path was null");
        return xi_str_intern_cstr("");
    };
    const f = fopen(p, "rb") orelse {
        xi_err_set_message(XI_ERR_NOT_FOUND, "failed to open file for reading");
        return xi_str_intern_cstr("");
    };
    if (fseek(f, 0, SEEK_END) != 0) {
        _ = fclose(f);
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "failed to seek file");
        return xi_str_intern_cstr("");
    }
    const size = ftell(f);
    if (size < 0) {
        _ = fclose(f);
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "failed to determine file size");
        return xi_str_intern_cstr("");
    }
    if (fseek(f, 0, SEEK_SET) != 0) {
        _ = fclose(f);
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "failed to seek file");
        return xi_str_intern_cstr("");
    }
    const usize_size: usize = @intCast(size);
    const buf: [*]u8 = @ptrCast(malloc(usize_size + 1) orelse {
        _ = fclose(f);
        xi_oom();
    });
    const rd = fread(buf, 1, usize_size, f);
    if (rd != usize_size and ferror(f) != 0) {
        cfree(buf);
        _ = fclose(f);
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "failed to read file");
        return xi_str_intern_cstr("");
    }
    _ = fclose(f);
    buf[rd] = 0;
    const interned = xi_str_intern_bytes(buf, rd);
    cfree(buf);
    return interned;
}

export fn xi_io_read_bytes(path: OptStr) callconv(.c) *XiStrArray {
    const arr = xi_arr_new();
    const p = path orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "file path was null");
        return arr;
    };
    const f = fopen(p, "rb") orelse {
        xi_err_set_message(XI_ERR_NOT_FOUND, "failed to open file for reading");
        return arr;
    };
    if (fseek(f, 0, SEEK_END) != 0) {
        _ = fclose(f);
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "failed to seek file");
        return arr;
    }
    const size = ftell(f);
    if (size < 0) {
        _ = fclose(f);
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "failed to determine file size");
        return arr;
    }
    if (fseek(f, 0, SEEK_SET) != 0) {
        _ = fclose(f);
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "failed to seek file");
        return arr;
    }
    if (size == 0) {
        _ = fclose(f);
        return arr;
    }
    const usize_size: usize = @intCast(size);
    const buf: [*]u8 = @ptrCast(malloc(usize_size) orelse {
        _ = fclose(f);
        xi_oom();
    });
    const got = fread(buf, 1, usize_size, f);
    if (got != usize_size and ferror(f) != 0) {
        cfree(buf);
        _ = fclose(f);
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "failed to read file");
        return arr;
    }
    _ = fclose(f);
    var i: usize = 0;
    while (i < got) : (i += 1) {
        xi_arr_set_uint_i64_at_i64(arr, @intCast(i), @as(i64, buf[i]));
    }
    cfree(buf);
    return arr;
}

export fn xi_io_write_bytes(path: OptStr, data: ?*XiStrArray) callconv(.c) *XiNum {
    if (path == null or data == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "file path or data was null");
        return xi_num_make_int(0);
    }
    const f = fopen(path.?, "wb") orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "failed to open file for writing");
        return xi_num_make_int(0);
    };
    const n = data.?.len;
    var i: usize = 0;
    while (i < n) : (i += 1) {
        var byte: u8 = @truncate(@as(u64, @bitCast(xi_num_to_i64(xi_arr_get_num_at_i64(data.?, @intCast(i))))) & 0xFF);
        if (fwrite(&byte, 1, 1, f) != 1) {
            _ = fclose(f);
            xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "failed to write file");
            return xi_num_make_int(0);
        }
    }
    if (fclose(f) != 0) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "failed to write file");
        return xi_num_make_int(0);
    }
    return xi_num_make_int(1);
}

fn xi_io_write_file_mode(path: OptStr, contents: OptStr, mode: [*:0]const u8) *XiNum {
    if (path == null or contents == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "file path or contents was null");
        return xi_num_make_int(0);
    }
    const f = fopen(path.?, mode) orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "failed to open file for writing");
        return xi_num_make_int(0);
    };
    const len = xi_str_len_known(contents);
    const written = fwrite(contents.?, 1, len, f);
    const close_ok = fclose(f) == 0;
    if (written != len or !close_ok) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "failed to write file");
        return xi_num_make_int(0);
    }
    return xi_num_make_int(1);
}

export fn xi_io_write_file(path: OptStr, contents: OptStr) callconv(.c) *XiNum {
    return xi_io_write_file_mode(path, contents, "wb");
}

export fn xi_io_append_file(path: OptStr, contents: OptStr) callconv(.c) *XiNum {
    return xi_io_write_file_mode(path, contents, "ab");
}

export fn xi_io_file_exists(path: OptStr) callconv(.c) *XiNum {
    const p = path orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "file path was null");
        return xi_num_make_int(0);
    };
    const f = fopen(p, "rb") orelse return xi_num_make_int(0);
    _ = fclose(f);
    return xi_num_make_int(1);
}

extern "c" fn __p___argc() *c_int;
extern "c" fn __p___argv() *[*][*:0]u8;

export fn xi_cli_load_args() callconv(.c) void {
    if (xi_cli_args_loaded) return;
    xi_cli_args_loaded = true;
    xi_gc_pause_begin();
    defer xi_gc_pause_end();

    if (builtin.os.tag == .windows) {
        const argc = __p___argc().*;
        const argv = __p___argv().*;
        if (argc <= 1) return;
        const count: usize = @intCast(argc - 1);
        const parts: [*]OptStr = @ptrCast(@alignCast(malloc(count * @sizeOf(OptStr)) orelse xi_oom()));
        var i: usize = 0;
        while (i < count) : (i += 1) parts[i] = xi_str_intern_cstr(argv[i + 1]);
        xi_cli_argc = @intCast(count);
        xi_cli_argv = parts;
        xi_cli_argv_storage = parts;
    } else {
        const f = fopen("/proc/self/cmdline", "rb") orelse return;
        var cap: usize = 8;
        var count: usize = 0;
        var parts: [*]OptStr = @ptrCast(@alignCast(malloc(cap * @sizeOf(OptStr)) orelse xi_oom()));
        var token: [4096]u8 = undefined;
        var len: usize = 0;
        var skipped_program = false;
        while (true) {
            const ch = fgetc(f);
            if (ch == EOF) break;
            if (ch != 0) {
                if (len + 1 < token.len) {
                    token[len] = @intCast(ch);
                    len += 1;
                }
                continue;
            }
            token[len] = 0;
            if (!skipped_program) {
                skipped_program = true;
            } else {
                if (count >= cap) {
                    cap *= 2;
                    parts = @ptrCast(@alignCast(realloc(@ptrCast(parts), cap * @sizeOf(OptStr)) orelse xi_oom()));
                }
                parts[count] = xi_str_intern_cstr(@ptrCast(&token));
                count += 1;
            }
            len = 0;
        }
        if (len > 0) {
            token[len] = 0;
            if (skipped_program) {
                if (count >= cap) {
                    cap *= 2;
                    parts = @ptrCast(@alignCast(realloc(@ptrCast(parts), cap * @sizeOf(OptStr)) orelse xi_oom()));
                }
                parts[count] = xi_str_intern_cstr(@ptrCast(&token));
                count += 1;
            }
        }
        _ = fclose(f);
        if (count == 0) {
            cfree(parts);
            xi_cli_argv = null;
            return;
        }
        xi_cli_argc = @intCast(count);
        xi_cli_argv = parts;
        xi_cli_argv_storage = parts;
    }
}

export fn xi_arg_count() callconv(.c) *XiNum {
    xi_cli_load_args();
    return xi_num_make_int(xi_cli_argc);
}

export fn xi_arg_get(idx: ?*XiNum) callconv(.c) Str {
    xi_cli_load_args();
    const pos = xi_num_to_i64(idx);
    if (pos < 0 or pos >= xi_cli_argc or xi_cli_argv == null) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "argv index out of range");
        return xi_str_intern_cstr("");
    }
    const value = xi_cli_argv.?[@intCast(pos)];
    return if (value) |val| val else "";
}

export fn xi_arr_new() callconv(.c) *XiStrArray {
    xi_runtime_gc_ensure_init();
    const arr = cnew(XiStrArray);
    arr.len = 0;
    arr.cap = 0;
    arr.items = null;
    arr.packed_nums = null;
    arr.packed_bits = 0;
    arr.packed_unsigned = 0;
    arr.i64_cache = null;
    arr.gc_mark = 0;
    arr.gc_next = xi_gc_arr_head;
    xi_gc_arr_head = arr;
    xi_gc_note_allocation();
    return arr;
}

export fn xi_arr_from_bytes(data: ?[*]const u8, len: usize) callconv(.c) *XiStrArray {
    const arr = xi_arr_new();
    if (len == 0) return arr;
    const source = data orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "byte source was null");
        return arr;
    };
    const byte_copy: [*]u8 = @ptrCast(malloc(len) orelse xi_oom());
    cmemcpy(byte_copy, source, len);
    arr.len = len;
    arr.cap = len;
    arr.packed_nums = byte_copy;
    arr.packed_bits = 8;
    arr.packed_unsigned = 1;
    return arr;
}

fn xi_arr_ensure_capacity(arr: *XiStrArray, min_cap: usize) void {
    if (arr.cap >= min_cap) return;
    var new_cap: usize = if (arr.cap == 0) 8 else arr.cap;
    while (new_cap < min_cap) {
        if (new_cap > SIZE_MAX / 2) {
            new_cap = min_cap;
            break;
        }
        new_cap *= 2;
    }
    if (new_cap < min_cap) fatal("xi_runtime: array capacity overflow\n");
    const new_items: [*]XiValue = @ptrCast(@alignCast(realloc(@ptrCast(arr.items), new_cap * @sizeOf(XiValue)) orelse xi_oom()));
    arr.items = new_items;
    arr.cap = new_cap;
}

fn xi_arr_invalidate_i64_cache(arr: *XiStrArray) void {
    cfree(arr.i64_cache);
    arr.i64_cache = null;
}

fn xi_arr_packed_elem_bytes(arr: *const XiStrArray) usize {
    return if (arr.packed_bits != 0) @as(usize, arr.packed_bits) / 8 else 0;
}

fn xi_num_is_packable_int(value: ?*const XiNum) bool {
    const v = value orelse return false;
    return v.tag == XI_NUM_INT or v.tag == XI_NUM_BIG or v.tag == XI_NUM_WIDE;
}

fn xi_wide_unsigned_bitlen(bytes: [*]const u8, n_in: usize) usize {
    var n = n_in;
    while (n > 0 and bytes[n - 1] == 0) n -= 1;
    if (n == 0) return 0;
    return (n - 1) * 8 + xi_bitlen_u64(bytes[n - 1]);
}

fn xi_num_pack_bits(value: *XiNum, is_unsigned: c_int) usize {
    if (!xi_num_is_packable_int(value)) return 0;
    var need: usize = 0;
    switch (value.tag) {
        XI_NUM_INT => {
            if (is_unsigned != 0) {
                need = if (value.i < 0) 0 else xi_bitlen_u64(@bitCast(value.i));
            } else {
                need = xi_signed_bits_i64(value.i);
            }
        },
        XI_NUM_BIG => {
            const magbits = if (value.big != null) xi_big_bitlen(value.big.?) else 0;
            if (is_unsigned != 0) {
                need = if (value.big != null and value.big.?.sign < 0) 0 else magbits;
            } else if (value.big != null and value.big.?.sign < 0) {
                need = magbits + (if (xi_big_is_pow2(value.big) != 0) @as(usize, 0) else 1);
            } else {
                need = magbits + 1;
            }
        },
        XI_NUM_WIDE => {
            if (is_unsigned != 0) {
                need = xi_wide_unsigned_bitlen(&value.wide, value.wide_bits / 8);
            } else {
                const b = xi_big_from_wide(&value.wide, value.wide_bits / 8, 1);
                const magbits = xi_big_bitlen(b);
                need = if (b.sign < 0) magbits + (if (xi_big_is_pow2(b) != 0) @as(usize, 0) else 1) else magbits + 1;
                xi_big_free(b);
            }
        },
        else => {},
    }
    return xi_native_ladder_width(need);
}

fn xi_num_write_pack_bytes(value: ?*XiNum, out: [*]u8, nbytes: usize, is_unsigned: c_int) void {
    if (nbytes == 0) return;
    cmemset(out, 0, nbytes);
    const v = value orelse return;
    if (is_unsigned != 0 and v.tag == XI_NUM_INT and v.i < 0) return;
    if (is_unsigned != 0 and v.tag == XI_NUM_BIG and v.big != null and v.big.?.sign < 0) return;
    switch (v.tag) {
        XI_NUM_INT => {
            const raw: u64 = @bitCast(v.i);
            const fill: u8 = if (is_unsigned == 0 and v.i < 0) 0xff else 0x00;
            var i: usize = 0;
            while (i < nbytes) : (i += 1) {
                out[i] = if (i < 8) @truncate(raw >> @as(u6, @intCast(i * 8))) else fill;
            }
        },
        XI_NUM_BIG => xi_big_to_wide(v.big.?, out, nbytes),
        XI_NUM_WIDE => {
            const have: usize = v.wide_bits / 8;
            const fill: u8 = if (is_unsigned == 0 and have > 0 and (v.wide[have - 1] & 0x80) != 0) 0xff else 0x00;
            var i: usize = 0;
            while (i < nbytes) : (i += 1) {
                out[i] = if (i < have) v.wide[i] else fill;
            }
        },
        else => {},
    }
}

fn xi_i64_pack_bits(value: i64, is_unsigned: c_int) usize {
    if (is_unsigned != 0) {
        return if (value < 0) 8 else xi_native_ladder_width(xi_bitlen_u64(@bitCast(value)));
    }
    return xi_native_ladder_width(xi_signed_bits_i64(value));
}

inline fn xi_packed_read_signed_i64(bytes: [*]const u8, nbytes: usize) i64 {
    return switch (nbytes) {
        1 => @as(i8, @bitCast(bytes[0])),
        2 => @as(i16, @bitCast(@as(u16, bytes[0]) | (@as(u16, bytes[1]) << 8))),
        4 => @as(i32, @bitCast(@as(u32, bytes[0]) | (@as(u32, bytes[1]) << 8) |
             (@as(u32, bytes[2]) << 16) | (@as(u32, bytes[3]) << 24))),
        8 => @bitCast(@as(u64, bytes[0]) | (@as(u64, bytes[1]) << 8) |
             (@as(u64, bytes[2]) << 16) | (@as(u64, bytes[3]) << 24) |
             (@as(u64, bytes[4]) << 32) | (@as(u64, bytes[5]) << 40) |
             (@as(u64, bytes[6]) << 48) | (@as(u64, bytes[7]) << 56)),
        else => 0,
    };
}

fn xi_i64_write_pack_bytes(value: i64, out: [*]u8, nbytes: usize, is_unsigned: c_int) void {
    if (nbytes == 0) return;
    var v = value;
    if (is_unsigned != 0 and v < 0) v = 0;
    const raw: u64 = @bitCast(v);
    if (nbytes == 1) {
        out[0] = @truncate(raw);
        return;
    }
    if (nbytes == 2 or nbytes == 4 or nbytes == 8) {
        out[0] = @truncate(raw);
        out[1] = @truncate(raw >> 8);
        if (nbytes >= 4) {
            out[2] = @truncate(raw >> 16);
            out[3] = @truncate(raw >> 24);
        }
        if (nbytes == 8) {
            out[4] = @truncate(raw >> 32);
            out[5] = @truncate(raw >> 40);
            out[6] = @truncate(raw >> 48);
            out[7] = @truncate(raw >> 56);
        }
        return;
    }
    const fill: u8 = if (is_unsigned == 0 and v < 0) 0xff else 0x00;
    var i: usize = 0;
    while (i < nbytes) : (i += 1) {
        out[i] = if (i < 8) @truncate(raw >> @as(u6, @intCast(i * 8))) else fill;
    }
}

fn xi_num_from_pack_bytes(bytes: [*]const u8, nbytes: usize, is_unsigned: c_int) *XiNum {
    if (nbytes == 0) return xi_num_make_int(0);
    if (is_unsigned != 0) return xi_num_make_big(xi_big_from_wide(bytes, nbytes, 0));
    if (nbytes <= 8) {
        var raw: u64 = 0;
        var i: usize = 0;
        while (i < nbytes) : (i += 1) raw |= @as(u64, bytes[i]) << @as(u6, @intCast(i * 8));
        if (nbytes < 8 and (bytes[nbytes - 1] & 0x80) != 0) {
            raw |= UINT64_MAX << @as(u6, @intCast(nbytes * 8));
        }
        return xi_num_make_int(@bitCast(raw));
    }
    return xi_num_from_wide(bytes, @intCast(nbytes), 1);
}

fn xi_arr_ensure_packed_capacity(arr: *XiStrArray, min_cap: usize) void {
    if (arr.cap >= min_cap) return;
    var elem_bytes = xi_arr_packed_elem_bytes(arr);
    if (elem_bytes == 0) elem_bytes = 1;
    var new_cap: usize = if (arr.cap == 0) 8 else arr.cap;
    while (new_cap < min_cap) {
        if (new_cap > SIZE_MAX / 2) {
            new_cap = min_cap;
            break;
        }
        new_cap *= 2;
    }
    if (new_cap < min_cap or new_cap > SIZE_MAX / elem_bytes) fatal("xi_runtime: array capacity overflow\n");
    const new_items: [*]u8 = @ptrCast(realloc(@ptrCast(arr.packed_nums), new_cap * elem_bytes) orelse xi_oom());
    if (new_cap > arr.cap) {
        cmemset(new_items + arr.cap * elem_bytes, 0, (new_cap - arr.cap) * elem_bytes);
    }
    arr.packed_nums = new_items;
    arr.cap = new_cap;
}

fn xi_arr_materialize_packed_nums(arr: *XiStrArray) void {
    const packed_nums = arr.packed_nums orelse return;
    const elem_bytes = xi_arr_packed_elem_bytes(arr);
    var items: ?[*]XiValue = null;
    if (arr.cap > 0) {
        items = @ptrCast(@alignCast(malloc(arr.cap * @sizeOf(XiValue)) orelse xi_oom()));
    }
    var i: usize = 0;
    while (i < arr.len) : (i += 1) {
        items.?[i] = valNum(xi_num_from_pack_bytes(packed_nums + i * elem_bytes, elem_bytes, arr.packed_unsigned));
    }
    i = arr.len;
    while (i < arr.cap) : (i += 1) items.?[i] = valNull();
    cfree(packed_nums);
    arr.packed_nums = null;
    arr.packed_bits = 0;
    arr.packed_unsigned = 0;
    arr.items = items;
}

fn xi_arr_repack_nums(arr: *XiStrArray, new_bits: u16) void {
    const old_bytes = xi_arr_packed_elem_bytes(arr);
    const new_bytes: usize = @as(usize, new_bits) / 8;
    const packed_nums = arr.packed_nums orelse return;
    if (old_bytes == 0 or new_bytes == 0) return;
    if (arr.cap > SIZE_MAX / new_bytes) fatal("xi_runtime: array capacity overflow\n");
    const new_items: [*]u8 = @ptrCast(calloc(if (arr.cap == 0) 1 else arr.cap, new_bytes) orelse xi_oom());
    var i: usize = 0;
    while (i < arr.len) : (i += 1) {
        const value = xi_num_from_pack_bytes(packed_nums + i * old_bytes, old_bytes, arr.packed_unsigned);
        xi_num_write_pack_bytes(value, new_items + i * new_bytes, new_bytes, arr.packed_unsigned);
    }
    cfree(packed_nums);
    arr.packed_nums = new_items;
    arr.packed_bits = new_bits;
}

fn xi_arr_prepare_packed_num(arr: *XiStrArray, value: ?*XiNum, is_unsigned: c_int, min_cap: usize) void {
    xi_arr_invalidate_i64_cache(arr);
    if (!xi_num_is_packable_int(value)) {
        xi_arr_materialize_packed_nums(arr);
        return;
    }
    const needed_bits = xi_num_pack_bits(value.?, is_unsigned);
    if (needed_bits > UINT16_MAX) {
        xi_arr_materialize_packed_nums(arr);
        return;
    }
    var bits: u16 = @intCast(needed_bits);

    if (arr.packed_nums == null and arr.items != null) {
        const items = arr.items.?;
        var i: usize = 0;
        while (i < arr.len) : (i += 1) {
            const slot = &items[i];
            if (slot.tag != XI_VALUE_NUM or !xi_num_is_packable_int(valAsNum(slot.*))) return;
            const slot_bits = xi_num_pack_bits(valAsNum(slot.*).?, is_unsigned);
            if (slot_bits > bits) {
                if (slot_bits > UINT16_MAX) return;
                bits = @intCast(slot_bits);
            }
        }
        const elem_bytes: usize = @as(usize, bits) / 8;
        if (arr.cap > SIZE_MAX / elem_bytes) fatal("xi_runtime: array capacity overflow\n");
        const packed_buf: [*]u8 = @ptrCast(calloc(if (arr.cap == 0) 1 else arr.cap, elem_bytes) orelse xi_oom());
        i = 0;
        while (i < arr.len) : (i += 1) {
            xi_num_write_pack_bytes(valAsNum(items[i]), packed_buf + i * elem_bytes, elem_bytes, is_unsigned);
        }
        cfree(arr.items);
        arr.items = null;
        arr.packed_nums = packed_buf;
        arr.packed_bits = bits;
        arr.packed_unsigned = if (is_unsigned != 0) 1 else 0;
    } else if (arr.packed_nums == null) {
        arr.packed_bits = bits;
        arr.packed_unsigned = if (is_unsigned != 0) 1 else 0;
        xi_arr_ensure_packed_capacity(arr, min_cap);
    } else {
        if ((arr.packed_unsigned != 0) != (is_unsigned != 0)) {
            xi_arr_materialize_packed_nums(arr);
            return;
        }
        if (bits > arr.packed_bits) xi_arr_repack_nums(arr, bits);
    }
    xi_arr_ensure_packed_capacity(arr, min_cap);
}

fn xi_arr_prepare_packed_i64(arr: *XiStrArray, value_in: i64, is_unsigned: c_int, min_cap: usize) void {
    xi_arr_invalidate_i64_cache(arr);
    var value = value_in;
    if (is_unsigned != 0 and value < 0) value = 0;
    const bits: u16 = @intCast(xi_i64_pack_bits(value, is_unsigned));

    if (arr.packed_nums == null and arr.items != null) {
        var probe: XiNum = std.mem.zeroes(XiNum);
        probe.tag = XI_NUM_INT;
        probe.i = value;
        probe.d = @floatFromInt(value);
        xi_arr_prepare_packed_num(arr, &probe, is_unsigned, min_cap);
        return;
    }
    if (arr.packed_nums == null) {
        arr.packed_bits = bits;
        arr.packed_unsigned = if (is_unsigned != 0) 1 else 0;
        xi_arr_ensure_packed_capacity(arr, min_cap);
        return;
    }
    if ((arr.packed_unsigned != 0) != (is_unsigned != 0)) {
        xi_arr_materialize_packed_nums(arr);
        return;
    }
    if (bits > arr.packed_bits) xi_arr_repack_nums(arr, bits);
    xi_arr_ensure_packed_capacity(arr, min_cap);
}

fn xi_arr_fill_default_range(arr: *XiStrArray, from: usize, to: usize, expected_tag: u8) void {
    if (from >= to) return;
    const default_value = xi_value_default_for_tag(expected_tag);
    var i = from;
    while (i < to) : (i += 1) arr.items.?[i] = default_value;
}

fn xi_arr_store_value(arr: *XiStrArray, pos: usize, value: XiValue, expected_tag: u8) void {
    xi_arr_invalidate_i64_cache(arr);
    xi_arr_materialize_packed_nums(arr);
    if (pos >= arr.len) {
        if (pos == SIZE_MAX) fatal("xi_runtime: array index overflow\n");
        xi_arr_ensure_capacity(arr, pos + 1);
        xi_arr_fill_default_range(arr, arr.len, pos, expected_tag);
        arr.len = pos + 1;
    } else if (arr.items.?[pos].tag != XI_VALUE_NULL and arr.items.?[pos].tag != expected_tag) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "array element type mismatch");
    }
    if (value.tag != expected_tag and value.tag != XI_VALUE_NULL) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "array element type mismatch");
        arr.items.?[pos] = xi_value_default_for_tag(expected_tag);
        return;
    }
    arr.items.?[pos] = if (value.tag == XI_VALUE_NULL) xi_value_default_for_tag(expected_tag) else value;
}

fn xi_arr_store_num_value(arr: *XiStrArray, pos: usize, value_in: ?*XiNum, is_unsigned: c_int) void {
    const value = value_in orelse xi_num_make_int(0);
    xi_arr_prepare_packed_num(arr, value, is_unsigned, pos + 1);
    if (arr.packed_nums == null) {
        xi_arr_store_value(arr, pos, valNum(value), XI_VALUE_NUM);
        return;
    }
    if (pos >= arr.len) arr.len = pos + 1;
    const elem_bytes = xi_arr_packed_elem_bytes(arr);
    xi_num_write_pack_bytes(value, arr.packed_nums.? + pos * elem_bytes, elem_bytes, is_unsigned);
}

fn xi_arr_store_num_i64_value(arr: *XiStrArray, pos: usize, value_in: i64, is_unsigned: c_int) void {
    var value = value_in;
    if (is_unsigned != 0 and value < 0) value = 0;
    xi_arr_prepare_packed_i64(arr, value, is_unsigned, pos + 1);
    if (arr.packed_nums == null) {
        xi_arr_store_value(arr, pos, valNum(xi_num_make_int(value)), XI_VALUE_NUM);
        return;
    }
    if (pos >= arr.len) arr.len = pos + 1;
    const elem_bytes = xi_arr_packed_elem_bytes(arr);
    xi_i64_write_pack_bytes(value, arr.packed_nums.? + pos * elem_bytes, elem_bytes, is_unsigned);
}

fn xi_arr_read_num_slot(arr: *XiStrArray, pos: usize, is_unsigned: c_int) *XiNum {
    if (arr.packed_nums) |pn| {
        const elem_bytes = xi_arr_packed_elem_bytes(arr);
        return xi_num_from_pack_bytes(pn + pos * elem_bytes, elem_bytes, if (arr.packed_unsigned != 0 or is_unsigned != 0) 1 else 0);
    }
    const slot = &arr.items.?[pos];
    if (slot.tag != XI_VALUE_NUM or valAsNum(slot.*) == null) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "array element type mismatch");
        return xi_num_make_int(0);
    }
    return if (is_unsigned != 0) xi_num_clamp_nonneg(valAsNum(slot.*)) else valAsNum(slot.*).?;
}

fn xi_arr_push_num_impl(arr: ?*XiStrArray, value: ?*XiNum, is_unsigned: c_int) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    if (value == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "numeric value was null");
    if (a.len >= SIZE_MAX) fatal("xi_runtime: array length overflow\n");
    xi_arr_store_num_value(a, a.len, if (value != null) value else xi_num_make_int(0), is_unsigned);
}

export fn xi_arr_push_num(arr: ?*XiStrArray, value: ?*XiNum) callconv(.c) void {
    xi_arr_push_num_impl(arr, value, 0);
}
export fn xi_arr_push_uint(arr: ?*XiStrArray, value: ?*XiNum) callconv(.c) void {
    xi_arr_push_num_impl(arr, if (value != null) xi_num_clamp_nonneg(value) else value, 1);
}

fn xi_arr_push_num_i64_impl(arr: ?*XiStrArray, value: i64, is_unsigned: c_int) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    if (a.len >= SIZE_MAX) fatal("xi_runtime: array length overflow\n");
    xi_arr_store_num_i64_value(a, a.len, value, is_unsigned);
}

export fn xi_arr_push_num_i64(arr: ?*XiStrArray, value: i64) callconv(.c) void {
    xi_arr_push_num_i64_impl(arr, value, 0);
}

export fn xi_arr_push_uint_i64(arr: ?*XiStrArray, value: i64) callconv(.c) void {
    xi_arr_push_num_i64_impl(arr, value, 1);
}

export fn xi_arr_push_str(arr: ?*XiStrArray, value: OptStr) callconv(.c) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    if (value == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
    if (a.len >= SIZE_MAX) fatal("xi_runtime: array length overflow\n");
    xi_arr_store_value(a, a.len, valStr(if (value != null) value else xi_str_intern_cstr("")), XI_VALUE_STR);
}

export fn xi_arr_push_bool(arr: ?*XiStrArray, value: bool) callconv(.c) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    if (a.len >= SIZE_MAX) fatal("xi_runtime: array length overflow\n");
    xi_arr_store_value(a, a.len, valBool(value), XI_VALUE_BOOL);
}

export fn xi_arr_push_struct(arr: ?*XiStrArray, value: ?*XiStruct) callconv(.c) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    if (value == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "struct value was null");
    if (a.len >= SIZE_MAX) fatal("xi_runtime: array length overflow\n");
    xi_arr_store_value(a, a.len, valStruct(value), XI_VALUE_STRUCT);
}

export fn xi_arr_push_raw(arr: ?*XiStrArray, value: ?*anyopaque) callconv(.c) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    if (a.len >= SIZE_MAX) fatal("xi_runtime: array length overflow\n");
    xi_arr_store_value(a, a.len, valRaw(value), XI_VALUE_OPAQUE);
}

fn xi_arr_i64_index_for_mutation(arr: ?*XiStrArray, i: i64, allow_end: usize, out: *usize) bool {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        out.* = 0;
        return false;
    };
    if (i < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        out.* = 0;
        return false;
    }
    const pos: usize = @intCast(i);
    const max = if (allow_end != 0) a.len else (if (a.len == 0) 0 else a.len - 1);
    if ((allow_end != 0 and pos > a.len) or (allow_end == 0 and pos >= a.len)) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        out.* = max;
        return false;
    }
    out.* = pos;
    return true;
}

fn xi_arr_index_for_mutation(arr: ?*XiStrArray, idx: ?*XiNum, allow_end: usize, out: *usize) bool {
    return xi_arr_i64_index_for_mutation(arr, xi_num_to_i64(idx), allow_end, out);
}

fn xi_arr_pop_value(arr: ?*XiStrArray, expected_tag: u8) XiValue {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_value_default_for_tag(expected_tag);
    };
    if (expected_tag != XI_VALUE_NUM) xi_arr_materialize_packed_nums(a);
    xi_arr_invalidate_i64_cache(a);
    if (a.len == 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array is empty");
        return xi_value_default_for_tag(expected_tag);
    }
    const value = a.items.?[a.len - 1];
    a.items.?[a.len - 1] = valNull();
    a.len -= 1;
    if (value.tag != expected_tag) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array element type mismatch");
        return xi_value_default_for_tag(expected_tag);
    }
    return value;
}

fn xi_arr_pop_num_impl(arr: ?*XiStrArray, is_unsigned: c_int) *XiNum {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_num_make_int(0);
    };
    if (a.len == 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array is empty");
        return xi_num_make_int(0);
    }
    xi_arr_invalidate_i64_cache(a);
    if (a.packed_nums) |pn| {
        const out = xi_arr_read_num_slot(a, a.len - 1, is_unsigned);
        const elem_bytes = xi_arr_packed_elem_bytes(a);
        cmemset(pn + (a.len - 1) * elem_bytes, 0, elem_bytes);
        a.len -= 1;
        return out;
    }
    const value = xi_arr_pop_value(a, XI_VALUE_NUM);
    return if (value.tag == XI_VALUE_NUM and valAsNum(value) != null) valAsNum(value).? else xi_num_make_int(0);
}

export fn xi_arr_pop_num(arr: ?*XiStrArray) callconv(.c) *XiNum {
    return xi_arr_pop_num_impl(arr, 0);
}
export fn xi_arr_pop_uint(arr: ?*XiStrArray) callconv(.c) *XiNum {
    return xi_arr_pop_num_impl(arr, 1);
}
export fn xi_arr_pop_str(arr: ?*XiStrArray) callconv(.c) Str {
    const value = xi_arr_pop_value(arr, XI_VALUE_STR);
    return if (value.tag == XI_VALUE_STR and valAsStr(value) != null) valAsStr(value).? else xi_str_intern_cstr("");
}
export fn xi_arr_pop_bool(arr: ?*XiStrArray) callconv(.c) bool {
    const value = xi_arr_pop_value(arr, XI_VALUE_BOOL);
    return if (value.tag == XI_VALUE_BOOL) value.boolean else false;
}
export fn xi_arr_pop_struct(arr: ?*XiStrArray) callconv(.c) ?*XiStruct {
    const value = xi_arr_pop_value(arr, XI_VALUE_STRUCT);
    return if (value.tag == XI_VALUE_STRUCT) valAsStruct(value) else null;
}
export fn xi_arr_pop_raw(arr: ?*XiStrArray) callconv(.c) ?*anyopaque {
    const value = xi_arr_pop_value(arr, XI_VALUE_OPAQUE);
    return if (value.tag == XI_VALUE_OPAQUE) value.ptr else null;
}

fn xi_arr_insert_value(arr: ?*XiStrArray, idx: ?*XiNum, value: XiValue, expected_tag: u8) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    if (expected_tag != XI_VALUE_NUM) xi_arr_materialize_packed_nums(a);
    xi_arr_invalidate_i64_cache(a);
    if (value.tag != expected_tag and value.tag != XI_VALUE_NULL) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array element type mismatch");
        return;
    }
    var pos: usize = 0;
    if (!xi_arr_index_for_mutation(a, idx, 1, &pos)) return;
    if (a.len >= SIZE_MAX) fatal("xi_runtime: array length overflow\n");
    xi_arr_ensure_capacity(a, a.len + 1);
    if (pos < a.len) {
        cmemmove(a.items.? + pos + 1, a.items.? + pos, (a.len - pos) * @sizeOf(XiValue));
    }
    a.items.?[pos] = if (value.tag == XI_VALUE_NULL) xi_value_default_for_tag(expected_tag) else value;
    a.len += 1;
}

fn xi_arr_insert_num_impl(arr: ?*XiStrArray, idx: ?*XiNum, value_in: ?*XiNum, is_unsigned: c_int) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    if (value_in == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "numeric value was null");
    var value = if (value_in != null) value_in.? else xi_num_make_int(0);
    if (is_unsigned != 0) value = xi_num_clamp_nonneg(value);
    var pos: usize = 0;
    if (!xi_arr_index_for_mutation(a, idx, 1, &pos)) return;
    if (a.len >= SIZE_MAX) fatal("xi_runtime: array length overflow\n");
    xi_arr_prepare_packed_num(a, value, is_unsigned, a.len + 1);
    if (a.packed_nums == null) {
        xi_arr_insert_value(a, idx, valNum(value), XI_VALUE_NUM);
        return;
    }
    const elem_bytes = xi_arr_packed_elem_bytes(a);
    if (pos < a.len) {
        cmemmove(a.packed_nums.? + (pos + 1) * elem_bytes, a.packed_nums.? + pos * elem_bytes, (a.len - pos) * elem_bytes);
    }
    xi_num_write_pack_bytes(value, a.packed_nums.? + pos * elem_bytes, elem_bytes, is_unsigned);
    a.len += 1;
}

fn xi_arr_insert_num_i64_impl(arr: ?*XiStrArray, idx: i64, value_in: i64, is_unsigned: c_int) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    var value = value_in;
    if (is_unsigned != 0 and value < 0) value = 0;
    var pos: usize = 0;
    if (!xi_arr_i64_index_for_mutation(a, idx, 1, &pos)) return;
    if (a.len >= SIZE_MAX) fatal("xi_runtime: array length overflow\n");
    xi_arr_prepare_packed_i64(a, value, is_unsigned, a.len + 1);
    if (a.packed_nums == null) {
        xi_arr_invalidate_i64_cache(a);
        xi_arr_ensure_capacity(a, a.len + 1);
        if (pos < a.len) {
            cmemmove(a.items.? + pos + 1, a.items.? + pos, (a.len - pos) * @sizeOf(XiValue));
        }
        a.items.?[pos] = valNum(xi_num_make_int(value));
        a.len += 1;
        return;
    }
    const elem_bytes = xi_arr_packed_elem_bytes(a);
    if (pos < a.len) {
        cmemmove(a.packed_nums.? + (pos + 1) * elem_bytes, a.packed_nums.? + pos * elem_bytes, (a.len - pos) * elem_bytes);
    }
    xi_i64_write_pack_bytes(value, a.packed_nums.? + pos * elem_bytes, elem_bytes, is_unsigned);
    a.len += 1;
}

export fn xi_arr_insert_num(arr: ?*XiStrArray, idx: ?*XiNum, value: ?*XiNum) callconv(.c) void {
    xi_arr_insert_num_impl(arr, idx, value, 0);
}
export fn xi_arr_insert_uint(arr: ?*XiStrArray, idx: ?*XiNum, value: ?*XiNum) callconv(.c) void {
    xi_arr_insert_num_impl(arr, idx, value, 1);
}
export fn xi_arr_insert_num_i64_at_i64(arr: ?*XiStrArray, idx: i64, value: i64) callconv(.c) void {
    xi_arr_insert_num_i64_impl(arr, idx, value, 0);
}
export fn xi_arr_insert_uint_i64_at_i64(arr: ?*XiStrArray, idx: i64, value: i64) callconv(.c) void {
    xi_arr_insert_num_i64_impl(arr, idx, value, 1);
}
export fn xi_arr_insert_str(arr: ?*XiStrArray, idx: ?*XiNum, value: OptStr) callconv(.c) void {
    if (value == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
    xi_arr_insert_value(arr, idx, valStr(if (value != null) value else xi_str_intern_cstr("")), XI_VALUE_STR);
}
export fn xi_arr_insert_bool(arr: ?*XiStrArray, idx: ?*XiNum, value: bool) callconv(.c) void {
    xi_arr_insert_value(arr, idx, valBool(value), XI_VALUE_BOOL);
}
export fn xi_arr_insert_struct(arr: ?*XiStrArray, idx: ?*XiNum, value: ?*XiStruct) callconv(.c) void {
    if (value == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "struct value was null");
    xi_arr_insert_value(arr, idx, valStruct(value), XI_VALUE_STRUCT);
}
export fn xi_arr_insert_raw(arr: ?*XiStrArray, idx: ?*XiNum, value: ?*anyopaque) callconv(.c) void {
    xi_arr_insert_value(arr, idx, valRaw(value), XI_VALUE_OPAQUE);
}

fn xi_arr_remove_value(arr: ?*XiStrArray, idx: ?*XiNum, expected_tag: u8) XiValue {
    const a = arr orelse return xi_value_default_for_tag(expected_tag);
    xi_arr_invalidate_i64_cache(a);
    if (expected_tag != XI_VALUE_NUM) xi_arr_materialize_packed_nums(a);
    var pos: usize = 0;
    if (!xi_arr_index_for_mutation(a, idx, 0, &pos)) return xi_value_default_for_tag(expected_tag);
    const value = a.items.?[pos];
    if (pos + 1 < a.len) {
        cmemmove(a.items.? + pos, a.items.? + pos + 1, (a.len - pos - 1) * @sizeOf(XiValue));
    }
    a.len -= 1;
    a.items.?[a.len] = valNull();
    if (value.tag != expected_tag) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array element type mismatch");
        return xi_value_default_for_tag(expected_tag);
    }
    return value;
}

fn xi_arr_remove_num_impl(arr: ?*XiStrArray, idx: ?*XiNum, is_unsigned: c_int) *XiNum {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_num_make_int(0);
    };
    xi_arr_invalidate_i64_cache(a);
    var pos: usize = 0;
    if (!xi_arr_index_for_mutation(a, idx, 0, &pos)) return xi_num_make_int(0);
    if (a.packed_nums) |pn| {
        const elem_bytes = xi_arr_packed_elem_bytes(a);
        const out = xi_arr_read_num_slot(a, pos, is_unsigned);
        if (pos + 1 < a.len) {
            cmemmove(pn + pos * elem_bytes, pn + (pos + 1) * elem_bytes, (a.len - pos - 1) * elem_bytes);
        }
        a.len -= 1;
        cmemset(pn + a.len * elem_bytes, 0, elem_bytes);
        return out;
    }
    const value = xi_arr_remove_value(a, idx, XI_VALUE_NUM);
    return if (value.tag == XI_VALUE_NUM and valAsNum(value) != null) valAsNum(value).? else xi_num_make_int(0);
}

fn xi_arr_remove_num_i64_impl(arr: ?*XiStrArray, idx: i64, is_unsigned: c_int) *XiNum {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_num_make_int(0);
    };
    xi_arr_invalidate_i64_cache(a);
    var pos: usize = 0;
    if (!xi_arr_i64_index_for_mutation(a, idx, 0, &pos)) return xi_num_make_int(0);
    if (a.packed_nums) |packed_buf| {
        const elem_bytes = xi_arr_packed_elem_bytes(a);
        const out = xi_arr_read_num_slot(a, pos, is_unsigned);
        if (pos + 1 < a.len) {
            cmemmove(packed_buf + pos * elem_bytes, packed_buf + (pos + 1) * elem_bytes, (a.len - pos - 1) * elem_bytes);
        }
        a.len -= 1;
        cmemset(packed_buf + a.len * elem_bytes, 0, elem_bytes);
        return out;
    }
    const value = a.items.?[pos];
    if (pos + 1 < a.len) {
        cmemmove(a.items.? + pos, a.items.? + pos + 1, (a.len - pos - 1) * @sizeOf(XiValue));
    }
    a.len -= 1;
    a.items.?[a.len] = valNull();
    if (value.tag != XI_VALUE_NUM or valAsNum(value) == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array element type mismatch");
        return xi_num_make_int(0);
    }
    return if (is_unsigned != 0) xi_num_clamp_nonneg(valAsNum(value)) else valAsNum(value).?;
}

export fn xi_arr_remove_num(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) *XiNum {
    return xi_arr_remove_num_impl(arr, idx, 0);
}
export fn xi_arr_remove_uint(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) *XiNum {
    return xi_arr_remove_num_impl(arr, idx, 1);
}
export fn xi_arr_remove_num_at_i64(arr: ?*XiStrArray, idx: i64) callconv(.c) *XiNum {
    return xi_arr_remove_num_i64_impl(arr, idx, 0);
}
export fn xi_arr_remove_uint_at_i64(arr: ?*XiStrArray, idx: i64) callconv(.c) *XiNum {
    return xi_arr_remove_num_i64_impl(arr, idx, 1);
}
export fn xi_arr_remove_str(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) Str {
    const value = xi_arr_remove_value(arr, idx, XI_VALUE_STR);
    return if (value.tag == XI_VALUE_STR and valAsStr(value) != null) valAsStr(value).? else xi_str_intern_cstr("");
}
export fn xi_arr_remove_bool(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) bool {
    const value = xi_arr_remove_value(arr, idx, XI_VALUE_BOOL);
    return if (value.tag == XI_VALUE_BOOL) value.boolean else false;
}
export fn xi_arr_remove_struct(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) ?*XiStruct {
    const value = xi_arr_remove_value(arr, idx, XI_VALUE_STRUCT);
    return if (value.tag == XI_VALUE_STRUCT) valAsStruct(value) else null;
}
export fn xi_arr_remove_raw(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) ?*anyopaque {
    const value = xi_arr_remove_value(arr, idx, XI_VALUE_OPAQUE);
    return if (value.tag == XI_VALUE_OPAQUE) value.ptr else null;
}

export fn xi_arr_clear(arr: ?*XiStrArray) callconv(.c) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    xi_arr_invalidate_i64_cache(a);
    if (a.packed_nums) |pn| {
        const elem_bytes = xi_arr_packed_elem_bytes(a);
        cmemset(pn, 0, a.len * elem_bytes);
        a.len = 0;
        return;
    }
    var i: usize = 0;
    while (i < a.len) : (i += 1) a.items.?[i] = valNull();
    a.len = 0;
}

fn idxToNum(i: usize) *XiNum {
    return xi_num_make_int(if (i > @as(usize, @bitCast(INT64_MAX))) INT64_MAX else @intCast(i));
}

fn xi_packed_num_equals_i64(bytes: [*]const u8, nbytes: usize, is_unsigned: bool, value: i64) bool {
    if (is_unsigned and value < 0) return false;
    const raw: u64 = @bitCast(value);
    const fill: u8 = if (!is_unsigned and value < 0) 0xff else 0x00;
    var i: usize = 0;
    while (i < nbytes) : (i += 1) {
        const expected = if (i < 8) @as(u8, @truncate(raw >> @as(u6, @intCast(i * 8)))) else fill;
        if (bytes[i] != expected) return false;
    }
    return true;
}

export fn xi_arr_index_of_num(arr: ?*XiStrArray, value: ?*XiNum) callconv(.c) *XiNum {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_num_make_int(-1);
    };
    const v = value orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "numeric value was null");
        return xi_num_make_int(-1);
    };
    var native: i64 = 0;
    if (xi_num_try_i64(v, &native)) return xi_arr_index_of_num_i64(a, native);
    if (a.packed_nums != null) {
        var i: usize = 0;
        while (i < a.len) : (i += 1) {
            const slot = xi_arr_read_num_slot(a, i, if (a.packed_unsigned != 0) 1 else 0);
            if (xi_num_eq(slot, v)) return idxToNum(i);
        }
        return xi_num_make_int(-1);
    }
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        const slot = &a.items.?[i];
        if (slot.tag == XI_VALUE_NUM and valAsNum(slot.*) != null and xi_num_eq(valAsNum(slot.*).?, v)) return idxToNum(i);
    }
    return xi_num_make_int(-1);
}

export fn xi_arr_index_of_num_i64(arr: ?*XiStrArray, value: i64) callconv(.c) *XiNum {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_num_make_int(-1);
    };
    if (a.packed_nums) |packed_buf| {
        const elem_bytes = xi_arr_packed_elem_bytes(a);
        var i: usize = 0;
        while (i < a.len) : (i += 1) {
            if (xi_packed_num_equals_i64(packed_buf + i * elem_bytes, elem_bytes, a.packed_unsigned != 0, value)) return idxToNum(i);
        }
        return xi_num_make_int(-1);
    }
    var probe = xi_num_i64_operand(value);
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        const slot = &a.items.?[i];
        if (slot.tag == XI_VALUE_NUM and valAsNum(slot.*) != null and xi_num_eq(valAsNum(slot.*).?, &probe)) return idxToNum(i);
    }
    return xi_num_make_int(-1);
}

export fn xi_arr_index_of_str(arr: ?*XiStrArray, value: OptStr) callconv(.c) *XiNum {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_num_make_int(-1);
    };
    xi_arr_materialize_packed_nums(a);
    if (value == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return xi_num_make_int(-1);
    }
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        const slot = &a.items.?[i];
        if (slot.tag == XI_VALUE_STR and valAsStr(slot.*) != null and xi_str_equal_known(valAsStr(slot.*), value) == 1) return idxToNum(i);
    }
    return xi_num_make_int(-1);
}

export fn xi_arr_index_of_bool(arr: ?*XiStrArray, value: bool) callconv(.c) *XiNum {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_num_make_int(-1);
    };
    xi_arr_materialize_packed_nums(a);
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        const slot = &a.items.?[i];
        if (slot.tag == XI_VALUE_BOOL and slot.boolean == value) return idxToNum(i);
    }
    return xi_num_make_int(-1);
}

export fn xi_arr_index_of_struct(arr: ?*XiStrArray, value: ?*XiStruct) callconv(.c) *XiNum {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_num_make_int(-1);
    };
    xi_arr_materialize_packed_nums(a);
    if (value == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "struct value was null");
        return xi_num_make_int(-1);
    }
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        const slot = &a.items.?[i];
        if (slot.tag == XI_VALUE_STRUCT and ptrEq(valAsStruct(slot.*), value)) return idxToNum(i);
    }
    return xi_num_make_int(-1);
}

export fn xi_arr_index_of_raw(arr: ?*XiStrArray, value: ?*anyopaque) callconv(.c) *XiNum {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_num_make_int(-1);
    };
    xi_arr_materialize_packed_nums(a);
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        const slot = &a.items.?[i];
        if (slot.tag == XI_VALUE_OPAQUE and ptrEq(slot.ptr, value)) return idxToNum(i);
    }
    return xi_num_make_int(-1);
}

export fn xi_arr_contains_num(arr: ?*XiStrArray, value: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_make_int(if (xi_num_to_i64(xi_arr_index_of_num(arr, value)) >= 0) 1 else 0);
}
export fn xi_arr_contains_num_i64(arr: ?*XiStrArray, value: i64) callconv(.c) *XiNum {
    return xi_num_make_int(if (xi_num_to_i64(xi_arr_index_of_num_i64(arr, value)) >= 0) 1 else 0);
}
export fn xi_arr_contains_str(arr: ?*XiStrArray, value: OptStr) callconv(.c) *XiNum {
    return xi_num_make_int(if (xi_num_to_i64(xi_arr_index_of_str(arr, value)) >= 0) 1 else 0);
}
export fn xi_arr_contains_bool(arr: ?*XiStrArray, value: bool) callconv(.c) *XiNum {
    return xi_num_make_int(if (xi_num_to_i64(xi_arr_index_of_bool(arr, value)) >= 0) 1 else 0);
}
export fn xi_arr_contains_struct(arr: ?*XiStrArray, value: ?*XiStruct) callconv(.c) *XiNum {
    return xi_num_make_int(if (xi_num_to_i64(xi_arr_index_of_struct(arr, value)) >= 0) 1 else 0);
}
export fn xi_arr_contains_raw(arr: ?*XiStrArray, value: ?*anyopaque) callconv(.c) *XiNum {
    return xi_num_make_int(if (xi_num_to_i64(xi_arr_index_of_raw(arr, value)) >= 0) 1 else 0);
}

export fn xi_arr_len(arr: ?*XiStrArray) callconv(.c) *XiNum {
    return xi_num_make_int(xi_arr_len_i64(arr));
}
export fn xi_arr_len_i64(arr: ?*XiStrArray) callconv(.c) i64 {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return 0;
    };
    if (a.len > @as(usize, @bitCast(INT64_MAX))) return INT64_MAX;
    return @intCast(a.len);
}

export fn xi_arr_all_num_fit_i64(arr: ?*XiStrArray) callconv(.c) bool {
    const a = arr orelse return false;
    if (a.i64_cache != null) return true;
    if (a.len == 0) return true;
    if (a.len > SIZE_MAX / @sizeOf(i64)) return false;
    const cache: [*]i64 = @ptrCast(@alignCast(malloc(a.len * @sizeOf(i64)) orelse xi_oom()));
    if (a.packed_nums != null) {
        if (a.packed_bits > 64) {
            cfree(cache);
            return false;
        }
        const elem_bytes = xi_arr_packed_elem_bytes(a);
        var i: usize = 0;
        while (i < a.len) : (i += 1) {
            const src = a.packed_nums.? + i * elem_bytes;
            var raw: u64 = 0;
            var n: usize = 0;
            while (n < elem_bytes) : (n += 1) raw |= @as(u64, src[n]) << @as(u6, @intCast(n * 8));
            if (a.packed_unsigned != 0) {
                if (elem_bytes == 8 and (raw & (@as(u64, 1) << 63)) != 0) {
                    cfree(cache);
                    return false;
                }
            } else if (elem_bytes < 8 and (src[elem_bytes - 1] & 0x80) != 0) {
                raw |= UINT64_MAX << @as(u6, @intCast(elem_bytes * 8));
            }
            cache[i] = @bitCast(raw);
        }
        a.i64_cache = cache;
        return true;
    }
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        const slot = &a.items.?[i];
        if (slot.tag != XI_VALUE_NUM) {
            cfree(cache);
            return false;
        }
        var exact: i64 = 0;
        if (!xi_num_try_i64(valAsNum(slot.*), &exact)) {
            cfree(cache);
            return false;
        }
        cache[i] = exact;
    }
    a.i64_cache = cache;
    return true;
}

fn xi_arr_get_num_i64_impl(arr: ?*XiStrArray, i: i64, is_unsigned: c_int) *XiNum {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_num_make_int(0);
    };
    if (i < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return xi_num_make_int(0);
    }
    const pos: usize = @intCast(i);
    if (pos >= a.len) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return xi_num_make_int(0);
    }
    return xi_arr_read_num_slot(a, pos, is_unsigned);
}

fn xi_arr_get_num_raw_i64_impl(arr: ?*XiStrArray, i: i64, is_unsigned: c_int) i64 {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return 0;
    };
    if (i < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return 0;
    }
    const pos: usize = @intCast(i);
    if (pos >= a.len) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return 0;
    }
    if (a.i64_cache) |cache| return cache[pos];
    if (a.packed_nums) |pn| {
        const elem_bytes = xi_arr_packed_elem_bytes(a);
        const src = pn + pos * elem_bytes;
        var raw: u64 = 0;
        const take: usize = if (elem_bytes < 8) elem_bytes else 8;
        var n: usize = 0;
        while (n < take) : (n += 1) raw |= @as(u64, src[n]) << @as(u6, @intCast(n * 8));
        if (is_unsigned == 0 and elem_bytes > 0 and elem_bytes < 8 and (src[elem_bytes - 1] & 0x80) != 0) {
            raw |= UINT64_MAX << @as(u6, @intCast(elem_bytes * 8));
        }
        return @bitCast(raw);
    }
    const slot = &a.items.?[pos];
    if (slot.tag != XI_VALUE_NUM or valAsNum(slot.*) == null) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "array element type mismatch");
        return 0;
    }
    const value = if (is_unsigned != 0) xi_num_clamp_nonneg(valAsNum(slot.*)) else valAsNum(slot.*).?;
    return xi_num_to_i64(value);
}

export fn xi_arr_get_num(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) *XiNum {
    return xi_arr_get_num_i64_impl(arr, xi_num_to_i64(idx), 0);
}
export fn xi_arr_get_uint(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) *XiNum {
    return xi_arr_get_num_i64_impl(arr, xi_num_to_i64(idx), 1);
}
export fn xi_arr_get_num_at_i64(arr: ?*XiStrArray, idx: i64) callconv(.c) *XiNum {
    return xi_arr_get_num_i64_impl(arr, idx, 0);
}
export fn xi_arr_get_uint_at_i64(arr: ?*XiStrArray, idx: i64) callconv(.c) *XiNum {
    return xi_arr_get_num_i64_impl(arr, idx, 1);
}
export fn xi_arr_get_num_i64_at_i64(arr: ?*XiStrArray, idx: i64) callconv(.c) i64 {
    return xi_arr_get_num_raw_i64_impl(arr, idx, 0);
}
export fn xi_arr_get_uint_i64_at_i64(arr: ?*XiStrArray, idx: i64) callconv(.c) i64 {
    return xi_arr_get_num_raw_i64_impl(arr, idx, 1);
}

export fn xi_arr_get_num_i64_at_i64_unchecked(arr: *XiStrArray, idx: i64) callconv(.c) i64 {
    const pos: usize = @intCast(idx);
    if (arr.i64_cache) |cache| return cache[pos];
    if (arr.packed_nums) |pn| {
        const elem_bytes = xi_arr_packed_elem_bytes(arr);
        const src = pn + pos * elem_bytes;
        var raw: u64 = 0;
        const take: usize = if (elem_bytes < 8) elem_bytes else 8;
        var n: usize = 0;
        while (n < take) : (n += 1) raw |= @as(u64, src[n]) << @as(u6, @intCast(n * 8));
        if (elem_bytes > 0 and elem_bytes < 8 and (src[elem_bytes - 1] & 0x80) != 0) {
            raw |= UINT64_MAX << @as(u6, @intCast(elem_bytes * 8));
        }
        return @bitCast(raw);
    }
    return xi_num_to_i64(valAsNum(arr.items.?[pos]));
}

export fn xi_arr_get_uint_i64_at_i64_unchecked(arr: *XiStrArray, idx: i64) callconv(.c) i64 {
    const pos: usize = @intCast(idx);
    if (arr.packed_nums) |pn| {
        const elem_bytes = xi_arr_packed_elem_bytes(arr);
        const src = pn + pos * elem_bytes;
        var raw: u64 = 0;
        const take: usize = if (elem_bytes < 8) elem_bytes else 8;
        var n: usize = 0;
        while (n < take) : (n += 1) raw |= @as(u64, src[n]) << @as(u6, @intCast(n * 8));
        return @bitCast(raw);
    }
    return xi_num_to_i64(xi_num_clamp_nonneg(valAsNum(arr.items.?[pos])));
}

export fn xi_arr_get_str(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) Str {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_str_intern_cstr("");
    };
    xi_arr_materialize_packed_nums(a);
    const i = xi_num_to_i64(idx);
    if (i < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return xi_str_intern_cstr("");
    }
    const pos: usize = @intCast(i);
    if (pos >= a.len) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return xi_str_intern_cstr("");
    }
    const slot = &a.items.?[pos];
    if (slot.tag != XI_VALUE_STR or valAsStr(slot.*) == null) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "array element type mismatch");
        return xi_str_intern_cstr("");
    }
    return valAsStr(slot.*).?;
}

export fn xi_arr_get_bool(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) bool {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return false;
    };
    xi_arr_materialize_packed_nums(a);
    const i = xi_num_to_i64(idx);
    if (i < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return false;
    }
    const pos: usize = @intCast(i);
    if (pos >= a.len) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return false;
    }
    const slot = &a.items.?[pos];
    if (slot.tag != XI_VALUE_BOOL) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "array element type mismatch");
        return false;
    }
    return slot.boolean;
}

export fn xi_arr_get_struct(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) ?*XiStruct {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return null;
    };
    xi_arr_materialize_packed_nums(a);
    const i = xi_num_to_i64(idx);
    if (i < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return null;
    }
    const pos: usize = @intCast(i);
    if (pos >= a.len) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return null;
    }
    const slot = &a.items.?[pos];
    if (slot.tag != XI_VALUE_STRUCT) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "array element type mismatch");
        return null;
    }
    return valAsStruct(slot.*);
}

export fn xi_arr_get_raw(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) ?*anyopaque {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return null;
    };
    xi_arr_materialize_packed_nums(a);
    const i = xi_num_to_i64(idx);
    if (i < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return null;
    }
    const pos: usize = @intCast(i);
    if (pos >= a.len) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return null;
    }
    const slot = &a.items.?[pos];
    if (slot.tag != XI_VALUE_OPAQUE) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "array element type mismatch");
        return null;
    }
    return slot.ptr;
}

fn xi_arr_set_num_i64_impl(arr: ?*XiStrArray, i: i64, value_in: ?*XiNum, is_unsigned: c_int) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    if (value_in == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "numeric value was null");
    if (i < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return;
    }
    const pos: usize = @intCast(i);
    var value = if (value_in != null) value_in.? else xi_num_make_int(0);
    if (is_unsigned != 0) value = xi_num_clamp_nonneg(value);
    xi_arr_store_num_value(a, pos, value, is_unsigned);
}

fn xi_arr_set_num_raw_i64_impl(arr: ?*XiStrArray, i: i64, value: i64, is_unsigned: c_int) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    if (i < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return;
    }
    xi_arr_store_num_i64_value(a, @intCast(i), value, is_unsigned);
}

export fn xi_arr_set_num(arr: ?*XiStrArray, idx: ?*XiNum, value: ?*XiNum) callconv(.c) void {
    xi_arr_set_num_i64_impl(arr, xi_num_to_i64(idx), value, 0);
}
export fn xi_arr_set_uint(arr: ?*XiStrArray, idx: ?*XiNum, value: ?*XiNum) callconv(.c) void {
    xi_arr_set_num_i64_impl(arr, xi_num_to_i64(idx), value, 1);
}
export fn xi_arr_set_num_at_i64(arr: ?*XiStrArray, idx: i64, value: ?*XiNum) callconv(.c) void {
    xi_arr_set_num_i64_impl(arr, idx, value, 0);
}
export fn xi_arr_set_uint_at_i64(arr: ?*XiStrArray, idx: i64, value: ?*XiNum) callconv(.c) void {
    xi_arr_set_num_i64_impl(arr, idx, value, 1);
}
export fn xi_arr_set_num_i64_at_i64(arr: ?*XiStrArray, idx: i64, value: i64) callconv(.c) void {
    xi_arr_set_num_raw_i64_impl(arr, idx, value, 0);
}
export fn xi_arr_set_uint_i64_at_i64(arr: ?*XiStrArray, idx: i64, value: i64) callconv(.c) void {
    xi_arr_set_num_raw_i64_impl(arr, idx, value, 1);
}

noinline fn xi_arr_set_num_zero_slow(arr: ?*XiStrArray, idx: i64) void {
    xi_arr_set_num_raw_i64_impl(arr, idx, 0, 0);
}

// elements can therefore be cleared without running capacity/packing logic;
export fn xi_arr_set_num_zero_at_i64(arr: ?*XiStrArray, idx: i64) callconv(.c) void {
    const a = arr orelse {
        xi_arr_set_num_zero_slow(arr, idx);
        return;
    };
    if (idx < 0 or @as(usize, @intCast(idx)) >= a.len or
        a.packed_nums == null or a.packed_unsigned != 0) {
        xi_arr_set_num_zero_slow(a, idx);
        return;
    }
    const elem_bytes = xi_arr_packed_elem_bytes(a);
    const dst = a.packed_nums.? + @as(usize, @intCast(idx)) * elem_bytes;
    switch (elem_bytes) {
        1 => dst[0] = 0,
        2 => { dst[0] = 0; dst[1] = 0; },
        4 => { dst[0] = 0; dst[1] = 0; dst[2] = 0; dst[3] = 0; },
        8 => {
            dst[0] = 0; dst[1] = 0; dst[2] = 0; dst[3] = 0;
            dst[4] = 0; dst[5] = 0; dst[6] = 0; dst[7] = 0;
        },
        else => {},
    }
}

noinline fn xi_arr_update_num_i64_slow(a: *XiStrArray, pos: usize, rhs: i64, op: u8) void {
    const old = xi_arr_read_num_slot(a, pos, 0);
    const result = switch (op) {
        '+' => xi_num_add_i64(old, rhs),
        '-' => xi_num_sub_i64(old, rhs),
        '*' => xi_num_mul_i64(old, rhs),
        else => unreachable,
    };
    xi_arr_store_num_value(a, pos, result, 0);
}

inline fn xi_arr_update_num_i64_at_i64_impl(arr: ?*XiStrArray, idx: i64, rhs: i64, comptime op: u8) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    if (idx < 0 or @as(usize, @intCast(idx)) >= a.len) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return;
    }
    const pos: usize = @intCast(idx);
    // loops.  Stay entirely in machine integers when the exact result fits;
    if (a.packed_nums != null and a.packed_bits <= 64 and a.packed_unsigned == 0) {
        const elem_bytes = xi_arr_packed_elem_bytes(a);
        const src = a.packed_nums.? + pos * elem_bytes;
        const old = xi_packed_read_signed_i64(src, elem_bytes);
        var native: i64 = 0;
        const overflow = switch (op) {
            '+' => xi_add_ovf_i64(old, rhs, &native),
            '-' => xi_sub_ovf_i64(old, rhs, &native),
            '*' => xi_mul_ovf_i64(old, rhs, &native),
            else => unreachable,
        };
        if (!overflow) {
            const bits: u16 = @intCast(xi_i64_pack_bits(native, 0));
            if (bits <= a.packed_bits) {
                xi_i64_write_pack_bytes(native, a.packed_nums.? + pos * elem_bytes, elem_bytes, 0);
                return;
            }
            xi_arr_store_num_i64_value(a, pos, native, 0);
            return;
        }
    }
    xi_arr_update_num_i64_slow(a, pos, rhs, op);
}

export fn xi_arr_add_num_i64_at_i64(arr: ?*XiStrArray, idx: i64, rhs: i64) callconv(.c) void {
    xi_arr_update_num_i64_at_i64_impl(arr, idx, rhs, '+');
}

export fn xi_arr_sub_num_i64_at_i64(arr: ?*XiStrArray, idx: i64, rhs: i64) callconv(.c) void {
    xi_arr_update_num_i64_at_i64_impl(arr, idx, rhs, '-');
}

export fn xi_arr_mul_num_i64_at_i64(arr: ?*XiStrArray, idx: i64, rhs: i64) callconv(.c) void {
    xi_arr_update_num_i64_at_i64_impl(arr, idx, rhs, '*');
}

// Exact `a[dst] += a[src] * scale` fusion. This shape is emitted by ordinary
export fn xi_arr_add_scaled_num_i64_at_i64(arr: ?*XiStrArray, dst_idx: i64, src_idx: i64, scale: i64) callconv(.c) void {
    const a = arr orelse {
        _ = xi_arr_get_num_i64_impl(arr, dst_idx, 0);
        return;
    };
    if (dst_idx >= 0 and src_idx >= 0 and
        @as(usize, @intCast(dst_idx)) < a.len and @as(usize, @intCast(src_idx)) < a.len and
        a.packed_nums != null and a.packed_bits <= 64 and a.packed_unsigned == 0) {
        const elem_bytes = xi_arr_packed_elem_bytes(a);
        const dst_pos: usize = @intCast(dst_idx);
        const src_pos: usize = @intCast(src_idx);
        const old = xi_packed_read_signed_i64(a.packed_nums.? + dst_pos * elem_bytes, elem_bytes);
        const source = xi_packed_read_signed_i64(a.packed_nums.? + src_pos * elem_bytes, elem_bytes);
        var product: i64 = 0;
        var result: i64 = 0;
        if (!xi_mul_ovf_i64(source, scale, &product) and
            !xi_add_ovf_i64(old, product, &result)) {
            const bits: u16 = @intCast(xi_i64_pack_bits(result, 0));
            if (bits <= a.packed_bits) {
                xi_i64_write_pack_bytes(result, a.packed_nums.? + dst_pos * elem_bytes, elem_bytes, 0);
            } else {
                xi_arr_store_num_i64_value(a, dst_pos, result, 0);
            }
            return;
        }
    }
    const old = xi_arr_get_num_i64_impl(a, dst_idx, 0);
    const source = xi_arr_get_num_i64_impl(a, src_idx, 0);
    const product = xi_num_mul_i64(source, scale);
    xi_arr_set_num_i64_impl(a, dst_idx, xi_num_add(old, product), 0);
}

inline fn xi_arr_cmp_num_i64_at_i64_impl(arr: ?*XiStrArray, idx: i64, rhs: i64, comptime op: u8) bool {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return switch (op) {
            0 => rhs == 0, 1 => rhs != 0, 2 => 0 < rhs,
            3 => 0 <= rhs, 4 => 0 > rhs, 5 => 0 >= rhs,
            else => false,
        };
    };
    if (idx < 0 or @as(usize, @intCast(idx)) >= a.len) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return switch (op) {
            0 => rhs == 0, 1 => rhs != 0, 2 => 0 < rhs,
            3 => 0 <= rhs, 4 => 0 > rhs, 5 => 0 >= rhs,
            else => false,
        };
    }
    if (a.packed_nums != null and a.packed_bits <= 64 and a.packed_unsigned == 0) {
        const elem_bytes = xi_arr_packed_elem_bytes(a);
        const src = a.packed_nums.? + @as(usize, @intCast(idx)) * elem_bytes;
        const lhs = xi_packed_read_signed_i64(src, elem_bytes);
        return switch (op) {
            0 => lhs == rhs, 1 => lhs != rhs, 2 => lhs < rhs,
            3 => lhs <= rhs, 4 => lhs > rhs, 5 => lhs >= rhs,
            else => false,
        };
    }
    const lhs = xi_arr_read_num_slot(a, @intCast(idx), 0);
    var probe = xi_num_i64_operand(rhs);
    return switch (op) {
        0 => xi_num_eq(lhs, &probe), 1 => xi_num_ne(lhs, &probe),
        2 => xi_num_lt(lhs, &probe), 3 => xi_num_le(lhs, &probe),
        4 => xi_num_gt(lhs, &probe), 5 => xi_num_ge(lhs, &probe),
        else => false,
    };
}

export fn xi_arr_eq_num_i64_at_i64(arr: ?*XiStrArray, idx: i64, rhs: i64) callconv(.c) bool {
    return xi_arr_cmp_num_i64_at_i64_impl(arr, idx, rhs, 0);
}
export fn xi_arr_ne_num_i64_at_i64(arr: ?*XiStrArray, idx: i64, rhs: i64) callconv(.c) bool {
    return xi_arr_cmp_num_i64_at_i64_impl(arr, idx, rhs, 1);
}
export fn xi_arr_lt_num_i64_at_i64(arr: ?*XiStrArray, idx: i64, rhs: i64) callconv(.c) bool {
    return xi_arr_cmp_num_i64_at_i64_impl(arr, idx, rhs, 2);
}
export fn xi_arr_le_num_i64_at_i64(arr: ?*XiStrArray, idx: i64, rhs: i64) callconv(.c) bool {
    return xi_arr_cmp_num_i64_at_i64_impl(arr, idx, rhs, 3);
}
export fn xi_arr_gt_num_i64_at_i64(arr: ?*XiStrArray, idx: i64, rhs: i64) callconv(.c) bool {
    return xi_arr_cmp_num_i64_at_i64_impl(arr, idx, rhs, 4);
}
export fn xi_arr_ge_num_i64_at_i64(arr: ?*XiStrArray, idx: i64, rhs: i64) callconv(.c) bool {
    return xi_arr_cmp_num_i64_at_i64_impl(arr, idx, rhs, 5);
}

export fn xi_arr_cmp_num_i64_at_i64(arr: ?*XiStrArray, idx: i64, rhs: i64, op: c_int) callconv(.c) bool {
    return switch (op) {
        0 => xi_arr_cmp_num_i64_at_i64_impl(arr, idx, rhs, 0),
        1 => xi_arr_cmp_num_i64_at_i64_impl(arr, idx, rhs, 1),
        2 => xi_arr_cmp_num_i64_at_i64_impl(arr, idx, rhs, 2),
        3 => xi_arr_cmp_num_i64_at_i64_impl(arr, idx, rhs, 3),
        4 => xi_arr_cmp_num_i64_at_i64_impl(arr, idx, rhs, 4),
        5 => xi_arr_cmp_num_i64_at_i64_impl(arr, idx, rhs, 5),
        else => false,
    };
}

noinline fn xi_arr_num_nonzero_slow(a: *XiStrArray, pos: usize) bool {
    var zero = xi_num_i64_operand(0);
    return xi_num_ne(xi_arr_read_num_slot(a, pos, 0), &zero);
}

export fn xi_arr_num_nonzero_at_i64(arr: ?*XiStrArray, idx: i64) callconv(.c) bool {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return false;
    };
    if (idx < 0 or @as(usize, @intCast(idx)) >= a.len) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return false;
    }
    if (a.packed_nums != null and a.packed_bits <= 64 and a.packed_unsigned == 0) {
        const elem_bytes = xi_arr_packed_elem_bytes(a);
        const src = a.packed_nums.? + @as(usize, @intCast(idx)) * elem_bytes;
        return xi_packed_read_signed_i64(src, elem_bytes) != 0;
    }
    return xi_arr_num_nonzero_slow(a, @intCast(idx));
}

export fn xi_arr_is_packed_signed_i64(arr: ?*XiStrArray) callconv(.c) bool {
    const a = arr orelse return false;
    return a.packed_nums != null and a.packed_bits <= 64 and a.packed_unsigned == 0;
}

export fn xi_arr_packed_nonzero_at_i64(arr: *XiStrArray, idx: i64) callconv(.c) bool {
    if (idx < 0 or @as(usize, @intCast(idx)) >= arr.len) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return false;
    }
    const elem_bytes = xi_arr_packed_elem_bytes(arr);
    const src = arr.packed_nums.? + @as(usize, @intCast(idx)) * elem_bytes;
    return xi_packed_read_signed_i64(src, elem_bytes) != 0;
}

export fn xi_arr_set_num_i64_at_i64_unchecked(arr: *XiStrArray, idx: i64, value: i64) callconv(.c) void {
    const pos: usize = @intCast(idx);
    xi_arr_prepare_packed_i64(arr, value, 0, pos + 1);
    if (arr.packed_nums == null) {
        xi_arr_store_value(arr, pos, valNum(xi_num_make_int(value)), XI_VALUE_NUM);
        return;
    }
    if (pos >= arr.len) arr.len = pos + 1;
    const elem_bytes = xi_arr_packed_elem_bytes(arr);
    xi_i64_write_pack_bytes(value, arr.packed_nums.? + pos * elem_bytes, elem_bytes, 0);
}

export fn xi_arr_set_uint_i64_at_i64_unchecked(arr: *XiStrArray, idx: i64, value: i64) callconv(.c) void {
    const pos: usize = @intCast(idx);
    xi_arr_prepare_packed_i64(arr, value, 1, pos + 1);
    if (arr.packed_nums == null) {
        xi_arr_store_value(arr, pos, valNum(xi_num_make_int(if (value < 0) 0 else value)), XI_VALUE_NUM);
        return;
    }
    if (pos >= arr.len) arr.len = pos + 1;
    const elem_bytes = xi_arr_packed_elem_bytes(arr);
    xi_i64_write_pack_bytes(value, arr.packed_nums.? + pos * elem_bytes, elem_bytes, 1);
}

export fn xi_arr_set_str(arr: ?*XiStrArray, idx: ?*XiNum, value: OptStr) callconv(.c) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    xi_arr_materialize_packed_nums(a);
    if (value == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
    const i = xi_num_to_i64(idx);
    if (i < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return;
    }
    xi_arr_store_value(a, @intCast(i), valStr(if (value != null) value else xi_str_intern_cstr("")), XI_VALUE_STR);
}

export fn xi_arr_set_bool(arr: ?*XiStrArray, idx: ?*XiNum, value: bool) callconv(.c) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    xi_arr_materialize_packed_nums(a);
    const i = xi_num_to_i64(idx);
    if (i < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return;
    }
    xi_arr_store_value(a, @intCast(i), valBool(value), XI_VALUE_BOOL);
}

export fn xi_arr_set_struct(arr: ?*XiStrArray, idx: ?*XiNum, value: ?*XiStruct) callconv(.c) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    xi_arr_materialize_packed_nums(a);
    if (value == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "struct value was null");
    const i = xi_num_to_i64(idx);
    if (i < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return;
    }
    xi_arr_store_value(a, @intCast(i), valStruct(value), XI_VALUE_STRUCT);
}

export fn xi_arr_set_raw(arr: ?*XiStrArray, idx: ?*XiNum, value: ?*anyopaque) callconv(.c) void {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return;
    };
    xi_arr_materialize_packed_nums(a);
    const i = xi_num_to_i64(idx);
    if (i < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array index out of range");
        return;
    }
    xi_arr_store_value(a, @intCast(i), valRaw(value), XI_VALUE_OPAQUE);
}

fn xi_arr_clamp_start(start: ?*XiNum, len: usize) usize {
    const raw = xi_num_to_i64(start);
    if (raw < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array slice start out of range");
        return 0;
    }
    const pos: usize = @intCast(raw);
    if (pos > len) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array slice start out of range");
        return len;
    }
    return pos;
}

fn xi_arr_clamp_take(take: ?*XiNum, max_take: usize) usize {
    const raw = xi_num_to_i64(take);
    if (raw <= 0) {
        if (raw < 0) xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array slice length out of range");
        return 0;
    }
    const out: usize = @intCast(raw);
    if (out > max_take) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "array slice length out of range");
        return max_take;
    }
    return out;
}

fn xi_arr_slice_copy(arr: ?*XiStrArray, begin: usize, take_in: usize) *XiStrArray {
    const out = xi_arr_new();
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return out;
    };
    var take = take_in;
    if (take == 0 or begin >= a.len) return out;
    const remaining = a.len - begin;
    if (take > remaining) take = remaining;
    if (take == 0) return out;
    if (a.packed_nums) |pn| {
        const elem_bytes = xi_arr_packed_elem_bytes(a);
        out.packed_bits = a.packed_bits;
        out.packed_unsigned = a.packed_unsigned;
        xi_arr_ensure_packed_capacity(out, take);
        cmemcpy(out.packed_nums.?, pn + begin * elem_bytes, take * elem_bytes);
        out.len = take;
        return out;
    }
    xi_arr_ensure_capacity(out, take);
    cmemcpy(out.items.?, a.items.? + begin, take * @sizeOf(XiValue));
    out.len = take;
    return out;
}

export fn xi_arr_slice_num(arr: ?*XiStrArray, start: ?*XiNum, len: ?*XiNum) callconv(.c) *XiStrArray {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_arr_new();
    };
    const begin = xi_arr_clamp_start(start, a.len);
    const take = xi_arr_clamp_take(len, a.len - begin);
    return xi_arr_slice_copy(a, begin, take);
}

fn xi_arr_slice_materialized(arr: ?*XiStrArray, start: ?*XiNum, len: ?*XiNum) *XiStrArray {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_arr_new();
    };
    xi_arr_materialize_packed_nums(a);
    const begin = xi_arr_clamp_start(start, a.len);
    const take = xi_arr_clamp_take(len, a.len - begin);
    return xi_arr_slice_copy(a, begin, take);
}

export fn xi_arr_slice_str(arr: ?*XiStrArray, start: ?*XiNum, len: ?*XiNum) callconv(.c) *XiStrArray {
    return xi_arr_slice_materialized(arr, start, len);
}
export fn xi_arr_slice_bool(arr: ?*XiStrArray, start: ?*XiNum, len: ?*XiNum) callconv(.c) *XiStrArray {
    return xi_arr_slice_materialized(arr, start, len);
}
export fn xi_arr_slice_struct(arr: ?*XiStrArray, start: ?*XiNum, len: ?*XiNum) callconv(.c) *XiStrArray {
    return xi_arr_slice_materialized(arr, start, len);
}
export fn xi_arr_slice_raw(arr: ?*XiStrArray, start: ?*XiNum, len: ?*XiNum) callconv(.c) *XiStrArray {
    return xi_arr_slice_materialized(arr, start, len);
}

export fn xi_arr_slice_from_num(arr: ?*XiStrArray, start: ?*XiNum) callconv(.c) *XiStrArray {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_arr_new();
    };
    const begin = xi_arr_clamp_start(start, a.len);
    return xi_arr_slice_copy(a, begin, a.len - begin);
}

fn xi_arr_slice_from_materialized(arr: ?*XiStrArray, start: ?*XiNum) *XiStrArray {
    const a = arr orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "array value was null");
        return xi_arr_new();
    };
    xi_arr_materialize_packed_nums(a);
    const begin = xi_arr_clamp_start(start, a.len);
    return xi_arr_slice_copy(a, begin, a.len - begin);
}

export fn xi_arr_slice_from_str(arr: ?*XiStrArray, start: ?*XiNum) callconv(.c) *XiStrArray {
    return xi_arr_slice_from_materialized(arr, start);
}
export fn xi_arr_slice_from_bool(arr: ?*XiStrArray, start: ?*XiNum) callconv(.c) *XiStrArray {
    return xi_arr_slice_from_materialized(arr, start);
}
export fn xi_arr_slice_from_struct(arr: ?*XiStrArray, start: ?*XiNum) callconv(.c) *XiStrArray {
    return xi_arr_slice_from_materialized(arr, start);
}
export fn xi_arr_slice_from_raw(arr: ?*XiStrArray, start: ?*XiNum) callconv(.c) *XiStrArray {
    return xi_arr_slice_from_materialized(arr, start);
}

export fn xi_arr_push(arr: ?*XiStrArray, value: OptStr) callconv(.c) void {
    xi_arr_push_str(arr, value);
}
export fn xi_arr_get(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) Str {
    return xi_arr_get_str(arr, idx);
}
export fn xi_arr_set(arr: ?*XiStrArray, idx: ?*XiNum, value: OptStr) callconv(.c) void {
    xi_arr_set_str(arr, idx, value);
}

export fn xi_mem_copy(dst: ?*XiStrArray, dst_off: ?*XiNum, src: ?*XiStrArray, src_off: ?*XiNum, count: ?*XiNum) callconv(.c) void {
    if (dst == null or src == null) return;
    const d = xi_num_to_i64(dst_off);
    const s = xi_num_to_i64(src_off);
    const n = xi_num_to_i64(count);
    if (n <= 0 or d < 0 or s < 0) return;
    if (ptrEq(dst, src) and d > s) {
        var i: i64 = n - 1;
        while (i >= 0) : (i -= 1) {
            xi_arr_set_num_at_i64(dst, d + i, xi_arr_get_num_at_i64(src, s + i));
        }
    } else {
        var i: i64 = 0;
        while (i < n) : (i += 1) {
            xi_arr_set_num_at_i64(dst, d + i, xi_arr_get_num_at_i64(src, s + i));
        }
    }
}

export fn xi_mem_set(buf: ?*XiStrArray, off: ?*XiNum, value: ?*XiNum, count: ?*XiNum) callconv(.c) void {
    if (buf == null) return;
    const o = xi_num_to_i64(off);
    const n = xi_num_to_i64(count);
    if (n <= 0 or o < 0) return;
    var i: i64 = 0;
    while (i < n) : (i += 1) xi_arr_set_num_at_i64(buf, o + i, value);
}

export fn xi_mem_cmp(a: ?*XiStrArray, a_off: ?*XiNum, b: ?*XiStrArray, b_off: ?*XiNum, count: ?*XiNum) callconv(.c) *XiNum {
    const ao = xi_num_to_i64(a_off);
    const bo = xi_num_to_i64(b_off);
    const n = xi_num_to_i64(count);
    if (a == null or b == null or n <= 0 or ao < 0 or bo < 0) return xi_num_make_int(0);
    var i: i64 = 0;
    while (i < n) : (i += 1) {
        const av = xi_arr_get_num_at_i64(a, ao + i);
        const bv = xi_arr_get_num_at_i64(b, bo + i);
        if (xi_num_lt(av, bv)) return xi_num_make_int(-1);
        if (xi_num_lt(bv, av)) return xi_num_make_int(1);
    }
    return xi_num_make_int(0);
}

fn xi_mem_byte_at(buf: ?*XiStrArray, idx: i64) i64 {
    return xi_num_to_i64(xi_arr_get_num_at_i64(buf, idx)) & 0xFF;
}

export fn xi_mem_read_be(buf: ?*XiStrArray, off: ?*XiNum, nbytes: ?*XiNum) callconv(.c) *XiNum {
    const o = xi_num_to_i64(off);
    const n = xi_num_to_i64(nbytes);
    if (buf == null or n <= 0 or o < 0) return xi_num_make_int(0);
    var acc = xi_num_make_int(0);
    const base = xi_num_make_int(256);
    var i: i64 = 0;
    while (i < n) : (i += 1) {
        acc = xi_num_add(xi_num_mul(acc, base), xi_num_make_int(xi_mem_byte_at(buf, o + i)));
    }
    return acc;
}

export fn xi_mem_read_le(buf: ?*XiStrArray, off: ?*XiNum, nbytes: ?*XiNum) callconv(.c) *XiNum {
    const o = xi_num_to_i64(off);
    const n = xi_num_to_i64(nbytes);
    if (buf == null or n <= 0 or o < 0) return xi_num_make_int(0);
    var acc = xi_num_make_int(0);
    const base = xi_num_make_int(256);
    var i: i64 = n - 1;
    while (i >= 0) : (i -= 1) {
        acc = xi_num_add(xi_num_mul(acc, base), xi_num_make_int(xi_mem_byte_at(buf, o + i)));
    }
    return acc;
}

export fn xi_mem_write_be(buf: ?*XiStrArray, off: ?*XiNum, value: ?*XiNum, nbytes: ?*XiNum) callconv(.c) void {
    const o = xi_num_to_i64(off);
    const n = xi_num_to_i64(nbytes);
    if (buf == null or n <= 0 or o < 0) return;
    var i: i64 = 0;
    while (i < n) : (i += 1) {
        const shift = xi_num_make_int(8 * (n - 1 - i));
        const byte = xi_num_to_i64(xi_bit_shr(value.?, shift)) & 0xFF;
        xi_arr_set_num_at_i64(buf, o + i, xi_num_make_int(byte));
    }
}

export fn xi_mem_write_le(buf: ?*XiStrArray, off: ?*XiNum, value: ?*XiNum, nbytes: ?*XiNum) callconv(.c) void {
    const o = xi_num_to_i64(off);
    const n = xi_num_to_i64(nbytes);
    if (buf == null or n <= 0 or o < 0) return;
    var i: i64 = 0;
    while (i < n) : (i += 1) {
        const shift = xi_num_make_int(8 * i);
        const byte = xi_num_to_i64(xi_bit_shr(value.?, shift)) & 0xFF;
        xi_arr_set_num_at_i64(buf, o + i, xi_num_make_int(byte));
    }
}

export fn xi_cptr_alloc(nbytes: i64) callconv(.c) ?*anyopaque {
    if (nbytes <= 0) return null;
    const n: usize = @intCast(nbytes);
    const p = malloc(n) orelse xi_oom();
    cmemset(@as([*]u8, @ptrCast(p)), 0, n);
    return p;
}

export fn xi_cptr_free(p: ?*anyopaque) callconv(.c) void {
    if (p) |pp| free(pp);
}

export fn xi_cptr_read_cstr(p: ?*anyopaque, off: i64) callconv(.c) Str {
    if (p == null or off < 0) return xi_str_intern_bytes(empty_cstr, 0);
    const start: [*]const u8 = @as([*]const u8, @ptrCast(p.?)) + @as(usize, @intCast(off));
    var len: usize = 0;
    while (start[len] != 0) : (len += 1) {}
    return xi_str_intern_bytes(start, len);
}

export fn xi_cptr_write_cstr(p: ?*anyopaque, off: i64, s: OptStr) callconv(.c) i64 {
    if (p == null or off < 0) return 0;
    const ss = s orelse return 0;
    const len = xi_str_len_known(ss);
    const dst: [*]u8 = @as([*]u8, @ptrCast(p.?)) + @as(usize, @intCast(off));
    cmemcpy(dst, ss, len);
    dst[len] = 0;
    return @intCast(len);
}

const XI_NS_PER_US: i64 = 1000;
const XI_NS_PER_MS: i64 = 1000 * XI_NS_PER_US;
const XI_NS_PER_S: i64 = 1000 * XI_NS_PER_MS;
const XI_CLOCKS_PER_SEC: i64 = 1000;
const XI_CLOCK_MONOTONIC: c_int = 1;

const XiDateTime = struct {
    year: i64,
    month: i64,
    day: i64,
    hour: i64,
    minute: i64,
    second: i64,
    weekday: i64,
    yearday: i64,
};

export fn xi_num_i64(v: i64) callconv(.c) *XiNum {
    return xi_num_make_int(v);
}

export fn xi_wall_time_ns() callconv(.c) i64 {
    if (builtin.os.tag == .windows) {
        var ft: XiFileTime = undefined;
        GetSystemTimeAsFileTime(&ft);
        const raw = (@as(u64, ft.dwHighDateTime) << 32) | @as(u64, ft.dwLowDateTime);
        const ns_since_1601: u128 = @as(u128, raw) * 100;
        const unix_offset_ns: u128 = 11644473600 * @as(u128, XI_NS_PER_S);
        if (ns_since_1601 <= unix_offset_ns) return 0;
        return @intCast(ns_since_1601 - unix_offset_ns);
    }
    var ts: XiTimespec = undefined;
    if (clock_gettime(0, &ts) == 0) return @as(i64, @intCast(ts.tv_sec)) * XI_NS_PER_S + @as(i64, @intCast(ts.tv_nsec));
    return @as(i64, @intCast(time(null))) * XI_NS_PER_S;
}

export fn xi_mono_ns_i64() callconv(.c) i64 {
    if (builtin.os.tag == .windows) {
        var freq: i64 = 0;
        var count: i64 = 0;
        if (QueryPerformanceFrequency(&freq) == 0 or QueryPerformanceCounter(&count) == 0 or freq <= 0) {
            return @as(i64, @intCast(clock())) * @divTrunc(XI_NS_PER_S, XI_CLOCKS_PER_SEC);
        }
        return @intCast(@divTrunc(@as(i128, count) * @as(i128, XI_NS_PER_S), @as(i128, freq)));
    }
    var ts: XiTimespec = undefined;
    if (clock_gettime(XI_CLOCK_MONOTONIC, &ts) == 0) return @as(i64, @intCast(ts.tv_sec)) * XI_NS_PER_S + @as(i64, @intCast(ts.tv_nsec));
    return @as(i64, @intCast(clock())) * @divTrunc(XI_NS_PER_S, XI_CLOCKS_PER_SEC);
}

export fn xi_append_bytes(buf: [*]u8, cap: usize, pos: *usize, bytes: [*]const u8, len: usize) callconv(.c) void {
    var i: usize = 0;
    while (i < len and pos.* + 1 < cap) : (i += 1) {
        buf[pos.*] = bytes[i];
        pos.* += 1;
    }
}

export fn xi_append_cstr(buf: [*]u8, cap: usize, pos: *usize, s: [*:0]const u8) callconv(.c) void {
    xi_append_bytes(buf, cap, pos, s, strlen(s));
}

export fn xi_str_len(s: OptStr) callconv(.c) *XiNum {
    return xi_num_make_int(xi_str_len_i64(s));
}

export fn xi_str_len_i64(s: OptStr) callconv(.c) i64 {
    if (s == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return 0;
    }
    const len = xi_str_len_known(s);
    if (len > @as(usize, @bitCast(INT64_MAX))) return INT64_MAX;
    return @intCast(len);
}

export fn xi_str_concat(a: OptStr, b: OptStr) callconv(.c) Str {
    if (a == null or b == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return xi_str_intern_cstr("");
    }
    const lhs = a.?;
    const rhs = b.?;
    const lhs_len = xi_str_len_known(lhs);
    const rhs_len = xi_str_len_known(rhs);
    if (lhs_len == 0) return rhs;
    if (rhs_len == 0) return lhs;
    if (lhs_len > SIZE_MAX - rhs_len - 1) fatal("xi_runtime: string concat overflow\n");

    const out_len = lhs_len + rhs_len;
    xi_runtime_gc_ensure_init();
    xi_str_intern_ensure_capacity(xi_str_intern_used + 1);

    var hash: u64 = 1469598103934665603;
    hash = xi_str_hash_update(hash, lhs, lhs_len);
    hash = xi_str_hash_update(hash, rhs, rhs_len);
    if (hash == 0) hash = 1;

    const bucket: usize = @intCast(hash & @as(u64, xi_str_intern_cap - 1));
    var entry = xi_str_intern_buckets.?[bucket];
    while (entry) |en| : (entry = en.bucket_next) {
        if (en.hash == hash and en.len == out_len) {
            if (cmemcmp(en.value, lhs, lhs_len) == 0 and cmemcmp(en.value + lhs_len, rhs, rhs_len) == 0) {
                return @ptrCast(en.value);
            }
        }
    }

    const out: [*]u8 = @ptrCast(malloc(out_len + 1) orelse xi_oom());
    cmemcpy(out, lhs, lhs_len);
    cmemcpy(out + lhs_len, rhs, rhs_len);
    out[out_len] = 0;

    const node = cnew(XiStrInternEntry);
    node.value = out;
    node.len = out_len;
    node.hash = hash;
    node.kind = XI_STR_KIND_INTERNED;
    node.owner = null;
    node.gc_mark = 0;
    node.bucket_next = xi_str_intern_buckets.?[bucket];
    node.gc_next = xi_gc_str_head;
    xi_gc_str_head = node;
    xi_str_intern_buckets.?[bucket] = node;
    xi_str_intern_used += 1;
    xi_gc_note_allocation();
    return @ptrCast(out);
}

export fn xi_str_substr(s: OptStr, start: ?*XiNum, len: ?*XiNum) callconv(.c) Str {
    const input = s orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return xi_str_intern_cstr("");
    };
    var start_i = xi_num_to_i64(start);
    var len_i = xi_num_to_i64(len);
    var out_of_range = false;
    if (start_i < 0) {
        start_i = 0;
        out_of_range = true;
    }
    if (len_i < 0) {
        len_i = 0;
        out_of_range = true;
    }
    const src_len = xi_str_len_known(input);
    var begin: usize = @intCast(start_i);
    if (begin > src_len) {
        begin = src_len;
        out_of_range = true;
    }
    var take: usize = @intCast(len_i);
    if (take > src_len - begin) {
        take = src_len - begin;
        out_of_range = true;
    }
    if (out_of_range) xi_err_set_message(XI_ERR_OUT_OF_RANGE, "substring range out of bounds");
    if (take == 0) return xi_str_intern_cstr("");
    if (begin == 0 and take == src_len) return input;
    return xi_str_copy_range(input, begin, take);
}

export fn xi_str_char_code_at(s: OptStr, idx: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_make_int(xi_str_char_code_at_i64(s, xi_num_to_i64(idx)));
}

export fn xi_str_char_code_at_i64(s: OptStr, pos: i64) callconv(.c) i64 {
    const ss = s orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return -1;
    };
    if (pos < 0) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "string index out of range");
        return -1;
    }
    const len = xi_str_len_known(ss);
    const index: usize = @intCast(pos);
    if (index >= len) {
        xi_err_set_message(XI_ERR_OUT_OF_RANGE, "string index out of range");
        return -1;
    }
    return @as(i64, ss[index]);
}

export fn xi_str_from_num(n: ?*XiNum) callconv(.c) Str {
    var buf: [64]u8 = undefined;
    const nn = n orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "numeric value was null");
        return xi_str_dup("0");
    };
    if (nn.tag == XI_NUM_BIG) {
        const s = xi_big_to_decimal(nn.big.?);
        const interned = xi_str_intern_cstr(@ptrCast(s));
        cfree(s);
        return interned;
    }
    if (nn.tag == XI_NUM_BIGDEC) {
        const s = xi_bigdec_to_decimal(nn.big.?, nn.dec_exp);
        const interned = xi_str_intern_cstr(@ptrCast(s));
        cfree(s);
        return interned;
    }
    if (nn.tag == XI_NUM_SOFTFLOAT) {
        const s = xi_soft_to_string(nn);
        const interned = xi_str_intern_cstr(@ptrCast(s));
        cfree(s);
        return interned;
    }
    if (nn.tag == XI_NUM_WIDE) {
        const t = xi_big_from_wide(&nn.wide, nn.wide_bits / 8, 1);
        const s = xi_big_to_decimal(t);
        xi_big_free(t);
        const interned = xi_str_intern_cstr(@ptrCast(s));
        cfree(s);
        return interned;
    }
    if (nn.tag == XI_NUM_DEC) {
        xi_fmt_float_shortest(&buf, buf.len, nn.d, 64);
    } else {
        _ = snprintf(&buf, buf.len, "%lld", @as(c_longlong, nn.i));
    }
    return xi_str_intern_cstr(@ptrCast(&buf));
}

export fn xi_str_from_char_code(n: ?*XiNum) callconv(.c) Str {
    if (n == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "numeric value was null");
        return xi_str_intern_cstr("");
    }
    return xi_str_from_char_code_i64(xi_num_to_i64(n));
}

export fn xi_str_from_char_code_i64(code: i64) callconv(.c) Str {
    if (code < 0 or code > 255) xi_err_set_message(XI_ERR_OUT_OF_RANGE, "character code out of byte range");
    var ch: u8 = @truncate(@as(u64, @bitCast(code)) & 0xff);
    return xi_str_intern_bytes(@ptrCast(&ch), 1);
}

export fn xi_str_eq(a: OptStr, b: OptStr) callconv(.c) bool {
    if (a == null or b == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return false;
    }
    if (strAddr(a) == strAddr(b)) return true;
    return xi_str_equal_known(a, b) == 1;
}

fn xi_mem_find_bytes(haystack: [*]const u8, hay_len: usize, needle: [*]const u8, needle_len: usize) ?[*]const u8 {
    if (needle_len == 0) return haystack;
    if (needle_len > hay_len) return null;
    const limit = hay_len - needle_len;
    var i: usize = 0;
    while (i <= limit) : (i += 1) {
        if (haystack[i] == needle[0] and cmemcmp(haystack + i, needle, needle_len) == 0) return haystack + i;
    }
    return null;
}

fn xi_ascii_space(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == '\x0c' or c == '\x0b';
}

export fn xi_str_find(s: OptStr, needle: OptStr) callconv(.c) *XiNum {
    if (s == null or needle == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return xi_num_make_int(-1);
    }
    const s_len = xi_str_len_known(s);
    const needle_len = xi_str_len_known(needle);
    const found = xi_mem_find_bytes(s.?, s_len, needle.?, needle_len) orelse return xi_num_make_int(-1);
    const idx = @intFromPtr(found) - strAddr(s);
    if (idx > @as(usize, @bitCast(INT64_MAX))) return xi_num_make_int(INT64_MAX);
    return xi_num_make_int(@intCast(idx));
}

export fn xi_str_contains(s: OptStr, needle: OptStr) callconv(.c) bool {
    if (s == null or needle == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return false;
    }
    return xi_mem_find_bytes(s.?, xi_str_len_known(s), needle.?, xi_str_len_known(needle)) != null;
}

export fn xi_str_starts_with(s: OptStr, prefix: OptStr) callconv(.c) bool {
    if (s == null or prefix == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return false;
    }
    const s_len = xi_str_len_known(s);
    const prefix_len = xi_str_len_known(prefix);
    return prefix_len <= s_len and cmemcmp(s.?, prefix.?, prefix_len) == 0;
}

export fn xi_str_ends_with(s: OptStr, suffix: OptStr) callconv(.c) bool {
    if (s == null or suffix == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return false;
    }
    const s_len = xi_str_len_known(s);
    const suffix_len = xi_str_len_known(suffix);
    return suffix_len <= s_len and cmemcmp(s.? + (s_len - suffix_len), suffix.?, suffix_len) == 0;
}

fn xi_str_trim_range(s: OptStr, trim_left: bool, trim_right: bool) Str {
    const ss = s orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return xi_str_intern_cstr("");
    };
    const len = xi_str_len_known(ss);
    var begin: usize = 0;
    var end = len;
    if (trim_left) {
        while (begin < end and xi_ascii_space(ss[begin])) begin += 1;
    }
    if (trim_right) {
        while (end > begin and xi_ascii_space(ss[end - 1])) end -= 1;
    }
    if (begin == 0 and end == len) return ss;
    return xi_str_copy_range(ss, begin, end - begin);
}

export fn xi_str_trim(s: OptStr) callconv(.c) Str {
    return xi_str_trim_range(s, true, true);
}
export fn xi_str_trim_start(s: OptStr) callconv(.c) Str {
    return xi_str_trim_range(s, true, false);
}
export fn xi_str_trim_end(s: OptStr) callconv(.c) Str {
    return xi_str_trim_range(s, false, true);
}

export fn xi_str_index_of_char(s: OptStr, code: ?*XiNum) callconv(.c) *XiNum {
    const ss = s orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return xi_num_make_int(-1);
    };
    if (code == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "numeric value was null");
        return xi_num_make_int(-1);
    }
    const raw = xi_num_to_i64(code);
    if (raw < 0 or raw > 255) xi_err_set_message(XI_ERR_OUT_OF_RANGE, "character code out of byte range");
    const target: u8 = @truncate(@as(u64, @bitCast(raw)) & 0xff);
    const len = xi_str_len_known(ss);
    var i: usize = 0;
    while (i < len) : (i += 1) {
        if (ss[i] == target) {
            if (i > @as(usize, @bitCast(INT64_MAX))) return xi_num_make_int(INT64_MAX);
            return xi_num_make_int(@intCast(i));
        }
    }
    return xi_num_make_int(-1);
}

fn xi_str_count_occurrences(s: [*]const u8, s_len: usize, needle: [*]const u8, needle_len: usize) i64 {
    if (needle_len == 0 or needle_len > s_len) return 0;
    var count: i64 = 0;
    var pos: usize = 0;
    while (pos <= s_len - needle_len) {
        const found = xi_mem_find_bytes(s + pos, s_len - pos, needle, needle_len) orelse break;
        count += 1;
        if (count == INT64_MAX) return INT64_MAX;
        pos = (@intFromPtr(found) - @intFromPtr(s)) + needle_len;
    }
    return count;
}

export fn xi_str_count(s: OptStr, needle: OptStr) callconv(.c) *XiNum {
    if (s == null or needle == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return xi_num_make_int(0);
    }
    const s_len = xi_str_len_known(s);
    const needle_len = xi_str_len_known(needle);
    return xi_num_make_int(xi_str_count_occurrences(s.?, s_len, needle.?, needle_len));
}

export fn xi_str_replace(s: OptStr, old: OptStr, repl: OptStr) callconv(.c) Str {
    if (s == null or old == null or repl == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return xi_str_intern_cstr("");
    }
    const s_len = xi_str_len_known(s);
    const old_len = xi_str_len_known(old);
    const repl_len = xi_str_len_known(repl);
    if (old_len == 0 or old_len > s_len) return s.?;

    const count_num = xi_str_count_occurrences(s.?, s_len, old.?, old_len);
    if (count_num <= 0) return s.?;
    const count: usize = @intCast(count_num);
    var out_len = s_len;
    if (repl_len >= old_len) {
        const grow = repl_len - old_len;
        if (grow != 0 and count > (SIZE_MAX - out_len) / grow) fatal("xi_runtime: string replace overflow\n");
        out_len += count * grow;
    } else {
        out_len -= count * (old_len - repl_len);
    }

    const out: [*]u8 = @ptrCast(malloc(out_len + 1) orelse xi_oom());
    var src: usize = 0;
    var dst: usize = 0;
    while (src <= s_len - old_len) {
        const found = xi_mem_find_bytes(s.? + src, s_len - src, old.?, old_len) orelse break;
        const idx = @intFromPtr(found) - strAddr(s);
        const chunk = idx - src;
        cmemcpy(out + dst, s.? + src, chunk);
        dst += chunk;
        cmemcpy(out + dst, repl.?, repl_len);
        dst += repl_len;
        src = idx + old_len;
    }
    cmemcpy(out + dst, s.? + src, s_len - src);
    dst += s_len - src;
    out[dst] = 0;

    const interned = xi_str_intern_bytes(out, out_len);
    cfree(out);
    return interned;
}

export fn xi_str_repeat(s: OptStr, count: ?*XiNum) callconv(.c) Str {
    const ss = s orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        return xi_str_intern_cstr("");
    };
    if (count == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "numeric value was null");
        return xi_str_intern_cstr("");
    }
    const n = xi_num_to_i64(count);
    if (n <= 0) {
        if (n < 0) xi_err_set_message(XI_ERR_OUT_OF_RANGE, "repeat count out of range");
        return xi_str_intern_cstr("");
    }
    const len = xi_str_len_known(ss);
    if (len == 0) return ss;
    if (@as(u64, @bitCast(n)) > @as(u64, SIZE_MAX / len)) fatal("xi_runtime: string repeat overflow\n");
    const out_len = len * @as(usize, @intCast(n));
    const out: [*]u8 = @ptrCast(malloc(out_len + 1) orelse xi_oom());
    var dst: usize = 0;
    var i: i64 = 0;
    while (i < n) : (i += 1) {
        cmemcpy(out + dst, ss, len);
        dst += len;
    }
    out[out_len] = 0;
    const interned = xi_str_intern_bytes(out, out_len);
    cfree(out);
    return interned;
}

export fn xi_map_new() callconv(.c) *XiMap {
    xi_runtime_gc_ensure_init();
    const map = cnew(XiMap);
    map.len = 0;
    map.used = 0;
    map.cap = 0;
    map.keys = null;
    map.values = null;
    map.hashes = null;
    map.states = null;
    map.gc_mark = 0;
    map.gc_next = xi_gc_map_head;
    xi_gc_map_head = map;
    xi_gc_note_allocation();
    return map;
}

export fn xi_map_key_num(key: ?*XiNum) callconv(.c) Str {
    return xi_str_from_num(key);
}

export fn xi_map_key_bool(key: bool) callconv(.c) Str {
    return xi_str_intern_cstr(if (key) "true" else "false");
}

fn xi_map_hash_key(key: OptStr) u64 {
    const stable: OptStr = if (key) |k| k else empty_cstr;
    const len = xi_str_len_known(stable);
    const hash = xi_str_hash_bytes(stable, len);
    if (hash == 0) return 1;
    return hash;
}

fn xi_map_round_pow2(min_cap: usize) usize {
    var cap: usize = 16;
    while (cap < min_cap) {
        if (cap > SIZE_MAX / 2) fatal("xi_runtime: map capacity overflow\n");
        cap *= 2;
    }
    return cap;
}

fn xi_map_probe_start(hash: u64, cap: usize) usize {
    return @intCast(hash & @as(u64, cap - 1));
}

fn xi_map_probe_distance(idx: usize, hash: u64, cap: usize) usize {
    const start = xi_map_probe_start(hash, cap);
    if (idx >= start) return idx - start;
    return cap - start + idx;
}

fn xi_map_find_index(map: *XiMap, key: OptStr, hash: u64) usize {
    if (map.cap == 0) return SIZE_MAX;
    const probe: OptStr = if (key) |k| k else empty_cstr;
    var idx = xi_map_probe_start(hash, map.cap);
    var dist: usize = 0;
    var step: usize = 0;
    while (step < map.cap) : (step += 1) {
        const state = map.states.?[idx];
        if (state == 0) return SIZE_MAX;
        if (state == 1 and map.hashes.?[idx] == hash) {
            const existing: OptStr = if (map.keys.?[idx]) |k| k else empty_cstr;
            if (xi_str_equal_known(existing, probe) == 1) return idx;
        }
        if (state == 1) {
            const occupant_dist = xi_map_probe_distance(idx, map.hashes.?[idx], map.cap);
            if (occupant_dist < dist) return SIZE_MAX;
        }
        idx = (idx + 1) & (map.cap - 1);
        dist += 1;
    }
    return SIZE_MAX;
}

fn xi_map_insert_entry(map: *XiMap, key: OptStr, hash: u64, value: XiValue) void {
    if (map.cap == 0) return;
    var cur_key: OptStr = if (key) |k| k else empty_cstr;
    var cur_value = value;
    var cur_hash = hash;
    var idx = xi_map_probe_start(cur_hash, map.cap);
    var dist: usize = 0;
    var step: usize = 0;
    while (step < map.cap) : (step += 1) {
        const state = map.states.?[idx];
        if (state == 0) {
            map.states.?[idx] = 1;
            map.keys.?[idx] = cur_key;
            map.values.?[idx] = cur_value;
            map.hashes.?[idx] = cur_hash;
            map.len += 1;
            map.used += 1;
            return;
        }
        if (state == 1 and map.hashes.?[idx] == cur_hash) {
            const existing: OptStr = if (map.keys.?[idx]) |k| k else empty_cstr;
            if (xi_str_equal_known(existing, cur_key) == 1) {
                if (map.values.?[idx].tag != XI_VALUE_NULL and map.values.?[idx].tag != cur_value.tag) {
                    xi_err_set(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
                }
                map.values.?[idx] = cur_value;
                return;
            }
        }
        const occupant_dist = xi_map_probe_distance(idx, map.hashes.?[idx], map.cap);
        if (occupant_dist < dist) {
            const tmp_key = map.keys.?[idx];
            const tmp_value = map.values.?[idx];
            const tmp_hash = map.hashes.?[idx];
            map.keys.?[idx] = cur_key;
            map.values.?[idx] = cur_value;
            map.hashes.?[idx] = cur_hash;
            cur_key = tmp_key;
            cur_value = tmp_value;
            cur_hash = tmp_hash;
            dist = occupant_dist;
        }
        idx = (idx + 1) & (map.cap - 1);
        dist += 1;
    }
    fatal("xi_runtime: internal map insert failure\n");
}

fn xi_map_rehash(map: *XiMap, min_cap: usize) void {
    const new_cap = xi_map_round_pow2(min_cap);
    const new_keys: [*]OptStr = @ptrCast(@alignCast(calloc(new_cap, @sizeOf(OptStr)) orelse xi_oom()));
    const new_values: [*]XiValue = @ptrCast(@alignCast(calloc(new_cap, @sizeOf(XiValue)) orelse xi_oom()));
    const new_hashes: [*]u64 = @ptrCast(@alignCast(calloc(new_cap, @sizeOf(u64)) orelse xi_oom()));
    const new_states: [*]u8 = @ptrCast(calloc(new_cap, @sizeOf(u8)) orelse xi_oom());

    const old_keys = map.keys;
    const old_values = map.values;
    const old_hashes = map.hashes;
    const old_states = map.states;
    const old_cap = map.cap;

    map.keys = new_keys;
    map.values = new_values;
    map.hashes = new_hashes;
    map.states = new_states;
    map.cap = new_cap;
    map.used = 0;
    map.len = 0;

    var i: usize = 0;
    while (i < old_cap) : (i += 1) {
        if (old_states == null or old_states.?[i] != 1) continue;
        xi_map_insert_entry(map, old_keys.?[i], old_hashes.?[i], old_values.?[i]);
    }
    cfree(old_keys);
    cfree(old_values);
    cfree(old_hashes);
    cfree(old_states);
}

fn xi_map_prepare_insert(map: *XiMap) void {
    if (map.cap == 0) {
        xi_map_rehash(map, 16);
        return;
    }
    if ((map.len + 1) * 100 >= map.cap * 70) {
        if (map.cap > SIZE_MAX / 2) fatal("xi_runtime: map capacity overflow\n");
        xi_map_rehash(map, map.cap * 2);
        return;
    }
}

fn xi_map_set_value(map: ?*XiMap, key: OptStr, value_in: XiValue, expected_tag: u8) void {
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return;
    };
    if (key == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map key was null");
        return;
    }
    var value = value_in;
    if (value.tag != expected_tag and value.tag != XI_VALUE_NULL) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
        value = xi_value_default_for_tag(expected_tag);
    } else if (value.tag == XI_VALUE_NULL) {
        value = xi_value_default_for_tag(expected_tag);
    }
    const stable_key: OptStr = if (key) |k| k else empty_cstr;
    const hash = xi_map_hash_key(stable_key);
    xi_map_prepare_insert(m);
    xi_map_insert_entry(m, stable_key, hash, value);
}

export fn xi_map_set_num(map: ?*XiMap, key: OptStr, value: ?*XiNum) callconv(.c) void {
    if (map == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return;
    }
    if (value == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "numeric value was null");
    xi_map_set_value(map, key, valNum(if (value != null) value else xi_num_make_int(0)), XI_VALUE_NUM);
}
export fn xi_map_set_str(map: ?*XiMap, key: OptStr, value: OptStr) callconv(.c) void {
    if (map == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return;
    }
    if (value == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
    xi_map_set_value(map, key, valStr(if (value != null) value else xi_str_intern_cstr("")), XI_VALUE_STR);
}
export fn xi_map_set_bool(map: ?*XiMap, key: OptStr, value: bool) callconv(.c) void {
    if (map == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return;
    }
    xi_map_set_value(map, key, valBool(value), XI_VALUE_BOOL);
}
export fn xi_map_set_struct(map: ?*XiMap, key: OptStr, value: ?*XiStruct) callconv(.c) void {
    if (map == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return;
    }
    if (value == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "struct value was null");
    xi_map_set_value(map, key, valStruct(value), XI_VALUE_STRUCT);
}
export fn xi_map_set_raw(map: ?*XiMap, key: OptStr, value: ?*anyopaque) callconv(.c) void {
    if (map == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return;
    }
    xi_map_set_value(map, key, valRaw(value), XI_VALUE_OPAQUE);
}

fn xi_map_lookup(map: ?*XiMap, key: OptStr) ?*XiValue {
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return null;
    };
    if (key == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map key was null");
        return null;
    }
    const idx = xi_map_find_index(m, key, xi_map_hash_key(key));
    if (idx == SIZE_MAX) {
        xi_err_set_message(XI_ERR_NOT_FOUND, "map key not found");
        return null;
    }
    return &m.values.?[idx];
}

export fn xi_map_get_num(map: ?*XiMap, key: OptStr) callconv(.c) *XiNum {
    const slot = xi_map_lookup(map, key) orelse return xi_num_make_int(0);
    if (slot.tag != XI_VALUE_NUM or valAsNum(slot.*) == null) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
        return xi_num_make_int(0);
    }
    return valAsNum(slot.*).?;
}
export fn xi_map_get_str(map: ?*XiMap, key: OptStr) callconv(.c) Str {
    const slot = xi_map_lookup(map, key) orelse return xi_str_intern_cstr("");
    if (slot.tag != XI_VALUE_STR or valAsStr(slot.*) == null) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
        return xi_str_intern_cstr("");
    }
    return valAsStr(slot.*).?;
}
export fn xi_map_get_bool(map: ?*XiMap, key: OptStr) callconv(.c) bool {
    const slot = xi_map_lookup(map, key) orelse return false;
    if (slot.tag != XI_VALUE_BOOL) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
        return false;
    }
    return slot.boolean;
}
export fn xi_map_get_struct(map: ?*XiMap, key: OptStr) callconv(.c) ?*XiStruct {
    const slot = xi_map_lookup(map, key) orelse return null;
    if (slot.tag != XI_VALUE_STRUCT) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
        return null;
    }
    return valAsStruct(slot.*);
}
export fn xi_map_get_raw(map: ?*XiMap, key: OptStr) callconv(.c) ?*anyopaque {
    const slot = xi_map_lookup(map, key) orelse return null;
    if (slot.tag != XI_VALUE_OPAQUE) {
        xi_err_set(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
        return null;
    }
    return slot.ptr;
}

export fn xi_map_has(map: ?*XiMap, key: OptStr) callconv(.c) *XiNum {
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return xi_num_make_int(0);
    };
    if (key == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map key was null");
        return xi_num_make_int(0);
    }
    return xi_num_make_int(if (xi_map_find_index(m, key, xi_map_hash_key(key)) == SIZE_MAX) 0 else 1);
}

export fn xi_map_del(map: ?*XiMap, key: OptStr) callconv(.c) void {
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return;
    };
    if (key == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map key was null");
        return;
    }
    if (m.len == 0 or m.cap == 0) {
        xi_err_set_message(XI_ERR_NOT_FOUND, "map key not found");
        return;
    }
    const idx = xi_map_find_index(m, key, xi_map_hash_key(key));
    if (idx == SIZE_MAX) {
        xi_err_set_message(XI_ERR_NOT_FOUND, "map key not found");
        return;
    }
    const mask = m.cap - 1;
    var hole = idx;
    var next = (hole + 1) & mask;
    while (m.states.?[next] == 1) {
        const home = xi_map_probe_start(m.hashes.?[next], m.cap);
        if (home == next) break;
        m.keys.?[hole] = m.keys.?[next];
        m.values.?[hole] = m.values.?[next];
        m.hashes.?[hole] = m.hashes.?[next];
        m.states.?[hole] = 1;
        hole = next;
        next = (next + 1) & mask;
    }
    m.states.?[hole] = 0;
    m.keys.?[hole] = null;
    m.values.?[hole] = valNull();
    m.hashes.?[hole] = 0;
    m.len -= 1;
    if (m.used > 0) m.used -= 1;
    if (m.len == 0) {
        cmemset(m.states.?, 0, m.cap * @sizeOf(u8));
        m.used = 0;
        return;
    }
    if (m.cap > 16 and m.len * 100 <= m.cap * 20) xi_map_rehash(m, m.cap / 2);
}

export fn xi_map_clear(map: ?*XiMap) callconv(.c) void {
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return;
    };
    if (m.cap == 0) {
        m.len = 0;
        m.used = 0;
        return;
    }
    cmemset(m.keys.?, 0, m.cap * @sizeOf(OptStr));
    cmemset(m.values.?, 0, m.cap * @sizeOf(XiValue));
    cmemset(m.hashes.?, 0, m.cap * @sizeOf(u64));
    cmemset(m.states.?, 0, m.cap * @sizeOf(u8));
    m.len = 0;
    m.used = 0;
}

export fn xi_map_len(map: ?*XiMap) callconv(.c) *XiNum {
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return xi_num_make_int(0);
    };
    if (m.len > @as(usize, @bitCast(INT64_MAX))) return xi_num_make_int(INT64_MAX);
    return xi_num_make_int(@intCast(m.len));
}

fn xi_map_lookup_or(map: ?*XiMap, key: OptStr) ?*XiValue {
    const m = map orelse return null;
    if (key == null) return null;
    const idx = xi_map_find_index(m, key, xi_map_hash_key(key));
    if (idx == SIZE_MAX) return null;
    return &m.values.?[idx];
}

export fn xi_map_get_or_num(map: ?*XiMap, key: OptStr, fallback_in: ?*XiNum) callconv(.c) *XiNum {
    var fallback = fallback_in;
    if (fallback == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "numeric value was null");
        fallback = xi_num_make_int(0);
    }
    if (map == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return fallback.?;
    }
    if (key == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map key was null");
        return fallback.?;
    }
    const slot = xi_map_lookup_or(map, key) orelse return fallback.?;
    if (slot.tag != XI_VALUE_NUM or valAsNum(slot.*) == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
        return fallback.?;
    }
    return valAsNum(slot.*).?;
}
export fn xi_map_get_or_str(map: ?*XiMap, key: OptStr, fallback_in: OptStr) callconv(.c) Str {
    var fallback = fallback_in;
    if (fallback == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
        fallback = xi_str_intern_cstr("");
    }
    if (map == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return fallback.?;
    }
    if (key == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map key was null");
        return fallback.?;
    }
    const slot = xi_map_lookup_or(map, key) orelse return fallback.?;
    if (slot.tag != XI_VALUE_STR or valAsStr(slot.*) == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
        return fallback.?;
    }
    return valAsStr(slot.*).?;
}
export fn xi_map_get_or_bool(map: ?*XiMap, key: OptStr, fallback: bool) callconv(.c) bool {
    if (map == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return fallback;
    }
    if (key == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map key was null");
        return fallback;
    }
    const slot = xi_map_lookup_or(map, key) orelse return fallback;
    if (slot.tag != XI_VALUE_BOOL) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
        return fallback;
    }
    return slot.boolean;
}
export fn xi_map_get_or_struct(map: ?*XiMap, key: OptStr, fallback: ?*XiStruct) callconv(.c) ?*XiStruct {
    if (map == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return fallback;
    }
    if (key == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map key was null");
        return fallback;
    }
    const slot = xi_map_lookup_or(map, key) orelse return fallback;
    if (slot.tag != XI_VALUE_STRUCT) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
        return fallback;
    }
    return valAsStruct(slot.*);
}
export fn xi_map_get_or_raw(map: ?*XiMap, key: OptStr, fallback: ?*anyopaque) callconv(.c) ?*anyopaque {
    if (map == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return fallback;
    }
    if (key == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map key was null");
        return fallback;
    }
    const slot = xi_map_lookup_or(map, key) orelse return fallback;
    if (slot.tag != XI_VALUE_OPAQUE) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
        return fallback;
    }
    return slot.ptr;
}

export fn xi_map_keys(map: ?*XiMap) callconv(.c) *XiStrArray {
    const out = xi_arr_new();
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return out;
    };
    var i: usize = 0;
    while (i < m.cap) : (i += 1) {
        if (m.states == null or m.states.?[i] != 1) continue;
        xi_arr_push_str(out, if (m.keys.?[i]) |k| k else xi_str_intern_cstr(""));
    }
    return out;
}

export fn xi_map_keys_num(map: ?*XiMap) callconv(.c) *XiStrArray {
    const out = xi_arr_new();
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return out;
    };
    var i: usize = 0;
    while (i < m.cap) : (i += 1) {
        if (m.states == null or m.states.?[i] != 1) continue;
        xi_arr_push_num(out, xi_num_from_decimal(if (m.keys.?[i]) |k| k else @as(Str, "0")));
    }
    return out;
}

export fn xi_map_keys_dec(map: ?*XiMap) callconv(.c) *XiStrArray {
    const out = xi_arr_new();
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return out;
    };
    var i: usize = 0;
    while (i < m.cap) : (i += 1) {
        if (m.states == null or m.states.?[i] != 1) continue;
        xi_arr_push_num(out, xi_num_dec_from_decimal(if (m.keys.?[i]) |k| k else @as(Str, "0")));
    }
    return out;
}

export fn xi_map_keys_bool(map: ?*XiMap) callconv(.c) *XiStrArray {
    const out = xi_arr_new();
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return out;
    };
    var i: usize = 0;
    while (i < m.cap) : (i += 1) {
        if (m.states == null or m.states.?[i] != 1) continue;
        const key: Str = if (m.keys.?[i]) |k| k else "false";
        xi_arr_push_bool(out, strcmp(key, "true") == 0);
    }
    return out;
}

export fn xi_map_values_num(map: ?*XiMap) callconv(.c) *XiStrArray {
    const out = xi_arr_new();
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return out;
    };
    var i: usize = 0;
    while (i < m.cap) : (i += 1) {
        if (m.states == null or m.states.?[i] != 1) continue;
        const value = m.values.?[i];
        if (value.tag == XI_VALUE_NUM and valAsNum(value) != null) {
            xi_arr_push_num(out, valAsNum(value));
        } else {
            xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
            xi_arr_push_num(out, xi_num_make_int(0));
        }
    }
    return out;
}
export fn xi_map_values_str(map: ?*XiMap) callconv(.c) *XiStrArray {
    const out = xi_arr_new();
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return out;
    };
    var i: usize = 0;
    while (i < m.cap) : (i += 1) {
        if (m.states == null or m.states.?[i] != 1) continue;
        const value = m.values.?[i];
        if (value.tag == XI_VALUE_STR and valAsStr(value) != null) {
            xi_arr_push_str(out, valAsStr(value));
        } else {
            xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
            xi_arr_push_str(out, xi_str_intern_cstr(""));
        }
    }
    return out;
}
export fn xi_map_values_bool(map: ?*XiMap) callconv(.c) *XiStrArray {
    const out = xi_arr_new();
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return out;
    };
    var i: usize = 0;
    while (i < m.cap) : (i += 1) {
        if (m.states == null or m.states.?[i] != 1) continue;
        const value = m.values.?[i];
        if (value.tag == XI_VALUE_BOOL) {
            xi_arr_push_bool(out, value.boolean);
        } else {
            xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
            xi_arr_push_bool(out, false);
        }
    }
    return out;
}
export fn xi_map_values_struct(map: ?*XiMap) callconv(.c) *XiStrArray {
    const out = xi_arr_new();
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return out;
    };
    var i: usize = 0;
    while (i < m.cap) : (i += 1) {
        if (m.states == null or m.states.?[i] != 1) continue;
        const value = m.values.?[i];
        if (value.tag == XI_VALUE_STRUCT) {
            xi_arr_push_struct(out, valAsStruct(value));
        } else {
            xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
            xi_arr_push_struct(out, null);
        }
    }
    return out;
}
export fn xi_map_values_raw(map: ?*XiMap) callconv(.c) *XiStrArray {
    const out = xi_arr_new();
    const m = map orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map value was null");
        return out;
    };
    var i: usize = 0;
    while (i < m.cap) : (i += 1) {
        if (m.states == null or m.states.?[i] != 1) continue;
        const value = m.values.?[i];
        if (value.tag == XI_VALUE_OPAQUE) {
            xi_arr_push_raw(out, value.ptr);
        } else {
            xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "map element type mismatch");
            xi_arr_push_raw(out, null);
        }
    }
    return out;
}

export fn xi_struct_new() callconv(.c) *XiStruct {
    xi_runtime_gc_ensure_init();
    const st = cnew(XiStruct);
    st.len = 0;
    st.cap = 0;
    st.fields = null;
    st.values = null;
    st.gc_mark = 0;
    st.gc_next = xi_gc_struct_head;
    xi_gc_struct_head = st;
    xi_gc_note_allocation();
    return st;
}

fn xi_struct_ensure_capacity(st: *XiStruct, min_cap: usize) void {
    if (st.cap >= min_cap) return;
    var new_cap: usize = if (st.cap == 0) 8 else st.cap;
    while (new_cap < min_cap) {
        if (new_cap > SIZE_MAX / 2) {
            new_cap = min_cap;
            break;
        }
        new_cap *= 2;
    }
    if (new_cap < min_cap) fatal("xi_runtime: struct capacity overflow\n");
    const new_fields: [*]OptStr = @ptrCast(@alignCast(realloc(@ptrCast(st.fields), new_cap * @sizeOf(OptStr)) orelse xi_oom()));
    const new_values: [*]XiValue = @ptrCast(@alignCast(realloc(@ptrCast(st.values), new_cap * @sizeOf(XiValue)) orelse xi_oom()));
    st.fields = new_fields;
    st.values = new_values;
    st.cap = new_cap;
}

fn xi_struct_find_index(st: *XiStruct, field: OptStr) usize {
    const probe: Str = if (field) |f| f else "";
    var i: usize = 0;
    while (i < st.len) : (i += 1) {
        const existing: Str = if (st.fields.?[i]) |f| f else "";
        if (strcmp(existing, probe) == 0) return i;
    }
    return SIZE_MAX;
}

fn xi_struct_store_value(st: *XiStruct, field: OptStr, value: XiValue) void {
    const stable_field: OptStr = if (field) |f| f else empty_cstr;
    const idx = xi_struct_find_index(st, stable_field);
    if (idx != SIZE_MAX) {
        st.values.?[idx] = if (value.tag == XI_VALUE_NULL) valNull() else value;
        return;
    }
    if (st.len >= SIZE_MAX) fatal("xi_runtime: struct field count overflow\n");
    xi_struct_ensure_capacity(st, st.len + 1);
    st.fields.?[st.len] = stable_field;
    st.values.?[st.len] = if (value.tag == XI_VALUE_NULL) valNull() else value;
    st.len += 1;
}

export fn xi_struct_set(st: ?*XiStruct, field: OptStr, value: ?*anyopaque) callconv(.c) void {
    const s = st orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "struct value was null");
        return;
    };
    if (field == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "struct field was null");
        return;
    }
    if (value == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "struct field value was null");
    xi_struct_store_value(s, field, valRaw(value));
}

export fn xi_struct_set_str(st: ?*XiStruct, field: OptStr, value: OptStr) callconv(.c) void {
    const s = st orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "struct value was null");
        return;
    };
    if (field == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "struct field was null");
        return;
    }
    if (value == null) xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "string value was null");
    xi_struct_store_value(s, field, valStr(if (value != null) value else xi_str_intern_cstr("")));
}

export fn xi_struct_get(st: ?*XiStruct, field: OptStr) callconv(.c) ?*anyopaque {
    const s = st orelse {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "struct value was null");
        return null;
    };
    if (field == null) {
        xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "struct field was null");
        return null;
    }
    const idx = xi_struct_find_index(s, field);
    if (idx == SIZE_MAX) {
        xi_err_set_message(XI_ERR_NOT_FOUND, "struct field not found");
        return null;
    }
    return xi_value_as_raw_or_null(&s.values.?[idx]);
}

// Arrays, maps and structs are handles: `b = a` gives two names for one object.
// from that table.

const XiCopySeen = struct {
    originals: ?[*]?*anyopaque,
    copies: ?[*]?*anyopaque,
    cap: usize,
    len: usize,
};

fn xi_copy_seen_hash(key: *anyopaque) usize {
    const mixed = (@intFromPtr(key) >> 4) *% 0x9E3779B97F4A7C15;
    return @intCast(mixed);
}

fn xi_copy_seen_grow(seen: *XiCopySeen) void {
    const new_cap: usize = if (seen.cap == 0) 64 else seen.cap * 2;
    const new_originals: [*]?*anyopaque = @ptrCast(@alignCast(calloc(new_cap, @sizeOf(?*anyopaque)) orelse xi_oom()));
    const new_copies: [*]?*anyopaque = @ptrCast(@alignCast(calloc(new_cap, @sizeOf(?*anyopaque)) orelse xi_oom()));
    var i: usize = 0;
    while (i < seen.cap) : (i += 1) {
        const original = seen.originals.?[i] orelse continue;
        var idx = xi_copy_seen_hash(original) & (new_cap - 1);
        while (new_originals[idx] != null) idx = (idx + 1) & (new_cap - 1);
        new_originals[idx] = original;
        new_copies[idx] = seen.copies.?[i];
    }
    cfree(seen.originals);
    cfree(seen.copies);
    seen.originals = new_originals;
    seen.copies = new_copies;
    seen.cap = new_cap;
}

fn xi_copy_seen_find(seen: *XiCopySeen, key: *anyopaque) ?*anyopaque {
    if (seen.cap == 0) return null;
    var idx = xi_copy_seen_hash(key) & (seen.cap - 1);
    while (seen.originals.?[idx]) |original| {
        if (original == key) return seen.copies.?[idx];
        idx = (idx + 1) & (seen.cap - 1);
    }
    return null;
}

fn xi_copy_seen_put(seen: *XiCopySeen, key: *anyopaque, copy: *anyopaque) void {
    if (seen.cap == 0 or (seen.len + 1) * 100 >= seen.cap * 70) xi_copy_seen_grow(seen);
    var idx = xi_copy_seen_hash(key) & (seen.cap - 1);
    while (seen.originals.?[idx]) |original| {
        if (original == key) {
            seen.copies.?[idx] = copy;
            return;
        }
        idx = (idx + 1) & (seen.cap - 1);
    }
    seen.originals.?[idx] = key;
    seen.copies.?[idx] = copy;
    seen.len += 1;
}

fn xi_copy_seen_deinit(seen: *XiCopySeen) void {
    cfree(seen.originals);
    cfree(seen.copies);
    seen.originals = null;
    seen.copies = null;
    seen.cap = 0;
    seen.len = 0;
}

fn xi_value_deep_copy(value: XiValue, seen: *XiCopySeen) XiValue {
    if (value.ptr == null) return value;
    return switch (value.tag) {
        XI_VALUE_ARR => valArr(xi_arr_deep_copy(@ptrCast(@alignCast(value.ptr)), seen)),
        XI_VALUE_MAP => valMap(xi_map_deep_copy(@ptrCast(@alignCast(value.ptr)), seen)),
        XI_VALUE_STRUCT => valStruct(xi_struct_deep_copy(@ptrCast(@alignCast(value.ptr)), seen)),
        else => value,
    };
}

fn xi_arr_deep_copy(arr: ?*XiStrArray, seen: *XiCopySeen) *XiStrArray {
    const a = arr orelse return xi_arr_new();
    if (xi_copy_seen_find(seen, a)) |hit| return @ptrCast(@alignCast(hit));

    const out = xi_arr_new();
    xi_copy_seen_put(seen, a, out);
    if (a.len == 0) return out;

    if (a.packed_nums) |source| {
        out.packed_bits = a.packed_bits;
        out.packed_unsigned = a.packed_unsigned;
        xi_arr_ensure_packed_capacity(out, a.len);
        cmemcpy(out.packed_nums.?, source, a.len * xi_arr_packed_elem_bytes(a));
        out.len = a.len;
        return out;
    }

    xi_arr_ensure_capacity(out, a.len);
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        out.items.?[i] = xi_value_deep_copy(a.items.?[i], seen);
    }
    out.len = a.len;
    return out;
}

fn xi_map_deep_copy(map: ?*XiMap, seen: *XiCopySeen) *XiMap {
    const m = map orelse return xi_map_new();
    if (xi_copy_seen_find(seen, m)) |hit| return @ptrCast(@alignCast(hit));

    const out = xi_map_new();
    xi_copy_seen_put(seen, m, out);
    if (m.cap == 0) return out;

    var i: usize = 0;
    while (i < m.cap) : (i += 1) {
        if (m.states == null or m.states.?[i] != 1) continue;
        const value = xi_value_deep_copy(m.values.?[i], seen);
        xi_map_prepare_insert(out);
        xi_map_insert_entry(out, m.keys.?[i], m.hashes.?[i], value);
    }
    return out;
}

fn xi_struct_deep_copy(st: ?*XiStruct, seen: *XiCopySeen) *XiStruct {
    const s = st orelse return xi_struct_new();
    if (xi_copy_seen_find(seen, s)) |hit| return @ptrCast(@alignCast(hit));

    const out = xi_struct_new();
    xi_copy_seen_put(seen, s, out);
    if (s.len == 0) return out;

    xi_struct_ensure_capacity(out, s.len);
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        out.fields.?[i] = s.fields.?[i];
        out.values.?[i] = xi_value_deep_copy(s.values.?[i], seen);
    }
    out.len = s.len;
    return out;
}

fn xi_result_new(ok: bool, value: XiValue, message: OptStr) *XiStruct {
    const out = xi_struct_new();
    xi_struct_store_value(out, xi_str_intern_cstr("ok"), valNum(xi_num_make_int(if (ok) 1 else 0)));
    xi_struct_store_value(out, xi_str_intern_cstr("value"), value);
    xi_struct_store_value(out, xi_str_intern_cstr("error"), valStr(xi_str_intern_cstr(if (message) |m| m else "")));
    return out;
}

const XiNumTextKind = enum { integer, decimal, invalid };

fn xi_classify_number_text(s: OptStr) XiNumTextKind {
    const text: Str = if (s) |t| t else "";
    var i: usize = 0;
    if (text[i] == '+' or text[i] == '-') i += 1;

    var digits: usize = 0;
    var dots: usize = 0;
    var seen_digit_after_dot = false;
    while (text[i] != 0) : (i += 1) {
        const c = text[i];
        if (c >= '0' and c <= '9') {
            digits += 1;
            if (dots == 1) seen_digit_after_dot = true;
        } else if (c == '.') {
            dots += 1;
            if (dots > 1) return .invalid;
        } else return .invalid;
    }
    if (digits == 0) return .invalid;
    if (dots == 0) return .integer;
    if (!seen_digit_after_dot) return .invalid;
    return .decimal;
}

fn xi_parse_error_message(buffer: []u8, what: Str, s: OptStr) Str {
    const text: Str = if (s) |t| t else "";
    _ = snprintf(buffer.ptr, buffer.len, "not %s: \"%s\"", what, text);
    return @ptrCast(buffer.ptr);
}

export fn xi_str_to_int_result(s: OptStr) callconv(.c) *XiStruct {
    if (xi_classify_number_text(s) != .integer) {
        var buffer: [160]u8 = undefined;
        return xi_result_new(false, valNum(xi_num_make_int(0)), xi_str_intern_cstr(xi_parse_error_message(&buffer, "an integer", s)));
    }
    return xi_result_new(true, valNum(xi_num_from_decimal(s)), null);
}

export fn xi_str_to_dec_result(s: OptStr) callconv(.c) *XiStruct {
    const kind = xi_classify_number_text(s);
    if (kind == .invalid) {
        var buffer: [160]u8 = undefined;
        return xi_result_new(false, valNum(xi_num_dec_from_decimal(xi_str_intern_cstr("0"))), xi_str_intern_cstr(xi_parse_error_message(&buffer, "a decimal", s)));
    }
    return xi_result_new(true, valNum(xi_num_dec_from_decimal(s)), null);
}

export fn xi_arr_copy_deep(arr: ?*XiStrArray) callconv(.c) *XiStrArray {
    xi_gc_pause_begin();
    defer xi_gc_pause_end();
    var seen = XiCopySeen{ .originals = null, .copies = null, .cap = 0, .len = 0 };
    defer xi_copy_seen_deinit(&seen);
    return xi_arr_deep_copy(arr, &seen);
}

export fn xi_map_copy_deep(map: ?*XiMap) callconv(.c) *XiMap {
    xi_gc_pause_begin();
    defer xi_gc_pause_end();
    var seen = XiCopySeen{ .originals = null, .copies = null, .cap = 0, .len = 0 };
    defer xi_copy_seen_deinit(&seen);
    return xi_map_deep_copy(map, &seen);
}

export fn xi_struct_copy_deep(st: ?*XiStruct) callconv(.c) *XiStruct {
    xi_gc_pause_begin();
    defer xi_gc_pause_end();
    var seen = XiCopySeen{ .originals = null, .copies = null, .cap = 0, .len = 0 };
    defer xi_copy_seen_deinit(&seen);
    return xi_struct_deep_copy(st, &seen);
}

const XiVarEntry = struct {
    name: [64]u8,
    value: f64,
};

const XiExprParser = struct {
    input: [*:0]const u8,
    pos: usize,
    vars: [64]XiVarEntry,
    var_count: usize,
    had_error: c_int,
};

const XI_SYM_MAX_VARS: usize = 16;
const XI_SYM_MAX_TERMS: usize = 512;
const XI_SYM_MAX_NAME_LEN: usize = 31;
const XI_SYM_MAX_OUTPUT: usize = 8192;
const XI_SYM_EPS: f64 = 1e-12;

const XiSymTerm = struct {
    coeff: f64,
    exps: [XI_SYM_MAX_VARS]u16,
};

const XiSymPoly = struct {
    terms: [XI_SYM_MAX_TERMS]XiSymTerm,
    count: usize,
};

const XiSymParser = struct {
    input: [*:0]const u8,
    pos: usize,
    had_error: c_int,
    var_names: [XI_SYM_MAX_VARS][XI_SYM_MAX_NAME_LEN + 1]u8,
    var_count: usize,
};
