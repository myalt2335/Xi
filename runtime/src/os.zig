const std = @import("std");
const builtin = @import("builtin");

var xi_os_last_err_message: [256]u8 = std.mem.zeroes([256]u8);
var xi_os_process_exit_codes: [64]i64 = std.mem.zeroes([64]i64);
var xi_os_rng_state: u64 = 0;
var xi_os_process_used: [64]bool = std.mem.zeroes([64]bool);

const FILE = opaque {};
extern "c" fn malloc(usize) ?*anyopaque;
extern "c" fn realloc(?*anyopaque, usize) ?*anyopaque;
extern "c" fn free(?*anyopaque) void;
extern "c" fn abort() noreturn;
extern "c" fn exit(c_int) noreturn;
extern "c" fn memcpy(noalias ?*anyopaque, noalias ?*const anyopaque, usize) ?*anyopaque;
extern "c" fn strlen([*:0]const u8) usize;
extern "c" fn strcmp([*:0]const u8, [*:0]const u8) c_int;
extern "c" fn strcspn([*:0]const u8, [*:0]const u8) usize;
extern "c" fn strchr([*:0]const u8, c_int) ?[*:0]u8;
extern "c" fn snprintf(noalias [*]u8, usize, noalias [*:0]const u8, ...) c_int;
extern "c" fn system([*:0]const u8) c_int;
extern "c" fn getenv([*:0]const u8) ?[*:0]const u8;
extern "c" fn fopen([*:0]const u8, [*:0]const u8) ?*FILE;
extern "c" fn fclose(*FILE) c_int;
extern "c" fn fread(?*anyopaque, usize, usize, *FILE) usize;
extern "c" fn fwrite(?*const anyopaque, usize, usize, *FILE) usize;
extern "c" fn fgets([*]u8, c_int, *FILE) ?[*]u8;
extern "c" fn ferror(*FILE) c_int;
extern "c" fn remove([*:0]const u8) c_int;
extern "c" fn rename([*:0]const u8, [*:0]const u8) c_int;
extern "c" fn signal(c_int, ?*const fn (c_int) callconv(.c) void) ?*const fn (c_int) callconv(.c) void;
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
extern "c" fn getcwd([*]u8, usize) ?[*]u8;
extern "c" fn chdir([*:0]const u8) c_int;
extern "c" fn mkdir([*:0]const u8, c_int) c_int;
extern "c" fn rmdir([*:0]const u8) c_int;
extern "c" fn getpid() c_int;
extern "c" fn getppid() c_int;
extern "c" fn readlink([*:0]const u8, [*]u8, usize) isize;
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
const SIZE_MAX: usize = std.math.maxInt(usize);
const XI_VALUE_NULL: u8 = 0;
const XiNum = opaque {};
const XiValue = struct {
    tag: u8 = XI_VALUE_NULL,
    ptr: ?*anyopaque = null,
    boolean: bool = false,
};
const XiStrArray = struct {
    len: usize,
    cap: usize,
    items: ?[*]XiValue,
    packed_nums: ?[*]u8,
    packed_bits: u16,
    packed_unsigned: u8,
    gc_mark: u8,
    gc_next: ?*XiStrArray,
};
const Str = [*:0]const u8;
const OptStr = ?[*:0]const u8;

extern var xi_cli_argv_storage: ?[*]OptStr;

extern fn isalpha(c: u8) callconv(.c) bool;
extern fn stderrFile() callconv(.c) *FILE;
extern fn stdinFile() callconv(.c) *FILE;
extern fn stdoutFile() callconv(.c) *FILE;
extern fn xi_append_bytes(buf: [*]u8, cap: usize, pos: *usize, bytes: [*]const u8, len: usize) callconv(.c) void;
extern fn xi_append_cstr(buf: [*]u8, cap: usize, pos: *usize, s: [*:0]const u8) callconv(.c) void;
extern fn xi_arr_get_str(arr: ?*XiStrArray, idx: ?*XiNum) callconv(.c) Str;
extern fn xi_arr_new() callconv(.c) *XiStrArray;
extern fn xi_arr_push_str(arr: ?*XiStrArray, value: OptStr) callconv(.c) void;
extern fn xi_arr_set_uint_i64_at_i64(arr: ?*XiStrArray, idx: i64, value: i64) callconv(.c) void;
extern fn xi_cli_load_args() callconv(.c) void;
extern fn xi_mono_ns_i64() callconv(.c) i64;
extern fn xi_num_i64(v: i64) callconv(.c) *XiNum;
extern fn xi_num_to_i64(n: ?*XiNum) callconv(.c) i64;
extern fn xi_oom() callconv(.c) noreturn;
extern fn xi_str_intern_bytes(s: ?[*]const u8, len_in: usize) callconv(.c) Str;
extern fn xi_str_intern_cstr(s: OptStr) callconv(.c) Str;
extern fn xi_str_len_known(s: OptStr) callconv(.c) usize;
extern fn xi_wall_time_ns() callconv(.c) i64;

fn cfree(p: anytype) void {
    free(@ptrCast(p));
}

fn cmemcpy(dst: anytype, src: anytype, n: usize) void {
    _ = memcpy(@ptrCast(dst), @ptrCast(src), n);
}


fn xi_os_set_last_error(message: OptStr) void {
    _ = snprintf(&xi_os_last_err_message, xi_os_last_err_message.len, "%s", if (message) |m| m else @as(Str, ""));
}

fn xi_os_bool(ok: bool, message: OptStr) *XiNum {
    if (!ok) xi_os_set_last_error(message) else xi_os_last_err_message[0] = 0;
    return xi_num_i64(if (ok) 1 else 0);
}

fn xi_os_cwd_raw(buf: [*]u8, len: usize) ?[*]u8 {
    if (builtin.os.tag == .windows) return _getcwd(buf, @intCast(len));
    return getcwd(buf, len);
}

fn xi_os_chdir_raw(path: Str) c_int {
    if (builtin.os.tag == .windows) return _chdir(path);
    return chdir(path);
}

fn xi_os_mkdir_raw(path: Str) c_int {
    if (builtin.os.tag == .windows) return _mkdir(path);
    return mkdir(path, 0o777);
}

fn xi_os_rmdir_raw(path: Str) c_int {
    if (builtin.os.tag == .windows) return _rmdir(path);
    return rmdir(path);
}

fn xi_os_popen_raw(command: Str) ?*FILE {
    if (builtin.os.tag == .windows) return _popen(command, "r");
    return popen(command, "r");
}

fn xi_os_pclose_raw(f: *FILE) c_int {
    if (builtin.os.tag == .windows) return _pclose(f);
    return pclose(f);
}

fn xi_os_is_tty_file(f: *FILE) bool {
    if (builtin.os.tag == .windows) return _isatty(_fileno(f)) != 0;
    return isatty(fileno(f)) != 0;
}

fn xi_os_env_block() ?[*]?[*:0]u8 {
    if (builtin.os.tag == .windows) return __p__environ().*;
    return environ;
}

fn xi_os_path_sep_char() u8 {
    return if (builtin.os.tag == .windows) '\\' else '/';
}

fn xi_os_is_sep(ch: u8) bool {
    return ch == '/' or ch == '\\';
}

fn xi_os_path_is_absolute_bytes(path: Str) bool {
    const len = xi_str_len_known(path);
    if (len == 0) return false;
    if (xi_os_is_sep(path[0])) return true;
    return builtin.os.tag == .windows and len >= 3 and isalpha(path[0]) and path[1] == ':' and xi_os_is_sep(path[2]);
}

fn xi_os_join_into(out: [*]u8, cap: usize, parts: []const OptStr) usize {
    const sep = xi_os_path_sep_char();
    var pos: usize = 0;
    for (parts) |part_opt| {
        const part = part_opt orelse continue;
        const len = xi_str_len_known(part);
        if (len == 0) continue;
        if (pos > 0 and !xi_os_is_sep(out[pos - 1]) and !xi_os_is_sep(part[0]) and pos + 1 < cap) {
            out[pos] = sep;
            pos += 1;
        }
        var start: usize = 0;
        if (pos > 0) {
            while (start < len and xi_os_is_sep(part[start])) : (start += 1) {}
        }
        xi_append_bytes(out, cap, &pos, part + start, len - start);
    }
    out[pos] = 0;
    return pos;
}

fn xi_os_shell_quote_into(out: [*]u8, cap: usize, pos: *usize, s: OptStr) void {
    const text = s orelse "";
    const len = xi_str_len_known(text);
    if (pos.* + 1 < cap) {
        out[pos.*] = '"';
        pos.* += 1;
    }
    var i: usize = 0;
    while (i < len and pos.* + 2 < cap) : (i += 1) {
        if (text[i] == '"') {
            out[pos.*] = '\\';
            pos.* += 1;
        }
        out[pos.*] = text[i];
        pos.* += 1;
    }
    if (pos.* + 1 < cap) {
        out[pos.*] = '"';
        pos.* += 1;
    }
}

fn xi_os_read_dir_command(path: OptStr, out: [*]u8, cap: usize) void {
    var pos: usize = 0;
    if (builtin.os.tag == .windows) {
        xi_append_cstr(out, cap, &pos, "cmd /C dir /B ");
    } else {
        xi_append_cstr(out, cap, &pos, "ls -1 ");
    }
    xi_os_shell_quote_into(out, cap, &pos, path);
    out[pos] = 0;
}

fn xi_os_read_dir_filtered(path: OptStr, filter: c_int) *XiStrArray {
    const arr = xi_arr_new();
    const p = path orelse ".";
    var cmd: [2048]u8 = undefined;
    xi_os_read_dir_command(p, &cmd, cmd.len);
    const pipe = xi_os_popen_raw(@ptrCast(&cmd)) orelse {
        xi_os_set_last_error("failed to list directory");
        return arr;
    };
    var line: [1024]u8 = undefined;
    while (fgets(&line, line.len, pipe) != null) {
        const len0 = strcspn(@ptrCast(&line), "\r\n");
        line[len0] = 0;
        if (len0 == 0) continue;
        const name = xi_str_intern_bytes(&line, len0);
        if (filter == 0) {
            xi_arr_push_str(arr, name);
        } else {
            var joined_buf: [2048]u8 = undefined;
            _ = xi_os_join_into(&joined_buf, joined_buf.len, &.{ p, name });
            const joined: Str = @ptrCast(&joined_buf);
            if ((filter == 1 and xi_os_is_file(joined)) or (filter == 2 and xi_os_is_dir(joined))) xi_arr_push_str(arr, name);
        }
    }
    _ = xi_os_pclose_raw(pipe);
    return arr;
}

fn xi_os_copy_file_impl(src: OptStr, dst: OptStr) bool {
    if (src == null or dst == null) return false;
    const in_f = fopen(src.?, "rb") orelse return false;
    const out_f = fopen(dst.?, "wb") orelse {
        _ = fclose(in_f);
        return false;
    };
    var buf: [8192]u8 = undefined;
    while (true) {
        const rd = fread(&buf, 1, buf.len, in_f);
        if (rd > 0 and fwrite(&buf, 1, rd, out_f) != rd) {
            _ = fclose(in_f);
            _ = fclose(out_f);
            return false;
        }
        if (rd < buf.len) break;
    }
    const ok = ferror(in_f) == 0 and fclose(in_f) == 0 and fclose(out_f) == 0;
    return ok;
}

fn xi_os_file_size_i64(path: OptStr) i64 {
    var st: XiFileMetadata = undefined;
    if (!xi_os_stat(path, &st)) return -1;
    return st.size;
}

const XiFileMetadata = struct {
    mode: u16,
    size: i64,
    atime: i64,
    mtime: i64,
    ctime: i64,
};

fn xi_os_stat(path: OptStr, out: *XiFileMetadata) bool {
    const p = path orelse return false;
    if (builtin.os.tag == .windows) {
        var st: XiStat64 = undefined;
        if (_stat64(p, &st) != 0) return false;
        out.* = .{
            .mode = st.st_mode,
            .size = st.st_size,
            .atime = st.st_atime,
            .mtime = st.st_mtime,
            .ctime = st.st_ctime,
        };
        return true;
    }

    const linux = std.os.linux;
    var st: linux.Statx = undefined;
    const mask: linux.STATX = .{
        .TYPE = true,
        .MODE = true,
        .ATIME = true,
        .MTIME = true,
        .CTIME = true,
        .SIZE = true,
    };
    if (linux.errno(linux.statx(linux.AT.FDCWD, p, 0, mask, &st)) != .SUCCESS)
        return false;
    out.* = .{
        .mode = st.mode,
        .size = @intCast(st.size),
        .atime = st.atime.sec,
        .mtime = st.mtime.sec,
        .ctime = st.ctime.sec,
    };
    return true;
}

fn xi_os_stat_mode(path: OptStr) c_ushort {
    var st: XiFileMetadata = undefined;
    if (!xi_os_stat(path, &st)) return 0;
    return st.mode;
}

fn xi_os_stat_time(path: OptStr, which: u8) i64 {
    var st: XiFileMetadata = undefined;
    if (!xi_os_stat(path, &st)) {
        xi_os_set_last_error("failed to get file metadata");
        return 0;
    }
    return switch (which) {
        0 => st.mtime,
        1 => st.ctime,
        2 => st.atime,
        else => 0,
    };
}

export fn xi_os_name() callconv(.c) Str {
    return xi_str_intern_cstr(switch (builtin.os.tag) {
        .windows => "windows",
        .linux => "linux",
        .macos => "macos",
        .freebsd => "freebsd",
        else => @tagName(builtin.os.tag),
    });
}

export fn xi_os_version() callconv(.c) Str {
    return xi_str_intern_cstr("");
}

export fn xi_os_arch() callconv(.c) Str {
    return xi_str_intern_cstr(@tagName(builtin.cpu.arch));
}

export fn xi_os_cpu_count() callconv(.c) *XiNum {
    const n = std.Thread.getCpuCount() catch 0;
    return xi_num_i64(@intCast(n));
}

export fn xi_os_page_size() callconv(.c) *XiNum {
    return xi_num_i64(std.heap.page_size_min);
}

export fn xi_os_hostname() callconv(.c) Str {
    if (getenv(if (builtin.os.tag == .windows) "COMPUTERNAME" else "HOSTNAME")) |v| return xi_str_intern_cstr(v);
    return xi_str_intern_cstr("");
}

export fn xi_os_username() callconv(.c) Str {
    if (getenv(if (builtin.os.tag == .windows) "USERNAME" else "USER")) |v| return xi_str_intern_cstr(v);
    return xi_str_intern_cstr("");
}

export fn xi_os_pid() callconv(.c) *XiNum {
    return xi_num_i64(if (builtin.os.tag == .windows) _getpid() else getpid());
}

export fn xi_os_ppid() callconv(.c) *XiNum {
    return xi_num_i64(if (builtin.os.tag == .windows) 0 else getppid());
}

export fn xi_os_exe_path() callconv(.c) Str {
    if (builtin.os.tag == .linux) {
        var buf: [4096]u8 = undefined;
        const length = readlink("/proc/self/exe", &buf, buf.len - 1);
        if (length > 0) {
            buf[@intCast(length)] = 0;
            return xi_str_intern_cstr(@ptrCast(&buf));
        }
    }
    xi_cli_load_args();
    if (xi_cli_argv_storage) |args| if (args[0]) |arg0| return arg0;
    return xi_str_intern_cstr("");
}

export fn xi_os_cwd() callconv(.c) Str {
    var buf: [4096]u8 = undefined;
    if (xi_os_cwd_raw(&buf, buf.len)) |p| return xi_str_intern_cstr(@ptrCast(p));
    xi_os_set_last_error("failed to get cwd");
    return xi_str_intern_cstr("");
}

export fn xi_os_chdir(path: OptStr) callconv(.c) *XiNum {
    if (path == null) return xi_os_bool(false, "path was null");
    return xi_os_bool(xi_os_chdir_raw(path.?) == 0, "failed to change directory");
}

export fn xi_os_exit(code: ?*XiNum) callconv(.c) *XiNum {
    exit(@intCast(xi_num_to_i64(code) & 0xff));
}

export fn xi_os_abort() callconv(.c) *XiNum {
    abort();
}

export fn xi_os_getenv(name: OptStr) callconv(.c) Str {
    const n = name orelse return xi_str_intern_cstr("");
    if (getenv(n)) |v| return xi_str_intern_cstr(v);
    return xi_str_intern_cstr("");
}

export fn xi_os_getenv_or(name: OptStr, fallback: OptStr) callconv(.c) Str {
    const n = name orelse return if (fallback) |fb| fb else xi_str_intern_cstr("");
    if (getenv(n)) |v| return xi_str_intern_cstr(v);
    return if (fallback) |fb| fb else xi_str_intern_cstr("");
}

export fn xi_os_has_env(name: OptStr) callconv(.c) bool {
    const n = name orelse return false;
    return getenv(n) != null;
}

export fn xi_os_setenv(name: OptStr, value: OptStr) callconv(.c) *XiNum {
    if (name == null or value == null) return xi_os_bool(false, "environment name or value was null");
    const ok = if (builtin.os.tag == .windows) _putenv_s(name.?, value.?) == 0 else setenv(name.?, value.?, 1) == 0;
    return xi_os_bool(ok, "failed to set environment variable");
}

export fn xi_os_unsetenv(name: OptStr) callconv(.c) *XiNum {
    if (name == null) return xi_os_bool(false, "environment name was null");
    const ok = if (builtin.os.tag == .windows) _putenv_s(name.?, "") == 0 else unsetenv(name.?) == 0;
    return xi_os_bool(ok, "failed to unset environment variable");
}

export fn xi_os_env_keys() callconv(.c) *XiStrArray {
    const out = xi_arr_new();
    const env = xi_os_env_block() orelse return out;
    var i: usize = 0;
    while (env[i]) |entry| : (i += 1) {
        const eq = strchr(@ptrCast(entry), '=');
        if (eq) |p| {
            const len = @intFromPtr(p) - @intFromPtr(entry);
            xi_arr_push_str(out, xi_str_intern_bytes(entry, len));
        }
    }
    return out;
}

export fn xi_os_env_values() callconv(.c) *XiStrArray {
    const out = xi_arr_new();
    const env = xi_os_env_block() orelse return out;
    var i: usize = 0;
    while (env[i]) |entry| : (i += 1) {
        const eq = strchr(@ptrCast(entry), '=');
        if (eq) |p| xi_arr_push_str(out, xi_str_intern_cstr(@ptrCast(p + 1)));
    }
    return out;
}

export fn xi_os_home_dir() callconv(.c) Str {
    if (getenv(if (builtin.os.tag == .windows) "USERPROFILE" else "HOME")) |v| return xi_str_intern_cstr(v);
    return xi_str_intern_cstr("");
}

export fn xi_os_temp_dir() callconv(.c) Str {
    if (getenv(if (builtin.os.tag == .windows) "TEMP" else "TMPDIR")) |v| return xi_str_intern_cstr(v);
    return xi_str_intern_cstr(if (builtin.os.tag == .windows) "." else "/tmp");
}

export fn xi_os_config_dir() callconv(.c) Str {
    if (getenv(if (builtin.os.tag == .windows) "APPDATA" else "XDG_CONFIG_HOME")) |v| return xi_str_intern_cstr(v);
    return xi_os_home_dir();
}

export fn xi_os_data_dir() callconv(.c) Str {
    if (getenv(if (builtin.os.tag == .windows) "LOCALAPPDATA" else "XDG_DATA_HOME")) |v| return xi_str_intern_cstr(v);
    return xi_os_home_dir();
}

export fn xi_os_cache_dir() callconv(.c) Str {
    if (getenv(if (builtin.os.tag == .windows) "LOCALAPPDATA" else "XDG_CACHE_HOME")) |v| return xi_str_intern_cstr(v);
    return xi_os_temp_dir();
}

export fn xi_os_is_file(path: OptStr) callconv(.c) bool {
    return (xi_os_stat_mode(path) & 0x8000) != 0;
}

export fn xi_os_is_dir(path: OptStr) callconv(.c) bool {
    return (xi_os_stat_mode(path) & 0x4000) != 0;
}

export fn xi_os_exists(path: OptStr) callconv(.c) bool {
    return xi_os_is_file(path) or xi_os_is_dir(path);
}

export fn xi_os_is_symlink(path: OptStr) callconv(.c) bool {
    _ = path;
    return false;
}

export fn xi_os_file_size(path: OptStr) callconv(.c) *XiNum {
    const size = xi_os_file_size_i64(path);
    if (size < 0) xi_os_set_last_error("failed to get file size");
    return xi_num_i64(if (size < 0) 0 else size);
}

export fn xi_os_file_modified_unix(path: OptStr) callconv(.c) *XiNum {
    return xi_num_i64(xi_os_stat_time(path, 0));
}

export fn xi_os_file_created_unix(path: OptStr) callconv(.c) *XiNum {
    return xi_num_i64(xi_os_stat_time(path, 1));
}

export fn xi_os_file_accessed_unix(path: OptStr) callconv(.c) *XiNum {
    return xi_num_i64(xi_os_stat_time(path, 2));
}

export fn xi_os_mkdir(path: OptStr) callconv(.c) *XiNum {
    if (path == null) return xi_os_bool(false, "path was null");
    return xi_os_bool(xi_os_mkdir_raw(path.?) == 0 or xi_os_is_dir(path), "failed to create directory");
}

export fn xi_os_mkdir_all(path: OptStr) callconv(.c) *XiNum {
    const p = path orelse return xi_os_bool(false, "path was null");
    const len = xi_str_len_known(p);
    if (len == 0) return xi_os_bool(false, "path was empty");
    var buf: [4096]u8 = undefined;
    if (len >= buf.len) return xi_os_bool(false, "path was too long");
    cmemcpy(&buf, p, len);
    buf[len] = 0;
    var i: usize = 0;
    while (i < len) : (i += 1) {
        if (xi_os_is_sep(buf[i])) {
            const saved = buf[i];
            buf[i] = 0;
            if (i > 0) _ = xi_os_mkdir_raw(@ptrCast(&buf));
            buf[i] = saved;
        }
    }
    return xi_os_bool(xi_os_mkdir_raw(@ptrCast(&buf)) == 0 or xi_os_is_dir(@ptrCast(&buf)), "failed to create directory");
}

export fn xi_os_remove_file(path: OptStr) callconv(.c) *XiNum {
    if (path == null) return xi_os_bool(false, "path was null");
    return xi_os_bool(remove(path.?) == 0, "failed to remove file");
}

export fn xi_os_remove_dir(path: OptStr) callconv(.c) *XiNum {
    if (path == null) return xi_os_bool(false, "path was null");
    return xi_os_bool(xi_os_rmdir_raw(path.?) == 0, "failed to remove directory");
}

export fn xi_os_remove_dir_all(path: OptStr) callconv(.c) *XiNum {
    if (path == null) return xi_os_bool(false, "path was null");
    var cmd: [4096]u8 = undefined;
    var pos: usize = 0;
    if (builtin.os.tag == .windows) xi_append_cstr(&cmd, cmd.len, &pos, "cmd /C rmdir /S /Q ") else xi_append_cstr(&cmd, cmd.len, &pos, "rm -rf ");
    xi_os_shell_quote_into(&cmd, cmd.len, &pos, path);
    cmd[pos] = 0;
    return xi_os_bool(system(@ptrCast(&cmd)) == 0, "failed to remove directory tree");
}

export fn xi_os_rename(old_path: OptStr, new_path: OptStr) callconv(.c) *XiNum {
    if (old_path == null or new_path == null) return xi_os_bool(false, "path was null");
    return xi_os_bool(rename(old_path.?, new_path.?) == 0, "failed to rename path");
}

export fn xi_os_copy_file(src: OptStr, dst: OptStr) callconv(.c) *XiNum {
    return xi_os_bool(xi_os_copy_file_impl(src, dst), "failed to copy file");
}

export fn xi_os_read_dir(path: OptStr) callconv(.c) *XiStrArray {
    return xi_os_read_dir_filtered(path, 0);
}

export fn xi_os_read_dir_files(path: OptStr) callconv(.c) *XiStrArray {
    return xi_os_read_dir_filtered(path, 1);
}

export fn xi_os_read_dir_dirs(path: OptStr) callconv(.c) *XiStrArray {
    return xi_os_read_dir_filtered(path, 2);
}

export fn xi_os_path_join(a: OptStr, b: OptStr) callconv(.c) Str {
    var buf: [4096]u8 = undefined;
    const len = xi_os_join_into(&buf, buf.len, &.{ a, b });
    return xi_str_intern_bytes(&buf, len);
}

export fn xi_os_path_join3(a: OptStr, b: OptStr, c: OptStr) callconv(.c) Str {
    var buf: [4096]u8 = undefined;
    const len = xi_os_join_into(&buf, buf.len, &.{ a, b, c });
    return xi_str_intern_bytes(&buf, len);
}

export fn xi_os_path_basename(path: OptStr) callconv(.c) Str {
    const p = path orelse return xi_str_intern_cstr("");
    const len = xi_str_len_known(p);
    var start: usize = 0;
    var i: usize = 0;
    while (i < len) : (i += 1) {
        if (xi_os_is_sep(p[i])) start = i + 1;
    }
    return xi_str_intern_bytes(p + start, len - start);
}

export fn xi_os_path_dirname(path: OptStr) callconv(.c) Str {
    const p = path orelse return xi_str_intern_cstr("");
    const len = xi_str_len_known(p);
    var end: usize = 0;
    var i: usize = 0;
    while (i < len) : (i += 1) {
        if (xi_os_is_sep(p[i])) end = i;
    }
    if (end == 0) return xi_str_intern_cstr(".");
    return xi_str_intern_bytes(p, end);
}

export fn xi_os_path_extension(path: OptStr) callconv(.c) Str {
    const base = xi_os_path_basename(path);
    const len = xi_str_len_known(base);
    var dot: usize = SIZE_MAX;
    var i: usize = 0;
    while (i < len) : (i += 1) {
        if (base[i] == '.') dot = i;
    }
    if (dot == SIZE_MAX or dot + 1 >= len) return xi_str_intern_cstr("");
    return xi_str_intern_bytes(base + dot + 1, len - dot - 1);
}

export fn xi_os_path_stem(path: OptStr) callconv(.c) Str {
    const base = xi_os_path_basename(path);
    const len = xi_str_len_known(base);
    var dot: usize = len;
    var i: usize = 0;
    while (i < len) : (i += 1) {
        if (base[i] == '.') dot = i;
    }
    return xi_str_intern_bytes(base, dot);
}

export fn xi_os_path_absolute(path: OptStr) callconv(.c) Str {
    const p = path orelse return xi_os_cwd();
    if (xi_os_path_is_absolute_bytes(p)) return xi_str_intern_cstr(p);
    return xi_os_path_join(xi_os_cwd(), p);
}

export fn xi_os_path_normalize(path: OptStr) callconv(.c) Str {
    const p = path orelse return xi_str_intern_cstr("");
    const len = xi_str_len_known(p);
    var buf: [4096]u8 = undefined;
    var pos: usize = 0;
    var prev_sep = false;
    var i: usize = 0;
    const sep = xi_os_path_sep_char();
    while (i < len and pos + 1 < buf.len) : (i += 1) {
        if (xi_os_is_sep(p[i])) {
            if (!prev_sep) {
                buf[pos] = sep;
                pos += 1;
            }
            prev_sep = true;
        } else {
            buf[pos] = p[i];
            pos += 1;
            prev_sep = false;
        }
    }
    buf[pos] = 0;
    return xi_str_intern_bytes(&buf, pos);
}

export fn xi_os_path_is_absolute(path: OptStr) callconv(.c) bool {
    return if (path) |p| xi_os_path_is_absolute_bytes(p) else false;
}

export fn xi_os_path_separator() callconv(.c) Str {
    return xi_str_intern_cstr(if (builtin.os.tag == .windows) "\\" else "/");
}

export fn xi_os_run(command: OptStr) callconv(.c) *XiNum {
    return xi_os_run_status(command);
}

export fn xi_os_run_status(command: OptStr) callconv(.c) *XiNum {
    const c = command orelse return xi_num_i64(-1);
    return xi_num_i64(system(c));
}

export fn xi_os_run_capture(command: OptStr) callconv(.c) Str {
    const c = command orelse return xi_str_intern_cstr("");
    const pipe = xi_os_popen_raw(c) orelse return xi_str_intern_cstr("");
    var cap: usize = 4096;
    var len: usize = 0;
    var out: [*]u8 = @ptrCast(malloc(cap) orelse xi_oom());
    var buf: [512]u8 = undefined;
    while (fgets(&buf, buf.len, pipe) != null) {
        const got = strlen(@ptrCast(&buf));
        if (len + got + 1 > cap) {
            while (len + got + 1 > cap) cap *= 2;
            out = @ptrCast(realloc(out, cap) orelse xi_oom());
        }
        cmemcpy(out + len, &buf, got);
        len += got;
    }
    _ = xi_os_pclose_raw(pipe);
    out[len] = 0;
    const interned = xi_str_intern_bytes(out, len);
    cfree(out);
    return interned;
}

export fn xi_os_spawn(exe: OptStr, args: ?*XiStrArray) callconv(.c) *XiNum {
    var cmd: [4096]u8 = undefined;
    var pos: usize = 0;
    xi_os_shell_quote_into(&cmd, cmd.len, &pos, exe);
    if (args) |arr| {
        var i: usize = 0;
        while (i < arr.len and pos + 2 < cmd.len) : (i += 1) {
            cmd[pos] = ' ';
            pos += 1;
            xi_os_shell_quote_into(&cmd, cmd.len, &pos, xi_arr_get_str(arr, xi_num_i64(@intCast(i))));
        }
    }
    cmd[pos] = 0;
    const status = system(@ptrCast(&cmd));
    var slot: usize = 1;
    while (slot < xi_os_process_used.len and xi_os_process_used[slot]) : (slot += 1) {}
    if (slot >= xi_os_process_used.len) return xi_num_i64(0);
    xi_os_process_used[slot] = true;
    xi_os_process_exit_codes[slot] = status;
    return xi_num_i64(@intCast(slot));
}

export fn xi_os_wait(process: ?*XiNum) callconv(.c) *XiNum {
    _ = process;
    return xi_num_i64(0);
}

export fn xi_os_kill(process: ?*XiNum) callconv(.c) *XiNum {
    _ = process;
    return xi_num_i64(0);
}

export fn xi_os_process_exit_code(process: ?*XiNum) callconv(.c) *XiNum {
    const id = xi_num_to_i64(process);
    if (id <= 0 or id >= @as(i64, @intCast(xi_os_process_used.len)) or !xi_os_process_used[@intCast(id)]) return xi_num_i64(-1);
    return xi_num_i64(xi_os_process_exit_codes[@intCast(id)]);
}

fn xi_os_random_next_u64() u64 {
    if (xi_os_rng_state == 0) {
        xi_os_rng_state = @as(u64, @bitCast(xi_wall_time_ns())) ^ @as(u64, @bitCast(xi_mono_ns_i64())) ^ 0x9e3779b97f4a7c15;
        if (xi_os_rng_state == 0) xi_os_rng_state = 0x2545f4914f6cdd1d;
    }
    var x = xi_os_rng_state;
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    xi_os_rng_state = x;
    return x *% 0x2545f4914f6cdd1d;
}

export fn xi_os_random_bytes(n_num: ?*XiNum) callconv(.c) *XiStrArray {
    const out = xi_arr_new();
    var n = xi_num_to_i64(n_num);
    if (n < 0) n = 0;
    var i: i64 = 0;
    while (i < n) : (i += 1) xi_arr_set_uint_i64_at_i64(out, i, @intCast(xi_os_random_next_u64() & 0xff));
    return out;
}

export fn xi_os_random_u64() callconv(.c) u64 {
    return xi_os_random_next_u64();
}

export fn xi_os_random_u32() callconv(.c) u32 {
    return @truncate(xi_os_random_next_u64());
}

export fn xi_os_stdin_is_tty() callconv(.c) bool {
    return xi_os_is_tty_file(stdinFile());
}

export fn xi_os_stdout_is_tty() callconv(.c) bool {
    return xi_os_is_tty_file(stdoutFile());
}

export fn xi_os_stderr_is_tty() callconv(.c) bool {
    return xi_os_is_tty_file(stderrFile());
}

fn xi_signal_by_name(name: OptStr) c_int {
    const n = name orelse return 0;
    if (strcmp(n, "int") == 0 or strcmp(n, "sigint") == 0 or strcmp(n, "SIGINT") == 0) return SIGINT;
    if (strcmp(n, "term") == 0 or strcmp(n, "sigterm") == 0 or strcmp(n, "SIGTERM") == 0) return SIGTERM;
    if (strcmp(n, "abrt") == 0 or strcmp(n, "sigabrt") == 0 or strcmp(n, "SIGABRT") == 0) return SIGABRT;
    return 0;
}

export fn xi_os_signal_ignore(signal_name: OptStr) callconv(.c) *XiNum {
    const sig = xi_signal_by_name(signal_name);
    if (sig == 0) return xi_os_bool(false, "unknown signal");
    _ = signal(sig, SIG_IGN);
    return xi_os_bool(true, null);
}

export fn xi_os_signal_default(signal_name: OptStr) callconv(.c) *XiNum {
    const sig = xi_signal_by_name(signal_name);
    if (sig == 0) return xi_os_bool(false, "unknown signal");
    _ = signal(sig, SIG_DFL);
    return xi_os_bool(true, null);
}

export fn xi_os_last_error() callconv(.c) Str {
    return xi_str_intern_cstr(@ptrCast(&xi_os_last_err_message));
}

export fn xi_os_clear_error() callconv(.c) *XiNum {
    xi_os_last_err_message[0] = 0;
    return xi_num_i64(0);
}
