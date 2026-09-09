// use `image::` do not pull the decoder into their executables.

const std = @import("std");

const XiStrArray = opaque {};
const Str = [*:0]const u8;
const OptStr = ?[*:0]const u8;

extern "c" fn malloc(usize) ?*anyopaque;
extern "c" fn free(?*anyopaque) void;
extern "c" fn memcpy(noalias ?*anyopaque, noalias ?*const anyopaque, usize) ?*anyopaque;

extern "c" fn stbi_load_from_memory(
    buffer: [*]const u8,
    len: c_int,
    x: *c_int,
    y: *c_int,
    channels_in_file: *c_int,
    desired_channels: c_int,
) ?[*]u8;
extern "c" fn stbi_image_free(data: ?*anyopaque) void;
extern "c" fn stbi_failure_reason() OptStr;

extern fn xi_arr_new() callconv(.c) *XiStrArray;
extern fn xi_arr_len_i64(arr: ?*XiStrArray) callconv(.c) i64;
extern fn xi_arr_get_uint_i64_at_i64_unchecked(arr: *XiStrArray, idx: i64) callconv(.c) i64;
extern fn xi_arr_from_bytes(data: ?[*]const u8, len: usize) callconv(.c) *XiStrArray;
extern fn xi_str_intern_cstr(s: OptStr) callconv(.c) Str;
extern fn xi_oom() callconv(.c) noreturn;

const packet_header_len: usize = 12;
const rgba_channels: c_int = 4;
const max_c_int: i64 = 2147483647;
var last_error: [256]u8 = std.mem.zeroes([256]u8);

fn setError(message: OptStr) void {
    last_error[0] = 0;
    const text = message orelse return;
    var i: usize = 0;
    while (i + 1 < last_error.len and text[i] != 0) : (i += 1) {
        last_error[i] = text[i];
    }
    last_error[i] = 0;
}

fn fail(message: OptStr) *XiStrArray {
    setError(message);
    return xi_arr_new();
}

fn writeU32Le(dst: [*]u8, value: u32) void {
    dst[0] = @truncate(value);
    dst[1] = @truncate(value >> 8);
    dst[2] = @truncate(value >> 16);
    dst[3] = @truncate(value >> 24);
}

export fn xi_image_decode(encoded: ?*XiStrArray) callconv(.c) *XiStrArray {
    last_error[0] = 0;
    const source = encoded orelse return fail("image data was null");
    const len_i64 = xi_arr_len_i64(source);
    if (len_i64 <= 0) return fail("image data was empty");
    if (len_i64 > max_c_int) {
        return fail("encoded image is too large to decode");
    }
    const len: usize = @intCast(len_i64);
    const input: [*]u8 = @ptrCast(malloc(len) orelse xi_oom());
    defer free(input);

    var i: usize = 0;
    while (i < len) : (i += 1) {
        input[i] = @intCast(xi_arr_get_uint_i64_at_i64_unchecked(source, @intCast(i)) & 0xff);
    }

    var width: c_int = 0;
    var height: c_int = 0;
    var source_channels: c_int = 0;
    const pixels = stbi_load_from_memory(
        input,
        @intCast(len),
        &width,
        &height,
        &source_channels,
        rgba_channels,
    ) orelse return fail(stbi_failure_reason());
    defer stbi_image_free(pixels);

    if (width <= 0 or height <= 0) return fail("decoded image has invalid dimensions");
    const pixel_count = @mulWithOverflow(@as(usize, @intCast(width)), @as(usize, @intCast(height)));
    if (pixel_count[1] != 0) return fail("decoded image dimensions overflow");
    const rgba_len = @mulWithOverflow(pixel_count[0], @as(usize, rgba_channels));
    if (rgba_len[1] != 0 or rgba_len[0] > (std_math_max_usize - packet_header_len)) {
        return fail("decoded image is too large");
    }
    const packet_len = packet_header_len + rgba_len[0];
    const packet: [*]u8 = @ptrCast(malloc(packet_len) orelse xi_oom());
    defer free(packet);

    writeU32Le(packet, @intCast(width));
    writeU32Le(packet + 4, @intCast(height));
    writeU32Le(packet + 8, @intCast(rgba_channels));
    _ = memcpy(packet + packet_header_len, pixels, rgba_len[0]);
    return xi_arr_from_bytes(packet, packet_len);
}

const std_math_max_usize = ~@as(usize, 0);

export fn xi_image_last_error() callconv(.c) Str {
    return xi_str_intern_cstr(@ptrCast(&last_error));
}
