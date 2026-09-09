const std = @import("std");
const builtin = @import("builtin");

extern "c" fn snprintf(noalias [*]u8, usize, noalias [*:0]const u8, ...) c_int;
extern "c" fn getenv([*:0]const u8) ?[*:0]const u8;
const XiTimeT = if (builtin.os.tag == .windows) i64 else c_long;
const XiTimespec = extern struct {
    tv_sec: XiTimeT,
    tv_nsec: c_long,
};
extern "kernel32" fn Sleep(c_ulong) callconv(.winapi) void;
extern "c" fn nanosleep(*const XiTimespec, ?*XiTimespec) c_int;
const XI_ERR_INVALID_ARGUMENT: c_int = 1;
const XiNum = opaque {};
const Str = [*:0]const u8;
const OptStr = ?[*:0]const u8;
const XI_NS_PER_US: i64 = 1000;
const XI_NS_PER_MS: i64 = 1000 * XI_NS_PER_US;
const XI_NS_PER_S: i64 = 1000 * XI_NS_PER_MS;
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

extern fn xi_append_cstr(buf: [*]u8, cap: usize, pos: *usize, s: [*:0]const u8) callconv(.c) void;
extern fn xi_err_set_message(code: c_int, message: OptStr) callconv(.c) void;
extern fn xi_mono_ns_i64() callconv(.c) i64;
extern fn xi_num_i64(v: i64) callconv(.c) *XiNum;
extern fn xi_num_to_i64(n: ?*XiNum) callconv(.c) i64;
extern fn xi_str_intern_bytes(s: ?[*]const u8, len_in: usize) callconv(.c) Str;
extern fn xi_str_intern_cstr(s: OptStr) callconv(.c) Str;
extern fn xi_str_len_known(s: OptStr) callconv(.c) usize;
extern fn xi_wall_time_ns() callconv(.c) i64;


fn xi_floor_div(a: i64, b: i64) i64 {
    var q = @divTrunc(a, b);
    const r = @rem(a, b);
    if (r != 0 and ((r < 0) != (b < 0))) q -= 1;
    return q;
}

fn xi_pos_mod(a: i64, b: i64) i64 {
    var r = @rem(a, b);
    if (r < 0) r += b;
    return r;
}

fn xi_is_leap_year_i64(year: i64) bool {
    return @rem(year, 4) == 0 and (@rem(year, 100) != 0 or @rem(year, 400) == 0);
}

fn xi_days_in_month_i64(year: i64, month: i64) i64 {
    return switch (month) {
        1, 3, 5, 7, 8, 10, 12 => 31,
        4, 6, 9, 11 => 30,
        2 => if (xi_is_leap_year_i64(year)) 29 else 28,
        else => 0,
    };
}

fn xi_days_from_civil(year_in: i64, month_in: i64, day: i64) i64 {
    var y = year_in;
    const m = month_in;
    y -= if (m <= 2) 1 else 0;
    const era = if (y >= 0) @divTrunc(y, 400) else @divTrunc(y - 399, 400);
    const yoe = y - era * 400;
    const mp = m + if (m > 2) @as(i64, -3) else @as(i64, 9);
    const doy = @divTrunc(153 * mp + 2, 5) + day - 1;
    const doe = yoe * 365 + @divTrunc(yoe, 4) - @divTrunc(yoe, 100) + doy;
    return era * 146097 + doe - 719468;
}

fn xi_civil_from_days(days_in: i64) XiDateTime {
    const z = days_in + 719468;
    const era = if (z >= 0) @divTrunc(z, 146097) else @divTrunc(z - 146096, 146097);
    const doe = z - era * 146097;
    const yoe = @divTrunc(doe - @divTrunc(doe, 1460) + @divTrunc(doe, 36524) - @divTrunc(doe, 146096), 365);
    var y = yoe + era * 400;
    const doy = doe - (365 * yoe + @divTrunc(yoe, 4) - @divTrunc(yoe, 100));
    const mp = @divTrunc(5 * doy + 2, 153);
    const d = doy - @divTrunc(153 * mp + 2, 5) + 1;
    const m = mp + if (mp < 10) @as(i64, 3) else -9;
    y += if (m <= 2) 1 else 0;
    return .{
        .year = y,
        .month = m,
        .day = d,
        .hour = 0,
        .minute = 0,
        .second = 0,
        .weekday = xi_pos_mod(days_in + 4, 7),
        .yearday = days_in - xi_days_from_civil(y, 1, 1) + 1,
    };
}

fn xi_datetime_from_unix(seconds: i64) XiDateTime {
    const days = xi_floor_div(seconds, 86400);
    var rem = seconds - days * 86400;
    if (rem < 0) rem += 86400;
    var dt = xi_civil_from_days(days);
    dt.hour = @divTrunc(rem, 3600);
    rem = @rem(rem, 3600);
    dt.minute = @divTrunc(rem, 60);
    dt.second = @rem(rem, 60);
    return dt;
}

fn xi_make_unix_i64(year: i64, month: i64, day: i64, hour: i64, minute: i64, second: i64) i64 {
    if (month < 1 or month > 12 or day < 1 or day > xi_days_in_month_i64(year, month)) return 0;
    if (hour < 0 or hour > 23 or minute < 0 or minute > 59 or second < 0 or second > 60) return 0;
    return xi_days_from_civil(year, month, day) * 86400 + hour * 3600 + minute * 60 + second;
}

fn xi_sleep_ns_i64(ns_in: i64) void {
    if (ns_in <= 0) return;
    if (builtin.os.tag == .windows) {
        const ms = @divTrunc(ns_in + XI_NS_PER_MS - 1, XI_NS_PER_MS);
        Sleep(@intCast(if (ms > std.math.maxInt(c_ulong)) std.math.maxInt(c_ulong) else ms));
        return;
    }
    var ts = XiTimespec{ .tv_sec = @intCast(@divTrunc(ns_in, XI_NS_PER_S)), .tv_nsec = @intCast(@rem(ns_in, XI_NS_PER_S)) };
    while (nanosleep(&ts, &ts) != 0) {}
}

fn xi_append_int(buf: [*]u8, cap: usize, pos: *usize, value: i64, width: c_int) void {
    var tmp: [64]u8 = undefined;
    _ = snprintf(&tmp, tmp.len, "%0*lld", width, @as(c_longlong, value));
    xi_append_cstr(buf, cap, pos, @ptrCast(&tmp));
}

fn xi_format_unix_into(seconds: i64, fmt: OptStr, out: [*]u8, cap: usize) usize {
    const dt = xi_datetime_from_unix(seconds);
    const f = fmt orelse "%Y-%m-%dT%H:%M:%SZ";
    const flen = xi_str_len_known(f);
    var pos: usize = 0;
    var i: usize = 0;
    while (i < flen and pos + 1 < cap) : (i += 1) {
        if (f[i] != '%' or i + 1 >= flen) {
            out[pos] = f[i];
            pos += 1;
            continue;
        }
        i += 1;
        switch (f[i]) {
            '%' => {
                out[pos] = '%';
                pos += 1;
            },
            'Y' => xi_append_int(out, cap, &pos, dt.year, 4),
            'm' => xi_append_int(out, cap, &pos, dt.month, 2),
            'd' => xi_append_int(out, cap, &pos, dt.day, 2),
            'H' => xi_append_int(out, cap, &pos, dt.hour, 2),
            'M' => xi_append_int(out, cap, &pos, dt.minute, 2),
            'S' => xi_append_int(out, cap, &pos, dt.second, 2),
            'j' => xi_append_int(out, cap, &pos, dt.yearday, 3),
            'w' => xi_append_int(out, cap, &pos, dt.weekday, 1),
            'u' => xi_append_int(out, cap, &pos, if (dt.weekday == 0) 7 else dt.weekday, 1),
            's' => xi_append_int(out, cap, &pos, seconds, 1),
            'F' => {
                xi_append_int(out, cap, &pos, dt.year, 4);
                xi_append_cstr(out, cap, &pos, "-");
                xi_append_int(out, cap, &pos, dt.month, 2);
                xi_append_cstr(out, cap, &pos, "-");
                xi_append_int(out, cap, &pos, dt.day, 2);
            },
            'T' => {
                xi_append_int(out, cap, &pos, dt.hour, 2);
                xi_append_cstr(out, cap, &pos, ":");
                xi_append_int(out, cap, &pos, dt.minute, 2);
                xi_append_cstr(out, cap, &pos, ":");
                xi_append_int(out, cap, &pos, dt.second, 2);
            },
            else => |ch| {
                out[pos] = ch;
                pos += 1;
            },
        }
    }
    out[pos] = 0;
    return pos;
}

fn xi_parse_int_width(text: [*]const u8, len: usize, pos: *usize, width: usize) ?i64 {
    if (pos.* + width > len) return null;
    var value: i64 = 0;
    var i: usize = 0;
    while (i < width) : (i += 1) {
        const ch = text[pos.* + i];
        if (ch < '0' or ch > '9') return null;
        value = value * 10 + @as(i64, ch - '0');
    }
    pos.* += width;
    return value;
}

fn xi_parse_unix_custom(text: OptStr, fmt: OptStr) i64 {
    const t = text orelse return 0;
    const f = fmt orelse "%Y-%m-%dT%H:%M:%SZ";
    const tlen = xi_str_len_known(t);
    const flen = xi_str_len_known(f);
    var ti: usize = 0;
    var fi: usize = 0;
    var year: i64 = 1970;
    var month: i64 = 1;
    var day: i64 = 1;
    var hour: i64 = 0;
    var minute: i64 = 0;
    var second: i64 = 0;
    while (fi < flen) : (fi += 1) {
        if (f[fi] != '%' or fi + 1 >= flen) {
            if (ti >= tlen or t[ti] != f[fi]) return 0;
            ti += 1;
            continue;
        }
        fi += 1;
        switch (f[fi]) {
            'Y' => year = xi_parse_int_width(t, tlen, &ti, 4) orelse return 0,
            'm' => month = xi_parse_int_width(t, tlen, &ti, 2) orelse return 0,
            'd' => day = xi_parse_int_width(t, tlen, &ti, 2) orelse return 0,
            'H' => hour = xi_parse_int_width(t, tlen, &ti, 2) orelse return 0,
            'M' => minute = xi_parse_int_width(t, tlen, &ti, 2) orelse return 0,
            'S' => second = xi_parse_int_width(t, tlen, &ti, 2) orelse return 0,
            'F' => {
                year = xi_parse_int_width(t, tlen, &ti, 4) orelse return 0;
                if (ti >= tlen or t[ti] != '-') return 0;
                ti += 1;
                month = xi_parse_int_width(t, tlen, &ti, 2) orelse return 0;
                if (ti >= tlen or t[ti] != '-') return 0;
                ti += 1;
                day = xi_parse_int_width(t, tlen, &ti, 2) orelse return 0;
            },
            'T' => {
                hour = xi_parse_int_width(t, tlen, &ti, 2) orelse return 0;
                if (ti >= tlen or t[ti] != ':') return 0;
                ti += 1;
                minute = xi_parse_int_width(t, tlen, &ti, 2) orelse return 0;
                if (ti >= tlen or t[ti] != ':') return 0;
                ti += 1;
                second = xi_parse_int_width(t, tlen, &ti, 2) orelse return 0;
            },
            '%' => {
                if (ti >= tlen or t[ti] != '%') return 0;
                ti += 1;
            },
            else => return 0,
        }
    }
    return xi_make_unix_i64(year, month, day, hour, minute, second);
}

export fn xi_time_now() callconv(.c) *XiNum {
    return xi_num_i64(@divTrunc(xi_wall_time_ns(), XI_NS_PER_S));
}

export fn xi_time_now_unix() callconv(.c) *XiNum {
    return xi_time_now();
}

export fn xi_time_now_unix_ms() callconv(.c) *XiNum {
    return xi_num_i64(@divTrunc(xi_wall_time_ns(), XI_NS_PER_MS));
}

export fn xi_time_now_unix_us() callconv(.c) *XiNum {
    return xi_num_i64(@divTrunc(xi_wall_time_ns(), XI_NS_PER_US));
}

export fn xi_time_now_unix_ns() callconv(.c) *XiNum {
    return xi_num_i64(xi_wall_time_ns());
}

export fn xi_time_mono() callconv(.c) *XiNum {
    return xi_num_i64(@divTrunc(xi_mono_ns_i64(), XI_NS_PER_S));
}

export fn xi_time_mono_ms() callconv(.c) *XiNum {
    return xi_num_i64(@divTrunc(xi_mono_ns_i64(), XI_NS_PER_MS));
}

export fn xi_time_mono_us() callconv(.c) *XiNum {
    return xi_num_i64(@divTrunc(xi_mono_ns_i64(), XI_NS_PER_US));
}

export fn xi_time_mono_ns() callconv(.c) *XiNum {
    return xi_num_i64(xi_mono_ns_i64());
}

export fn xi_time_sleep(seconds: ?*XiNum) callconv(.c) *XiNum {
    xi_sleep_ns_i64(xi_num_to_i64(seconds) * XI_NS_PER_S);
    return xi_num_i64(0);
}

export fn xi_time_sleep_ms(ms: ?*XiNum) callconv(.c) *XiNum {
    xi_sleep_ns_i64(xi_num_to_i64(ms) * XI_NS_PER_MS);
    return xi_num_i64(0);
}

export fn xi_time_sleep_us(us: ?*XiNum) callconv(.c) *XiNum {
    xi_sleep_ns_i64(xi_num_to_i64(us) * XI_NS_PER_US);
    return xi_num_i64(0);
}

export fn xi_time_sleep_ns(ns: ?*XiNum) callconv(.c) *XiNum {
    xi_sleep_ns_i64(xi_num_to_i64(ns));
    return xi_num_i64(0);
}

export fn xi_time_sleep_until(unix_ms: ?*XiNum) callconv(.c) *XiNum {
    const target = xi_num_to_i64(unix_ms);
    const now_ms = @divTrunc(xi_wall_time_ns(), XI_NS_PER_MS);
    if (target > now_ms) xi_sleep_ns_i64((target - now_ms) * XI_NS_PER_MS);
    return xi_num_i64(0);
}

export fn xi_time_timer_start() callconv(.c) *XiNum {
    return xi_num_i64(xi_mono_ns_i64());
}

export fn xi_time_timer_elapsed(timer: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(@divTrunc(xi_mono_ns_i64() - xi_num_to_i64(timer), XI_NS_PER_S));
}

export fn xi_time_timer_elapsed_ms(timer: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(@divTrunc(xi_mono_ns_i64() - xi_num_to_i64(timer), XI_NS_PER_MS));
}

export fn xi_time_timer_elapsed_us(timer: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(@divTrunc(xi_mono_ns_i64() - xi_num_to_i64(timer), XI_NS_PER_US));
}

export fn xi_time_timer_elapsed_ns(timer: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_mono_ns_i64() - xi_num_to_i64(timer));
}

export fn xi_time_timer_reset(timer_slot: ?*?*XiNum) callconv(.c) *XiNum {
    const now = xi_time_timer_start();
    if (timer_slot) |slot| slot.* = now else xi_err_set_message(XI_ERR_INVALID_ARGUMENT, "timer reference was null");
    return now;
}

export fn xi_time_deadline_after_ms(ms: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_mono_ns_i64() + xi_num_to_i64(ms) * XI_NS_PER_MS);
}

export fn xi_time_deadline_after_us(us: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_mono_ns_i64() + xi_num_to_i64(us) * XI_NS_PER_US);
}

export fn xi_time_deadline_after_ns(ns: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_mono_ns_i64() + xi_num_to_i64(ns));
}

export fn xi_time_deadline_expired(deadline: ?*XiNum) callconv(.c) bool {
    return xi_mono_ns_i64() >= xi_num_to_i64(deadline);
}

export fn xi_time_deadline_remaining_ms(deadline: ?*XiNum) callconv(.c) *XiNum {
    const rem = xi_num_to_i64(deadline) - xi_mono_ns_i64();
    return xi_num_i64(if (rem <= 0) 0 else @divTrunc(rem, XI_NS_PER_MS));
}

export fn xi_time_deadline_remaining_ns(deadline: ?*XiNum) callconv(.c) *XiNum {
    const rem = xi_num_to_i64(deadline) - xi_mono_ns_i64();
    return xi_num_i64(if (rem <= 0) 0 else rem);
}

export fn xi_time_iso_now() callconv(.c) Str {
    return xi_time_iso_from_unix(xi_time_now());
}

export fn xi_time_iso_from_unix(seconds: ?*XiNum) callconv(.c) Str {
    var buf: [64]u8 = undefined;
    const len = xi_format_unix_into(xi_num_to_i64(seconds), "%Y-%m-%dT%H:%M:%SZ", &buf, buf.len);
    return xi_str_intern_bytes(&buf, len);
}

export fn xi_time_format_unix(seconds: ?*XiNum, fmt: OptStr) callconv(.c) Str {
    var buf: [512]u8 = undefined;
    const len = xi_format_unix_into(xi_num_to_i64(seconds), fmt, &buf, buf.len);
    return xi_str_intern_bytes(&buf, len);
}

export fn xi_time_parse_unix(text: OptStr, fmt: OptStr) callconv(.c) *XiNum {
    return xi_num_i64(xi_parse_unix_custom(text, fmt));
}

export fn xi_time_year_from_unix(seconds: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_datetime_from_unix(xi_num_to_i64(seconds)).year);
}

export fn xi_time_month_from_unix(seconds: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_datetime_from_unix(xi_num_to_i64(seconds)).month);
}

export fn xi_time_day_from_unix(seconds: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_datetime_from_unix(xi_num_to_i64(seconds)).day);
}

export fn xi_time_hour_from_unix(seconds: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_datetime_from_unix(xi_num_to_i64(seconds)).hour);
}

export fn xi_time_minute_from_unix(seconds: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_datetime_from_unix(xi_num_to_i64(seconds)).minute);
}

export fn xi_time_second_from_unix(seconds: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_datetime_from_unix(xi_num_to_i64(seconds)).second);
}

export fn xi_time_weekday_from_unix(seconds: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_datetime_from_unix(xi_num_to_i64(seconds)).weekday);
}

export fn xi_time_yearday_from_unix(seconds: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_datetime_from_unix(xi_num_to_i64(seconds)).yearday);
}

export fn xi_time_year() callconv(.c) *XiNum {
    return xi_time_year_from_unix(xi_time_now());
}

export fn xi_time_month() callconv(.c) *XiNum {
    return xi_time_month_from_unix(xi_time_now());
}

export fn xi_time_day() callconv(.c) *XiNum {
    return xi_time_day_from_unix(xi_time_now());
}

export fn xi_time_hour() callconv(.c) *XiNum {
    return xi_time_hour_from_unix(xi_time_now());
}

export fn xi_time_minute() callconv(.c) *XiNum {
    return xi_time_minute_from_unix(xi_time_now());
}

export fn xi_time_second() callconv(.c) *XiNum {
    return xi_time_second_from_unix(xi_time_now());
}

export fn xi_time_weekday() callconv(.c) *XiNum {
    return xi_time_weekday_from_unix(xi_time_now());
}

export fn xi_time_yearday() callconv(.c) *XiNum {
    return xi_time_yearday_from_unix(xi_time_now());
}

export fn xi_time_is_leap_year(year: ?*XiNum) callconv(.c) bool {
    return xi_is_leap_year_i64(xi_num_to_i64(year));
}

export fn xi_time_days_in_month(year: ?*XiNum, month: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_days_in_month_i64(xi_num_to_i64(year), xi_num_to_i64(month)));
}

export fn xi_time_make_unix(year: ?*XiNum, month: ?*XiNum, day: ?*XiNum, hour: ?*XiNum, minute: ?*XiNum, second: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_make_unix_i64(xi_num_to_i64(year), xi_num_to_i64(month), xi_num_to_i64(day), xi_num_to_i64(hour), xi_num_to_i64(minute), xi_num_to_i64(second)));
}

export fn xi_time_add_seconds(unix_seconds: ?*XiNum, seconds: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_num_to_i64(unix_seconds) + xi_num_to_i64(seconds));
}

export fn xi_time_add_minutes(unix_seconds: ?*XiNum, minutes: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_num_to_i64(unix_seconds) + xi_num_to_i64(minutes) * 60);
}

export fn xi_time_add_hours(unix_seconds: ?*XiNum, hours: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_num_to_i64(unix_seconds) + xi_num_to_i64(hours) * 3600);
}

export fn xi_time_add_days(unix_seconds: ?*XiNum, days: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_num_to_i64(unix_seconds) + xi_num_to_i64(days) * 86400);
}

export fn xi_time_diff_seconds(unix_a: ?*XiNum, unix_b: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_num_to_i64(unix_a) - xi_num_to_i64(unix_b));
}

export fn xi_time_diff_ms(unix_ms_a: ?*XiNum, unix_ms_b: ?*XiNum) callconv(.c) *XiNum {
    return xi_num_i64(xi_num_to_i64(unix_ms_a) - xi_num_to_i64(unix_ms_b));
}

export fn xi_time_timezone_name() callconv(.c) Str {
    if (getenv("TZ")) |tz| return xi_str_intern_cstr(tz);
    return xi_str_intern_cstr("UTC");
}

export fn xi_time_timezone_offset_seconds() callconv(.c) *XiNum {
    return xi_num_i64(0);
}

export fn xi_time_utc_offset_seconds() callconv(.c) *XiNum {
    return xi_time_timezone_offset_seconds();
}
