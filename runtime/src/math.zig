var xi_sym_out: [XI_SYM_MAX_OUTPUT]u8 = undefined;
var xi_sym_sort_var_count: usize = 0;

const FILE = opaque {};
extern "c" fn strcmp([*:0]const u8, [*:0]const u8) c_int;
extern "c" fn strcspn([*:0]const u8, [*:0]const u8) usize;
extern "c" fn strtod(noalias [*:0]const u8, noalias ?*?[*:0]u8) f64;
extern "c" fn snprintf(noalias [*]u8, usize, noalias [*:0]const u8, ...) c_int;
extern "c" fn qsort(?*anyopaque, usize, usize, *const fn (?*const anyopaque, ?*const anyopaque) callconv(.c) c_int) void;
extern "c" fn fgets([*]u8, c_int, *FILE) ?[*]u8;
extern "c" fn fflush(?*FILE) c_int;
extern "c" fn printf(noalias [*:0]const u8, ...) c_int;
extern "c" fn fprintf(noalias *FILE, noalias [*:0]const u8, ...) c_int;
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
extern "c" fn llround(f64) c_longlong;
const XiNum = opaque {};
const Str = [*:0]const u8;
const OptStr = ?[*:0]const u8;
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

extern fn isalpha(c: u8) callconv(.c) bool;
extern fn stderrFile() callconv(.c) *FILE;
extern fn stdinFile() callconv(.c) *FILE;
extern fn stdoutFile() callconv(.c) *FILE;
extern fn xi_as_f64(n: *const XiNum) callconv(.c) f64;
extern fn xi_io_ask() callconv(.c) Str;
extern fn xi_num_make_dec(v: f64) callconv(.c) *XiNum;
extern fn xi_num_pow(a: *XiNum, b: *XiNum) callconv(.c) *XiNum;
extern fn xi_str_dup(s: OptStr) callconv(.c) Str;


fn isspace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == '\x0b' or c == '\x0c';
}

fn isdigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isalnum(c: u8) bool {
    return isdigit(c) or isalpha(c);
}

export fn xi_math_pi() callconv(.c) *XiNum {
    return xi_num_make_dec(3.14159265358979323846);
}

export fn xi_math_e() callconv(.c) *XiNum {
    return xi_num_make_dec(2.71828182845904523536);
}

export fn xi_math_tau() callconv(.c) *XiNum {
    return xi_num_make_dec(6.28318530717958647692);
}

export fn xi_math_sin(x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(sin(xi_as_f64(x)));
}

export fn xi_math_cos(x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(cos(xi_as_f64(x)));
}

export fn xi_math_tan(x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(tan(xi_as_f64(x)));
}

export fn xi_math_sqrt(x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(sqrt(xi_as_f64(x)));
}

export fn xi_math_abs(x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(fabs(xi_as_f64(x)));
}

export fn xi_math_floor(x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(floor(xi_as_f64(x)));
}

export fn xi_math_ceil(x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(ceil(xi_as_f64(x)));
}

export fn xi_math_round(x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(round(xi_as_f64(x)));
}

export fn xi_math_trunc(x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(trunc(xi_as_f64(x)));
}

export fn xi_math_log(x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(log(xi_as_f64(x)));
}

export fn xi_math_log10(x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(log10(xi_as_f64(x)));
}

export fn xi_math_log2(x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(log2(xi_as_f64(x)));
}

export fn xi_math_exp(x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(c_exp(xi_as_f64(x)));
}

export fn xi_math_pow(x: *XiNum, y: *XiNum) callconv(.c) *XiNum {
    return xi_num_pow(x, y);
}

export fn xi_math_min(x: *XiNum, y: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(fmin(xi_as_f64(x), xi_as_f64(y)));
}

export fn xi_math_max(x: *XiNum, y: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(fmax(xi_as_f64(x), xi_as_f64(y)));
}

export fn xi_math_atan2(y: *XiNum, x: *XiNum) callconv(.c) *XiNum {
    return xi_num_make_dec(atan2(xi_as_f64(y), xi_as_f64(x)));
}

fn xi_expr_skip_ws(p: *XiExprParser) void {
    while (p.input[p.pos] != 0 and isspace(p.input[p.pos])) p.pos += 1;
}

fn xi_expr_peek(p: *XiExprParser) u8 {
    xi_expr_skip_ws(p);
    return p.input[p.pos];
}

fn xi_expr_consume(p: *XiExprParser, expected: u8) bool {
    if (xi_expr_peek(p) == expected) {
        p.pos += 1;
        return true;
    }
    return false;
}

fn xi_prompt_variable_value(name: [*:0]const u8) f64 {
    var line: [256]u8 = undefined;
    while (true) {
        _ = printf("Value for %s: ", name);
        _ = fflush(stdoutFile());
        if (fgets(&line, line.len, stdinFile()) == null) return 0.0;
        var start: [*:0]const u8 = @ptrCast(&line);
        while (start[0] != 0 and isspace(start[0])) start += 1;
        var end: ?[*:0]u8 = null;
        const value = strtod(start, &end);
        if (end != null and @intFromPtr(end.?) != @intFromPtr(start)) {
            var e = end.?;
            while (e[0] != 0 and isspace(e[0])) e += 1;
            if (e[0] == 0) return value;
        }
        _ = fprintf(stderrFile(), "xi_runtime: expected a numeric value for `%s`\n", name);
    }
}

fn xi_lookup_or_prompt_var(p: *XiExprParser, name: [*:0]const u8) f64 {
    var i: usize = 0;
    while (i < p.var_count) : (i += 1) {
        if (strcmp(@ptrCast(&p.vars[i].name), name) == 0) return p.vars[i].value;
    }
    if (p.var_count >= p.vars.len) {
        _ = fprintf(stderrFile(), "xi_runtime: too many variables in expression\n");
        p.had_error = 1;
        return 0.0;
    }
    const entry = &p.vars[p.var_count];
    p.var_count += 1;
    _ = snprintf(&entry.name, entry.name.len, "%s", name);
    entry.value = xi_prompt_variable_value(@ptrCast(&entry.name));
    return entry.value;
}

fn xi_is_primary_start(c: u8) bool {
    return c == '(' or c == '.' or isdigit(c) or isalpha(c) or c == '_';
}

fn xi_parse_primary(p: *XiExprParser) f64 {
    const c = xi_expr_peek(p);
    if (c == 0) {
        _ = fprintf(stderrFile(), "xi_runtime: unexpected end of expression\n");
        p.had_error = 1;
        return 0.0;
    }
    if (c == '(') {
        p.pos += 1;
        const value = xi_parse_expr(p);
        if (!xi_expr_consume(p, ')')) {
            _ = fprintf(stderrFile(), "xi_runtime: expected `)` in expression\n");
            p.had_error = 1;
            return 0.0;
        }
        return value;
    }
    if (isdigit(c) or c == '.') {
        const start: [*:0]const u8 = p.input + p.pos;
        var end: ?[*:0]u8 = null;
        const value = strtod(start, &end);
        if (end == null or @intFromPtr(end.?) == @intFromPtr(start)) {
            _ = fprintf(stderrFile(), "xi_runtime: invalid number literal\n");
            p.had_error = 1;
            return 0.0;
        }
        p.pos += @intFromPtr(end.?) - @intFromPtr(start);
        return value;
    }
    if (isalpha(c) or c == '_') {
        var name: [64]u8 = undefined;
        var len: usize = 0;
        while (isalnum(p.input[p.pos]) or p.input[p.pos] == '_') {
            if (len + 1 < name.len) {
                name[len] = p.input[p.pos];
                len += 1;
            }
            p.pos += 1;
        }
        name[len] = 0;
        return xi_lookup_or_prompt_var(p, @ptrCast(&name));
    }
    _ = fprintf(stderrFile(), "xi_runtime: unexpected token `%c` in expression\n", @as(c_int, c));
    p.had_error = 1;
    return 0.0;
}

fn xi_parse_unary(p: *XiExprParser) f64 {
    if (xi_expr_consume(p, '+')) return xi_parse_unary(p);
    if (xi_expr_consume(p, '-')) return -xi_parse_unary(p);
    return xi_parse_primary(p);
}

fn xi_parse_power(p: *XiExprParser) f64 {
    const lhs = xi_parse_unary(p);
    if (xi_expr_consume(p, '^')) {
        const rhs = xi_parse_power(p);
        return pow(lhs, rhs);
    }
    return lhs;
}

fn xi_parse_implicit_product(p: *XiExprParser) f64 {
    var lhs = xi_parse_power(p);
    while (p.had_error == 0) {
        const c = xi_expr_peek(p);
        if (!xi_is_primary_start(c)) break;
        lhs *= xi_parse_power(p);
    }
    return lhs;
}

fn xi_parse_term(p: *XiExprParser) f64 {
    var lhs = xi_parse_implicit_product(p);
    while (p.had_error == 0) {
        const c = xi_expr_peek(p);
        if (c == '*') {
            p.pos += 1;
            lhs *= xi_parse_implicit_product(p);
            continue;
        }
        if (c == '/') {
            p.pos += 1;
            lhs /= xi_parse_implicit_product(p);
            continue;
        }
        break;
    }
    return lhs;
}

fn xi_parse_expr(p: *XiExprParser) f64 {
    var lhs = xi_parse_term(p);
    while (p.had_error == 0) {
        const c = xi_expr_peek(p);
        if (c == '+') {
            p.pos += 1;
            lhs += xi_parse_term(p);
            continue;
        }
        if (c == '-') {
            p.pos += 1;
            lhs -= xi_parse_term(p);
            continue;
        }
        break;
    }
    return lhs;
}

export fn xi_math_eval_expr_input() callconv(.c) *XiNum {
    var line: [2048]u8 = undefined;
    _ = fflush(stdoutFile());
    if (fgets(&line, line.len, stdinFile()) == null) return xi_num_make_dec(0.0);
    line[strcspn(@ptrCast(&line), "\r\n")] = 0;
    var parser: XiExprParser = undefined;
    parser.input = @ptrCast(&line);
    parser.pos = 0;
    parser.var_count = 0;
    parser.had_error = 0;
    const value = xi_parse_expr(&parser);
    if (parser.had_error == 0 and xi_expr_peek(&parser) != 0) {
        _ = fprintf(stderrFile(), "xi_runtime: unexpected trailing input near `%s`\n", parser.input + parser.pos);
        parser.had_error = 1;
    }
    if (parser.had_error != 0) return xi_num_make_dec(0.0);
    return xi_num_make_dec(value);
}

fn xi_sym_poly_zero(poly: *XiSymPoly) void {
    poly.count = 0;
}

fn xi_sym_is_zero(value: f64) bool {
    return fabs(value) < XI_SYM_EPS;
}

fn xi_sym_is_integer(value: f64) bool {
    const rounded = round(value);
    return fabs(value - rounded) < 1e-9;
}

fn xi_sym_set_error(p: *XiSymParser, message: [*:0]const u8) void {
    if (p.had_error == 0) _ = fprintf(stderrFile(), "xi_runtime: %s\n", message);
    p.had_error = 1;
}

fn xi_sym_same_exps(a: *const XiSymTerm, b: *const XiSymTerm) bool {
    var i: usize = 0;
    while (i < XI_SYM_MAX_VARS) : (i += 1) {
        if (a.exps[i] != b.exps[i]) return false;
    }
    return true;
}

fn xi_sym_remove_term_at(poly: *XiSymPoly, idx: usize) void {
    if (idx >= poly.count) return;
    var i = idx + 1;
    while (i < poly.count) : (i += 1) poly.terms[i - 1] = poly.terms[i];
    poly.count -= 1;
}

fn xi_sym_poly_add_term(poly: *XiSymPoly, coeff: f64, exps: *const [XI_SYM_MAX_VARS]u16, parser: *XiSymParser) void {
    if (xi_sym_is_zero(coeff) or parser.had_error != 0) return;
    var candidate: XiSymTerm = undefined;
    candidate.coeff = coeff;
    var i: usize = 0;
    while (i < XI_SYM_MAX_VARS) : (i += 1) candidate.exps[i] = exps[i];
    i = 0;
    while (i < poly.count) : (i += 1) {
        if (xi_sym_same_exps(&poly.terms[i], &candidate)) {
            poly.terms[i].coeff += coeff;
            if (xi_sym_is_zero(poly.terms[i].coeff)) xi_sym_remove_term_at(poly, i);
            return;
        }
    }
    if (poly.count >= XI_SYM_MAX_TERMS) {
        xi_sym_set_error(parser, "symbolic polynomial is too large");
        return;
    }
    poly.terms[poly.count] = candidate;
    poly.count += 1;
}

fn xi_sym_poly_from_const(value: f64) XiSymPoly {
    var poly: XiSymPoly = undefined;
    xi_sym_poly_zero(&poly);
    if (!xi_sym_is_zero(value)) {
        var term: XiSymTerm = undefined;
        term.coeff = value;
        var i: usize = 0;
        while (i < XI_SYM_MAX_VARS) : (i += 1) term.exps[i] = 0;
        poly.terms[poly.count] = term;
        poly.count += 1;
    }
    return poly;
}

fn xi_sym_poly_from_var(var_index: u16) XiSymPoly {
    var poly: XiSymPoly = undefined;
    xi_sym_poly_zero(&poly);
    var term: XiSymTerm = undefined;
    term.coeff = 1.0;
    var i: usize = 0;
    while (i < XI_SYM_MAX_VARS) : (i += 1) term.exps[i] = 0;
    if (var_index < XI_SYM_MAX_VARS) term.exps[var_index] = 1;
    poly.terms[poly.count] = term;
    poly.count += 1;
    return poly;
}

fn xi_sym_poly_add(a: *const XiSymPoly, b: *const XiSymPoly, parser: *XiSymParser, b_scale: f64) XiSymPoly {
    var out: XiSymPoly = undefined;
    xi_sym_poly_zero(&out);
    var i: usize = 0;
    while (i < a.count) : (i += 1) xi_sym_poly_add_term(&out, a.terms[i].coeff, &a.terms[i].exps, parser);
    i = 0;
    while (i < b.count) : (i += 1) xi_sym_poly_add_term(&out, b.terms[i].coeff * b_scale, &b.terms[i].exps, parser);
    return out;
}

fn xi_sym_poly_mul(a: *const XiSymPoly, b: *const XiSymPoly, parser: *XiSymParser) XiSymPoly {
    var out: XiSymPoly = undefined;
    xi_sym_poly_zero(&out);
    var i: usize = 0;
    while (i < a.count) : (i += 1) {
        var j: usize = 0;
        while (j < b.count) : (j += 1) {
            const coeff = a.terms[i].coeff * b.terms[j].coeff;
            var exps: [XI_SYM_MAX_VARS]u16 = undefined;
            var k: usize = 0;
            while (k < XI_SYM_MAX_VARS) : (k += 1) {
                const sum: u32 = @as(u32, a.terms[i].exps[k]) + @as(u32, b.terms[j].exps[k]);
                if (sum > 65535) {
                    xi_sym_set_error(parser, "symbolic exponent overflow");
                    return out;
                }
                exps[k] = @intCast(sum);
            }
            xi_sym_poly_add_term(&out, coeff, &exps, parser);
            if (parser.had_error != 0) return out;
        }
    }
    return out;
}

fn xi_sym_poly_as_nonneg_int(poly: *const XiSymPoly, out_exp: *c_int) bool {
    if (poly.count == 0) {
        out_exp.* = 0;
        return true;
    }
    if (poly.count != 1) return false;
    const t = &poly.terms[0];
    var i: usize = 0;
    while (i < XI_SYM_MAX_VARS) : (i += 1) {
        if (t.exps[i] != 0) return false;
    }
    if (!xi_sym_is_integer(t.coeff)) return false;
    const value: c_longlong = llround(t.coeff);
    if (value < 0 or value > 1024) return false;
    out_exp.* = @intCast(value);
    return true;
}

fn xi_sym_poly_pow(base: *const XiSymPoly, exp_v: c_int, parser: *XiSymParser) XiSymPoly {
    var result = xi_sym_poly_from_const(1.0);
    var factor = base.*;
    var power = exp_v;
    while (power > 0 and parser.had_error == 0) {
        if (power & 1 != 0) result = xi_sym_poly_mul(&result, &factor, parser);
        power >>= 1;
        if (power > 0) factor = xi_sym_poly_mul(&factor, &factor, parser);
    }
    return result;
}

fn xi_sym_poly_div(lhs: *const XiSymPoly, rhs: *const XiSymPoly, parser: *XiSymParser) XiSymPoly {
    var out: XiSymPoly = undefined;
    xi_sym_poly_zero(&out);
    if (rhs.count != 1) {
        xi_sym_set_error(parser, "symbolic division supports only monomial divisors");
        return out;
    }
    const div = &rhs.terms[0];
    if (xi_sym_is_zero(div.coeff)) {
        xi_sym_set_error(parser, "division by zero polynomial");
        return out;
    }
    var i: usize = 0;
    while (i < lhs.count) : (i += 1) {
        const t = &lhs.terms[i];
        var exps: [XI_SYM_MAX_VARS]u16 = undefined;
        var k: usize = 0;
        while (k < XI_SYM_MAX_VARS) : (k += 1) {
            if (t.exps[k] < div.exps[k]) {
                xi_sym_set_error(parser, "division would produce non-polynomial negative exponents");
                return out;
            }
            exps[k] = t.exps[k] - div.exps[k];
        }
        xi_sym_poly_add_term(&out, t.coeff / div.coeff, &exps, parser);
        if (parser.had_error != 0) return out;
    }
    return out;
}

fn xi_sym_skip_ws(p: *XiSymParser) void {
    while (p.input[p.pos] != 0 and isspace(p.input[p.pos])) p.pos += 1;
}

fn xi_sym_peek(p: *XiSymParser) u8 {
    xi_sym_skip_ws(p);
    return p.input[p.pos];
}

fn xi_sym_consume(p: *XiSymParser, expected: u8) bool {
    if (xi_sym_peek(p) == expected) {
        p.pos += 1;
        return true;
    }
    return false;
}

fn xi_sym_is_primary_start(c: u8) bool {
    return c == '(' or c == '.' or isdigit(c) or isalpha(c) or c == '_';
}

fn xi_sym_var_index(p: *XiSymParser, name: [*:0]const u8) c_int {
    var i: usize = 0;
    while (i < p.var_count) : (i += 1) {
        if (strcmp(@ptrCast(&p.var_names[i]), name) == 0) return @intCast(i);
    }
    if (p.var_count >= XI_SYM_MAX_VARS) {
        xi_sym_set_error(p, "too many symbolic variables");
        return -1;
    }
    _ = snprintf(&p.var_names[p.var_count], XI_SYM_MAX_NAME_LEN + 1, "%s", name);
    p.var_count += 1;
    return @intCast(p.var_count - 1);
}

fn xi_sym_parse_primary(p: *XiSymParser) XiSymPoly {
    const c = xi_sym_peek(p);
    if (c == 0) {
        xi_sym_set_error(p, "unexpected end of symbolic expression");
        return xi_sym_poly_from_const(0.0);
    }
    if (c == '(') {
        p.pos += 1;
        const inner = xi_sym_parse_expr(p);
        if (!xi_sym_consume(p, ')')) xi_sym_set_error(p, "expected `)` in symbolic expression");
        return inner;
    }
    if (isdigit(c) or c == '.') {
        const start: [*:0]const u8 = p.input + p.pos;
        var end: ?[*:0]u8 = null;
        const value = strtod(start, &end);
        if (end == null or @intFromPtr(end.?) == @intFromPtr(start)) {
            xi_sym_set_error(p, "invalid number in symbolic expression");
            return xi_sym_poly_from_const(0.0);
        }
        p.pos += @intFromPtr(end.?) - @intFromPtr(start);
        return xi_sym_poly_from_const(value);
    }
    if (isalpha(c) or c == '_') {
        var name: [XI_SYM_MAX_NAME_LEN + 1]u8 = undefined;
        var len: usize = 0;
        while (isalnum(p.input[p.pos]) or p.input[p.pos] == '_') {
            if (len < XI_SYM_MAX_NAME_LEN) {
                name[len] = p.input[p.pos];
                len += 1;
            }
            p.pos += 1;
        }
        name[len] = 0;
        const idx = xi_sym_var_index(p, @ptrCast(&name));
        if (idx < 0) return xi_sym_poly_from_const(0.0);
        return xi_sym_poly_from_var(@intCast(idx));
    }
    xi_sym_set_error(p, "unexpected token in symbolic expression");
    return xi_sym_poly_from_const(0.0);
}

fn xi_sym_parse_unary(p: *XiSymParser) XiSymPoly {
    if (xi_sym_consume(p, '+')) return xi_sym_parse_unary(p);
    if (xi_sym_consume(p, '-')) {
        var rhs = xi_sym_parse_unary(p);
        var i: usize = 0;
        while (i < rhs.count) : (i += 1) rhs.terms[i].coeff = -rhs.terms[i].coeff;
        return rhs;
    }
    return xi_sym_parse_primary(p);
}

fn xi_sym_parse_power(p: *XiSymParser) XiSymPoly {
    const lhs = xi_sym_parse_unary(p);
    if (xi_sym_consume(p, '^')) {
        const rhs = xi_sym_parse_power(p);
        var exp_v: c_int = 0;
        if (!xi_sym_poly_as_nonneg_int(&rhs, &exp_v)) {
            xi_sym_set_error(p, "symbolic exponent must be a non-negative integer constant");
            return xi_sym_poly_from_const(0.0);
        }
        return xi_sym_poly_pow(&lhs, exp_v, p);
    }
    return lhs;
}

fn xi_sym_parse_implicit_product(p: *XiSymParser) XiSymPoly {
    var lhs = xi_sym_parse_power(p);
    while (p.had_error == 0) {
        const c = xi_sym_peek(p);
        if (!xi_sym_is_primary_start(c)) break;
        const rhs = xi_sym_parse_power(p);
        lhs = xi_sym_poly_mul(&lhs, &rhs, p);
    }
    return lhs;
}

fn xi_sym_parse_term(p: *XiSymParser) XiSymPoly {
    var lhs = xi_sym_parse_implicit_product(p);
    while (p.had_error == 0) {
        const c = xi_sym_peek(p);
        if (c == '*') {
            p.pos += 1;
            const rhs = xi_sym_parse_implicit_product(p);
            lhs = xi_sym_poly_mul(&lhs, &rhs, p);
            continue;
        }
        if (c == '/') {
            p.pos += 1;
            const rhs = xi_sym_parse_implicit_product(p);
            lhs = xi_sym_poly_div(&lhs, &rhs, p);
            continue;
        }
        break;
    }
    return lhs;
}

fn xi_sym_parse_expr(p: *XiSymParser) XiSymPoly {
    var lhs = xi_sym_parse_term(p);
    while (p.had_error == 0) {
        const c = xi_sym_peek(p);
        if (c == '+') {
            p.pos += 1;
            const rhs = xi_sym_parse_term(p);
            lhs = xi_sym_poly_add(&lhs, &rhs, p, 1.0);
            continue;
        }
        if (c == '-') {
            p.pos += 1;
            const rhs = xi_sym_parse_term(p);
            lhs = xi_sym_poly_add(&lhs, &rhs, p, -1.0);
            continue;
        }
        break;
    }
    return lhs;
}

fn xi_sym_cmp_terms_desc(a_ptr: ?*const anyopaque, b_ptr: ?*const anyopaque) callconv(.c) c_int {
    const a: *const XiSymTerm = @ptrCast(@alignCast(a_ptr));
    const b: *const XiSymTerm = @ptrCast(@alignCast(b_ptr));
    var i: usize = 0;
    while (i < xi_sym_sort_var_count) : (i += 1) {
        if (a.exps[i] > b.exps[i]) return -1;
        if (a.exps[i] < b.exps[i]) return 1;
    }
    return 0;
}

fn xi_sym_append_char(out: [*]u8, used: *usize, ch: u8) void {
    if (used.* + 1 >= XI_SYM_MAX_OUTPUT) return;
    out[used.*] = ch;
    used.* += 1;
    out[used.*] = 0;
}

fn xi_sym_append_str(out: [*]u8, used: *usize, s: [*:0]const u8) void {
    var i: usize = 0;
    while (s[i] != 0) : (i += 1) xi_sym_append_char(out, used, s[i]);
}

fn xi_sym_append_number(out: [*]u8, used: *usize, value: f64) void {
    var buf: [64]u8 = undefined;
    if (xi_sym_is_integer(value)) {
        _ = snprintf(&buf, buf.len, "%lld", @as(c_longlong, llround(value)));
    } else {
        _ = snprintf(&buf, buf.len, "%.15g", value);
    }
    xi_sym_append_str(out, used, @ptrCast(&buf));
}

fn xi_sym_poly_to_string(poly: *const XiSymPoly, parser: *XiSymParser) [*:0]const u8 {
    const out: [*]u8 = &xi_sym_out;
    out[0] = 0;
    var used: usize = 0;

    if (poly.count == 0) {
        xi_sym_append_str(out, &used, "0");
        return @ptrCast(out);
    }

    var sorted: [XI_SYM_MAX_TERMS]XiSymTerm = undefined;
    var i: usize = 0;
    while (i < poly.count) : (i += 1) sorted[i] = poly.terms[i];
    xi_sym_sort_var_count = parser.var_count;
    qsort(&sorted, poly.count, @sizeOf(XiSymTerm), &xi_sym_cmp_terms_desc);

    var emitted_any = false;
    i = 0;
    while (i < poly.count) : (i += 1) {
        const coeff = sorted[i].coeff;
        if (xi_sym_is_zero(coeff)) continue;
        var has_vars = false;
        var v: usize = 0;
        while (v < parser.var_count) : (v += 1) {
            if (sorted[i].exps[v] != 0) {
                has_vars = true;
                break;
            }
        }
        if (coeff < 0.0) {
            xi_sym_append_char(out, &used, '-');
        } else if (emitted_any) {
            xi_sym_append_char(out, &used, '+');
        }
        const abs_coeff = fabs(coeff);
        const omit_coeff_one = has_vars and xi_sym_is_integer(abs_coeff) and llround(abs_coeff) == 1;
        if (!omit_coeff_one) xi_sym_append_number(out, &used, abs_coeff);
        v = 0;
        while (v < parser.var_count) : (v += 1) {
            const exp_v = sorted[i].exps[v];
            if (exp_v == 0) continue;
            xi_sym_append_str(out, &used, @ptrCast(&parser.var_names[v]));
            if (exp_v > 1) {
                xi_sym_append_char(out, &used, '^');
                var buf: [16]u8 = undefined;
                _ = snprintf(&buf, buf.len, "%u", @as(c_uint, exp_v));
                xi_sym_append_str(out, &used, @ptrCast(&buf));
            }
        }
        emitted_any = true;
    }
    if (!emitted_any) xi_sym_append_str(out, &used, "0");
    return @ptrCast(out);
}

export fn xi_math_expand_expr(expr: OptStr) callconv(.c) Str {
    const e = expr orelse return xi_str_dup("0");
    var parser: XiSymParser = undefined;
    parser.input = e;
    parser.pos = 0;
    parser.had_error = 0;
    parser.var_count = 0;
    var i: usize = 0;
    while (i < XI_SYM_MAX_VARS) : (i += 1) parser.var_names[i][0] = 0;

    const poly = xi_sym_parse_expr(&parser);
    if (parser.had_error == 0 and xi_sym_peek(&parser) != 0) {
        xi_sym_set_error(&parser, "unexpected trailing input in symbolic expression");
    }
    if (parser.had_error != 0) return xi_str_dup("0");
    return xi_str_dup(xi_sym_poly_to_string(&poly, &parser));
}

export fn xi_math_expand_expr_input() callconv(.c) Str {
    return xi_math_expand_expr(xi_io_ask());
}
