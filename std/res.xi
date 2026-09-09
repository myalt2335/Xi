// Xi standard library: res - results.
//
// A `result<T>` is either a value of type T or an error message. It is Xi's
// answer to "this call can fail": the failure is part of the type, so a caller
// cannot read the value without having somewhere to put the failure.
//
//     f half(int n) {
//         if (n % 2 != 0) { return res::err("odd"); }
//         return res::ok(n / 2);           // infers result<int>
//     }
//
//     result<int> r = half(7);
//     if (res::is_ok(r)) { io::wrtl << res::value(r); }
//     else               { io::wrtl << res::error(r); }
//
//     int n = res::or(half(7), 0);         // fallback instead of branching
//
// These are compiler intrinsics rather than runtime symbols: `ok` takes any
// payload type and `value` gives that same type back, which is type-directed
// behaviour the compiler resolves per call site (the same way `arr::` and
// `map::` work).
//
// `res::err` on its own does not know what payload type it belongs to. It gets
// one from context - the return type it is inferred into, or the `result<T>`
// variable it is assigned to.

intrinsic ok;      // ok(value)          -> result<T>
intrinsic err;     // err(message)       -> result<T>, payload from context
intrinsic is_ok;   // is_ok(result)      -> bool
intrinsic is_err;  // is_err(result)     -> bool
intrinsic value;   // value(result)      -> T   (defaulted when the result is an error)
intrinsic error;   // error(result)      -> string ("" when the result is ok)
intrinsic or;      // or(result, fallback) -> T
