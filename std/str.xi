// Xi standard library: str - string operations.
//
// All of these are monomorphic, so they bind directly to their runtime
// symbols. `from_num` takes the general `dec` number type (any numeric coerces
// to it); indices are `int`.

extern f len(string s) -> int = "xi_str_len";
extern f concat(string a, string b) -> string = "xi_str_concat";
extern f substr(string s, int start, int length) -> string = "xi_str_substr";
extern f char_code_at(string s, int index) -> int = "xi_str_char_code_at";
extern f from_num(dec n) -> string = "xi_str_from_num";
extern f from_char_code(int code) -> string = "xi_str_from_char_code";
extern f eq(string a, string b) -> bool = "xi_str_eq";
extern f find(string s, string needle) -> int = "xi_str_find";
extern f contains(string s, string needle) -> bool = "xi_str_contains";
extern f starts_with(string s, string prefix) -> bool = "xi_str_starts_with";
extern f ends_with(string s, string suffix) -> bool = "xi_str_ends_with";
extern f trim(string s) -> string = "xi_str_trim";
extern f trim_start(string s) -> string = "xi_str_trim_start";
extern f trim_end(string s) -> string = "xi_str_trim_end";
extern f index_of_char(string s, int code) -> int = "xi_str_index_of_char";
extern f count(string s, string needle) -> int = "xi_str_count";
extern f replace(string s, string old, string new) -> string = "xi_str_replace";
extern f repeat(string s, int count) -> string = "xi_str_repeat";

// Parsing can fail, so these return a `result<T>` rather than a value plus a
// silent zero (see Basics.txt section 19). Both are exact: an integer grows
// into a bignum instead of saturating, and a decimal stays exact base-10.
//
//     result<int> r = str::to_int(line);
//     int n = res::or(str::to_int(line), 0);      // with a fallback
//     int n = try str::to_int(line);              // propagate to the caller
//
// Accepted: an optional leading `+`/`-`, then digits, plus a single `.` with
// digits on both sides for `to_dec`. Anything else - empty text, spaces,
// exponents, trailing characters - is an error rather than a truncated parse,
// so trim the string first if it may carry whitespace.
extern f to_int(string s) -> result<int> = "xi_str_to_int_result";
extern f to_dec(string s) -> result<dec> = "xi_str_to_dec_result";
