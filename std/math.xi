// Xi standard library: math
//
// This is a real module file, resolved from the module search path when a
// program writes `#include math`. Each `extern` declaration binds an Xi-level
// signature to the runtime symbol that implements it (the C functions in
// xi_runtime). Without `#include math`, none of these names resolve.

extern f sin(dec x) -> dec = "xi_math_sin";
extern f cos(dec x) -> dec = "xi_math_cos";
extern f tan(dec x) -> dec = "xi_math_tan";
extern f sqrt(dec x) -> dec = "xi_math_sqrt";
extern f abs(dec x) -> dec = "xi_math_abs";
extern f floor(dec x) -> dec = "xi_math_floor";
extern f ceil(dec x) -> dec = "xi_math_ceil";
extern f round(dec x) -> dec = "xi_math_round";
extern f trunc(dec x) -> dec = "xi_math_trunc";
extern f log(dec x) -> dec = "xi_math_log";
extern f log10(dec x) -> dec = "xi_math_log10";
extern f log2(dec x) -> dec = "xi_math_log2";
extern f exp(dec x) -> dec = "xi_math_exp";
extern f pow(dec x, dec y) -> dec = "xi_math_pow";
extern f min(dec x, dec y) -> dec = "xi_math_min";
extern f max(dec x, dec y) -> dec = "xi_math_max";
extern f atan2(dec y, dec x) -> dec = "xi_math_atan2";
extern f expand_expr(string s) -> string = "xi_math_expand_expr";
extern f eval_expr_input() -> dec = "xi_math_eval_expr_input";
extern f expand_expr_input() -> string = "xi_math_expand_expr_input";

extern dec e = "xi_math_e";
extern dec pi = "xi_math_pi";
extern dec tau = "xi_math_tau";
