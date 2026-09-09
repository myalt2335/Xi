// Xi standard library: arr - dynamic arrays.
//
// Array operations are polymorphic in the element type (int / uint / dec / bool
// / string / structs, including sized integer arrays such as array<uint8> and
// array<int32>). `arr::new` produces an as-yet-unknown element type that is
// fixed by assignment context or first use. That type-directed behaviour is handled by compiler
// intrinsics, so these are declared `intrinsic` rather than bound to a single
// runtime symbol.

intrinsic new;
intrinsic push;
intrinsic pop;
intrinsic insert;
intrinsic remove;
intrinsic clear;
intrinsic contains;
intrinsic index_of;
intrinsic len;
intrinsic get;
intrinsic set;
// Unchecked int/uint-array access: the caller guarantees the index is in bounds,
// so these skip null/negative/bounds validation for hot inner loops. Out-of-range
// use is undefined behaviour. Reach for these only after proving safety.
intrinsic get_unchecked;
intrinsic set_unchecked;
intrinsic slice;
intrinsic slice_from;
