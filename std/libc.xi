// Xi standard library: libc - a few C standard-library bindings via `cextern`.
// The C runtime is linked by default, so these need no `#link`.

cextern f qsort(cptr base, uint64 count, uint64 size, cptr compare) -> void = "qsort";
cextern f bsearch(cptr key, cptr base, uint64 count, uint64 size, cptr compare) -> cptr = "bsearch";
cextern f malloc(uint64 size) -> cptr = "malloc";
cextern f free(cptr p) -> void = "free";
cextern f memcmp(cptr a, cptr b, uint64 n) -> int32 = "memcmp";
