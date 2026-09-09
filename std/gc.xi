// Xi standard library: explicit control of the managed heap.
//
// Using any operation in this module selects manual mode when no --gc option
// was supplied. Explicit --gc=auto rejects these manual-control operations.
// With manual mode, allocations are tracked but only collect() traces and sweeps. With
// --gc=off, collect() is a no-op and allocations live until process exit.

intrinsic collect;       // collect() -> void
intrinsic collections;   // collections() -> int; completed collections
intrinsic live;          // live() -> int; currently tracked managed objects
