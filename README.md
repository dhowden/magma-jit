# magma-jit

A prototype JIT compiler for MAGMA (see http://magma.maths.usyd.edu.au).

# Requirements

* Bison (tested on 3.0.4)
* Flex (tested on 2.5.3)
* LLVM 3.5 (tested on 3.5.1)

# Build Instructions

    $ make
    $ ./tests
    ok   tests/assert.m
    ok   tests/assign.m
    ok   tests/break.m
    ok   tests/change-type.m
    ok   tests/func-loop-bug.m
    ok   tests/func-scope-bug.m
    ok   tests/func.m
    ok   tests/if.m
    ok   tests/iter.m
    ok   tests/new-scope.m
    ok   tests/op.m
    ok   tests/op_jmp.m
    ok   tests/print.m
    ok   tests/proc.m
    ok   tests/repeat.m
    ok   tests/return.m
    ok   tests/seq.m
    ok   tests/string.m
    ok   tests/tuple.m
    ok   tests/types.m
    ok   tests/while.m

# Done
* Binary operations
* If statements
* For statements
* Variable scoping
* Functions
* Procedures
* Executable Params (output RAW LLVM IR/Optimised/Run (default))
* Repeat/Until
* Break, Break x;
* Strings

# In Progress

* Sequences
* Multiple return values

# Goals

* LLVM code generation for sall tandard language statements
* Handle MAGMA container types (sequences/sets/indexed sets/etc)
* Be able to specify a new MAGMA type and devolve operations to C functions
* Socket server, accept compile + run commands
