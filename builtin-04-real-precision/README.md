### builtin-04-real-precision

Test if the REAL(10) type compiles if offloading is enabled. This test was specifically added to highlight missing support in LLVM flang.

Use case: MGLET depends on the HDF5 library, which uses REAL(10) if the underlying compiler supports it. As such, compiling MGLET with offloading enabled relies on this test passing.
