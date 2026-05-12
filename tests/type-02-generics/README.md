### type-02-generics

Tests if generic procedures of a derived type can be called in a target region and the correct corresponding subroutine is called depending on the function signature.

Use case: MGLET's grids have similar functionality to access its cells with a 3D or 1D pointer as in this test. Depending on the shape of the pointer passed to the function, the correct procedure of the derived type shall be called.
