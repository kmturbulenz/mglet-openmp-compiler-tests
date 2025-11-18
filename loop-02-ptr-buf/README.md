### loop-02-ptr-buf

Tests if a contiguous 1D array can be looped in two different levels. The array contains data of n domains, but is contiguously stored.
First, a pointer is constructed in a separate subroutine which wraps a specific domain i of the array. This pointer is passed into a kernel function where operations are performed on the data wrapped by the pointer.

Use case: MGLET's fields store its grids and cells as one dimensional arrays in a derived type. This testcase simplifies this by using a buffer outside of a derived type first. A contiguous array stores all grids in succession which can be accessed in a kernel function for computation. This is the best practice in MGLET and used frequently to access grid data. It is slightly more demainding of the compiler due to the indirect way of accessing the data by construction a pointer first instead of directly accessing data by the first element of a grid.

