### loop-03-ptr-type

Tests if a contiguous 1D array which is a member of a derived type can be looped in two different levels. The array contains data of n domains, but is contiguously stored.
First, a pointer is constructed in a procedure of the derived type which wraps a specific domain i of the array. This pointer is passed into a kernel function where operations are performed on the data wrapped by the pointer.

Use case: MGLET's fields store its grids and cells in the same way as described above. A contiguous array stores all grids in succession which can be accessed in a kernel function for computation. This is the best practice in MGLET and used frequently to access grid data. It is slightly more demainding of the compiler due to the indirect way of accessing the data by construction a pointer first instead of directly accessing data by the first element of a grid.

