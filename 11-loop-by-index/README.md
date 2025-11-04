### 11-loop-by-index

Tests if a contiguous 1D array can be looped in two different levels. The array contains data of n domains, but is contiguously stored.
First, an index is calculated to the index of first element of the i'th domain. Then, the array accessed at that element is passed into a kernel function operating on this specific domain, acting as way to access the slice of data.

Use case: MGLET's fields store its grids and cells in the same way as described above. A contiguous array stores all grids in succession which can be accessed in a kernel function for computation. This way of accessing grids by calculating the index inline is not the best practice in MGLET, but rather an acceptable workaround we found to work on HLRS Hunter with the Cray compilers.

