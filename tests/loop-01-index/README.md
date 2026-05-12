### loop-01-index

Tests if a typical MGLET field can be looped in a target region using a grid loop and an inner loop over the cells.

This test passes the address of a grid's start by computing its corresponding index in the continuous cell array in a field type. While this way of indexing into grids is not used in MGLET, it is not as demanding of the compiler as constructing a pointer in a new subroutine.
