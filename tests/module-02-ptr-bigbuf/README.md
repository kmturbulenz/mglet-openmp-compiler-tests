### module-02-bigbuf

Tests if a big byte array on the target can be accessed by pointers of a different type that live on module level and can be accessed globally.

Use case: MGLET uses the same structure of pointers on a module level to store receive and send MPI buffers. A GPU aware MPI implementation shall be able to work in this architecture.
