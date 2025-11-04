### 07-module-variable

Tests if an array module variable can be marked as `declare target` and used as a global in target regions.

Use case: MGLET uses module variables frequently, but most importantly for the grid information. Each grid is a member of an array of derived types. Grid information must be accessible in target regions to get the number of cells, indices for boundary conditions etc. 
