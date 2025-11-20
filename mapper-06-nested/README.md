### mapper-06-nested

Tests if a derived type containing a nested derived type with an allocatable can be mapped with a custom mapper that defines the behavior for both structures.

Use case: In MGLET, some fields contain an additional buffer structure for grid faces that might needs to be mapped in some cases.
