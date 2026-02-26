### mapper-01-inter-module

Tests if a derived type defined in module A that is PRIVATE can be used in module B in a custom mapper via an intermediate mapper module that is PUBLIC by default.

Use case: fields are defined in their own PRIVATE modules. To make their mappers available in many other modules, an intermediate mapper module is mandatory to work on all compilers.
