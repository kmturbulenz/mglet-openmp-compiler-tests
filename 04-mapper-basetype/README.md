### 04-mapper-basetype

Tests if a custom mapper of a chilid type can map members of its parent type and its own type.

Use case: field structures and related custom mappers in MGLET are defined as a child type extending from a base field containing basic field information relevant for all kinds of field types. Information in the base field must be accessible on the target for easy access to e.g. grid indices and extents in code.
