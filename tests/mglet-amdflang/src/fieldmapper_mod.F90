MODULE fieldmapper_mod
    USE realfield_mod, ONLY: field_t
    IMPLICIT NONE
    ! NOT private by default

    PRIVATE :: field_t

    !$omp declare mapper(field_t :: t) map(t%arr, t%ptr, t%length)
    !$omp declare mapper(mapall: field_t :: t) map(t%arr, t%ptr, t%length)
    !$omp declare mapper(maparr: field_t :: t) map(t%arr)
END MODULE fieldmapper_mod

