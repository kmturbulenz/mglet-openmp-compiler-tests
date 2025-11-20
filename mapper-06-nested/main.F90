PROGRAM main
    IMPLICIT NONE

    TYPE :: nested_t
        REAL, ALLOCATABLE :: buf(:)
    END TYPE nested_t
    ! (Intel) Default mapper is explicitly required to make test pass
    !$omp declare mapper(nested_t :: n) map(n%buf)

    TYPE :: field_t
        REAL, ALLOCATABLE :: buf(:)
        TYPE(nested_t) :: nested
    END TYPE field_t
    ! Default mapper shall NOT map the nested type
    ! (Intel) If you use the default mapper and still access the nested
    ! type in a target region, it will be allocated implicitly
    !$omp declare mapper(field_t :: f) map(f%buf)
    !$omp declare mapper(withnested: field_t :: f) map(f%buf, f%nested)

    INTEGER, PARAMETER :: n = 128

    REAL, ALLOCATABLE :: expected_buf(:)
    
    TYPE(field_t) :: field

    ALLOCATE(expected_buf(n), source= 0.0)
    ALLOCATE(field%buf(n), source=1.0)
    ALLOCATE(field%nested%buf(n), source=2.0)

    !$omp target enter data map(mapper(withnested), to: field)

    !$omp target
    field%buf(:) = 42.0
    field%nested%buf(:) = 10.0
    !$omp end target
    
    !$omp target update from(field%buf)
    !$omp target update from(field%nested%buf)
    expected_buf = 42.0
    CALL compare(field%buf, expected_buf)
    expected_buf = 10.0
    CALL compare(field%nested%buf, expected_buf)

    !$omp target exit data map(mapper(withnested), release: field)
    DEALLOCATE(expected_buf)
    DEALLOCATE(field%buf)
    DEALLOCATE(field%nested%buf)
CONTAINS
    SUBROUTINE compare(result, expected)
        REAL, INTENT(in), DIMENSION(:) :: result, expected
        REAL, PARAMETER :: tolerance = 1.0e-6

        IF (SIZE(result) /= SIZE(expected)) THEN
            PRINT *, "Test FAILED! Arrays have different sizes."
            RETURN
        END IF

        IF (ALL(ABS(result - expected) < tolerance)) THEN
            PRINT *, "Test passed!"
        ELSE
            PRINT *, "Test FAILED!"
        END IF
    END SUBROUTINE compare
END PROGRAM main
