MODULE field_mod
    IMPLICIT NONE

    TYPE :: field_t
        REAL, ALLOCATABLE :: buf(:)
    CONTAINS
        PROCEDURE :: set_vals
    END TYPE field_t
CONTAINS
    ELEMENTAL SUBROUTINE set_vals(this, val)
        !$omp declare target
        CLASS(field_t), INTENT(INOUT) :: this
        REAL, INTENT(IN) :: val

        this%buf(:) = val
    END SUBROUTINE set_vals
END MODULE field_mod

PROGRAM reproducer
    USE field_mod
    IMPLICIT NONE

    INTEGER, PARAMETER :: n = 3
    REAL, PARAMETER :: initial_val = 1.0, val = 42.0
    REAL :: expected_values(n)
    TYPE(field_t) :: field

    ALLOCATE(field%buf(n), source=initial_val)

    !$omp target
        ! Calling a procedure from a mapped derived type requires declaring with target
        CALL field%set_vals(val)
    !$omp end target

    expected_values(:) = val
    CALL compare(field%buf, expected_values)

    DEALLOCATE(field%buf)
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
            PRINT *, "Result =", result
            PRINT *, "Expected =", expected
        ELSE
            PRINT *, "Test FAILED!"
            PRINT *, "Result =", result
            PRINT *, "Expected =", expected
        END IF
    END SUBROUTINE compare
END PROGRAM reproducer
