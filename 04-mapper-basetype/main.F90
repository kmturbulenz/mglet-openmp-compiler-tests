PROGRAM reproducer
    IMPLICIT NONE

    TYPE, ABSTRACT :: base_t
        REAL, ALLOCATABLE :: base_arr(:)
    END TYPE base_t

    TYPE, EXTENDS(base_t) :: child_t
        REAL, ALLOCATABLE :: child_arr(:)
    END TYPE child_t
    !$omp declare mapper(custommapper: child_t :: t) map(t%base_arr, t%child_arr)

    INTEGER, PARAMETER :: n = 3
    REAL, PARAMETER :: base_val = 2.0, child_val = 3.0
    REAL :: expected_base_arr(n)
    REAL :: expected_child_arr(n)
    TYPE(child_t) :: typ
    ALLOCATE(typ%base_arr(n), source=1.0)
    ALLOCATE(typ%child_arr(n), source=1.0)
    expected_base_arr = base_val
    expected_child_arr = child_val

    !$omp target map(tofrom: typ)
        typ%base_arr = base_val
        typ%child_arr = child_val
    !$omp end target

    CALL compare(typ%base_arr, expected_base_arr)
    CALL compare(typ%child_arr, expected_child_arr)

    DEALLOCATE(typ%child_arr)
    DEALLOCATE(typ%base_arr)
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
