MODULE field_mod
    IMPLICIT NONE
    PRIVATE

    TYPE :: field_t
        REAL, ALLOCATABLE :: buf(:)
        REAL :: val
    END TYPE field_t
    ! Scalar value must be mapped explicitly
    !$omp declare mapper(custom: field_t :: t) map(tofrom: t%buf, t%val)

    PUBLIC :: field_t
END MODULE field_mod

MODULE test_mod
    USE field_mod
    IMPLICIT NONE
    PRIVATE

    PUBLIC :: run_test
CONTAINS
    SUBROUTINE run_test()
        INTEGER, PARAMETER :: n = 3
        REAL, PARAMETER :: val = 42.0
        REAL :: expected_buf(n), result_buf(n)
        REAL :: expected_val(1), result_val(1)
        TYPE(field_t) :: field

        ALLOCATE(field%buf(n))
        field%buf = val
        field%val = val

        ! (Intel) Be careful with mapping scalars. There is a bug that changes 
        !         the mapping behavior of scalars when a custom mapper is used 
        !         in another mapping. To avoid this we just use a one 
        !         dimensional array instead
        ! (Cray)[likely fixed in CCE 21] The order of mapping matters here. 
        !        If the mapper(custom) is listed first in the map clause, then 
        !        the program will crash with a memory access fault
        !$omp target map(from: result_buf) map(mapper(custom), tofrom: field)
        result_buf(:) = field%buf(:)
        result_val(:) = field%val
        !$omp end target

        expected_buf = val
        expected_val = val
        CALL compare(result_buf, expected_buf)
        CALL compare(result_val, expected_val)

        DEALLOCATE(field%buf)
    END SUBROUTINE run_test

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
END MODULE test_mod

PROGRAM reproducer
    USE test_mod
    IMPLICIT NONE

    CALL run_test()
END PROGRAM reproducer
