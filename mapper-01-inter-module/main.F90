MODULE field_mod
    IMPLICIT NONE

    TYPE :: field_t
        REAL, ALLOCATABLE :: buf(:)
        REAL :: val
    END TYPE field_t
    ! Scalar value must be mapped explicitly
    !$omp declare mapper(custom: field_t :: t) map(tofrom: t%buf, t%val)
END MODULE field_mod

MODULE test_mod
    USE field_mod
    IMPLICIT NONE
CONTAINS
    SUBROUTINE test_mapper()
        INTEGER, PARAMETER :: n = 3
        REAL, PARAMETER :: val = 42.0
        REAL :: expected_buf(n), result_buf(n)
        REAL :: expected_val(1), result_val(1)
        TYPE(field_t) :: field

        ALLOCATE(field%buf(n))
        field%buf = val
        field%val = val

        ! (INTEL) Be careful with mapping scalars. There is a bug that changes the mapping behavior
        ! of scalars when a custom mapper is used in another mapping. Thus we use a one-sized array.
        ! (Cray) The order of mapping matters here. If the mapper(custom) is put first
        ! then the program will core dump with a memory access fault
        !$omp target map(from: result_buf) map(mapper(custom), tofrom: field)
        result_buf(:) = field%buf(:)
        result_val(:) = field%val
        !$omp end target

        expected_buf = val
        expected_val = val
        CALL compare(result_buf, expected_buf)
        CALL compare(result_val, expected_val)

        DEALLOCATE(field%buf)
    END SUBROUTINE test_mapper

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

    CALL test_mapper()
END PROGRAM reproducer
