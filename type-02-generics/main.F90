MODULE field_mod
    IMPLICIT NONE

    INTEGER, PARAMETER :: n = 10

    TYPE field_t
        INTEGER, ALLOCATABLE, DIMENSION(:) :: arr
    CONTAINS
        GENERIC :: get => get_int, get_real
        PROCEDURE, PRIVATE, NON_OVERRIDABLE :: get_int, get_real
    END TYPE field_t
CONTAINS
    SUBROUTINE get_int(this, val, index)
        !$omp declare target
        CLASS(field_t), INTENT(INOUT), TARGET :: this
        INTEGER, INTENT(OUT) :: val
        INTEGER, INTENT(IN) :: index

        val = this%arr(index)
    END SUBROUTINE get_int

    SUBROUTINE get_real(this, val, index)
        !$omp declare target
        CLASS(field_t), INTENT(INOUT), TARGET :: this
        REAL, INTENT(OUT) :: val
        INTEGER, INTENT(IN) :: index

        val = REAL(this%arr(index))
    END SUBROUTINE get_real
END MODULE field_mod

PROGRAM main
    USE field_mod

    IMPLICIT NONE

    PRINT *, "get --> get_int"
    CALL test_get_int()
    PRINT *, "get --> get_real"
    CALL test_get_real()
CONTAINS
    SUBROUTINE test_get_int()
        INTEGER :: values(n), expected(n)

        INTEGER, PARAMETER :: initial = 42
        TYPE(field_t) :: field
        INTEGER :: i, val

        ALLOCATE(field%arr(n), source=initial)

        !$omp target teams loop map(from: values) private(val)
        DO i = 1, n
            CALL field%get(val, i)
            values(i) = val
        END DO
        !$omp end target teams loop 

        expected(:) = initial
        CALL compare_int(values, expected)

        DEALLOCATE(field%arr)
    END SUBROUTINE test_get_int

    SUBROUTINE test_get_real()
        REAL :: values(n), expected(n)

        INTEGER, PARAMETER :: initial = 42
        TYPE(field_t) :: field
        INTEGER :: i
        REAL :: val

        ALLOCATE(field%arr(n), source=initial)

        !$omp target teams loop map(from: values) private(val)
        DO i = 1, n
            CALL field%get(val, i)
            values(i) = val
        END DO
        !$omp end target teams loop 

        expected(:) = REAL(initial)
        CALL compare_real(values, expected)

        DEALLOCATE(field%arr)
    END SUBROUTINE test_get_real

    SUBROUTINE compare_int(result, expected)
        INTEGER, INTENT(in), DIMENSION(:) :: result, expected

        IF (SIZE(result) /= SIZE(expected)) THEN
            PRINT *, "Test FAILED! Arrays have different sizes."
            RETURN
        END IF

        IF (ALL(ABS(result - expected) == 0)) THEN
            PRINT *, "Test passed!"
        ELSE
            PRINT *, "Test FAILED!"
        END IF
    END SUBROUTINE compare_int

    SUBROUTINE compare_real(result, expected)
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
    END SUBROUTINE compare_real
END PROGRAM main
