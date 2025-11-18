MODULE some_mod
    IMPLICIT NONE

    INTEGER, PARAMETER :: n = 3
    REAL, ALLOCATABLE, DIMENSION(:) :: buffer
    !$omp declare target(buffer)
CONTAINS
    SUBROUTINE init_data()
        INTEGER :: i

        ALLOCATE(buffer(n))

        DO i = 1, n
            buffer(i) = i
        END DO
        !$omp target enter data map(always, to: buffer)
    END SUBROUTINE init_data

    SUBROUTINE get_value(val, index)
        !$omp declare target
        REAL, INTENT(OUT) :: val
        INTEGER, INTENT(IN) :: index

        val = buffer(index)
    END SUBROUTINE get_value

    SUBROUTINE finalize()
        DEALLOCATE(buffer)
    END SUBROUTINE finalize
END MODULE some_mod

PROGRAM main
    USE some_mod
    IMPLICIT NONE

    INTEGER :: i
    REAL :: temp_val
    REAL :: values_read(n)

    CALL init_data()

    !$omp target map(from: values_read)
    DO i = 1, n
        CALL get_value(temp_val, i)
        values_read(i) = temp_val
    END DO
    !$omp end target

    CALL compare(values_read, buffer)

    CALL finalize()
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
END PROGRAM main
