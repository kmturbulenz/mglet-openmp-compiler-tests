PROGRAM exptest
    IMPLICIT NONE

    CALL test_exp()
    CALL test_trig()
CONTAINS
    SUBROUTINE test_exp()
        REAL :: result, expected
        REAL, PARAMETER :: input = 3.0

        !$omp target map(tofrom: result)
        result = EXP(input)
        !$omp end target

        expected = EXP(input)

        CALL compare(result, expected)
    END SUBROUTINE test_exp

    SUBROUTINE test_trig()
        REAL :: result, expected
        REAL, PARAMETER :: PI = 3.14159265358979323846, input = PI / 3

        !$omp target map(tofrom: result)
        result = COS(input)
        result = SIN(result)
        result = ATAN(result)
        result = ACOS(result)
        !$omp end target

        expected = ACOS(ATAN(SIN(COS(input))))

        CALL compare(result, expected)
    END SUBROUTINE test_trig

    SUBROUTINE compare(result, expected)
        REAL, INTENT(in) :: result, expected
        REAL, PARAMETER :: tolerance = 1.0e-6

        IF (ABS(result - expected) < tolerance) THEN
            PRINT *, "Test passed!"
            PRINT *, "Result =", result, "Expected =", expected
        ELSE
            PRINT *, "Test FAILED!"
            PRINT *, "Result =", result, "Expected =", expected
        END IF
    END SUBROUTINE compare
END PROGRAM exptest
