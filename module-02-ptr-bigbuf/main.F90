MODULE ptr_mod
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: int8, real32, int64
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_F_POINTER, C_LOC
    IMPLICIT NONE

    INTEGER(int64), PARAMETER :: n = 8 
    INTEGER(int64), PARAMETER :: nbytes = 4 * n

    INTEGER(int8), ALLOCATABLE, TARGET :: buffer(:)
    !$omp declare target(buffer)

    REAL(real32), POINTER, CONTIGUOUS :: bigbuf(:) => NULL()
    REAL(real32), POINTER, CONTIGUOUS :: left(:) => NULL()
    REAL(real32), POINTER, CONTIGUOUS :: right(:) => NULL()
    !$omp declare target(bigbuf, left, right)
CONTAINS
    SUBROUTINE init()
        TYPE(C_PTR) :: cptr
        INTEGER :: i

        ALLOCATE(buffer(nbytes))

        cptr = C_LOC(buffer)
        CALL C_F_POINTER(cptr, bigbuf, [n])
        left => bigbuf(1:n/2)
        right => bigbuf(n/2+1:n)

        ! Init the buffer with some dummy values for comparison
        DO i = 1, n
            bigbuf(i) = REAL(i)
        END DO
        !$omp target enter data map(always, to: buffer)
        !$omp target enter data map(always, to: bigbuf, left, right)
    END SUBROUTINE init
END MODULE ptr_mod

PROGRAM exptest
    USE ptr_mod
    IMPLICIT NONE

    CALL init()

    CALL test_bigbuf()
    CALL test_left()
    CALL test_right()
CONTAINS
    SUBROUTINE test_left()
        INTEGER :: i
        REAL(real32), ALLOCATABLE, DIMENSION(:) :: expected_vals, copied_val
        ALLOCATE(expected_vals(n/2))
        ALLOCATE(copied_val(n/2))

        !$omp target map(from: copied_val)
        copied_val(:) = left(:)
        !$omp end target
        DO i = 1, n/2
            expected_vals(i) = REAL(i)
        END DO
        CALL compare(copied_val, expected_vals)

        DEALLOCATE(expected_vals, copied_val)
    END SUBROUTINE test_left 

    SUBROUTINE test_right()
        INTEGER :: i
        REAL(real32), ALLOCATABLE, DIMENSION(:) :: expected_vals, copied_val
        ALLOCATE(expected_vals(n/2))
        ALLOCATE(copied_val(n/2))

        !$omp target map(from: copied_val)
        copied_val(:) = right(:)
        !$omp end target
        DO i = 1, n/2
            expected_vals(i) = REAL(i + n/2)
        END DO
        CALL compare(copied_val, expected_vals)

        DEALLOCATE(expected_vals, copied_val)
    END SUBROUTINE test_right 

    SUBROUTINE test_bigbuf()
        INTEGER :: i
        REAL(real32), ALLOCATABLE, DIMENSION(:) :: expected_vals, copied_val
        ALLOCATE(expected_vals(n))
        ALLOCATE(copied_val(n))

        !$omp target map(from: copied_val)
        copied_val(:) = bigbuf(:)
        !$omp end target
        DO i = 1, n
            expected_vals(i) = REAL(i)
        END DO
        CALL compare(copied_val, expected_vals)

        DEALLOCATE(expected_vals, copied_val)
    END SUBROUTINE test_bigbuf

    SUBROUTINE compare(result, expected)
        REAL, INTENT(in), DIMENSION(:) :: result, expected
        REAL, PARAMETER :: tolerance = 1.0e-6

        IF (ALL(ABS(result - expected) < tolerance)) THEN
            PRINT *, "Test passed!"
        ELSE
            PRINT *, "Test FAILED!"
        END IF
    END SUBROUTINE compare
END PROGRAM exptest
