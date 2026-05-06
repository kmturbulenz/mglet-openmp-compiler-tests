MODULE buffer_mod
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: int8, real32, int64
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_F_POINTER, C_LOC
    IMPLICIT NONE
    PRIVATE

    INTEGER(int8), ALLOCATABLE, TARGET :: buffer(:)
    !$omp declare target(buffer)

    REAL(real32), POINTER, CONTIGUOUS :: bigbuf(:) => NULL()
    REAL(real32), POINTER, CONTIGUOUS :: left(:) => NULL()
    REAL(real32), POINTER, CONTIGUOUS :: right(:) => NULL()
    !$omp declare target(bigbuf, left, right)

    PUBLIC :: bigbuf, left, right
    PUBLIC :: allocate_buffer, fill_buffer
CONTAINS
    SUBROUTINE allocate_buffer(length)
        INTEGER(int64), INTENT(IN) :: length

        TYPE(C_PTR) :: cptr
        INTEGER :: nbytes

        nbytes = 4 * length
        IF (ASSOCIATED(left)) NULLIFY(left)
        IF (ASSOCIATED(right)) NULLIFY(right)
        IF (ASSOCIATED(bigbuf)) NULLIFY(bigbuf)
        ! Not really necessary, just for sanity and debugging
        !$omp target update to(left, right, bigbuf)

        IF (ALLOCATED(buffer)) THEN
            !$omp target exit data map(delete: buffer)
            DEALLOCATE(buffer)
        END IF
        ALLOCATE(buffer(nbytes))
        !$omp target enter data map(always, to: buffer)

        cptr = C_LOC(buffer)
        CALL C_F_POINTER(cptr, bigbuf, [length])
        !$omp target update to(bigbuf)
        left => bigbuf(1:length/2)
        right => bigbuf(length/2+1:length)
        !$omp target update to(left, right)
    END SUBROUTINE allocate_buffer

    SUBROUTINE fill_buffer()
        INTEGER :: i

        DO i = 1, SIZE(bigbuf)
            bigbuf(i) = REAL(i, kind=4)
        END DO
        !$omp target update to(buffer)
    END SUBROUTINE fill_buffer
END MODULE buffer_mod

MODULE test_mod
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: int64, real32
    USE buffer_mod
    IMPLICIT NONE
    PRIVATE

    PUBLIC :: run_tests
CONTAINS
    SUBROUTINE run_tests()
        CALL test_left()
        CALL test_right()
        CALL test_bigbuf()
    END SUBROUTINE run_tests

    SUBROUTINE test_left()
        REAL(real32), ALLOCATABLE, DIMENSION(:) :: expected_vals, copied_val
        INTEGER :: i, length

        length = SIZE(bigbuf)
        ALLOCATE(expected_vals(length / 2))
        ALLOCATE(copied_val(length / 2))

        !$omp target map(from: copied_val)
        copied_val(:) = left(:)
        !$omp end target

        DO i = 1, length / 2
            expected_vals(i) = REAL(i)
        END DO

        CALL compare(copied_val, expected_vals)

        DEALLOCATE(expected_vals, copied_val)
    END SUBROUTINE test_left 

    SUBROUTINE test_right()
        REAL(real32), ALLOCATABLE, DIMENSION(:) :: expected_vals, copied_val
        INTEGER :: i, length

        length = SIZE(bigbuf)
        ALLOCATE(expected_vals(length / 2))
        ALLOCATE(copied_val(length / 2))

        !$omp target map(from: copied_val)
        copied_val(:) = right(:)
        !$omp end target
        DO i = 1, length / 2
            expected_vals(i) = REAL(i + length / 2)
        END DO
        CALL compare(copied_val, expected_vals)

        DEALLOCATE(expected_vals, copied_val)
    END SUBROUTINE test_right 

    SUBROUTINE test_bigbuf()
        REAL(real32), ALLOCATABLE, DIMENSION(:) :: expected_vals, copied_val
        INTEGER :: i, length

        length = SIZE(bigbuf)
        ALLOCATE(expected_vals(length))
        ALLOCATE(copied_val(length))

        !$omp target map(from: copied_val)
        copied_val(:) = bigbuf(:)
        !$omp end target

        DO i = 1, length
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
END MODULE test_mod

PROGRAM buffers
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: int64
    USE buffer_mod
    USE test_mod
    IMPLICIT NONE

    INTEGER(int64), PARAMETER :: initial_length = 256
    INTEGER(int64) :: current_length
    INTEGER, PARAMETER :: steps = 3
    INTEGER :: i

    current_length = initial_length

    DO i = 1, steps
        CALL allocate_buffer(current_length)
        CALL fill_buffer()
        PRINT*, "Testing length =", current_length
        CALL run_tests()
        current_length = current_length * 2
    END DO
END PROGRAM buffers
