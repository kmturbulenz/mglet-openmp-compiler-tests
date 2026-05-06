#if defined(_CRAYFTN) || defined(__GFORTRAN__)
#define _BIND_THREAD_
#else
#undef _BIND_THREAD_
#endif

MODULE const_mod
    INTEGER, PARAMETER :: ngrids = 64
    INTEGER, PARAMETER :: cpd = 32
    INTEGER, PARAMETER :: cpg = cpd**3
    INTEGER, PARAMETER :: ncells = ngrids * cpg
END MODULE const_mod


PROGRAM reproducer
    USE const_mod
    IMPLICIT NONE

    REAL, ALLOCATABLE, TARGET, DIMENSION(:) :: expected_even, expected_odd
    REAL, ALLOCATABLE, TARGET, DIMENSION(:) :: computed_even, computed_odd
    ALLOCATE(expected_even(ncells), source=0.0)
    ALLOCATE(expected_odd(ncells), source=0.0)
    ALLOCATE(computed_even(ncells), source=0.0)
    ALLOCATE(computed_odd(ncells), source=0.0)

    WRITE(*, *) "Computing expected solution on the host..."
    CALL compute_expected()

    WRITE(*, *) "Computing solution on the device..."
    CALL test()

    DEALLOCATE(expected_even)
    DEALLOCATE(expected_odd)
    DEALLOCATE(computed_even)
    DEALLOCATE(computed_odd)
CONTAINS
    SUBROUTINE test()
        REAL, POINTER, CONTIGUOUS :: arr_ptr(:, :, :)
        INTEGER :: igrid, idx

        !$omp target teams loop bind(teams) map(from: computed_even, computed_odd) private(idx)
        DO igrid = 1, ngrids
            idx = (igrid - 1) * cpg + 1

#ifndef _BIND_THREAD_
            !$omp parallel
#endif
            CALL inner1(computed_even(idx), REAL(igrid))
#ifndef _BIND_THREAD_
            !$omp end parallel
#endif

#ifndef _BIND_THREAD_
            !$omp parallel
#endif
            CALL inner2(computed_odd(idx), REAL(igrid))
#ifndef _BIND_THREAD_
            !$omp end parallel
#endif
        END DO
        !$omp end target teams loop

        CALL compare(computed_even, expected_even)
        CALL compare(computed_odd, expected_odd)
    END SUBROUTINE test


    SUBROUTINE inner1(ptr, val)
        !$omp declare target
        REAL, INTENT(INOUT), DIMENSION(cpd, cpd, cpd) :: ptr
        REAL, INTENT(IN) :: val

        INTEGER :: i, j, k

        !$omp loop &
#ifdef _BIND_THREAD_
        !$omp bind(thread) &
#else
        !$omp bind(parallel) &
#endif
        !$omp collapse(3)
        DO i = 1, cpd
            DO j = 1, cpd
                DO k = 1, cpd
                    ptr(k, j, i) = val
                END DO
            END DO
        END DO
        !$omp end loop
    END SUBROUTINE inner1


    SUBROUTINE inner2(ptr, val)
        !$omp declare target
        REAL, INTENT(INOUT), DIMENSION(cpd, cpd, cpd) :: ptr
        REAL, INTENT(IN) :: val

        INTEGER :: i, j, k

        !$omp loop &
#ifdef _BIND_THREAD_
        !$omp bind(thread) &
#else
        !$omp bind(parallel) &
#endif
        !$omp collapse(3)
        DO i = 1, cpd
            DO j = 1, cpd
                DO k = 1, cpd
                    ptr(k, j, i) = val**2
                END DO
            END DO
        END DO
        !$omp end loop
    END SUBROUTINE inner2


    SUBROUTINE compute_expected()
        INTEGER :: igrid, idx, odd

        DO igrid = 1, ngrids
            idx = (igrid - 1) * cpg + 1
            CALL inner1(expected_even(idx), REAL(igrid))
            CALL inner2(expected_odd(idx), REAL(igrid))
        END DO
    END SUBROUTINE compute_expected


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
END PROGRAM reproducer
