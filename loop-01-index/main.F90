PROGRAM reproducer
    IMPLICIT NONE

    INTEGER, PARAMETER :: ngrids = 3, cpd = 32, cpg = cpd**3, ncells = ngrids * cpg
    REAL, ALLOCATABLE, TARGET, DIMENSION(:) :: arr, expected_result
    INTEGER :: igrid, ip3

    ALLOCATE(arr(ncells), source=1.0)
    ALLOCATE(expected_result(ncells), source=0.0)

    !$omp target teams loop bind(teams) shared(arr) private(ip3)
    DO igrid = 1, ngrids
        ip3 = (igrid - 1) * cpg + 1
#if defined(__INTEL_COMPILER)
            !$omp parallel
#endif
        CALL inner(arr(ip3), REAL(igrid))
#if defined(__INTEL_COMPILER)
            !$omp end parallel
#endif
    END DO
    !$omp end target teams loop 

    CALL get_expected()
    CALL compare(arr, expected_result)

    DEALLOCATE(arr)
    DEALLOCATE(expected_result)
CONTAINS
    SUBROUTINE inner(ptr, val)
        !$omp declare target
        REAL, INTENT(INOUT), DIMENSION(cpd, cpd, cpd) :: ptr
        REAL, INTENT(IN) :: val

        INTEGER :: i, j, k

        !$omp loop &
#if defined(__INTEL_COMPILER) || defined(__NVCOMPILER)
        !$omp bind(parallel) &
#elif defined(__flang__) || defined(_CRAYFTN) || defined(__GFORTRAN__)
        !$omp bind(thread) &
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
    END SUBROUTINE inner

    SUBROUTINE get_expected()
        INTEGER :: i, idx

        DO i = 1, ngrids
            idx = (i - 1) * cpg + 1
            CALL inner(expected_result(idx), REAL(i))
        END DO
    END SUBROUTINE get_expected

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
