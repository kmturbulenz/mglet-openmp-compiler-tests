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
        CALL offload_kernel(arr(ip3), REAL(igrid))
    END DO
    !$omp end target teams loop 

    CALL get_expected()
    CALL compare(arr, expected_result)

    DEALLOCATE(arr)
    DEALLOCATE(expected_result)
CONTAINS
    SUBROUTINE offload_kernel(ptr, val)
        !$omp declare target
        REAL, INTENT(INOUT), DIMENSION(cpd, cpd, cpd) :: ptr
        REAL, INTENT(IN) :: val

        INTEGER :: i, j, k

        !$omp loop collapse(3) &
#if defined(__INTEL_COMPILER) || defined(__GFORTRAN__) || defined(_CRAYFTN)
        !$omp bind(thread)
#elif defined(__NVCOMPILER) || defined(__flang__)
        !$omp bind(parallel)
#endif
        DO i = 1, cpd 
            DO j = 1, cpd 
                DO k = 1, cpd 
                    ptr(k, j, i) = val
                END DO
            END DO
        END DO
        !$omp end loop
    END SUBROUTINE offload_kernel

    SUBROUTINE get_expected()
        INTEGER :: i, idx

        DO i = 1, ngrids
            idx = (i - 1) * cpg + 1
            CALL expected_kernel(expected_result(idx), REAL(i))
        END DO
    END SUBROUTINE get_expected

    SUBROUTINE expected_kernel(ptr, val)
        REAL, INTENT(INOUT), DIMENSION(cpd, cpd, cpd) :: ptr
        REAL, INTENT(IN) :: val

        INTEGER :: i, j, k

        DO i = 1, cpd 
            DO j = 1, cpd 
                DO k = 1, cpd 
                    ptr(k, j, i) = val
                END DO
            END DO
        END DO
    END SUBROUTINE expected_kernel

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
