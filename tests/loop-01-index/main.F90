MODULE const_mod
    INTEGER, PARAMETER :: ngrids = 64
    INTEGER, PARAMETER :: cpd = 32
    INTEGER, PARAMETER :: cpg = cpd**3
    INTEGER, PARAMETER :: ncells = ngrids * cpg
END MODULE const_mod

MODULE field_mod
    USE const_mod
    IMPLICIT NONE

    TYPE :: field_t
        REAL, ALLOCATABLE :: arr(:)
    END TYPE field_t
END MODULE field_mod

PROGRAM reproducer
    USE field_mod
    IMPLICIT NONE

    REAL, ALLOCATABLE, TARGET, DIMENSION(:) :: expected_result
    ALLOCATE(expected_result(ncells), source=0.0)
    CALL compute_expected()

    CALL test()
    DEALLOCATE(expected_result)
CONTAINS
    SUBROUTINE test()
        TYPE(field_t) :: field
        INTEGER :: igrid, ip3

        ALLOCATE(field%arr(ncells), source=1.0)

        !$omp target teams distribute shared(field) private(ip3)
        DO igrid = 1, ngrids
            ip3 = (igrid - 1) * cpg + 1

            !$omp parallel
            CALL inner(field%arr(ip3), REAL(igrid))
            !$omp end parallel
        END DO
        !$omp end target teams distribute

        CALL compare(field%arr, expected_result)

        DEALLOCATE(field%arr)
    END SUBROUTINE test

    SUBROUTINE inner(ptr, val)
        !$omp declare target
        REAL, INTENT(INOUT), DIMENSION(cpd, cpd, cpd) :: ptr
        REAL, INTENT(IN) :: val

        INTEGER :: i, j, k

        !$omp do collapse(3)
        DO i = 1, cpd 
            DO j = 1, cpd 
                DO k = 1, cpd 
                    ptr(k, j, i) = val
                END DO
            END DO
        END DO
        !$omp end do
    END SUBROUTINE inner

    SUBROUTINE compute_expected()
        INTEGER :: igrid, ip3

        DO igrid = 1, ngrids
            ip3 = (igrid - 1) * cpg + 1
            CALL inner(expected_result(ip3), REAL(igrid))
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
