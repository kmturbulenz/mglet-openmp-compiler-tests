MODULE grids_mod
    INTEGER, PARAMETER :: ngrids = 64

    INTEGER :: ncells
    INTEGER :: grid_dims(ngrids)
    INTEGER :: grid_offsets(ngrids)
    !$omp declare target(grid_dims, grid_offsets)
CONTAINS
    SUBROUTINE init_grids()
        INTEGER :: i, offset

        offset = 1
        DO i = 1, ngrids
            grid_dims(i) = 32 + i / 8
            grid_offsets(i) = offset
            ncells = ncells + grid_dims(i)**3
            offset = offset + grid_dims(i)**3
        END DO

        !$omp target update to(grid_dims, grid_offsets)
    END SUBROUTINE init_grids

    SUBROUTINE get_dims(kk, jj, ii, igrid)
        !$omp declare target
        INTEGER, INTENT(OUT) :: kk, jj, ii
        INTEGER, INTENT(IN) :: igrid

        kk = grid_dims(igrid)
        jj = grid_dims(igrid)
        ii = grid_dims(igrid)
    END SUBROUTINE get_dims

    SUBROUTINE get_ip3(ip3, igrid)
        !$omp declare target
        INTEGER, INTENT(OUT) :: ip3
        INTEGER, INTENT(IN) :: igrid

        ip3 = grid_offsets(igrid)
    END SUBROUTINE get_ip3
END MODULE grids_mod

MODULE field_mod
    USE grids_mod
    IMPLICIT NONE

    TYPE :: field_t
        REAL, ALLOCATABLE :: arr(:)
    END TYPE field_t
END MODULE field_mod

PROGRAM reproducer
    USE field_mod
    IMPLICIT NONE

    REAL, ALLOCATABLE, TARGET, DIMENSION(:) :: expected_result
    CALL init_grids()
    ALLOCATE(expected_result(ncells), source=0.0)

    CALL compute_expected()
    CALL test()
    DEALLOCATE(expected_result)
CONTAINS
    SUBROUTINE test()
        TYPE(field_t) :: field
        INTEGER :: igrid, ip3, kk, jj, ii

        ALLOCATE(field%arr(ncells), source=1.0)

        !$omp target teams loop bind(teams) shared(field) private(ip3, kk, jj, ii)
        DO igrid = 1, ngrids
            CALL get_dims(kk, jj, ii, igrid)
            CALL get_ip3(ip3, igrid)
#if defined(__INTEL_COMPILER)
            !$omp parallel
#endif
            CALL inner(kk, jj, ii, field%arr(ip3), REAL(igrid))
#if defined(__INTEL_COMPILER)
            !$omp end parallel
#endif
        END DO
        !$omp end target teams loop 

        CALL compare(field%arr, expected_result)

        DEALLOCATE(field%arr)
    END SUBROUTINE test

    SUBROUTINE inner(kk, jj, ii, ptr, val)
        !$omp declare target
        INTEGER, INTENT(IN) :: kk, jj, ii
        REAL, INTENT(INOUT), DIMENSION(kk, jj, ii) :: ptr
        REAL, INTENT(IN) :: val

        INTEGER :: i, j, k

        !$omp loop &
#if defined(__INTEL_COMPILER) || defined(__NVCOMPILER)
        !$omp bind(parallel) &
#elif defined(__flang__) || defined(_CRAYFTN) || defined(__GFORTRAN__)
        !$omp bind(thread) &
#endif
        !$omp collapse(3)
        DO i = 1, ii
            DO j = 1, jj
                DO k = 1, kk
                    ptr(k, j, i) = val
                END DO
            END DO
        END DO
        !$omp end loop
    END SUBROUTINE inner

    SUBROUTINE compute_expected()
        INTEGER :: igrid, ip3, kk, jj, ii

        DO igrid = 1, ngrids
            CALL get_dims(kk, jj, ii, igrid)
            CALL get_ip3(ip3, igrid)
            CALL inner(kk, jj, ii, expected_result(ip3), REAL(igrid))
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
