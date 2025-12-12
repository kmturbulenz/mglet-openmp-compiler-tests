MODULE compute_mod
    USE realfield_mod
    USE fields_mod
    USE grids_mod
    IMPLICIT NONE
    PRIVATE

    PUBLIC :: compute, inner ! Export inner to get the expected values
CONTAINS
    SUBROUTINE compute()
        TYPE(field_t), POINTER :: field

        INTEGER :: igrid, kk, jj, ii
        REAL, CONTIGUOUS, POINTER  :: grid_ptr(:, :, :)

        CALL get_field(field, "A")

        !$omp target teams loop bind(teams) shared(field) private(grid_ptr, kk, jj, ii)
        DO igrid = 1, ngrids
            CALL get_dims(kk, jj, ii, igrid)
            CALL field%get_ptr(grid_ptr, igrid)
            CALL inner(kk, jj, ii, grid_ptr, REAL(igrid))
        END DO
        !$omp end target teams loop
    END SUBROUTINE compute

    SUBROUTINE inner(kk, jj, ii, ptr, val)
        !$omp declare target
        INTEGER, INTENT(IN) :: kk, jj, ii
        REAL, INTENT(INOUT), DIMENSION(kk, jj, ii) :: ptr
        REAL, INTENT(IN) :: val
        INTEGER :: i, j, k

        !$omp loop collapse(3) &
#if defined(__INTEL_COMPILER) || defined(__GFORTRAN__) || defined(_CRAYFTN) || defined(__flang__)
        !$omp bind(thread)
#elif defined(__NVCOMPILER)
        !$omp bind(parallel)
#endif
        DO i = 1, ii 
            DO j = 1, jj 
                DO k = 1, kk 
                    ptr(k, j, i) = val
                END DO
            END DO
        END DO
        !$omp end loop
    END SUBROUTINE inner
END MODULE compute_mod