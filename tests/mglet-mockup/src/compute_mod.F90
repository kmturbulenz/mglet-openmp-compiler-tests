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

        INTEGER :: igrid, kk, jj, ii, ip3

        CALL get_field(field, "A")

        !$omp target teams distribute shared(field) private(ip3, kk, jj, ii)
        DO igrid = 1, ngrid
            CALL get_dims(kk, jj, ii, igrid)
            ip3 = field%ptr(igrid)
            !$omp parallel
            CALL inner(kk, jj, ii, field%arr(ip3), REAL(igrid))
            !$omp end parallel
        END DO
        !$omp end target teams distribute
    END SUBROUTINE compute

    SUBROUTINE inner(kk, jj, ii, ptr, val)
        !$omp declare target
        INTEGER, INTENT(IN) :: kk, jj, ii
        REAL, INTENT(INOUT), DIMENSION(kk, jj, ii) :: ptr
        REAL, INTENT(IN) :: val
        INTEGER :: i, j, k

        !$omp do collapse(3)
        DO i = 1, ii 
            DO j = 1, jj 
                DO k = 1, kk 
                    ptr(k, j, i) = val
                END DO
            END DO
        END DO
        !$omp end do
    END SUBROUTINE inner
END MODULE compute_mod