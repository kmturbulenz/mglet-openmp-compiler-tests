MODULE compute_mod
    USE realfield_mod
    USE fields_mod
    USE grids_mod

    IMPLICIT NONE
    PRIVATE

    PUBLIC :: compute, inner
CONTAINS
    SUBROUTINE compute()
        TYPE(field_t), POINTER :: x_f, y_f, z_f

        INTEGER :: igrid, kk, jj, ii
        REAL :: alpha
        REAL, CONTIGUOUS, POINTER, DIMENSION(:, :, :) :: x, y, z

        CALL get_field(x_f, "X")
        CALL get_field(y_f, "Y")
        CALL get_field(z_f, "Z")

        !$omp target teams distribute private(kk, jj, ii, x, y, z, alpha)
        DO igrid = 1, ngrid
            CALL get_dims(kk, jj, ii, igrid)
            CALL get_ptr3(x_f, x, igrid)
            CALL get_ptr3(y_f, y, igrid)
            CALL get_ptr3(z_f, z, igrid)

            alpha = REAL(igrid)
            CALL inner(kk, jj, ii, x, y, z, alpha)
        END DO
        !$omp end target teams distribute
    END SUBROUTINE compute

    SUBROUTINE inner(kk, jj, ii, x, y, z, alpha)
        !$omp declare target
        INTEGER, INTENT(IN) :: kk, jj, ii
        REAL, INTENT(IN), DIMENSION(kk, jj, ii) :: x, y
        REAL, INTENT(INOUT), DIMENSION(kk, jj, ii) :: z
        REAL, INTENT(IN) :: alpha 
        INTEGER :: i, j, k

        !$omp parallel do collapse(3)
        DO i = 3, ii - 2
            DO j = 3, jj - 2 
                DO k = 3, kk - 2 
                    z(k, j, i) = alpha * x(k, j, i) + y(k, j, i)
                END DO
            END DO
        END DO
        !$omp end parallel do 
    END SUBROUTINE inner
END MODULE compute_mod

