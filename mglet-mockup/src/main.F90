PROGRAM main
    USE realfield_mod
    USE grids_mod
    USE fields_mod
    USE const_mod
    USE compute_mod

    IMPLICIT NONE

    REAL, ALLOCATABLE, TARGET :: expected_result(:)
    TYPE(field_t), POINTER :: field, field_b

    CALL init_grids()
    CALL init_fields()
    CALL set_field("A")
    CALL set_field("B")
    ALLOCATE(expected_result(DOMAIN_NGRIDS * GRID_II * GRID_JJ * GRID_KK), source=0.0)

    CALL compute()
    CALL get_field(field, "A")
    ! (LLVM) Custom mappers are not yet supported in 'update'
    !$omp target update from(field%arr)

    CALL get_expected()
    CALL compare(field%arr, expected_result)

    CALL get_field(field_b, "B")
    field_b%arr = 2.0
    !$omp target update to(field_b%arr)
    !$omp target
    field_b%arr = 1.0
    !$omp end target


    !$omp target update from(field_b%arr)
    expected_result = 1.0
    CALL compare(field_b%arr, expected_result)

    CALL finish_fields()
    CALL finish_grids()
    DEALLOCATE(expected_result)
CONTAINS
    SUBROUTINE get_expected()
        INTEGER :: i, ip3, kk, jj, ii

        ip3 = 1
        DO i = 1, ngrids
            CALL get_dims(kk, jj, ii, i)
            CALL expected_kernel(kk, jj, ii, expected_result(ip3), REAL(i))
            ip3 = ip3 + ii * jj * kk
        END DO
    END SUBROUTINE get_expected

    SUBROUTINE expected_kernel(kk, jj, ii, buf, val)
        INTEGER, INTENT(IN) :: kk, jj, ii
        REAL, INTENT(INOUT), DIMENSION(kk, jj, ii) :: buf
        REAL, INTENT(IN) :: val

        INTEGER :: i, j, k

        DO i = 1, ii
            DO j = 1, jj 
                DO k = 1, kk 
                    buf(k, j, i) = val
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
END PROGRAM main
