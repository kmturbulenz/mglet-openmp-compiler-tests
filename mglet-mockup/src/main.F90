PROGRAM main
    USE realfield_mod
    USE grids_mod
    USE fields_mod
    USE const_mod
    USE compute_mod

    IMPLICIT NONE

    REAL, ALLOCATABLE, TARGET :: expected_result(:)
    INTEGER :: i, ncells
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
    ncells = field_b%idim
    !$omp target update to(field_b%arr)
    !$omp target teams loop
    DO i = 1, ncells
        field_b%arr(i) = 1.0
    END DO
    !$omp end target teams loop


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
        DO i = 1, ngrid
            CALL get_dims(kk, jj, ii, i)
            CALL inner(kk, jj, ii, expected_result(ip3), REAL(i))
            ip3 = ip3 + ii * jj * kk
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
END PROGRAM main
