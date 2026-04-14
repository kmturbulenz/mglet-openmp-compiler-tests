PROGRAM main
    USE realfield_mod
    USE grids_mod
    USE fields_mod
    USE fieldmapper_mod
    USE const_mod
    USE compute_mod

    IMPLICIT NONE

    TYPE(field_t), POINTER :: x_f, y_f, z_f, validation_f

    CALL init_grids()
    CALL init_fields()

    CALL set_field("X")
    CALL set_field("Y")
    CALL set_field("Z")
    CALL set_field("VALIDATION")

    CALL get_field(x_f, "X")
    CALL get_field(y_f, "Y")
    CALL get_field(z_f, "Z")
    CALL get_field(validation_f, "VALIDATION")

    x_f%arr = 1.0
    y_f%arr = 2.0
    z_f%arr = -1.0
    validation_f%arr = -1.0

    !$omp target update to(mapper(maparr): x_f)
    !$omp target update to(mapper(maparr): y_f)
    !$omp target update to(mapper(maparr): z_f)

    CALL get_expected()
    CALL compute()

    !$omp target update from(mapper(maparr): z_f)

    CALL compare(z_f%arr, validation_f%arr)

    CALL finish_fields()
    CALL finish_grids()
CONTAINS
    SUBROUTINE get_expected()
        INTEGER :: igrid, kk, jj, ii
        REAL :: alpha
        TYPE(field_t), POINTER :: x_f, y_f, validation_f
        REAL, CONTIGUOUS, POINTER, DIMENSION(:, :, :) :: x, y, validation

        CALL get_field(x_f, "X")
        CALL get_field(y_f, "Y")
        CALL get_field(validation_f, "VALIDATION")

        DO igrid = 1, ngrid
            CALL get_dims(kk, jj, ii, igrid)

            CALL get_ptr3(x_f, x, igrid)
            CALL get_ptr3(y_f, y, igrid)
            CALL get_ptr3(validation_f, validation, igrid)

            alpha = REAL(igrid)
            CALL inner(kk, jj, ii, x, y, validation, alpha)
        END DO
    END SUBROUTINE get_expected

    SUBROUTINE compare(res, expected)
        REAL, INTENT(in), DIMENSION(:) :: res, expected
        REAL, PARAMETER :: tolerance = 1.0e-6

        IF (SIZE(res) /= SIZE(expected)) THEN
            PRINT *, "Test FAILED! Arrays have different sizes."
            RETURN
        END IF

        IF (ALL(ABS(res - expected) < tolerance)) THEN
            PRINT *, "Test passed!"
        ELSE
            PRINT *, "Test FAILED!"
        END IF
    END SUBROUTINE compare
END PROGRAM main
