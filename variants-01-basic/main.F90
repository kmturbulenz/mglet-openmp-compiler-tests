MODULE omp_declare_variant_demo_mod

    IMPLICIT NONE

    PRIVATE

    INTEGER :: errc = 0
    !$omp declare target (errc)

    PUBLIC :: errr, check_errr

CONTAINS

    ! The function "errr" can be used in both host and target regions.
    ! The device variant "errr_dev" will be used in target regions and
    ! increments the error count instead of printing a message directly.

    SUBROUTINE errr()
        !$omp declare variant(errr_dev) match(construct={target})
        WRITE(*, *) "This would have detected a host error and stopped"
    END SUBROUTINE errr

    SUBROUTINE errr_dev()
        !$omp declare target
        errc = errc + 1
    END SUBROUTINE errr_dev


    SUBROUTINE check_errr()
        !$omp target update from(errc)
        IF (errc > 0) THEN
            WRITE(*, *) "This would have detected a device error and stopped"
        END IF
    END SUBROUTINE check_errr

END MODULE omp_declare_variant_demo_mod

PROGRAM test_omp_declare_variant

    USE omp_declare_variant_demo_mod
    IMPLICIT NONE


    ! >>> Now on host

    CALL errr()
    CALL check_errr()

    ! >>> Now on device

    !$omp target
    CALL errr()
    !$omp end target
    CALL check_errr()

END PROGRAM test_omp_declare_variant