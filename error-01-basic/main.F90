
! Idea by https://doku.lrz.de/files/11497087/11497089/8/1745835439403/OpenMP-Workshop-Day-3.pdf

MODULE errr_mod

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: int32

    IMPLICIT NONE
    PUBLIC :: errr

CONTAINS

    SUBROUTINE errr(fname, line)
    !$omp declare target

        CHARACTER(len=*), INTENT(in) :: fname
        INTEGER(int32), INTENT(in) :: line

        !$omp error message("File: " // fname)

    END SUBROUTINE errr

END MODULE errr_mod



PROGRAM reproducer

    USE errr_mod
    IMPLICIT NONE

    INTEGER, PARAMETER :: ncells = 1000
    REAL, ALLOCATABLE, TARGET, DIMENSION(:) :: arr

    ALLOCATE(arr(ncells), source=1.0)
    arr(456) = -1.0

    CALL test_negative(arr, ncells)

    DEALLOCATE(arr)

CONTAINS

    SUBROUTINE test_negative(arr, ncells)
        REAL, INTENT(inout), DIMENSION(:) :: arr
        INTEGER, INTENT(in) :: ncells

        REAL :: val
        INTEGER :: i

        !$omp target teams distribute parallel do
        DO i = 1, ncells

            val = arr(i)
            IF (val < 0.0) THEN
                CALL errr(__FILE__, __LINE__)
            END IF

        END DO
        !$omp end target teams distribute parallel do

    END SUBROUTINE test_negative

END PROGRAM reproducer
