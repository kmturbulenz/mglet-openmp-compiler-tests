PROGRAM reproducer
    IMPLICIT NONE

    CHARACTER(50) :: msg
    INTEGER :: num
    REAL :: val

    !$omp target
        PRINT *, "String literal printed from target!"

        msg = "String in variable printed from target"
        PRINT *, msg

        num = 42
        PRINT *, num

        val = 2.0
        PRINT *, val

        PRINT *, "Combined:", num, val
    !$omp end target
END PROGRAM reproducer
