PROGRAM reproducer
    IMPLICIT NONE

    CHARACTER(10) :: msg
    INTEGER :: num
    REAL :: val

    PRINT*, "Attempting to print string literal:"
    !$omp target
    PRINT*, "string literal"
    !$omp end target

    PRINT*, "Attempting to print char array:"
    msg = "char array"
    !$omp target
    PRINT*, msg 
    !$omp end target

    PRINT*, "Attempting to print integer:"
    !$omp target
    num = 42
    PRINT*, num
    !$omp end target

    PRINT*, "Attempting to print real:"
    !$omp target
    val = 10.0
    PRINT*, val
    !$omp end target

    PRINT*, "Attempting to print combination of all:" 
    !$omp target
    PRINT*, "string literal", msg, num, val
    !$omp end target
END PROGRAM reproducer
