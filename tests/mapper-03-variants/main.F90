PROGRAM reproducer
    USE omp_lib

    IMPLICIT NONE

    TYPE :: field_t
        REAL, ALLOCATABLE :: a(:)
        REAL, ALLOCATABLE :: b(:)
    END TYPE field_t
    !$omp declare mapper(field_t :: f) map(f%a, f%b)
    !$omp declare mapper(a: field_t :: f) map(f%a)
    !$omp declare mapper(b: field_t :: f) map(f%b)

    INTEGER, PARAMETER :: n = 3
    INTEGER :: initial_device
    REAL, ALLOCATABLE :: expected_buf(:)
    TYPE(field_t) :: field

    initial_device = omp_get_initial_device()
    ALLOCATE(expected_buf(n), source= 0.0)
    ALLOCATE(field%a(n), source=1.0)
    ALLOCATE(field%b(n), source=2.0)

    !$omp target enter data map(mapper(default), to: field)

    PRINT *, "enter data"
    !$omp target map(alloc: expected_buf)
#if defined(__INTEL_COMPILER) || defined(__NVCOMPILER)
    expected_buf = 1.0
    CALL compare(field%a, expected_buf)
    expected_buf = 2.0
    CALL compare(field%b, expected_buf)
#endif
    field%a = 3.0
    field%b = 4.0
    !$omp end target
    
    PRINT *, "update from"
    !$omp target update from(mapper(a): field)
    expected_buf = 3.0
    CALL compare(field%a, expected_buf)
    expected_buf = 2.0
    CALL compare(field%b, expected_buf)
    
    PRINT *, "update to"
    field%a = 99.0
    field%b = 98.0
    !$omp target update to(mapper(b): field)
    !$omp target map(alloc: expected_buf)
#if defined(__INTEL_COMPILER) || defined(__NVCOMPILER)
    expected_buf = 3.0
    CALL compare(field%a, expected_buf)
    expected_buf = 98.0
    CALL compare(field%b, expected_buf)
#endif
    field%a = 42.0
    field%b = 43.0
    !$omp end target

    PRINT *, "exit data"
    !$omp target exit data map(mapper(default), from: field)
    expected_buf = 42.0
    CALL compare(field%a, expected_buf)
    expected_buf = 43.0
    CALL compare(field%b, expected_buf)

    DEALLOCATE(expected_buf)
    DEALLOCATE(field%a)
    DEALLOCATE(field%b)
CONTAINS
    SUBROUTINE compare(result, expected)
        !$omp declare target
        REAL, INTENT(in), DIMENSION(:) :: result, expected
        REAL, PARAMETER :: tolerance = 1.0e-6

        IF (SIZE(result) /= SIZE(expected)) THEN
            PRINT *, "Test FAILED! Arrays have different sizes."
            RETURN
        END IF

        IF (omp_get_device_num() /= initial_device) THEN
            PRINT *, "(device)"
        ELSE
            PRINT *, "(host)"
        END IF

        IF (ALL(ABS(result - expected) < tolerance)) THEN
            PRINT *, "Test passed!"
        ELSE
            PRINT *, "Test FAILED!"
        END IF
    END SUBROUTINE compare
END PROGRAM reproducer
