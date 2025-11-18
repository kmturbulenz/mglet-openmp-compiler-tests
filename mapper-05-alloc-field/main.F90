PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    TYPE :: field_t
        REAL, ALLOCATABLE :: a(:)
        REAL, ALLOCATABLE :: b(:)
    END TYPE field_t
    !$omp declare mapper(field_t :: f) map(f%a, f%b)
    !$omp declare mapper(custom: field_t :: f) map(alloc: f%a) map(to: f%b)

    INTEGER, PARAMETER :: n = 3
    INTEGER :: initial_device
    REAL, ALLOCATABLE :: expected_buf(:)
    TYPE(field_t) :: field

    initial_device = omp_get_initial_device()
    ALLOCATE(expected_buf(n), source= 0.0)
    ALLOCATE(field%a(n), source=1.0)
    ALLOCATE(field%b(n), source=2.0)

    !$omp target enter data map(mapper(custom), to: field)

    !$omp target map(alloc: expected_buf)
#if defined(__INTEL_COMPILER) || defined(__NVCOMPILER)
    ! ========== field%a is just allocated and not mapped to ==========
    expected_buf = 1.0
    CALL compare(field%a, expected_buf, invert=.TRUE.)
    ! =================================================================
    expected_buf = 2.0
    CALL compare(field%b, expected_buf)
#endif
    field%b = 4.0
    !$omp end target
    
    expected_buf = 1.0
    CALL compare(field%a, expected_buf)
    expected_buf = 2.0
    CALL compare(field%b, expected_buf)
    !$omp target update from(field%a, field%b)
    expected_buf = 3.0
    CALL compare(field%a, expected_buf, invert=.TRUE.)
    expected_buf = 4.0
    CALL compare(field%b, expected_buf) 

    DEALLOCATE(expected_buf)
    DEALLOCATE(field%a)
    DEALLOCATE(field%b)
CONTAINS
    SUBROUTINE compare(result, expected, invert)
        !$omp declare target
        REAL, INTENT(in), DIMENSION(:) :: result, expected
        LOGICAL, INTENT(in), OPTIONAL :: invert
        REAL, PARAMETER :: tolerance = 1.0e-6
        LOGICAL :: do_invert, test_passed

        IF (PRESENT(invert)) THEN
            do_invert = invert
        ELSE
            do_invert = .FALSE.
        END IF

        IF (SIZE(result) /= SIZE(expected)) THEN
            PRINT *, "Test FAILED! Arrays have different sizes."
            RETURN
        END IF

        IF (omp_get_device_num() /= initial_device) THEN
            PRINT *, "(device)"
        ELSE
            PRINT *, "(host)"
        END IF

        test_passed = ALL(ABS(result - expected) < tolerance)
        IF (do_invert) test_passed = .NOT. test_passed

        IF (test_passed) THEN
            PRINT *, "Test passed!"
        ELSE
            PRINT *, "Test FAILED!"
        END IF
    END SUBROUTINE compare
END PROGRAM main
