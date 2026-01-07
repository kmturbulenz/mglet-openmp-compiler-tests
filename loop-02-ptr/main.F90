! Optionally set -D_PTR_TYP_ to switch to a global subroutine instead of a
! procedure of the field type to get ptr

MODULE const_mod
    INTEGER, PARAMETER :: ngrids = 64
    INTEGER, PARAMETER :: cpd = 32
    INTEGER, PARAMETER :: cpg = cpd**3
    INTEGER, PARAMETER :: ncells = ngrids * cpg
END MODULE const_mod

MODULE field_mod
    USE const_mod
    IMPLICIT NONE

    TYPE :: field_t
        REAL, ALLOCATABLE :: arr(:)
    CONTAINS
        PROCEDURE :: get_ptr_class
    END TYPE field_t
CONTAINS
    SUBROUTINE get_ptr_class(this, ptr, i)
        !$omp declare target
        CLASS(field_t), INTENT(INOUT), TARGET :: this
        REAL, POINTER, CONTIGUOUS, INTENT(OUT) :: ptr(:, :, :)
        INTEGER, INTENT(IN) :: i

        INTEGER :: ip3
        ip3 = (i - 1) * cpg + 1

        ptr(1:cpd, 1:cpd, 1:cpd) => this%arr(ip3:ip3+cpg-1)
    END SUBROUTINE get_ptr_class

    SUBROUTINE get_ptr_typ(field, ptr, i)
        !$omp declare target
        TYPE(field_t), INTENT(INOUT), TARGET :: field
        REAL, POINTER, CONTIGUOUS, INTENT(OUT) :: ptr(:, :, :)
        INTEGER, INTENT(IN) :: i

        INTEGER :: ip3
        ip3 = (i - 1) * cpg + 1

        ptr(1:cpd, 1:cpd, 1:cpd) => field%arr(ip3:ip3+cpg-1)
    END SUBROUTINE get_ptr_typ
END MODULE field_mod

PROGRAM reproducer
    USE field_mod
    IMPLICIT NONE

    REAL, ALLOCATABLE, TARGET, DIMENSION(:) :: expected_result
    ALLOCATE(expected_result(ncells), source=0.0)
    CALL compute_expected()

    CALL test()
    DEALLOCATE(expected_result)
CONTAINS
    SUBROUTINE test()
        TYPE(field_t) :: field
        REAL, POINTER, CONTIGUOUS :: arr_ptr(:, :, :)
        INTEGER :: igrid

        ALLOCATE(field%arr(ncells), source=1.0)

        !$omp target teams loop bind(teams) private(arr_ptr)
        DO igrid = 1, ngrids
#ifdef _PTR_TYP_
            CALL get_ptr_typ(field, arr_ptr, igrid)
#else
            CALL field%get_ptr_class(arr_ptr, igrid)
#endif
#if defined(__INTEL_COMPILER)
            !$omp parallel
#endif
#if defined(_CRAYFTN)
            ! Cray currently has a bug such that data copying is only
            ! correct if inlining is disabled when calling the kernel
            ! if the pointer is constructed through a subroutine
            ! taking in a derived type
            !DIR$ NOINLINE
#endif
            CALL inner(arr_ptr, REAL(igrid))
#if defined(_CRAYFTN)
            !DIR$ RESETINLINE
#endif
#if defined(__INTEL_COMPILER)
            !$omp end parallel
#endif
        END DO
        !$omp end target teams loop 

        CALL compare(field%arr, expected_result)

        DEALLOCATE(field%arr)
    END SUBROUTINE test

    SUBROUTINE inner(ptr, val)
        !$omp declare target
        REAL, INTENT(INOUT), DIMENSION(cpd, cpd, cpd) :: ptr
        REAL, INTENT(IN) :: val

        INTEGER :: i, j, k

        !$omp loop &
#if defined(__INTEL_COMPILER) || defined(__NVCOMPILER)
        !$omp bind(parallel) &
#elif defined(__flang__) || defined(_CRAYFTN) || defined(__GFORTRAN__)
        !$omp bind(thread) &
#endif
        !$omp collapse(3)
        DO i = 1, cpd 
            DO j = 1, cpd 
                DO k = 1, cpd 
                    ptr(k, j, i) = val
                END DO
            END DO
        END DO
        !$omp end loop
    END SUBROUTINE inner

    SUBROUTINE compute_expected()
        INTEGER :: i, idx

        DO i = 1, ngrids
            idx = (i - 1) * cpg + 1
            CALL inner(expected_result(idx), REAL(i))
        END DO
    END SUBROUTINE compute_expected

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
END PROGRAM reproducer
