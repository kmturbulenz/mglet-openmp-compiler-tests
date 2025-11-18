MODULE realfield_mod
    IMPLICIT NONE

    TYPE field_t
        REAL, ALLOCATABLE :: buf(:)
        INTEGER :: id
    CONTAINS
        PROCEDURE :: init
        PROCEDURE :: finish
    END TYPE field_t
    ! (Intel) If this mapper is not explicitly defined, then we get a runtime error
    !$omp declare mapper(field_t :: f) map(f%buf)

    INTEGER, PARAMETER :: n = 1024  
CONTAINS
    SUBROUTINE init(this, id)
        CLASS(field_t), INTENT(INOUT) :: this
        INTEGER, INTENT(IN) :: id
        
        this%id = id
        ALLOCATE(this%buf(n), source=1.0)
    END SUBROUTINE init

    SUBROUTINE finish(this)
        CLASS(field_t), INTENT(INOUT) :: this

        DEALLOCATE(this%buf)
    END SUBROUTINE
END MODULE realfield_mod

MODULE fields_mod
    USE realfield_mod
    IMPLICIT NONE

    INTEGER, PARAMETER :: nfields_max = 1000
    INTEGER :: nfields = 0
    TYPE(field_t), TARGET :: fields(nfields_max)
CONTAINS
    SUBROUTINE finish_fields()
        INTEGER :: i
        DO i = 1, nfields
            CALL fields(i)%finish()
            !$omp target exit data map(release: fields(i))
        END DO
    END SUBROUTINE finish_fields

    SUBROUTINE set_field(id)
        INTEGER, INTENT(IN) :: id

        IF (nfields + 1 > nfields_max) RETURN
        nfields = nfields + 1
        CALL fields(nfields)%init(id)

        !$omp target enter data map(to: fields(nfields)) 
    END SUBROUTINE set_field

    SUBROUTINE get_field(field, id)
        TYPE(field_t), POINTER, INTENT(INOUT) :: field
        INTEGER, INTENT(IN) :: id

        INTEGER :: i
        DO i = 1, nfields
            IF (fields(i)%id == id) THEN
                field => fields(i)
                EXIT
            END IF
        END DO
    END SUBROUTINE
END MODULE fields_mod

PROGRAM dict_fields
    USE fields_mod
    IMPLICIT NONE

    REAL, ALLOCATABLE :: expected_result(:)
    TYPE(field_t), POINTER :: field_1, field_2
    REAL, PARAMETER :: val1 = 42.0, val2 = 10.0, val3 = -1.0, val4 = -2.0
    LOGICAL :: equal1, equal2
    equal1 = .FALSE.
    equal2 = .FALSE.
    ALLOCATE(expected_result(n))

    ! Allocate fields
    CALL set_field(1)
    CALL set_field(2)

    ! Write to fields on device
    CALL modify_field(1, val1, .TRUE.)
    CALL modify_field(2, val2, .TRUE.)

    ! Copy values back to host and compare
    CALL get_field(field_1, 1)
    CALL get_field(field_2, 2)

    ! (LLVM) Mappers are not supported here yet
    ! (INTEL) Using mappers here crashes the compiler
    !$omp target update from(field_1%buf, field_2%buf)
    expected_result = val1
    CALL compare(field_1%buf, expected_result)
    expected_result = val2
    CALL compare(field_2%buf, expected_result)

    ! Write to field on host
    CALL modify_field(1, val3, .FALSE.)
    CALL modify_field(2, val4, .FALSE.)
    ! Copy values back to device (don't copy field_1)
    !$omp target update to(field_2%buf)

    !$omp target map(alloc: expected_result) map(from: equal1, equal2)
    ! field_1 should still be on val1 since we did not copy it back to the device
    expected_result = val1
    CALL compare_on_device(field_1%buf, expected_result, equal1)
    expected_result = val4
    CALL compare_on_device(field_2%buf, expected_result, equal2)
    !$omp end target

    CALL print_test_result(equal1)
    CALL print_test_result(equal2)

    CALL finish_fields()
    DEALLOCATE(expected_result)
CONTAINS
    SUBROUTINE modify_field(id, val, on_device)
        INTEGER, INTENT(IN) :: id
        REAL, INTENT(IN) :: val
        LOGICAL, INTENT(IN) :: on_device

        TYPE(field_t), POINTER :: ptr
        CALL get_field(ptr, id)

        IF (on_device) THEN
            !$omp target 
            ptr%buf(:) = val
            !$omp end target
        ELSE
            ptr%buf(:) = val
        END IF
    END SUBROUTINE modify_field

    SUBROUTINE compare(result, expected)
        REAL, INTENT(in), DIMENSION(:) :: result, expected
        REAL, PARAMETER :: tolerance = 1.0e-6

        LOGICAL :: is_equal

        is_equal = ALL(ABS(result - expected) < tolerance)
        CALL print_test_result(is_equal)
    END SUBROUTINE compare

    SUBROUTINE compare_on_device(result, expected, is_equal)
        !$omp declare target
        REAL, INTENT(IN), DIMENSION(:) :: result, expected
        LOGICAL, INTENT(OUT) :: is_equal
        REAL, PARAMETER :: tolerance = 1.0e-6
        
        IF (ALL(ABS(result - expected) < tolerance)) THEN
            is_equal = .TRUE.
        ELSE
            is_equal = .FALSE.
        END IF
    END SUBROUTINE compare_on_device

    SUBROUTINE print_test_result(result)
        LOGICAL, INTENT(IN) :: result

        IF (result) THEN
            PRINT *, "Test passed!"
        ELSE
            PRINT *, "Test FAILED!"
        END IF
    END SUBROUTINE print_test_result
END PROGRAM dict_fields
