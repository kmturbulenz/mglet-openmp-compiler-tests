MODULE const_mod
    IMPLICIT NONE
    PRIVATE

    INTEGER, PARAMETER :: num_elements = 1024

    PUBLIC :: num_elements
END MODULE const_mod

MODULE realfield_mod
    USE const_mod
    IMPLICIT NONE
    PRIVATE

    TYPE field_t
        REAL, ALLOCATABLE :: arr(:)
        REAL, ALLOCATABLE :: buffers(:)
        INTEGER :: id
    CONTAINS
        PROCEDURE :: init
        PROCEDURE :: finish
    END TYPE field_t

    PUBLIC :: field_t
CONTAINS
    SUBROUTINE init(this, id, buffers)
        CLASS(field_t), INTENT(INOUT) :: this
        INTEGER, INTENT(IN) :: id
        LOGICAL, INTENT(IN) :: buffers
        
        this%id = id
        ALLOCATE(this%arr(num_elements), source=1.0)
        IF (buffers) ALLOCATE(this%buffers(num_elements), source=2.0)
    END SUBROUTINE init

    SUBROUTINE finish(this)
        CLASS(field_t), INTENT(INOUT) :: this

        DEALLOCATE(this%arr)
        IF (ALLOCATED(this%buffers)) DEALLOCATE(this%buffers)
    END SUBROUTINE
END MODULE realfield_mod

MODULE mapper_mod
    USE realfield_mod, ONLY: field_t

    PRIVATE :: field_t

    ! (Intel) Using custom mappers for derived types POINTER types can cause
    !         crashes. It appears that in some cases the custom mapper is not
    !         properly recognized such that allocations are not mapped. The
    !         bug can be avoided if the custom mapper is not defined in the
    !         module of the derived type itself but more closely to the region
    !         where it is used. For this reason, this mapper_mod is introduced
    !         that can be USE'd whenever a custom mapper is required (and any
    !         target regions accessing member of POINTER types is started).
    !$omp declare mapper(arr: field_t :: t) map(t%arr)
    !$omp declare mapper(arrbufs: field_t :: t) map(t%arr, t%buffers)
END MODULE mapper_mod

MODULE fields_mod
    USE mapper_mod
    USE realfield_mod
    IMPLICIT NONE

    INTEGER, PARAMETER :: nfields_max = 1000
    INTEGER :: nfields = 0
    TYPE(field_t), TARGET :: fields(nfields_max)
CONTAINS
    SUBROUTINE finish_fields()
        INTEGER :: i
        DO i = 1, nfields
            IF (ALLOCATED(fields(i)%buffers)) THEN
                !$omp target exit data map(mapper(arrbufs), release: fields(i))
            ELSE
                !$omp target exit data map(mapper(arr), release: fields(i))
            END IF
            CALL fields(i)%finish()
        END DO
    END SUBROUTINE finish_fields

    SUBROUTINE set_field(id, buffers)
        INTEGER, INTENT(IN) :: id
        LOGICAL, INTENT(IN) :: buffers

        IF (nfields + 1 > nfields_max) RETURN
        nfields = nfields + 1
        CALL fields(nfields)%init(id, buffers)

        IF (buffers) THEN
            !$omp target enter data map(mapper(arrbufs), to: fields(nfields))
        ELSE
            !$omp target enter data map(mapper(arr), to: fields(nfields))
        END IF
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

PROGRAM dictfields
    USE const_mod
    USE fields_mod
    IMPLICIT NONE

    REAL, ALLOCATABLE :: expected_result(:)
    TYPE(field_t), POINTER :: field_1, field_2
    REAL, PARAMETER :: val1 = 42.0, val2 = 10.0, val3 = -1.0
    LOGICAL :: equal1, equal2, equal3
    equal1 = .FALSE.
    equal2 = .FALSE.
    equal3 = .FALSE.
    ALLOCATE(expected_result(num_elements))

    ! Allocate fields
    CALL set_field(1, .FALSE.) ! (host) arr=1.0,         (target) arr=1.0
    CALL set_field(2, .TRUE.)  ! (host) arr=1.0, buf=2.0 (target) arr=1.0, buf=2.0
    CALL get_field(field_1, 1)
    CALL get_field(field_2, 2)

    ! Write to fields on device
    CALL set_field_values(1, val1, .TRUE.) ! (host) arr=1.0           (target) arr=42.0
    CALL set_field_values(2, val2, .TRUE.) ! (host) arr=1.0, buf=2.0  (target) arr=42.0, buf=2.0

    ! Check values on target
    !$omp target map(alloc: expected_result) map(from: equal1, equal2, equal3)
    expected_result = val1
    CALL compare_on_device(field_1%arr, expected_result, equal1)
    expected_result = val2
    CALL compare_on_device(field_2%arr, expected_result, equal2)
    expected_result = 2.0
    CALL compare_on_device(field_2%buffers, expected_result, equal3)
    !$omp end target
    CALL print_test_result(equal1)
    CALL print_test_result(equal2)
    CALL print_test_result(equal3)

    ! Check values after copying back
    !$omp target update from(field_1%arr, field_2%arr)
    ! field_1: (host) arr=42.0           (target) arr=42.0
    ! field_2: (host) arr=42.0, buf=2.0  (target) arr=42.0, buf=2.0
    expected_result = val1
    CALL compare(field_1%arr, expected_result)
    expected_result = val2
    CALL compare(field_2%arr, expected_result)
    expected_result = 2.0
    CALL compare(field_2%buffers, expected_result)

    field_2%buffers = val3 ! (host) arr=42.0, buf=-1.0  (target) arr=42.0, buf=2.0
    expected_result = val3
    CALL compare(field_2%buffers, expected_result)

    !$omp target update to(field_2%buffers) ! (host) arr=42.0, buf=-1.0  (target) arr=42.0, buf=-1.0
    !$omp target map(alloc: expected_result) map(from: equal1)
    expected_result = val3
    CALL compare_on_device(field_2%buffers, expected_result, equal1)
    !$omp end target
    CALL print_test_result(equal1)

    CALL finish_fields()
    DEALLOCATE(expected_result)
CONTAINS
    SUBROUTINE set_field_values(field_id, val, on_device)
        INTEGER, INTENT(IN) :: field_id
        REAL, INTENT(IN) :: val
        LOGICAL, INTENT(IN) :: on_device

        INTEGER :: i
        TYPE(field_t), POINTER :: field
        CALL get_field(field, field_id)

        IF (on_device) THEN
            !$omp target teams loop
            DO i = 1, num_elements
                field%arr(i) = val
            END DO
            !$omp end target teams loop
        ELSE
            field%arr(:) = val
        END IF
    END SUBROUTINE set_field_values

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
END PROGRAM dictfields
