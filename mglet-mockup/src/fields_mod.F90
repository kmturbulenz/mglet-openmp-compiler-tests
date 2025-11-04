MODULE fields_mod
    USE realfield_mod
    USE basefield_mod

    IMPLICIT NONE
    PRIVATE

    ! Define mapper here since we only map the whole structure in fields_mod at the moment
    ! (LLVM) Mappers are only visible in the module they are defined in (is reported)
    ! (Intel) Not clear yet, but removing the mapper here crashes the whole system
    !$omp declare mapper(field_t :: f) map(f%arr, f%len, f%ptr)

    INTEGER, PARAMETER :: nfields_max = 1000
    INTEGER :: nfields = 0
    TYPE(field_t), TARGET :: fields(nfields_max)

    PUBLIC :: init_fields, finish_fields, set_field, get_field
CONTAINS
    SUBROUTINE init_fields()
        CONTINUE
    END SUBROUTINE init_fields

    SUBROUTINE finish_fields()
        INTEGER :: i

        DO i = 1, nfields
            CALL fields(i)%finish()
            !$omp target exit data map(release: fields(i))
        END DO
    END SUBROUTINE finish_fields

    SUBROUTINE set_field(name, found)
        CHARACTER(len=*), INTENT(IN) :: name
        LOGICAL, INTENT(OUT), OPTIONAL :: found

        TYPE(field_t), POINTER :: dummy
        LOGICAL :: exists
        
        CALL get_field(dummy, name, exists)
        IF (exists) THEN
            WRITE(*, '("Field ", A, " exists already!")') name
            CALL EXIT(1)
        END IF

        IF (nfields + 1 > nfields_max) CALL EXIT(1)
        nfields = nfields + 1

        CALL fields(nfields)%init(name)
        !$omp target enter data map(to: fields(nfields))

        IF (PRESENT(found)) THEN
            found = .FALSE.
        END IF
    END SUBROUTINE set_field

    SUBROUTINE get_field(field, name, found)
        TYPE(field_t), POINTER, INTENT(INOUT) :: field
        CHARACTER(len=*), INTENT(IN) :: name
        LOGICAL, OPTIONAL, INTENT(OUT) :: found

        INTEGER :: i
        LOGICAL :: thisfound
        CHARACTER(len=nchar_name) :: name2
        
        IF (LEN_TRIM(name) > nchar_name) CALL EXIT(1)
        name2 = name

        NULLIFY(field)
        thisfound = .FALSE.
        DO i = 1, nfields
            IF (fields(i)%name == name2) THEN
                field => fields(i)
                thisfound = .TRUE.
                EXIT
            END IF
        END DO

        IF (PRESENT(found)) THEN
            found = thisfound
        ELSE
            IF (.NOT. thisfound) THEN
                WRITE(*, *) "Could not find field: ", TRIM(name)
                CALL EXIT(1)
            END IF
        END IF
    END SUBROUTINE get_field
END MODULE fields_mod
