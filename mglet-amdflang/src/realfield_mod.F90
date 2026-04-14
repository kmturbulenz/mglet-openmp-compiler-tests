MODULE realfield_mod
    USE basefield_mod
    USE grids_mod
    IMPLICIT NONE
    PRIVATE

    TYPE, EXTENDS(basefield_t) :: field_t
        REAL, ALLOCATABLE :: arr(:)
    CONTAINS
        PROCEDURE :: init
        PROCEDURE :: finish
        FINAL :: destructor
    END TYPE field_t

    PUBLIC :: field_t, get_ptr3, get_ptr1
CONTAINS
    SUBROUTINE init(this, name)
        CLASS(field_t), INTENT(OUT), TARGET :: this
        CHARACTER(len=*), INTENT(in) :: name

        CALL this%init_corefield(name)
        ALLOCATE(this%arr(this%idim))
    END SUBROUTINE

    SUBROUTINE get_ptr3(field, ptr, igrid)
        !$omp declare target
        TYPE(field_t), INTENT(in), TARGET :: field 
        REAL, POINTER, CONTIGUOUS, INTENT(out) :: ptr(:, :, :)
        INTEGER, INTENT(in) :: igrid

        INTEGER :: ip, length, kk, jj, ii
        
        ip = field%ptr(igrid)
        length = field%length(igrid)
       
        CALL get_dims(kk, jj, ii, igrid)

        ptr(1:kk, 1:jj, 1:ii) => field%arr(ip:ip+length-1)
    END SUBROUTINE get_ptr3

    SUBROUTINE get_ptr1(field, ptr, igrid)
        !$omp declare target
        TYPE(field_t), INTENT(in), TARGET :: field
        REAL, POINTER, CONTIGUOUS, INTENT(out) :: ptr(:)
        INTEGER, INTENT(in) :: igrid

        INTEGER :: ip, length

        ip = field%ptr(igrid)
        length = field%length(igrid)

        ptr(1:length) => field%arr(ip:ip+length-1)
    END SUBROUTINE get_ptr1

    ELEMENTAL SUBROUTINE finish(this)
        CLASS(field_t), INTENT(inout) :: this

        IF (.NOT. this%is_init) RETURN
        CALL this%finish_corefield()

        DEALLOCATE(this%arr)
    END SUBROUTINE finish

    ELEMENTAL SUBROUTINE destructor(this)
        TYPE(field_t), INTENT(inout) :: this

        CALL this%finish()
    END SUBROUTINE destructor
END MODULE realfield_mod
