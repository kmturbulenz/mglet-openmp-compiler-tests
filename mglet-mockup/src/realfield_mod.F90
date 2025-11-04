MODULE realfield_mod
    USE basefield_mod
    USE grids_mod
    IMPLICIT NONE
    PRIVATE

    TYPE, EXTENDS(basefield_t) :: field_t
        REAL, ALLOCATABLE :: arr(:)
    CONTAINS
        PROCEDURE :: init
        GENERIC :: get_ptr => get_ptr1, get_ptr3
        PROCEDURE, PRIVATE, NON_OVERRIDABLE :: get_ptr1, get_ptr3
        PROCEDURE :: finish
        FINAL :: destructor
    END TYPE field_t

    PUBLIC :: field_t
CONTAINS
    SUBROUTINE init(this, name)
        CLASS(field_t), INTENT(OUT), TARGET :: this
        CHARACTER(len=*), INTENT(in) :: name

        CALL this%init_corefield(name)
        ALLOCATE(this%arr(this%idim))
    END SUBROUTINE

    SUBROUTINE get_ptr3(this, ptr, igrid)
        !$omp declare target
        CLASS(field_t), INTENT(in), TARGET :: this
        REAL, POINTER, CONTIGUOUS, INTENT(out) :: ptr(:, :, :)
        INTEGER, INTENT(in) :: igrid

        INTEGER :: ip, len, kk, jj, ii
        ! (Intel) Causes a linking error
        ! (Cray) Call to base type not supported
        !CALL this%get_ip(ip, igrid)
        !CALL this%get_len(len, igrid)

        ! Workaround: inline manually
        ip = this%ptr(igrid)
        len = this%len(igrid)
       
        CALL get_dims(kk, jj, ii, igrid)

        ptr(1:kk, 1:jj, 1:ii) => this%arr(ip:ip+len-1)
    END SUBROUTINE get_ptr3

    SUBROUTINE get_ptr1(this, ptr, igrid)
        !$omp declare target
        CLASS(field_t), INTENT(in), TARGET :: this
        REAL, POINTER, CONTIGUOUS, INTENT(out) :: ptr(:)
        INTEGER, INTENT(in) :: igrid

        INTEGER :: ip, len
        ! (Intel) Causes a linking error
        ! (Cray) Call to base type not supported
        !CALL this%get_ip(ip, igrid)
        !CALL this%get_len(len, igrid)

        ! Workaround: inline manually
        ip = this%ptr(igrid)
        len = this%len(igrid)

        ptr(1:len) => this%arr(ip:ip+len-1)
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
