MODULE basefield_mod
    USE grids_mod
    IMPLICIT NONE
    PRIVATE

    INTEGER, PARAMETER :: nchar_name = 16

    TYPE, ABSTRACT :: basefield_t
        LOGICAL :: is_init = .FALSE.
        CHARACTER(len=nchar_name) :: name = REPEAT(" ", nchar_name)

        INTEGER :: idim
        INTEGER, ALLOCATABLE :: ptr(:)
        INTEGER, ALLOCATABLE :: len(:)
    CONTAINS
        PROCEDURE :: init_corefield
        PROCEDURE :: get_ip
        PROCEDURE :: get_len
        PROCEDURE :: finish_corefield
    END TYPE basefield_t

    PUBLIC :: basefield_t, nchar_name
CONTAINS
    SUBROUTINE init_corefield(this, name)
        CLASS(basefield_t), INTENT(OUT) :: this
        CHARACTER(len=*), INTENT(in) :: name

        INTEGER :: igrid, kk, jj, ii
        
        this%is_init = .TRUE.

        IF (LEN_TRIM(name) > nchar_name) CALL EXIT(1)
        this%name = name

        ALLOCATE(this%ptr(ngrids), source=0)
        ALLOCATE(this%len(ngrids), source=0)

        this%idim = 0
        DO igrid = 1, ngrids
            CALL get_dims(kk, jj, ii, igrid)
            this%len(igrid) = kk * jj * ii  
            this%ptr(igrid) = this%idim + 1
            this%idim = this%idim + this%len(igrid)
        END DO
    END SUBROUTINE init_corefield

    SUBROUTINE get_ip(this, ip, igrid)
        !$omp declare target
        CLASS(basefield_t), INTENT(IN) :: this
        INTEGER, INTENT(OUT) :: ip
        INTEGER, INTENT(IN) :: igrid

        ip = this%ptr(igrid)
    END SUBROUTINE get_ip

    SUBROUTINE get_len(this, len, igrid)
        !$omp declare target
        CLASS(basefield_t), INTENT(IN) :: this
        INTEGER, INTENT(OUT) :: len
        INTEGER, INTENT(IN) :: igrid

        len = this%len(igrid)
    END SUBROUTINE get_len

    ELEMENTAL SUBROUTINE finish_corefield(this)
        CLASS(basefield_t), INTENT(INOUT) :: this

        IF (.NOT. this%is_init) RETURN

        this%is_init = .FALSE.
        this%idim = 0
        DEALLOCATE(this%len)
        DEALLOCATE(this%ptr)
    END SUBROUTINE finish_corefield
END MODULE basefield_mod
