MODULE grids_mod
    USE const_mod
    IMPLICIT NONE
    PRIVATE

    TYPE :: gridinfo_t
        INTEGER :: igrid
        INTEGER :: ii
        INTEGER :: jj
        INTEGER :: kk
    END TYPE gridinfo_t
    !$omp declare mapper(gridinfo_t :: g) map(g%ii, g%jj, g%kk, g%igrid)

    TYPE(gridinfo_t), ALLOCATABLE :: gridinfo(:)
    INTEGER :: ngrids

    !$omp declare target(gridinfo, ngrids)

    PUBLIC :: init_grids, get_dims, ngrids, finish_grids
CONTAINS
    SUBROUTINE init_grids()
        INTEGER :: i

        ngrids = DOMAIN_NGRIDS

        ALLOCATE(gridinfo(ngrids))
        DO i = 1, ngrids
            gridinfo(i)%igrid=i
            gridinfo(i)%ii = GRID_II
            gridinfo(i)%jj = GRID_JJ
            gridinfo(i)%kk = GRID_KK
        END DO
        !$omp target enter data map(always, to: gridinfo)

        ! (Intel) Putting ngrids into the mapping above does not work (mapping overridden by custom mapper - is reported)
        !$omp target enter data map(always, to: ngrids)
    END SUBROUTINE init_grids

    SUBROUTINE get_dims(kk, jj, ii, igrid)
        !$omp declare target
        INTEGER, INTENT(OUT) :: kk, jj, ii
        INTEGER, INTENT(IN) :: igrid

        kk = gridinfo(igrid)%kk
        jj = gridinfo(igrid)%jj
        ii = gridinfo(igrid)%ii
    END SUBROUTINE get_dims

    SUBROUTINE finish_grids()
        !$omp target exit data map(release: gridinfo)
        DEALLOCATE(gridinfo)
    END SUBROUTINE finish_grids
END MODULE grids_mod
