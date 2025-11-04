MODULE types_mod
    IMPLICIT NONE

    TYPE, ABSTRACT :: base_t
    CONTAINS
        PROCEDURE :: base_func
        PROCEDURE :: other_base_func
    END TYPE base_t

    TYPE, EXTENDS(base_t) :: child_t
    CONTAINS
        PROCEDURE :: child_func
        PROCEDURE :: other_child_func
    END TYPE child_t
CONTAINS
    SUBROUTINE other_child_func(this)
        CLASS(child_t), INTENT(IN) :: this
    END SUBROUTINE other_child_func

    SUBROUTINE other_base_func(this)
        CLASS(base_t), INTENT(IN) :: this
    END SUBROUTINE other_base_func

    SUBROUTINE base_func(this)
        !$omp declare target
        CLASS(base_t), INTENT(IN) :: this
    END SUBROUTINE base_func

    SUBROUTINE child_func(this)
        !$omp declare target
        CLASS(child_t), INTENT(IN) :: this

        ! This call is critical. If this is commented out then child_destructor
        ! is NOT required to be marked as !$omp declare target
        CALL this%base_func()
    END SUBROUTINE child_func
END MODULE types_mod

PROGRAM main
    USE types_mod
    IMPLICIT NONE

    TYPE(child_t) :: f_t

    !$omp target
    CALL f_t%child_func()
    !$omp end target

    PRINT*, "Passed!"
END PROGRAM main
