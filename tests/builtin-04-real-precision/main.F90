PROGRAM main
    IMPLICIT NONE

    ! This test exists since LLVM flang with NVIDIA hardware still seems to fail
    ! to compile REAL(10) if a target architecture is specified.

#if defined(_CRAYFTN) || defined(__INTEL_COMPILER)
    ! REAL(10) is not supported with ftn and ifx but REAL(16) is
    REAL(kind=16) :: val
#elif defined(__flang__) || defined(__GFORTRAN__)
    ! Using REAL(10) with -fopenmp-offload=<target> caused problems with flang
    REAL(kind=10) :: val
#elif defined(__NVCOMPILER)
    ! nvfotran and amdflang do not support REAL(10) nor REAL(16)
    REAL(kind=8) :: val
#endif
    val = 0.125

    PRINT*, val
    PRINT*, "Test passed!"
END PROGRAM main
