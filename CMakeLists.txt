cmake_minimum_required(VERSION 3.21)
project(mglet-openmp-compiler-tests LANGUAGES Fortran)

find_package(OpenMP REQUIRED)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_Fortran_FLAGS}")

set(OFFLOAD_TARGET_FLAG "" CACHE STRING "Offload target flag")
if(OFFLOAD_TARGET_FLAG STREQUAL "")
    message(WARNING "OFFLOAD_TARGET_FLAG is empty. No explicit offload target is configured for this build.")
endif()

if(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2 -fiopenmp")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2 -mp=gpu")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2 -fopenmp")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "LLVMFlang")
    # amdflang in ROCm is currently also identified as LLVMFlang.
    # Not a problem, since amdflang and flang share the same OpenMP flags.
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2 -fopenmp -fopenmp-version=52")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Cray")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2 -h omp")
else()
    message(FATAL_ERROR "Unsupported compiler: ${CMAKE_Fortran_COMPILER_ID}")
endif()
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OFFLOAD_TARGET_FLAG}")

add_subdirectory(tests)
