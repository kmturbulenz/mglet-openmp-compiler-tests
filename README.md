# MGLET OpenMP compiler tests

As maintainers of the [MGLET CFD 
code](https://github.com/kmturbulenz/mglet-base) we are investigating 
how to offload the MGLET code to GPU and APU devices as these become 
more common. MGLET is written in Fortran, with roots back to the early 
1980's but has been heavily modernized over the last decade. Today it is 
a modern, flexible code that allows for efficient implementation of new 
physics and features using many modern software development principles.

During the work with implementing OpenMP offloading in MGLET we 
discovered bugs, implementation differences and other issues that lead 
to a number of difficulties. In order to assess what works and what not, 
we have created this collection of small code examples, all 
demonstrating operations that needs to work in MGLET. These were 
originally created to be able to pick features that work stable, not 
only on one system but across different hardware and software 
implementations.

The implementations and platforms presented here are just those that we 
happen to have access to, and new platforms might be added in the future 
in case we gain access to any.

We have realized that these tests/demonstrations can be useful for other 
Fortran programmers and/or compiler developers. We therefore decided to 
make this public, and hope that this collection will help enhance the 
quality of OpenMP offloading implementations in Fortran, both on the 
application side using OpenMP and on the implementation side creating 
the libraries and compilers.

# Testcases

## Usage
Follow these steps to compile and run the testcases with the provided Makefiles:

1. Setup relevant compiler toolchain
2. `cd` into any testcase
3. Use `FFLAGS=<...> make <intel;nvidia;gnu;llvm;cray>` to compile a testcase. The options `intel`, `gnu` and `llvm` requires adding the OpenMP offloading target platform in `FFLAGS`. Available options are listed in the table [Compiler Flags](#compiler-flags). OpenMP is enabled in the Makefile by default. You can also use `FFLAGS` to append other flags
4. `make run` to run the executable with mandatory OpenMP offloading

## Feature matrix

We obtained the data shown in the feature matrix below using the following compilers and hardware

| | Version | GPU |
|---|---|---|
| Intel oneAPI | ifx (IFX) 2025.3.0 20251023 | Intel Corporation Lunar Lake [Intel Graphics] |
| NVIDIA HPCSDK | nvfortran 25.9-0 64-bit target on x86-64 Linux -tp znver5 | NVIDIA GeForce RTX 5070 Ti |
| GNU | GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0 | NVIDIA GeForce RTX 5070 Ti |
| LLVM | flang version 22.0.0git (git@github.com:llvm/llvm-project.git a8058c177d0e45993108936cfca532d3dab037fc) | NVIDIA GeForce RTX 5070 Ti |
| Cray HLRS | Cray Fortran : Version 19.0.0 (20250207225012_cc4d36e4ff3377d45f0e6e892b5dacd82009a0ca) | AMD Instinct MI300A |

| Case | Intel oneAPI | NVIDIA HPCSDK | GNU | LLVM | Cray HLRS |
|---|---|---|---|---|---|
| builtin-01-math | &check; | &check; | &check; | Compiler crash | &check; |
| builtin-02-print | Prints "*" for non-string literals | &check; | Linking error | Compiler crash | No combination of string literals and numbers allowed |
| mapper-01-inter-module | &check; | Custom mappers unsupported | Custom mappers unsupported | &check; | Memory access fault (caused by custom mapper) |
| mapper-02-basetype | &check; | Custom mappers unsupported | Custom mappers unsupported | &check; | &check; |
| mapper-03-variants | &check; | Custom mappers unsupported | Custom mappers unsupported | Mappers unsupported for target update construct  | Test invalid (USM) |
| mapper-04-dictfields | &check; | Custom mappers unsupported | Custom mappers unsupported | &check; | Test invalid (USM) |
| mapper-05-alloc-field | &check; | Custom mappers unsupported | Custom mappers unsupported | &check; | Test invalid (USM) |
| mapper-06-nested | &check; | Custom mappers unsupported | Custom mappers unsupported | Runtime segfault | Test invalid (USM) |
| module-01-buf | &check; | &check; | &check; | &check; | &check; |
| module-02-ptr-bigbuf | &check; | &check; | &check; | &check; | &check; |
| type-01-procedures | &check; | Call to type-bound procedures not allowed on device | &check; | &check; | &check; |
| type-02-generics | &check; | Call to type-bound procedures not allowed on device | &check; | &check; | &check; |
| type-03-basefunc | Runtime Linking error | Call to type-bound procedures not allowed on device | Runtime crash (llegal memory access) | &check; | Linker error |
| loop-01-index | &check; | &check; | &check; | &check; | &check; |
| loop-02-ptr-buf | &check; | Pointer assignment on target is unsupported | Runtime crash (llegal memory access) | &check; | &check; |
| loop-03-ptr-type | &check; | Call to type-bound procedures not allowed on device | &check; | &check; | Wrong result |

## MGLET mockup
The case `mglet-mockup` combines all complexity previously tested to run a very slimmed down version of the MGLET core functionality using OpenMP offloading. No specific computation is performed. Data management and best-practice iteration over the data is applied. Any necessary workarounds for compiler bugs or missing features that can be implemented with low effort are applied.
|  | Intel oneAPI | NVIDIA HPCSDK | GNU | LLVM | Cray HLRS |
|---|---|---|---|---|---|
| mglet-mockup | &check; | &cross; | &cross; | &check; | &cross; |

## Orphaned Loop Bind
Assume the following way of parallelizing a 3D multigrid domain in MGLET by distributing the grids over OpenMP teams and parallelizing within the teams.
```fortran
!$omp target teams loop bind(teams)
DO igrid = 1, ngrid
    CALL kernel()
END DO

SUBROUTINE kernel()
    !$omp declare target

    !$omp loop <bind(thread) or bind(parallel)> collapse(3)
    DO i = 1, ii
        DO j = 1, jj
            DO k = 1, kk
                ...
            END DO
        END DO
    END DO
END SUBROUTINE kernel
```
Depending on the compiler, either `bind(thread)` or `bind(parallel)` produce correct results or are supported in general.

|  | Intel oneAPI | NVIDIA HPCSDK | GNU | LLVM | Cray HLRS |
|---|---|---|---|---|---|
| `bind(thread)` | &check; | serializes within team | &check; | &check; <br> compiler warning | &check; |
| `bind(parallel)` | wrong result | &check; | application stalls indefinitely | &check; | wrong result

# OpenMP offloading notes
## Vendor compatability
The following table shows the OpenMP offloading compatibility between vendors and compilers.
| GPU Vendor | Intel oneAPI | NVIDIA HPCSDK | AOCC + [amdflang](https://github.com/amd/InfinityHub-CI/blob/main/fortran/README.md) | GNU | LLVM | Cray HLRS |
|---|---|---|---|---|---|---|
| Intel | &check; | &cross; | &cross; | &cross; | &cross; | &cross; |
| NVIDIA | &cross; | &check; | &cross; | &check; | &check; | &cross; |
| AMD | &cross; | &cross; | &check; | &check; | &check; | &check; |

## Compiler flags
The following table shows the flags for compilation in order to instruct the compiler to generate target code.
| Compiler | OpenMP Flag | Target Flag | Target Platforms |
|---|---|---|---|
| Intel oneAPI | `-fiopenmp` | `-fopenmp-targets=...` | `OFF`, `spir64`, `spir64_x86_64`, `spir64_gen` |
| GNU | `-fopenmp` | `-foffload=...` | `nvptx-none`, `amdgcn-amdhsa`, `default`, `disable` |
| NVIDIA HPCSDK | `-mp` | `-mp=gpu` (only NVIDIA GPUs, specify architecture with additional flag `-gpu=cc120`) | - |
| AOCC + [amdflang](https://github.com/amd/InfinityHub-CI/blob/main/fortran/README.md) | `-fopenmp` | e.g. `--offload-arch=...` (only AMD GPUs, specify architecture with e.g. `MI250` or `gfx90a`) | - |
| LLVM | `-fopenmp` | `-fopenmp-targets` | `x86_64-unknown-linux-gnu`, `nvptx64-nvidia-cuda`, `amdgcn-amd-amdhsa`, (`aarch64-unknown-linux-gnu`, `powerpc64le-ibm-linux-gnu`) | 
| Cray HLRS | `-fopenmp` for cc and CC, `-h omp` for ftn | activated by default | - | 

## Environment variables
The following table shows environment flags that are useful for debugging OpenMP offloading issues during runtime.
| Compiler | Flags | Reference |
|---|---|---|
| Intel oneAPI | `LIBOMPTARGET_DEBUG=<0;1;2>`: OpenMP runtime debug information runtime<br>`LIBOMPTARGET_INFO=<0;1;2;4;8;32>`: Basic Offloading information runtime<br>`LIBOMPTARGET_PLUGIN_PROFILE=<F;T;T,usec>`: Enables display of performance data | [Intel docs](https://www.intel.com/content/www/us/en/docs/oneapi/programming-guide/2023-1/oneapi-debug-tools.html) |
| GNU | `GOMP_DEBUG=<0;1>`: OpenMP runtime debug information | [GNU docs](https://gcc.gnu.org/onlinedocs/libgomp/Environment-Variables.html) |
| NVIDIA HPCSDK | `NVCOMPILER_ACC_NOTIFY=<0;1;2;4;8;16>`: Runtime debug information<br>`NVCOMPILER_OMP_DISABLE_WARNINGS=<false;true>`: Generate warnings during runtime | [HPCSDK docs](https://docs.nvidia.com/hpc-sdk/compilers/hpc-compilers-user-guide/) |
| AOCC + [amdflang](https://github.com/amd/InfinityHub-CI/blob/main/fortran/README.md) | `LIBOMPTARGET_DEBUG=<0;1>`: OpenMP runtime debug information<br>`LIBOMPTARGET_INFO=<0;1;-1>`: Device information<br>`LIBOMPTARGET_KERNEL_TRACE=<0;1;2>`: Kernel information | [AMD docs](https://rocm.docs.amd.com/en/docs-6.1.0/about/compatibility/openmp.html) |
| LLVM | `LIBOMPTARGET_DEBUG=<0;1>`: OpenMP runtime debug information (only if LLVM is compiled with `-DOMPTARGET_DEBUG`)<br>`LIBOMPTARGET_INFO=<0;1;2;4;8;16;32>`: Offloading information<br>`LIBOMPTARGET_PROFILE=<filename>`: Generate time profile output (only if LLVM is compiled with `OPENMP_ENABLE_LIBOMP_PROFILING=ON`)<br>`LIBOMPTARGET_PROFILE_GRANULARITY=<us>`: Set time profile granularity in us | [LLVM docs](https://openmp.llvm.org/design/Runtimes.html) |
| Cray HLRS | `CRAY_ACC_DEBUG=<0;1;2;3>`: OpenMP runtime debug information | [HLRS docs](https://kb.hlrs.de/platforms/index.php/Programming_Models), [HPE docs](https://cpe.ext.hpe.com/docs/24.03/guides/CCE/HPE_Cray_Fortran_Reference_Manual_17.0.1_S-3901.html) | 
