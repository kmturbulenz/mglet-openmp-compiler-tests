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

# Build Instructions

## Prerequisites

A compiler toolchain capable of OpenMP offloading must be available in the environment.

## Setup

Configure the project with CMake. Set OFFLOAD_TARGET_FLAG to the compiler-specific offload target option, see [Compiler Flags](#compiler-flags) below. OpenMP is automatically enabled for all relevant compiler suites.
```
mkdir build && cd build
cmake .. -DOFFLOAD_TARGET_FLAG=<...>
```

## Building Test Cases

Build all test cases with:
```
cmake --build .
```
To keep building independent targets after a build error, pass the underlying build-tool option:
- Make: `cmake --build . -- -k`
- Ninja: `cmake --build . -- -k 0`

To build an individual test case, specify the target:
```
cmake --build . --target <test-name>
```

## Running Test Cases

- Run individual tests with `OMP_TARGET_OFFLOAD=mandatory`.
- To execute all test cases, use the `run-tests.sh` script.

# Feature matrix

> [!WARNING]  
> The feature matrix below is no longer up to date. Several tests are currently being reevaluated and may not be correctly represented in the matrix below.

The data shown in the feature matrix below has been obtained using the following compilers and hardware:

| Compiler suite | Fortran Compiler version | GPU |
|---|---|---|
| Intel oneAPI | ifx (IFX) 2025.3.0 20251010 | Intel Data Center GPU Max 1550 |
| NVIDIA HPCSDK | nvfortran 26.3-0 64-bit target on x86-64 Linux -tp znver5 | NVIDIA GeForce RTX 5070 Ti |
| ROCm | AMD AFAR drop #23.2.0 04/18/26 flang version 23.0.0git (https://github.com/ROCm/llvm-project.git 35849413f758a222a8094acf1ec81eb80f601335+PATCHED:440716f8b87be9d8e20ed910e10e5b6d14d57cf6) | AMD Radeon AI PRO R9700 |
| GNU | GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0 | NVIDIA GeForce RTX 5070 Ti |
| LLVM | flang version 23.0.0git (git@github.com:llvm/llvm-project.git 84812fd1e7036ba28fb2136839b7a0a0d9010a63) | NVIDIA GeForce RTX 5070 Ti |
| Cray HLRS | Cray Fortran : Version 20.0.0 (20250827170814_8a1c0a28f36ae0bf2fce3f49eb977f008b7bbf87) | AMD Instinct MI300A |

| Case | Intel oneAPI | NVIDIA HPCSDK | ROCm | GNU | LLVM | Cray HLRS |
|---|---|---|---|---|---|---|
| builtin-01-math | &check; | &check; | &check; | &check; | &check; | &check; |
| builtin-02-print | Prints "*" for non-string literals | &check; | Cuts off char arrays | Linking error | &check; | No char arrays and combinations are allowed |
| builtin-03-ieee | &check; | &check; | &check; | &check; | &check; | &check; |
| builtin-04-real-precision | &check; | &check; | &check; | &check; | `real(10)` not supported on target | &check; |
| mapper-01-inter-module | &check; | Custom mappers unsupported | &check; | Custom mappers unsupported | &check; | &check; |
| mapper-02-basetype | &check; | Custom mappers unsupported | &check; | Custom mappers unsupported | &check; | &check; |
| mapper-03-variants | &check; | Custom mappers unsupported | &check; | Custom mappers unsupported | &check;  | mapper(default) does not compile |
| mapper-04-dictfields | Compiler crash | Custom mappers unsupported | &check; | Custom mappers unsupported | &check; | *Test invalid (USM)* |
| mapper-05-alloc-field | &check; | Custom mappers unsupported | &check; | Custom mappers unsupported | &check; | *Test invalid (USM)* |
| mapper-06-nested | &check; | Custom mappers unsupported | &check; | Custom mappers unsupported | &check; | *Test invalid (USM)* |
| module-01-buf | &check; | &check; | &check; | &check; | &check; | &check; |
| module-02-ptr-bigbuf | &check; | Illegal memory access | &check; | &check; | &check; | &check; |
| type-01-procedures | &check; | Call to type-bound procedures not allowed on device | &check; | &check; | &check; | &check; |
| type-02-generics | &check; | Call to type-bound procedures not allowed on device | &check; | &check; | &check; | &check; |
| type-03-basefunc | Runtime Linking error | Call to type-bound procedures not allowed on device | &check; | &check; | &check; | Linker error |
| loop-01-index | TBD | TBD | TBD | TBD | TBD | TBD |
| loop-02-ptr | TBD | TBD | TBD | TBD | TBD | TBD |
| loop-03-pass-dims | TBD | TBD | TBD | TBD | TBD | TBD |
| loop-04-multiple-parallel | TBD | TBD | TBD | TBD | TBD | TBD |

## MGLET mockup
The case `mglet-mockup` combines all complexity previously tested to run a very slimmed down version of the MGLET core functionality using OpenMP offloading. No specific computation is performed. Data management and best-practice iteration over the data is applied. Any necessary workarounds for compiler bugs or missing features that can be implemented with low effort are applied.
|  | Intel oneAPI | NVIDIA HPCSDK | ROCm | GNU | LLVM | Cray HLRS |
|---|---|---|---|---|---|---|
| mglet-mockup | &check; | &cross; | &check; | &cross; | &check; | Only with `-D_INDEX_` (default) and `-D_NO_CUSTOM_DEFAULT_MAPPER_` |

# OpenMP offloading notes
## Vendor compatability
The following table shows the OpenMP offloading compatibility between vendors and compiler suites:
| GPU Vendor | Intel oneAPI | NVIDIA HPCSDK | ROCm | GNU | LLVM | Cray HLRS |
|---|---|---|---|---|---|---|
| Intel | &check; | &cross; | &cross; | &cross; | &cross; | &cross; |
| NVIDIA | &cross; | &check; | &cross; | &check; | &check; | &check; |
| AMD | &cross; | &cross; | &check; | &check; | &check; | &check; |

## Compiler flags
The following table shows compiler flags required to instruct targets to offload to:
| Compiler | OpenMP Flag | Target Flag |
|---|---|---|
| ifx | `-fiopenmp` | `-fopenmp-targets=<spir64,spir64_x86_64,spir64_gen>` |
| nvfortran | `-mp` | `-mp=gpu`, optionally specify architecture with additional flag `-gpu=<cc80,cc90,cc120,etc.>`) |
| amdflang | `-fopenmp` | `--offload-arch=<gfx90a,gfx942,etc.>` |
| gfortran | `-fopenmp` | `-foffload=<nvptx-none,amdgcn-amdhsa,default,disable>` |
| flang | `-fopenmp` |`-fopenmp-targets=<nvptx64-nvidia-cuda,amdgcn-amd-amdhsa,etc.>` - architecture is automatically deduced for NVIDIA. Use `--offload-arch=<...>` for AMD GPUs (see amdflang reference) |
| ftn | `-fopenmp` works for cc, CC and ftn, `-h omp` only for ftn | Activated by default, `-fopenmp-targets=<...>` available but not mandatory |

## Environment variables
The following table shows environment flags that are useful for debugging OpenMP offloading issues during runtime:
| Compiler suite | Flags | Reference |
|---|---|---|
| Intel oneAPI | `LIBOMPTARGET_DEBUG=<0;1;2>`: OpenMP runtime debug information runtime<br>`LIBOMPTARGET_INFO=<0;1;2;4;8;32>`: Basic Offloading information runtime<br>`LIBOMPTARGET_PLUGIN_PROFILE=<F;T;T,usec>`: Enables display of performance data | [Intel docs](https://www.intel.com/content/www/us/en/docs/oneapi/programming-guide/2023-1/oneapi-debug-tools.html) |
| GNU | `GOMP_DEBUG=<0;1>`: OpenMP runtime debug information | [GNU docs](https://gcc.gnu.org/onlinedocs/libgomp/Environment-Variables.html) |
| NVIDIA HPCSDK | `NVCOMPILER_ACC_NOTIFY=<0;1;2;4;8;16>`: Runtime debug information<br>`NVCOMPILER_OMP_DISABLE_WARNINGS=<false;true>`: Generate warnings during runtime | [HPCSDK docs](https://docs.nvidia.com/hpc-sdk/compilers/hpc-compilers-user-guide/) |
| ROCm | `LIBOMPTARGET_DEBUG=<0;1>`: OpenMP runtime debug information<br>`LIBOMPTARGET_INFO=<0;1;-1>`: Device information<br>`LIBOMPTARGET_KERNEL_TRACE=<0;1;2>`: Kernel information | [AMD docs](https://rocm.docs.amd.com/projects/llvm-project/en/docs-7.2.0/conceptual/openmp.html) |
| LLVM | `LIBOMPTARGET_DEBUG=<0;1>`: OpenMP runtime debug information (only if LLVM is compiled with `-DOMPTARGET_DEBUG`)<br>`LIBOMPTARGET_INFO=<0;1;2;4;8;16;32>`: Offloading information<br>`LIBOMPTARGET_PROFILE=<filename>`: Generate time profile output (only if LLVM is compiled with `OPENMP_ENABLE_LIBOMP_PROFILING=ON`)<br>`LIBOMPTARGET_PROFILE_GRANULARITY=<us>`: Set time profile granularity in us | [LLVM docs](https://openmp.llvm.org/design/Runtimes.html) |
| Cray HLRS | `CRAY_ACC_DEBUG=<0;1;2;3>`: OpenMP runtime debug information | [HLRS docs](https://kb.hlrs.de/platforms/index.php/Programming_Models), [HPE docs](https://cpe.ext.hpe.com/docs/24.03/guides/CCE/HPE_Cray_Fortran_Reference_Manual_17.0.1_S-3901.html) |
