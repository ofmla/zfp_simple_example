# Fortran ZFP Example
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Fortran](https://img.shields.io/badge/Fortran-734f96?logo=fortran&style=flat)](https://fortran-lang.org)
![Build Status](https://github.com/ofmla/zfp_simple_example/actions/workflows/CI.yml/badge.svg)

This repository demonstrates how to integrate the [Fortran bindings for the ZFP library] (https://github.com/LLNL/zfp/blob/develop/fortran/zfp.f90) into a simple example that illustrates compression and decompression of a double-precision 3D array.

## Overview

[ZFP](https://github.com/LLNL/zfp) is an open-source library for compressed floating-point and integer arrays that support high throughput read and write random access. This repository focuses on integrating the Fortran bindings for ZFP into a Fortran project, showcasing its usage through a simple example. 

The `zfp_example.f90` file contains the main program, which demonstrates the compression and decompression of a 3D array of 64-bit floats using the ZFP library. It would be the equivalent of the [simple compressor example](https://github.com/LLNL/zfp/blob/develop/examples/simple.c). The `zfp_fct_wrapper.f90` file provides a Fortran module with functions for interacting with the ZFP library.


## Features

- Integration of Fortran bindings for the ZFP library.
- Simple example demonstrating compression and decompression of a 3D array of 64-bit floats.
- Easy-to-understand CMakeLists.txt file for building the example.

## Prerequisites

Before building and running the example, ensure you have the following prerequisites installed:

- Fortran compiler which fully supports F2018 (e.g., GNU Fortran, Intel Fortran)
- CMake (version 3.16 or higher)

## Build the app

1. Clone this repository:

    ```bash
    git clone https://github.com/ofmla/fortran-zfp-example.git
    ```

2. Build the project using CMake:

    ```bash
    cd fortran-zfp-example
    mkdir build
    cd build
    cmake ..
    cmake --build .
    ```

3. Run the executable with the appropriate command-line argument (`-c` for compress or `-d` for decompress). Executing

    ```bash
    ./exe -c
    ```
    will create an archive named `compressed.zfp`.

To decompress the compressed data and save it to `decompressed.zfp`, run the executable with the -d option:

  ```bash
    ./exe -d
  ```
  Attempting to decompress data without first compressing it will result in an error, as the `compressed.zfp` file does not exist. This example is built with double precision (real64) real values, but you can use `zfp_fct_wrapper.f90` in a project that uses real32 and explicitly specify the real kind with the preprocessor flag `-DREAL_KIND=REAL32`, i.e., `cmake -DREAL_KIND=REAL32 ..`.

## Contributing

Contributions are welcome! If you find any issues or have suggestions for improvements, please open an issue or create a pull request.

## License

This project is licensed under the [MIT License](https://github.com/ofmla/zfp_simple_example/blob/main/LICENSE)
