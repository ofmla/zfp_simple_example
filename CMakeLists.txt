cmake_minimum_required(VERSION 3.16)
project(fortran_zfp_example LANGUAGES C Fortran)

# Set the directory where Fortran module files will be placed
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

# Set the URL for retrieving the zfp library
set("zfp-url" "https://github.com/LLNL/zfp/releases/download/1.0.1/zfp-1.0.1.zip")
message(STATUS "Retrieving zfp from ${zfp-url}")

# Include FetchContent module to download zfp library
include(FetchContent)

# Configure zfp library build options
set(BUILD_ZFORP ON CACHE INTERNAL "")
set(BUILD_UTILITIES OFF CACHE INTERNAL "")
set(BUILD_TESTING OFF CACHE INTERNAL "")

# Declare the zfp library to be fetched from the specified URL
FetchContent_Declare(
  "zfp" 
  URL ${zfp-url}
)

# Download and make the zfp library available
FetchContent_MakeAvailable("zfp")

# Add executable target with Fortran source files
add_executable(exe zfp_example.f90 zfp_fct_wrapper.f90)

# Link the zfp and zFORp libraries to the executable target
target_link_libraries(exe zfp zFORp)
