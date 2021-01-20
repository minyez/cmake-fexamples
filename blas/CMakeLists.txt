# set minimum cmake version
cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

# project name and language
project(test_detect_blas LANGUAGES Fortran)

# For cmake to find lapack libraries, one way is to add lapack directory to prefix path
set(CMAKE_PREFIX_PATH "/home/stevezhang/software/lapack-3.8.0-gcc-7.3.1/")

# we can check the Fortran compiler and limit the lapack library accordingly
## For Intel MKL
#set(BLA_VENDOR Intel10_64lp_seq)
#
# FindLAPACK module of cmake. variables:
#     LAPACK_FOUND
#     LAPACK_LINKER_FLAGS
#     LAPACK_LIBRARIES
#
# ==================================
# common part
# ==================================
# require blas and lapack library
find_package(BLAS REQUIRED)
find_package(LAPACK REQUIRED)

# configure module
add_library(eigsolve "")
target_sources(eigsolve
  PRIVATE
  eigsolve.f90
  )
target_include_directories(eigsolve
  PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
  )
# linking to lapack
target_link_libraries(eigsolve
  PUBLIC
    ${LAPACK_LIBRARIES}
  )

add_executable(test_detect_blas "")

target_sources(test_detect_blas
  PRIVATE
  detect_blas.f90
  )

target_link_libraries(detect_blas
  PRIVATE
    eigsolve
  )
