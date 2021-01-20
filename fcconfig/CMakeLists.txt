# check and show the Fortran compiler flags, libraries, etc
# some code from FHI-aims CMakeLists.txt
#
# the compiler and related flags needs to be set *before* the project command

# set minimum cmake version
cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

set(CMAKE_Fortran_COMPILER "ifort" CACHE STRING "")
# set is used for setting environment variable or cache entry
# see https://cmake.org/cmake/help/latest/command/set.html
# CACHE <type> <docstring>
#
# instead of environment variable, cache entry is written to CMakeCache.txt
# Here for CMAKE_Fortran_COMPILER, it is automatically cached when include Fortran in LANGUAGES of the project
# but if not, removing CACHE will also remove CMAKE_Fortran_COMPILER from the CMakeCache.txt
#
# to make compiler change work, one needs to remove CMakeCache.txt and run cmake again
#
# cache entry can be defined from command line by -DCACHE_ENTRY="value"
# for more, see
#     https://hsf-training.github.io/hsf-training-cmake-webpage/05-variables/index.html

# set Fortran flags by set
set(CMAKE_Fortran_FLAGS "-O3 -ip -r8" CACHE STRING "")

set(USE_MPI OFF CACHE BOOL "use of MPI")
# option is a shortcut for bool cache entry.
# so the equivalent command follows
#option(USE_MPI "use of MPI" OFF)

# project name and language
project(fcconfig LANGUAGES Fortran)

# print some information by message
message("-- (minyez) Using Fortran compiler: ${CMAKE_Fortran_COMPILER}")
if(NOT CMAKE_Fortran_COMPILER)
message(FATAL_ERROR "fail to find fortran compiler")
endif()
message("-- (minyez) Fortran compiler vendor: ${CMAKE_Fortran_COMPILER_VERSION} from ${CMAKE_Fortran_COMPILER_ID}")
message("-- (minyez) Fortran compiler flags: ${CMAKE_Fortran_FLAGS}")
message("-- (minyez) is GNU gfortran: ${CMAKE_COMPILER_IS_GNUF}")
