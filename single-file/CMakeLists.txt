# set minimum cmake version
cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

# project name and language
project(single-file LANGUAGES Fortran)

add_executable(test_kind test_kind.f90)
