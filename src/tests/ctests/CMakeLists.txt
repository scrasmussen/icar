cmake_minimum_required(VERSION 3.0)

project("ICAR CTests" LANGUAGES Fortran)
enable_testing()
# set where module files are
set(icar_build ../../build)
include_directories(${icar_build})


# --- setup compiler ---
if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU.*")
  set(CMAKE_Fortran_COMPILER "caf")
  message("Compiling with ${CMAKE_Fortran_COMPILER}")
  # setup cafrun
  find_program(cafrun cafrun)
  if (cafrun STREQUAL "cafrun-NOTFOUND")
    message(FATAL_ERROR "cafrun NOT FOUND ")
  endif()

  # set number of processes use
  if(DEFINED ENV{CAF_NP})
    set(caf_np $ENV{CAF_NP})
  else()
    set(caf_np 1)
  endif()
  set(caf_np_arg -n)
  # add_executable(cafrun IMPORTED) doesn't work
  message("Running coarray program with `${cafrun}` and ${caf_np} process(es)")
endif()


# --- ICAR makefile targets ---
set(ICAR_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../)
add_custom_target(build
  COMMAND MODE=debugslow F90=caf ${CMAKE_MAKE_PROGRAM}
  WORKING_DIRECTORY ${ICAR_SOURCE_DIR})

add_custom_target(clean-icar
  COMMAND ${CMAKE_MAKE_PROGRAM} clean
  WORKING_DIRECTORY ${ICAR_SOURCE_DIR})

add_custom_target(clean-all
  COMMAND ${CMAKE_MAKE_PROGRAM} clean all
  WORKING_DIRECTORY ${ICAR_SOURCE_DIR})


# --- ICAR Unit Tests ---
# generic ctests
add_executable(fortran_ctest_should_fail
  should_fail.f90)
add_test(NAME fortran_ctest_should_fail
  COMMAND fortran_ctest_should_fail)
set_tests_properties(fortran_ctest_should_fail
  PROPERTIES WILL_FAIL TRUE)

add_executable(fortran_ctest_should_not_fail
  should_not_fail.f90)
add_test(NAME fortran_ctest_should_not_fail
  COMMAND fortran_ctest_should_not_fail)


# coarray gather tests
add_executable(coarray_gather_index_test
  coarray_gather_index_test.f90
  ${icar_build}/array_utilities.o)
add_test(NAME coarray_gather_index_test
  COMMAND ${cafrun}
  ${caf_np_arg}
  ${caf_np}
  ${CMAKE_CURRENT_BINARY_DIR}/coarray_gather_index_test)


# coarray scatter tests


# coarray gather scatter test
