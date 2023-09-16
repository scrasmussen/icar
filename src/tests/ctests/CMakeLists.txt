cmake_minimum_required(VERSION 3.5)

project("ICAR CTests")

# Set the Fortran compiler (replace "FortranCompiler" with your actual compiler)
enable_language(Fortran)
enable_testing()

set (CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake_files)
set (NETCDF_F90 "YES")
find_package (NetCDF REQUIRED)
find_package (FFTW REQUIRED)

message("CMAKE_Fortran_COMPILER_ID is ${CMAKE_Fortran_COMPILER_ID}")
message("ctest_fortran_compiler is ${CMAKE_Fortran_COMPILER}")
# make sure caf is the compiler
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(Fortran_FLAGS "-fopenmp -lgomp -O2 -mfma -ffree-line-length-none -ftree-vectorize -fimplicit-none -funroll-loops -march=native -fno-protect-parens")

  if (CMAKE_Fortran_COMPILER MATCHES "caf$")
    message("Success: caf compiler detected")
  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    find_library(caf_lib_found
      NAMES libcaf_mpi.so libcaf_mpi.a
      PATHS ENV LD_LIBRARY_PATH)
    message("need libcaf_mpi{.so,.a}: found = ${caf_lib_found}")
    if (caf_lib_found STREQUAL caf_lib_found-NOTFOUND)
      message(FATAL_ERROR "Library libcaf_mpi not found, GNU needs it to use OpenCoarrays")
    endif()
    set(Fortran_FLAGS "-fcoarray=lib ${Fortran_FLAGS}")
    add_link_options(${caf_lib_found})
  endif()

  set(CMAKE_Fortran_FLAGS "${Fortran_FLAGS} -DUSE_ASSERTIONS=.false. -cpp -DVERSION=2.1")
endif()


set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/../../../build/")
set(ICAR_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../)
file(GLOB ICAR_SOURCES
  ${ICAR_SOURCE_DIR}/constants/*.f90
  ${ICAR_SOURCE_DIR}/io/*.f90
  ${ICAR_SOURCE_DIR}/main/*.f90
  ${ICAR_SOURCE_DIR}/objects/*.f90
  ${ICAR_SOURCE_DIR}/physics/*.f90
  ${ICAR_SOURCE_DIR}/utilities/*.f90
  )

# these files won't compiler anymore, they need to be removed
list(REMOVE_ITEM ICAR_SOURCES "${ICAR_SOURCE_DIR}/objects/external_bnd.f90")
list(REMOVE_ITEM ICAR_SOURCES "${ICAR_SOURCE_DIR}/physics/cu_simple.f90")
list(REMOVE_ITEM ICAR_SOURCES "${ICAR_SOURCE_DIR}/physics/adv_mpdata_FCT_core.f90")
list(REMOVE_ITEM ICAR_SOURCES "${ICAR_SOURCE_DIR}/physics/lsm_basic.f90")
list(REMOVE_ITEM ICAR_SOURCES "${ICAR_SOURCE_DIR}/physics/lsm_simple.f90")
list(REMOVE_ITEM ICAR_SOURCES "${ICAR_SOURCE_DIR}/physics/winds_blocking.f90")
list(REMOVE_ITEM ICAR_SOURCES "${ICAR_SOURCE_DIR}/main/driver.f90")

add_library(icar_lib ${ICAR_SOURCES})
target_compile_definitions(icar_lib PUBLIC USE_ASSERTIONS=.false.)
target_link_libraries(icar_lib ${NETCDF_LIBRARIES} ${FFTW_LIBRARIES})


add_executable(icar ${ICAR_SOURCE_DIR}/main/driver.f90)
target_link_libraries(icar icar_lib)




# target_link_libraries(test_lt_parameters_namelist
#   ${NETCDF_LIBRARIES})




# --- ICAR makefile targets ---
add_custom_target(build
  COMMAND MODE=debugslow F90=caf ${CMAKE_MAKE_PROGRAM}
  WORKING_DIRECTORY ${ICAR_SOURCE_DIR})

add_custom_target(clean-icar
  COMMAND ${CMAKE_MAKE_PROGRAM} clean
  WORKING_DIRECTORY ${ICAR_SOURCE_DIR})

add_custom_target(clean-all
  COMMAND ${CMAKE_MAKE_PROGRAM} clean all
  WORKING_DIRECTORY ${ICAR_SOURCE_DIR})


# random build test
add_executable(hello_world hello.f90)
add_test(NAME hello_world_test
  COMMAND hello_world)

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

# specific ctests
add_executable(test_lt_parameters_namelist
  test_lt_parameters_namelist.f90)
add_test(NAME test_lt_parameters_namelist
  COMMAND test_lt_parameters_namelist ${CMAKE_CURRENT_SOURCE_DIR}/input/lt_options.nml)
target_link_libraries(test_lt_parameters_namelist icar_lib)

# target_link_libraries(test_lt_parameters_namelist
#   ${CMAKE_BINARY_DIR}/../../../build/options_obj.o
#   ${CMAKE_BINARY_DIR}/../../../build/icar_constants.o
#   ${CMAKE_BINARY_DIR}/../../../build/io_routines.o
#   ${CMAKE_BINARY_DIR}/../../../build/time_obj.o
#   ${CMAKE_BINARY_DIR}/../../../build/time_io.o
#   ${CMAKE_BINARY_DIR}/../../../build/advection_driver.o
#   ${CMAKE_BINARY_DIR}/../../../build/time_delta_obj.o
#   ${CMAKE_BINARY_DIR}/../../../build/co_utilities.o
#   ${NETCDF_LIBRARIES})



# message("linked ${CMAKE_BINARY_DIR}/../../../build/")
# message("linked ${CMAKE_Fortran_MODULE_DIRECTORY}")
# add ../../../build for object and mod files
# message("FC ${CMAKE_Fortran_COMPILER}")
