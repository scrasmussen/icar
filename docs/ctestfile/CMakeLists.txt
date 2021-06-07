cmake_minimum_required(VERSION 3.18)

project(test_exe)

enable_testing()

# --- Serial Tests ---
# find_program(CALENDAR_TEST calendar_test PATHS ${CTEST_DIR})
# add_test(calendar_test ${CALENDAR_TEST})


set(NP 2)
set(OPTIONS_FILE ${CTEST_SRC}/icar_options.nml)
# --- Coarray Tests ---
# find_program(CAF_BCAST_TEST caf_bcast_test PATHS ${CTEST_DIR})
# add_test(caf_bcast_test cafrun -n ${NP} ${CAF_BCAST_TEST})

# find_program(CAF_OPTIONS_TEST caf_options_test PATHS ${CTEST_DIR})
# add_test(caf_options_test cafrun -n ${NP} ${CAF_OPTIONS_TEST} ${OPTIONS_FILE})

# needs headwaters
# find_program(CAF_INIT_TEST caf_init_test  PATHS ${CTEST_DIR})
# add_test(caf_init_test  cafrun -n ${NP} ${CAF_INIT_TEST} ${OPTIONS_FILE})

# needs headwaters
# find_program(CAF_DOMAIN_INIT_TEST caf_domain_init_test PATHS ${CTEST_DIR})
# add_test(caf_domain_init_test cafrun -n ${NP} ${CAF_DOMAIN_INIT_TEST} ${OPTIONS_FILE})

# find_program(CAF_BCAST_TEST caf_bcast_test PATHS ${CTEST_DIR})
# add_test(caf_bcast_test cafrun -n ${NP} ${CAF_BCAST_TEST})

# needs headwaters
# find_program(CAF_WRITE_DOMAIN_TEST caf_write_domain_test PATHS ${CTEST_DIR})
# add_test(caf_write_domain_test cafrun -n ${NP} ${CAF_WRITE_DOMAIN_TEST} ${OPTIONS_FILE})

# find_program(CAF_BOUNDARY_TEST caf_boundary_test PATHS ${CTEST_DIR})
# add_test(caf_boundary_test cafrun -n ${NP} ${CAF_BOUNDARY_TEST} ${OPTIONS_FILE})

find_program(CAF_BOUNDARY_INIT_TEST caf_boundary_init_test PATHS ${CTEST_DIR})
add_test(caf_boundary_init_test cafrun -n ${NP} ${CAF_BOUNDARY_INIT_TEST} blah.txt)
# add_test(caf_boundary_init_test mpiexec --mca orte_base_help_aggregate 0 -n ${NP} ${CAF_BOUNDARY_INIT_TEST} blah.txt)


# find_program( PATHS ${CTEST_DIR})
# add_test( cafrun -n ${NP} ${})

# --- TODO SET THIS PROPERLY ---
# set_tests_properties(caf_bcast_test PROPERTIES RUN_SERIAL)
