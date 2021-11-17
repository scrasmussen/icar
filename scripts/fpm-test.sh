#!/bin/bash
# This script assumes that the test-on-cori.sh has already been 
# executed in the current login session to load various modules.

fpm test \
  --compiler caf \
  --profile release \
  --flag "-cpp -fopenmp -g -DUSE_ASSERTIONS=.true. -I$FFTW_INCLUDE_PATH -fallow-argument-mismatch -ffree-line-length-none -DVERSION=\\\"$GIT_VERSION\\\" -L$NETCDF_LIB_PATH -L$NETCDF_FORTRAN_LIB_PATH  -L$FFTW_LIB_PATH -Wl,-rpath,$NETCDF_LIB_PATH -Wl,-rpath,$NETCDF_FORTRAN_LIB_PATH -Wl,-rpath,$FFTW_LIB_PATH"
