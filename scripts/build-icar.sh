#!/bin/bash

set -e

source scripts/load-modules-on-cori.sh
source /global/homes/s/sameer/pkgs/OpenCoarrays/install/opencoarrays/setup.sh
source scripts/export-cori-environment-variables.sh

# /bin/rm -rf build

fpm build \
  --compiler caf \
  --profile release \
  --flag "-cpp -fopenmp -g -DUSE_ASSERTIONS=.true. -I$FFTW_INCLUDE_PATH -fallow-argument-mismatch -ffree-line-length-none -DVERSION=\\\"$GIT_VERSION\\\" -L$NETCDF_LIB_PATH -L$NETCDF_FORTRAN_LIB_PATH  -L$FFTW_LIB_PATH -Wl,-rpath,$NETCDF_LIB_PATH -Wl,-rpath,$NETCDF_FORTRAN_LIB_PATH -Wl,-rpath,$FFTW_LIB_PATH"


# echo $PWD
# bash ./scripts/fpm-build.sh
