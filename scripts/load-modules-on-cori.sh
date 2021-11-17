#!/bin/bash

# This script is sourced by {clean-,re}test-on-cori.sh

set -e

module use /global/common/software/spackecp/e4s-21.05/modules/cray-cnl7-haswell
module unload darshan
module unload PrgEnv-intel 
module load PrgEnv-gnu
module swap gcc gcc/11.1.0-gcc-9.3.0
module load netcdf-fortran/4.5.3-gcc-11.1.0
module load fftw/3.3.9-gcc-11.1.0
