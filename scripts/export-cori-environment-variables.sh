#!/bin/bash

# This script is sourced by {clean-,re}test-on-cori.sh

set -e

export PATH=/global/homes/r/rouson/pkgs/bin:$PATH
export FFTW_INCLUDE_PATH=/global/project/projectdirs/m3368/ecpsdk/spack-configs/UOREGON/E4S-21.05-Facility-Examples/NERSC-Cori/spack/opt/spack/cray-cnl7-haswell/gcc-11.1.0/fftw-3.3.9-v4uala6lnfag4lmu2uireru5tc2kflrn/include
export NETCDF_LIB_PATH=/global/project/projectdirs/m3368/ecpsdk/spack-configs/UOREGON/E4S-21.05-Facility-Examples/NERSC-Cori/spack/opt/spack/cray-cnl7-haswell/gcc-11.1.0/netcdf-c-4.8.0-7tgmfjazovtn5jo3odzpyi3cidfj3p4y/lib
export NETCDF_FORTRAN_LIB_PATH=/global/project/projectdirs/m3368/ecpsdk/spack-configs/UOREGON/E4S-21.05-Facility-Examples/NERSC-Cori/spack/opt/spack/cray-cnl7-haswell/gcc-11.1.0/netcdf-fortran-4.5.3-7pgh5zm3ltgo62bb6qtfz6h2ez7hbqkv/lib
export FFTW_LIB_PATH=/global/project/projectdirs/m3368/ecpsdk/spack-configs/UOREGON/E4S-21.05-Facility-Examples/NERSC-Cori/spack/opt/spack/cray-cnl7-haswell/gcc-11.1.0/fftw-3.3.9-v4uala6lnfag4lmu2uireru5tc2kflrn/lib
export NETCDF_INCLUDE_PATH=/global/project/projectdirs/m3368/ecpsdk/spack-configs/UOREGON/E4S-21.05-Facility-Examples/NERSC-Cori/spack/opt/spack/cray-cnl7-haswell/gcc-11.1.0/netcdf-c-4.8.0-7tgmfjazovtn5jo3odzpyi3cidfj3p4y/include
