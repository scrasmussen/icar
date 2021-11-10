#!/bin/bash
module use /global/common/software/spackecp/e4s-21.05/modules/cray-cnl7-haswell
module unload darshan
module unload PrgEnv-intel 
module load PrgEnv-gnu
module swap gcc gcc/11.1.0-gcc-9.3.0
source /global/homes/s/sameer/pkgs/OpenCoarrays/install/opencoarrays/setup.sh

# fpm
export PATH=/global/homes/r/rouson/pkgs/bin:$PATH
which fpm


/bin/rm -rf build
module load netcdf-fortran/4.5.3-gcc-11.1.0
module load fftw/3.3.9-gcc-11.1.0

export FFTW_INCLUDE_PATH=/global/project/projectdirs/m3368/ecpsdk/spack-configs/UOREGON/E4S-21.05-Facility-Examples/NERSC-Cori/spack/opt/spack/cray-cnl7-haswell/gcc-11.1.0/fftw-3.3.9-v4uala6lnfag4lmu2uireru5tc2kflrn/include
export NETCDF_LIB_PATH=/global/project/projectdirs/m3368/ecpsdk/spack-configs/UOREGON/E4S-21.05-Facility-Examples/NERSC-Cori/spack/opt/spack/cray-cnl7-haswell/gcc-11.1.0/netcdf-c-4.8.0-7tgmfjazovtn5jo3odzpyi3cidfj3p4y/lib
export NETCDF_FORTRAN_LIB_PATH=/global/project/projectdirs/m3368/ecpsdk/spack-configs/UOREGON/E4S-21.05-Facility-Examples/NERSC-Cori/spack/opt/spack/cray-cnl7-haswell/gcc-11.1.0/netcdf-fortran-4.5.3-7pgh5zm3ltgo62bb6qtfz6h2ez7hbqkv/lib
export FFTW_LIB_PATH=/global/project/projectdirs/m3368/ecpsdk/spack-configs/UOREGON/E4S-21.05-Facility-Examples/NERSC-Cori/spack/opt/spack/cray-cnl7-haswell/gcc-11.1.0/fftw-3.3.9-v4uala6lnfag4lmu2uireru5tc2kflrn/lib
export NETCDF_INCLUDE_PATH=/global/project/projectdirs/m3368/ecpsdk/spack-configs/UOREGON/E4S-21.05-Facility-Examples/NERSC-Cori/spack/opt/spack/cray-cnl7-haswell/gcc-11.1.0/netcdf-c-4.8.0-7tgmfjazovtn5jo3odzpyi3cidfj3p4y/include

fpm build --compiler caf --profile release --flag "-cpp -fopenmp -g -DUSE_ASSERTIONS=.true. -I$FFTW_INCLUDE_PATH -fallow-argument-mismatch -ffree-line-length-none -DVERSION=\\\"$GIT_VERSION\\\" -L$NETCDF_LIB_PATH -L$NETCDF_FORTRAN_LIB_PATH  -L$FFTW_LIB_PATH -Wl,-rpath,$NETCDF_LIB_PATH -Wl,-rpath,$NETCDF_FORTRAN_LIB_PATH -Wl,-rpath,$FFTW_LIB_PATH"

