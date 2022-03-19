# Instructions for Running on CORI

## Prerequisites

1. fpm: Fortran Package Manages
Easy install of fpm:
```
$ module load python
$ conda config --add channels conda-forge
$ conda create -n fpm fpm
$ conda activate fpm
```

## Build
Build from root of github repo.
Note: trying to load modules before building will cause window to close.
The build manager fpm will load everything correctly.

```
$ make
```

Resulting `icar` binary will be in `build/caf_*/app/icar`.

## Running
From slurm bash script, add and edit the following for your use:
```
icar_repo=/global/homes/s/scrasmus/src/fpm-scripts-icar
source ${icar_repo}/scripts/load-modules-on-cori.sh
source /global/homes/s/sameer/pkgs/OpenCoarrays/install/opencoarrays/setup.sh
source ${icar_repo}/scripts/export-cori-environment-variables.sh
export LD_LIBRARY_PATH=${icar_repo}/build/caf_[ENTER CORRECT INFO]/icar/:$LD_LIBRARY_PATH

cafrun -n 8 ./icar options.nml
```