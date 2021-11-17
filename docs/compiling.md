##Compiling ICAR

##Building with the Fortran Package Manager ([fpm])

### macOS
With homebrew installed,
```zsh
brew install netcdf fftw gcc
git clone https://github.com/Unidata/netcdf-fortran.git
cd netcdf-fortran
export FC=gfortran CC=gcc CXX=g++
cmake -B build
cd build
make install
cd <icar-source-location>
export GIT_VERSION=`git describe --long --dirty --all --always | sed -e's/heads\///'`
fpm build --profile debug --flag "-cpp -DUSE_ASSERTIONS=.true. -I/usr/local/Cellar/fftw/3.3.9_1/include/ -fallow-argument-mismatch -ffree-line-length-none -DVERSION=\\\"$GIT_VERSION\\\""
```


##Building with the Makefile

Edit the makefile to set the path to your compiled NetCDF and FFTW libraries

Also to set the compiler for your machine if necessary (defaults to gfortran)

    make clean
         # remove build by-products

    make
         # default (relatively high) optimization compile

    make install
        # compile if necessary, then install in the install directory [~/bin]

### Options:
    MODE=fast           # more optimization, slower compile, WARNING:not safe optimizations
    MODE=profile        # set profiling options for gnu or intel compilers
    MODE=debug          # debug compile with optimizations
    MODE=debugslow      # debug compile w/o optimizations
    MODE=debugomp       # debug compile with optimizations and OpenMP parallelization
    MODE=debugompslow   # debug compile w/o optimizations but with OpenMP parallelization

    make doc
    # build doxygen documentation in docs/html

    make test
        # compiles various test programs (mpdata_test, fftshift_test, and calendar_test)

    add -jn to parallelize the compile over n processors

### Example:
    make install MODE=debug -j4  # uses 4 processes to compile in debug mode

[fpm]:  https://github.com/fortran-lang/fpm
