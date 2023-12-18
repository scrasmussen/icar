# Reproduce Cray Derived Type Bug

## Bug Description
The setting of the logical value at this location is occuring incorrectly.

## Build
### Load environment
`$ . src/crayEnv.sh`

### Build with reproducer
```
$ BUG=1 make -j 4
```

### Build with fix
```
$ make -j 4
```

## Run
```
$ cd src
$ cp /path/to/testcase.tar.gz .
$ tar zxf testcase.tar.gz
$ cd testcase
$ make
```

The last two lines of the output will either be:

* Broken 
```
 T,  2*F
 STOP DEBUGGING
```

* Fixed
```
 3*T
 STOP DEBUGGING
```

