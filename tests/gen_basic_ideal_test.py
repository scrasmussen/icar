#!/usr/bin/env python3
from sys import path
from os import getcwd
path.insert(0, getcwd()+'/../helpers/generateTestFiles')
import Topography as tg
import Forcing as fc
import ICARoptions as opt

def main():
    opt.ICARoptions()
    print("Generated icar_options.nml")

    tg.Topography()
    print("Generated init.nc")

    forcing = fc.Forcing()
    print("Generated forcing.nc")

if __name__ == "__main__":
    main()
