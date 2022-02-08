#!/usr/bin/env python3
from netCDF4 import Dataset
import numpy as np
import math
from sys import exit, path
from os import getcwd
path.insert(0, getcwd()+'/../helpers/genNetCDF')
import Topography as tg
import Forcing as fc
import ICARoptions as opt

# from ideal test
sealevel_pressure = 100000.0 # pressure at sea level [Pa]
dz_value          = 500.0    # thickness of each model gridcell   [m]
# hill values currently do nothing
hill_height       = 1000.0   # height of the ideal hill(s) [m]
n_hills           = 1.0      # number of hills across the domain
mixing_ratio = 0.00 # water vapor # not if constant
# relative_humidity = 0.01
u_test_val = v_test_val = w_test_val = 0.0
water_vapor_test_val = 0.000
theta_test_val = 300.0
qv_val = mixing_ratio


def main():
    # choose dimensions
    nz = 32
    nx = ny = 40

    # ICAR Options generate the ICAR namelist
    opt.ICARoptions(nz=nz, output_vars=['pressure','temperature'])
    print("Generated icar_options.nml")

    tg.Topography(nz, nx, ny)
    print("Generated init.nc")

    # --- choose function for creating pressure ---
    pressure_func = 'calc_pressure_from_sea'
    # pressure_func = 'calc_pressure_dz_iter'
    # pressure_func = 'calc_pressure_1m_iter'

    # double check all passed variable get used
    forcing = fc.Forcing(nz, nx, ny, sealevel_pressure,
                         u_test_val, v_test_val, w_test_val,
                         water_vapor_test_val, theta_test_val,
                         dz_value=dz_value, qv_val=qv_val,
                         # weather_model='basic')
                         weather_model='WeismanKlemp',
                         pressure_func=pressure_func)
    print("Generated forcing.nc")

if __name__ == "__main__":
    main()
