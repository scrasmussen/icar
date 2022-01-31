from netCDF4 import Dataset
import numpy as np
import math
from sys import exit, path
from os import getcwd
path.insert(0, getcwd()+'/../helpers/genNetCDF')
import Topography as tg
import Forcing as fc
import ICARoptions as opt

# Python program generates an ideal case
class IdealTest:
    # from ideal test
    sealevel_pressure = 100000.0 # pressure at sea level [Pa]
    dz_value          = 500.0    # thickness of each model gridcell   [m]
    # hill values currently do nothing
    hill_height       = 1000.0   # height of the ideal hill(s) [m]
    n_hills           = 1.0      # number of hills across the domain

    def __init__(self, nz=10, nx=2, ny=2, n_hills=1.0):
        mixing_ratio = 0.01 # water vapor
        u_test_val = 0.0
        v_test_val = 0.0
        w_test_val = 0.0
        water_vapor_test_val = 0.000
        theta_test_val = 300.0
        qv_val = mixing_ratio

        # --- choose function for creating pressure ---
        pressure_func = 'calc_pressure_from_sea'
        # pressure_func = 'calc_pressure_dz_iter'
        # pressure_func = 'calc_pressure_1m_iter'

        # double check all passed variable get used
        self.forcing = fc.Forcing(nz, nx, ny, self.sealevel_pressure,
                                  u_test_val, v_test_val, w_test_val,
                                  water_vapor_test_val, theta_test_val,
                                  dz_value=self.dz_value, qv_val=qv_val,
                                  # weather_model='basic')
                                  weather_model='WeismanKlemp',
                                  pressure_func=pressure_func)
        print("Generated forcing.nc")

        self.init = tg.Topography(nz, nx, ny)
        print("Generated init.nc")


def main():
    # ICAR Options generate the ICAR namelist
    nz = 32 #40  # 32, good one for non-iter pressure
    options = opt.ICARoptions(parcel_count=0, output_interval=60, nz=nz,
                              hrs=0)
    print("Generated icar_options.nml")
    test = IdealTest(nz=nz, nx=40, ny=40, n_hills=1.0)

if __name__ == "__main__":
    main()
