#!/usr/bin/env python3
from netCDF4 import Dataset
import numpy as np
from sys import exit, path
from os import getcwd
path.insert(0, getcwd()+'/../helpers/generateTestFiles')
import Topography as tg
import Forcing as fc
import ICARoptions as opt

# ---------------------------------------
# ----- Settings For Generate Files -----
# ---------------------------------------
# choose dimensions
nz = 32
nx = ny = 40

# from ideal test
dz_value          = 500.0    # thickness of each model gridcell   [m]
# hill values currently do nothing, but one hill is created
# hill_height       = 1000.0   # height of the ideal hill(s) [m]
# n_hills           = 1.0      # number of hills across the domain

u_test_val = v_test_val = w_test_val = 0.0
qv_val = 0.001

# --- choose function for creating pressure ---
# options: calc_pressure_from_sea, calc_pressure_dz_iter, calc_pressure_1m_iter
pressure_func = 'calc_pressure_from_sea'
# --- choose weather model ---
# options: basic, WeismanKlemp
weather_model = 'WeismanKlemp'

# --- choose length of run ---
start_date = '2020-12-01 00:00:00'
end_date =   '2020-12-06 00:00:00'


# --- parcel initialization ---
total_num_parcels = 10
parc_environment_only = False
parc_velocity_init = 1.0
parc_velocity_prob_range = 0.0
parc_rh = 0.99


def main():
    # ICAR Options generate the ICAR namelist
    opt.ICARoptions(nz=nz,
                    output_vars=['pressure','temperature','parcels','potential_temperature', 'qv'],
                    phys_opt_conv=4,
                    parc_total_parcels=total_num_parcels,
                    parc_environment_only=parc_environment_only,
                    parc_velocity_init=parc_velocity_init,
                    parc_velocity_prob_range=parc_velocity_prob_range,
                    parc_rh=parc_rh,
                    start_date = start_date,
                    end_date = end_date)
    print("Generated icar_options.nml")

    tg.Topography(nz, nx, ny)
    print("Generated init.nc")

    # double check all passed variable get used
    forcing = fc.Forcing(nz, nx, ny,
                         u_val=u_test_val,
                         v_val=v_test_val,
                         w_val=w_test_val,
                         dz_value=dz_value,
                         qv_val=qv_val,
                         weather_model=weather_model,
                         pressure_func=pressure_func)
    print("Generated forcing.nc")

if __name__ == "__main__":
    main()
