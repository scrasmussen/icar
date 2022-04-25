#!/usr/bin/env python3
from sys import exit, path
from os import getcwd
path.insert(0, getcwd()+'/../helpers/genNetCDF')
import Topography as tg
import Forcing as fc
import ICARoptions as opt

# ---------------------------------------
# ----- Settings For Generate Files -----
# ---------------------------------------
# choose general options
nz = 32
nx = ny = 40
dz_value          = 500.0    # thickness of each model gridcell   [m]
u_test_val = v_test_val = w_test_val = 0.0
qv_val = 0.001 # water vapor
total_num_parcels = 10

# --- choose function for creating pressure ---
# - Options: calc_pressure_from_sea, calc_pressure_dz_iter,
#            or calc_pressure_1m_iter
pressure_func = 'calc_pressure_from_sea'

# --- choose weather model ---
# - Options: basic of WeismanKlemp
weather_model = 'WeismanKlemp'

def main():
    # ICAR Options generate the ICAR namelist
    opt.ICARoptions(nz=nz,
                    output_vars=['pressure','temperature','parcels'],
                    phys_opt_conv=4,
                    phys_opt_mp = 2,
                    total_parcels=total_num_parcels,
                    start_date = '2020-12-01 00:00:00',
                    end_date =   '2020-12-05 00:00:00')
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
