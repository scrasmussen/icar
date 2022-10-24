#!/usr/bin/env python3
from netCDF4 import Dataset
import numpy as np
from sys import exit, path
from os import getcwd
path.insert(0, getcwd()+'/../helpers/generateTestFiles')
import Topography as tg
import Forcing as fc
import ICARoptions as opt

def main():
    # --- parcel initialization ---
    total_num_parcels = 4 #20
    parc_environment_only = False
    parc_z_init = 2.0
    parc_velocity_init = 5.0
    parc_velocity_prob_range = 0.0
    parc_rh = 0.99

    # ICAR Options generate the ICAR namelist
    opt.ICARoptions(                    output_vars=['pressure','temperature','parcels','potential_temperature', 'qv', 'precipitation'],
                    phys_opt_conv=6,
                    # phys_opt_pbl=2,
                    # phys_opt_mp=1, # or 5
                    # phys_opt_lsm=3,
                    # phys_opt_rad=2,
                    # PARCEL CHOICES
                    parc_total_parcels=total_num_parcels,
                    parc_environment_only=parc_environment_only,
                    parc_z_init=parc_z_init,
                    parc_velocity_init=parc_velocity_init,
                    parc_velocity_prob_range=parc_velocity_prob_range,
                    parc_rh=parc_rh)
    print("Generated icar_options.nml")

    tg.Topography()
    print("Generated init.nc")

    forcing = fc.Forcing(qv_val=0.001,
                         weather_model = 'WeismanKlemp',
                         pressure_func = 'calc_pressure_from_sea')
    print("Generated forcing.nc")

if __name__ == "__main__":
    main()
