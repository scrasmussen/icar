#!/usr/bin/env python3
from ast import Or
from netCDF4 import Dataset
import numpy as np
from sys import exit, path
from os import getcwd
path.insert(0, getcwd()+'/../helpers/genNetCDF')
import Topography as tg
import Forcing as fc
import ICARoptions as opt

# ---------------------------------------
# ----- Settings For Generating Files -----
# ---------------------------------------
# choose dimensions of hi-res grid:
nz = 40
nx = 300
ny = 20
dx=dy=1000

dz_levels= [50., 75., 125., 200., 300., 400.] + [500.] * 50

# Paramters to generate hi-res topography:
hill_height       = 3000.0   # height of the ideal hill(s) [m]
n_hills           = 5.0      # number of hills across the domain (ignored for Schaer test)

# ---- Forcing specs  -----
nt_lo         = 4 # nr of timesteps (hours) - this is also how long the ideal sim will run.
nz_lo         = 51
nx_lo = 300; ny_lo = 20
dx_lo = dy_lo = 1000     # make sure dx_lo*nx_lo => dx*nx & dy_lo*ny_lo => dy*ny
dz_lo         = 500.0    # thickness of each (Forcing?) model gridcell   [m]

if dx_lo*nx_lo < dx*nx or dy_lo*ny_lo < dy*ny or dz_lo*nz_lo < np.sum(dz_levels) :
    print("\n   ERROR: Forcing domain smaller than hi-res domain. Incease forcing domain size \n")

# u field can be a constant, or an array of size nz. When Schaer_test is chosen, this get overwritten.
u_test_val = np.array([0., 0., 0., 0., 0., 0. , 0. ,2.] + [5.] *35)   # u field in z-direction
# u_test_val = 5.0
v_test_val = 0.0

# relative_humidity = 0.01
water_vapor_test_val = 0.000
mixing_ratio = 0.001 # water vapor # not if constant
qv_val = mixing_ratio
Schaer_test = False


# --- choose function for creating pressure ---
pressure_func = 'calc_pressure_from_sea'
# pressure_func = 'calc_pressure_dz_iter'
# pressure_func = 'calc_pressure_1m_iter'
# --- choose weather model ---
# weather_model = 'basic'
weather_model = 'WeismanKlemp'


def main():
    # ICAR Options generate the ICAR namelist
    opt.ICARoptions(nz=nz,
                    dx = dx,                       # <-   affects advection speed!
                    output_vars=['pressure','temperature', 'lon', 'lat', 'z', 'dz', 'dz_i', 'u', 'v', 'w', 'w_grid', 'qv', 'terrain' ],
                    mod_model_comment = 'flat_z_height=-10',
                    zinfo_dz_levels=dz_levels,
                    zinfo_flat_z_height=-10,
                    zinfo_sleve= ".False.",
                    zinfo_terrain_smooth_windowsize = 4,
                    zinfo_terrain_smooth_cycles = 5,
                    zinfo_decay_rate_L_topo = 1.5,
                    zinfo_decay_rate_S_topo = 3.0,
                    zinfo_sleve_n = 1.35,
                    zinfo_space_varying = ".True.",
#     pbl = 2,  ! 1=legacy (deprecated)      2=Simple (Local HP96)        3=YSU             (N/A)
#     lsm = 4,  ! 1=nuse prescribed fluxes    2=Simple LSM (N/A)           3=Noah LSM                  4=NoahMP (wishlist)
#     water=3,  ! 1=use prescribed (w/lsm=1) 2=Simple sea surface fluxes  3=Lake model (simple ocean)
#     mp  = 5,  ! 1=Thompson                 2=Simple (SB04)              3=Morrison                  4=WSM6      5=Thompson-Eidhammer
#     rad = 3,  ! 1=use prescribed fluxes    2=Simple (empirical)         3=RRTMG
#     conv= 5,  ! 1=Tiedke Scheme            2=Simple Scheme (wishlist)   3=Kain-Fritsch
#     adv = 1,  ! 1=Upwind                   2=MPDATA                     3=Adams-Bashforth (wishlist)
#     wind= 5   ! 1=Linear Theory           5= wind, not sure, but use this!!
# !    wind= 3   ! 1=Linear Theory
                    phys_opt_pbl = 2,
                    phys_opt_lsm = 4,
                    phys_opt_water = 3,
                    phys_opt_mp = 5,
                    phys_opt_rad = 3,
                    phys_opt_conv = 5,
                    phys_opt_adv = 1,
                    phys_opt_wind = 5,
                    smooth_wind_distance = dx_lo,  # Very important - has effect on vertical speeds!
                    use_agl_height = True,         # !   Use height above ground level to interpolate the wind field instead of height above sea level.
                    agl_cap = 400,                 # !   Height at which we switch from AGL-interpolation to using ASL-interpolation
                    output_file = 'icar_out_',
                    qv_is_relative_humidity ='false',
                    output_interval = 1200,
                    end_date = '2020-12-01 02:00:00',
                    par_use_adv_options = True,
                    par_use_lt_options = True,
                    par_use_mp_options = True,
                    par_use_lsm_options = True,
                    par_use_rad_options = True,
                    par_use_cu_options = True
)
    print("Generated icar_options.nml")

    tg.Topography(nz,
                  nx,
                  ny,
                  n_hills = n_hills,
                  hill_height = hill_height,
                  dx = dx,
                  dy = dy,
                  lat0 = 39.5,
                  lon0 = -105,
                  Schaer_test = Schaer_test
                  )
    print("Generated init.nc")

    # double check all passed variable get used
    forcing = fc.Forcing(nt=nt_lo,
                         nz=nz_lo,
                         nx=nx_lo+10,
                         ny=ny_lo+10,
                         u_val=u_test_val,
                         v_val=v_test_val,
                         water_vapor_val=water_vapor_test_val,
                         dz_value=dz_lo,
                         dx=dx_lo,
                         dy=dy_lo,
                         qv_val=qv_val,
                         weather_model=weather_model,
                         pressure_func=pressure_func,
                         hill_height=hill_height,
                         lat0 = 39.5,
                         lon0 = -105,
                         Schaer_test=Schaer_test
                        )
    print("Generated forcing.nc")

if __name__ == "__main__":
    main()
