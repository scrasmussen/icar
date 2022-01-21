from netCDF4 import Dataset
import xarray as xr
import pandas as pd
import numpy as np
import datetime
import math
from genNetCDF import fixType
from sys import exit

# Create NetCDF file containing the forcing data
class Forcing:
    # from ideal test
    attributes = {"history": "Dec 01 00:00:00 2020"}


    def __init__(self, nz=10, nx=2, ny=2, sealevel_pressure=100000.0,
                 u_val=0.5, v_val=0.5, w_val=0.0,
                 water_vapor_val=0.001, theta_val=300.0, nt=1,
                 height_value=500, dx=10, dy=10, dz_value=500.0,
                 qv_val=0.1, weather_model='basic'):

        self.setup_class_variables(nz, nx, ny, nt, sealevel_pressure)

        # --- Create and define variables for datafile
        # TODO make these values passed i
        lat_flat = np.arange(39,39+nx*dx, dx)
        lon_flat = np.arange(-107,-107+ny*dy, dy)

        self.define_data_variables(nt, nz, nx, ny, height_value, lat_flat,
                                   lon_flat, dz_value, theta_val, u_val,
                                   v_val, qv_val, weather_model)

        # define time
        t0 = datetime.datetime(2020,12,1)
        time = xr.DataArray([t0+datetime.timedelta(dt*100) for dt in range(nt)], name="time",
                            dims=["time"])

        # --- Write all variable to netcdf file
        self.write_netcdf_file(time)


    # Combine variables, create dataset and write to file
    def write_netcdf_file(self, time):
        data_vars = dict(
            u = self.u,
            v = self.v,
            theta = self.theta,
            qv = self.qv,
            height = self.height,
            z = self.z,
            pressure = self.pressure,
            temperature = self.temperature,
            lat_m = self.lat,
            lon_m = self.lon,
            time = time)

        ds = xr.Dataset(
            data_vars = data_vars,
            attrs = self.attributes
        )

        ds.to_netcdf("forcing.nc", "w", "NETCDF4", unlimited_dims='time')


    def set_water_vapor(self, water_vapor, temperature, pressure):
        print("TODO: vectorize function fall")
        water_vapor[:,:,:] = sat_mr(temperature[:,:,:], pressure[:,:,:])
        return water_vapor


    def setup_class_variables(self, nz, nx, ny, nt, sealevel_pressure):
        self.nt = nt
        self.nz = nz
        self.nx = nx
        self.ny = ny
        self.sealevel_pressure = sealevel_pressure
        dimensions4d = {
            "time": nt,
            "level": nz,
            "lat": nx,
            "lon": ny
        }
        dimensions3d = {
            "level": nz,
            "lat": nx,
            "lon": ny
        }
        dimensions3d_t = {
            "time": nt,
            "lat": nx,
            "lon": ny
        }
        dimensions2d = {
            "lat": nx,
            "lon": ny
        }
        dimensions1d = {
            "time": 1

        }
        self.dimensions4d = dimensions4d
        self.dimensions3d = dimensions3d
        self.dimensions3d_t = dimensions3d_t
        self.dimensions2d = dimensions2d
        self.dimensions1d = dimensions1d
        self.dims4d = list(dimensions4d.keys())
        self.dims3d = list(dimensions3d.keys())
        self.dims2d = list(dimensions2d.keys())
        self.dims1d = list(dimensions1d.keys())



    def define_data_variables(self, nt, nz, nx, ny, height_value,lat_flat,
                              lon_flat, dz_value, theta_val, u_val, v_val,
                              qv_val, weather_model):
        # --- u variable
        self.u = xr.Variable(self.dims4d,
                             np.full([nt, nz, nx, ny], u_val),
                             {'long_name':'U (E/W) wind speed', 'units':"m s**-1"})
        # --- v variable
        self.v = xr.Variable(self.dims4d,
                             np.full([nt, nz, nx, ny], v_val),
                             {'long_name':'V (N/S) wind speed', 'units':"m s**-1"})

        # --- height
        self.height = xr.Variable(self.dims2d,
                                  np.full([nx, ny], height_value),
                                  {'long_name':'Topographic Height',
                                   'units':'m'})

        # --- Atmospheric Elevation
        dz = np.full([nz,nx,ny], dz_value)
        z_data = np.full([nt,nz,nx,ny], height_value)
        for k in range(1,nz):
            z_data[:,k,:,:] = z_data[:,k-1,:,:] + dz[k,:,:]
        self.z_data = z_data
        self.z = xr.Variable(self.dims4d,
                             z_data,
                             {'long_name':'Atmospheric Elevation',
                              'units':'m',
                              'positive':'up'})
        del(z_data)

        # --- Latitude
        self.lat = xr.Variable(["lat"],
                               lat_flat,
                               {'long_name':'latitude',
                                'units':'degree_north'}
                               )

        # --- Longitude
        self.lon = xr.Variable(["lon"],
                               lon_flat,
                               {'long_name':'longitude',
                                'units':'degree_east'}
                               )

        # --- potential temperature variable
        self.set_theta(theta_val, weather_model)

        # --- Pressure
        self.set_pressure(weather_model)

        # --- Temperature
        self.set_temperature(weather_model)

        # --- qv variable
        self.qv = xr.Variable(self.dims4d,
                              np.full([nt, nz, nx, ny], qv_val),
                              {'long_name':'Relative Humidity',
                               'units':"kg kg**-1"})



    def set_theta(self, theta_val, model='basic'):
        if model in ['basic']:
            theta = np.full([self.nt, self.nz, self.nx, self.ny], theta_val)
        elif model in ['WeismanKlemp']:
            print('Note: theta value of', theta_val,
                  'has been replaced with a profile generated for', model,
                  'model')
            theta = np.zeros([self.nt, self.nz, self.nx, self.ny])
            theta[0,:,:,:] = np.vectorize(calc_wk_theta)(self.z_data[0,:,:,:])
            theta[:,:,:,:] = theta[0,:,:,:]
        self.theta = xr.Variable(self.dims4d, theta,
                                 {'long_name':'Potential Temperature',
                                  'units':"K"})

    def set_pressure(self, model='basic'):
        # basic is defined in ICAR's atm_utilities
        pressure_data = np.zeros([self.nt,self.nz,self.nx,self.ny])
        if model in ['basic', 'WeismanKlemp']:
            # pressure_data[:,:,:,:] = np.vectorize(calc_pressure_from_sea)(
            #     self.sealevel_pressure,
            #     self.z_data[:,:,:,:])
            pressure_data[:,0,:,:] = np.vectorize(calc_pressure_iter)(
                self.sealevel_pressure,
                0,
                self.z_data[0,0,:,:])
            for z in range(1,self.nz):
                pressure_data[:,z,:,:] = np.vectorize(calc_pressure_iter)(
                    pressure_data[0,z-1,:,:],
                    self.z_data[0,z-1,:,:],
                    self.z_data[0,z,:,:])
            self.pressure = xr.Variable(self.dims4d,
                                        pressure_data,
                                        {'long_name':'Pressure',
                                         'units':'Pa'})
        else:
             print("Error: ", pressure_model, " is not defined")
             exit()
        del(pressure_data)


    def set_temperature(self, model='basic'):
        if (model in ['basic', 'WeismanKlemp']):
            # --TODO--
            # get better equation with temp and humidity
            # look at ICAR
            temp = np.zeros([self.nt, self.nz, self.nx, self.ny])
            temp = np.vectorize(calc_temp)(self.pressure.values,
                                           self.theta.values)
        else:
             print("Error: ", weather_model, " temperature is not defined")
             exit()
        self.temperature = xr.Variable(self.dims4d, temp,
                                 {'long_name':'Temperature',
                                  'units':"K"})


#---
# Lambda like functions used for np.vectorize
#---

# Weisman Klemp Theta equation
# z is elevation (m)
def calc_wk_theta(z):
    z_tr =  12000. # m
    theta_0 = 300. #
    theta_tr = 343. # K
    T_tr = 213. # K
    c_p = 1.0 # kJ/kgK
    # q_v0 = 11 # g kg^-1
    # q_v0 = 16 # g kg^-1
    # q_v0 = 14 # g kg^-1
    if z <= z_tr:
        theta = theta_0 + (theta_tr - theta_0) * (z / z_tr) ** (5./4)
    else:
        theta = theta_tr * math.exp((gravity / (c_p * T_tr)) * (z - z_tr))
    return theta

# ---
# Functions taken from or based on functions from atm_utilities.f90
# ---
# Constancts from icar_constants.f90
gravity = 9.81
R_d = 287.058
C_p = 1003.5
Rd_over_Cp = R_d / C_p
P_0 = 100000

# p input pressure dz below, t temperature in layer between
# qv water vapor in layer between
def compute_p_offset(p, dz, t, qv):
    return p * exp( -dz / (Rd / gravity * ( t * ( 1 + 0.608 * qv ) )))

def calc_pressure_from_sea(sealevel_pressure, z):
    return sealevel_pressure * (1 - 2.25577E-5 * z)**5.25588

def calc_pressure(base_pressure, from_z, to_z):
    return base_pressure * (1 - 2.25577E-5 * (to_z - from_z))**5.25588

def calc_pressure_iter(pressure, from_z, to_z):
    dz = 1
    for i in range(from_z,to_z,dz):
        pressure = pressure * (1 - 2.25577E-5 * dz)**5.25588
    return pressure

# theta * exner
def calc_temp(pressure, theta):
    return theta * (pressure / P_0)**Rd_over_Cp

# Taken from atm_utilities.f90
def sat_mr(temperature,pressure):
    if (temperature < 273.15):
        a = 21.8745584
        b = 7.66
    else:
        a = 17.2693882
        b = 35.86
    e_s = 610.78 * math.exp(a * (temperature - 273.16) / (temperature - b))
    if ((pressure - e_s) <= 0):
        e_s = pressure * 0.99999
    sat_mr_val = 0.6219907 * e_s / (pressure - e_s)
    return sat_mr_val

def calc_exner(pressure):
    return (pressure / p_0) ** Rd_over_Cp
