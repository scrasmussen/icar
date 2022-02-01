import sys
import datetime as date

# class generates ICAR namelist file
class ICARoptions:
    def __init__(self, parcel_count=0, hrs=24, output_interval=3600, nz=15):
        # initialize a few variables
        if (nz > 50):
            print('Warning: number of z-layers may not be enough')
        if (parcel_count > 0):
            self.add_parcels(parcel_count)
        self.output_list['outputinterval'] = output_interval
        self.parameters['nz'] = nz

        # handle length of runtime
        fmt = "%Y-%m-%d %H:%M:%S"
        date_s = self.parameters['forcing_start_date'].strip('"')
        t_end = date.datetime.strptime(date_s, fmt) + date.timedelta(hours=hrs)
        self.parameters['end_date'] = '"' + t_end.strftime(fmt) + '"'

        # write to file
        self.f = open('icar_options.nml', 'w')
        self.gen(self.model_version)
        self.gen(self.output_list)
        self.gen(self.physics)
        self.gen(self.files_list)
        self.gen(self.parameters)
        self.clean_var_list()
        self.gen(self.var_list)
        self.gen(self.z_info)
        self.gen(self.parcels)
        self.close()

    def gen(self, nml):
        f = self.f
        f.write("&"+nml['name'])

        i = 0
        for name, val in nml.items():
            if name == 'name':
                continue
            if i != 0:
                f.write(',')
            else:
                f.write('\n')
                i += 1
            f.write(str(name)+'='+str(val))
        f.write('\n/\n')

    def close(self):
        self.f.close()

    def clean_var_list(self):
        for name, val in self.var_list.items():
            if name == 'name':
                continue
            self.var_list[name] = '"' + val + '"'
            # print(name, val)


    def add_parcels(self, parcel_count):
        self.output_list['names'] = self.output_list['names'] + ', "parcels"'
        self.physics['conv'] = 4
        self.parcels['total_parcels'] = parcel_count


    # namelist options to write
    model_version = {
        'name': 'model_version',
        'version': 2.0,
        'comment': '"Unit Test Data"'
    }

    output_list = {
        'name': 'output_list',
        'names': '"u","v","ta2m","hus2m", "precipitation", "swe"',
        'outputinterval': 3600,
        'output_file': '"icar_out_"'
    }

    physics = {
        'name': 'physics',
        'pbl': 0,  'lsm': 0,
        'water': 0, 'mp': 2,
        'rad': 0, 'conv': 0,
        'adv': 1, 'wind': 0
    }

    files_list = {
        'name': 'files_list',
        'init_conditions_file': '"init.nc"',
        # 'boundary_files':'TBD',
        'forcing_file_list': '"forcing.nc"'
    }

    z_info = {
        'name': 'z_info',
        'dz_levels': str([50., 75., 125., 200., 300., 400.] + [500.] * 50)[1:-1]
    }


    var_list = {
        # forcing variables
        'name': 'var_list',
        'uvar':'u',
        'vvar':'v',
        'pvar': 'pressure',
        'tvar': 'theta',
        'qvvar': 'qv', # water_vapor
        'hgtvar': 'height',
        'zvar': 'z',
        'latvar': 'lat_m',
        'lonvar': 'lon_m',
        # init conditions variables
        'lat_hi': 'lat_hi',
        'lon_hi': 'lon_hi',
        'hgt_hi': 'hgt_hi', # surface elevation
        'time_var':'time'
    }
        # 'wvar':'w'


    files_list = {
        'name': 'files_list',
        'init_conditions_file': '"init.nc"',
        'boundary_files': '"forcing.nc"'
    }

    parameters = {
        'name': 'parameters',
        'forcing_start_date': '"2020-12-01 00:00:00"',
        'end_date': '"2020-12-01 06:00:00"',
        'calendar': '"standard"',
        'inputinterval': '3600',
        'dx': '4000.0',
        'qv_is_relative_humidity':'true',
        'readdz': 'true',
        'nz': '15',
        'z_is_geopotential': 'True',
        'z_is_on_interface': 'True',
        'time_varying_z': 'False',
        'use_agl_height': 'False',
        'smooth_wind_distance': '72000'
    }

    parcels = {
        'name': 'parcel_parameters',
        'total_parcels': 0
    }
