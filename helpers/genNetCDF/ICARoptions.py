# class generates ICAR namelist file
class ICARoptions:
    def __init__(self,
                 filename = 'icar_options.nml',
                 # model namelist
                 mod_model_version = 2.1,
                 mod_model_comment = 'Unit Test Data',

                 # output namelist
                 output_vars = ['u','v','precipitation','swe'],
                 output_interval = 3600,
                 output_file = 'icar_out_',
                 restart_interval = 3600,
                 restart_file = 'icar_rst_',

                 # physics namelist
                 phys_opt_pbl = 0,
                 phys_opt_lsm = 0,
                 phys_opt_water = 0,
                 phys_opt_mp = 2,
                 phys_opt_rad = 0,
                 phys_opt_conv = 0,
                 phys_opt_adv = 1,
                 phys_opt_wind = 0,

                 # files namelist
                 init_conditions_file = 'init.nc',
                 boundary_files = 'forcing.nc',
                 forcing_file_list = [],

                 # z_info namelist
                 zinfo_dz_levels = [50., 75., 125., 200., 300., 400.] + [500.] * 50,
                 zinfo_space_varying = ".True.",
                 zinfo_flat_z_height = -1,
                 zinfo_fixed_dz_advection = ".True.",
                 zinfo_sleve=".True.",
                 zinfo_terrain_smooth_windowsize = 4,
                 zinfo_terrain_smooth_cycles = 5,
                 zinfo_decay_rate_L_topo = 1.0,
                 zinfo_decay_rate_S_topo = 5.0,
                 zinfo_sleve_n = 1.35,
                 zinfo_use_terrain_difference = '.False.',

                 # forcing variables namelist
                 forc_u_var = 'u',
                 forc_v_var = 'v',
                 forc_p_var = 'pressure',
                 forc_t_var = 'theta',
                 forc_qv_var = 'qv',
                 forc_hgt_var = 'height',
                 forc_z_var = 'z',
                 forc_lat_var = 'lat_m',
                 forc_lon_var = 'lon_m',
                 forc_lat_hi_var = 'lat_hi',
                 forc_lon_hi_var = 'lon_hi',
                 forc_hgt_hi_var = 'hgt_hi',
                 forc_time_var = 'time',

                 # parameters namelist
                 start_date = '2020-12-01 00:00:00',
                 end_date = '2020-12-02 00:00:00',
                 calendar = 'standard',
                 input_interval = '3600',
                 dx = '4000.0',
                 qv_is_relative_humidity ='true',
                 readdz = 'true',
                 nz = '15',
                 z_is_geopotential = 'False',
                 z_is_on_interface = 'False',
                 t_is_potential = 'True',
                 time_varying_z = 'False',
                 ideal='True',
                 debug='True',        # currently this writes the global jacobian to a netcdf file, and gives min/max values of the jacobian on runtime.
                 smooth_wind_distance = '72000',
                 use_agl_height = True,   #  Use height above ground level to interpolate the wind field instead of height above sea level.
                 agl_cap = 400,  #   Height at which we switch from AGL-interpolation to using ASL-interpolation
                 par_use_adv_options = False,
                 par_use_lt_options = False,
                 par_use_mp_options = False,
                 par_use_lsm_options = False,
                 par_use_bias_correction = False,
                 par_use_block_options = False,
                 par_use_rad_options = False,
                 par_use_cu_options = False,

                 # advection parameters
                 adv_flux_corrected_transport = True,
                 adv_mpdata_order = 2,
                 adv_boundary_buffer = False,

                 # linear theory parameters
                 lt_buffer = 200,
                 lt_stability_window_size = 15,
                 lt_vert_smooth = 5,
                 lt_smooth_nsq = True,
                 lt_max_stability = '5e-6',
                 lt_min_stability = '1e-8',
                 lt_N_squared = '1.0e-7',
                 lt_variable_N = True,
                 lt_linear_update_fraction = 0.25,
                 lt_linear_contribution = 0.5,
                 lt_spatial_linear_fields = True,
                 lt_dirmax = 6.283185307,
                 lt_dirmin = 0,
                 lt_n_dir_values = 36,
                 lt_spdmax = 30,
                 lt_spdmin = 0,
                 lt_n_spd_values = 10,
                 lt_nsqmax = -12.2,
                 lt_nsqmin = -18.42,
                 lt_n_nsq_values = 5,
                 lt_minimum_layer_size = 100.0,
                 lt_read_LUT  = True,
                 lt_write_LUT = True,
                 lt_LUT_filename = '"Linear_Theory_LUT.nc"',

                 # microphysics parameters
                 mp_update_interval = 30,
                 mp_Nt_c  = '100.e6',
                 mp_TNO   = 5.0,
                 mp_am_s  = 0.069,
                 mp_rho_g = 500.0,
                 mp_av_s  = 40.0,
                 mp_bv_s  = 0.55,
                 mp_fv_s  = 100.0,
                 mp_av_g  = 442.0,
                 mp_bv_g  = 0.89,
                 mp_av_i  = 1847.5,
                 mp_Ef_si = 0.05,
                 mp_Ef_rs = 0.95,
                 mp_Ef_rg = 0.75,
                 mp_Ef_ri = 0.95,
                 mp_C_cubes = 0.5,
                 mp_C_sqrd  = 0.3,
                 mp_mu_r    = 0., # 1,2,5
                 mp_t_adjust= 0.0,
                 mp_Ef_rw_l = False,
                 mp_Ef_sw_l = False,
                 mp_top_mp_level = -5,
                 mp_local_precip_fraction = 0.5,

                 # land surface model parameters
                 lsm_update_interval = 300,
                 lsm_LU_Categories = '"MODIFIED_IGBP_MODIS_NOAH"',
                 lsm_monthly_albedo = True,
                 lsm_lh_feedback_fraction = 1,
                 lsm_sh_feedback_fraction = 0.625,
                 lsm_sfc_layer_thickness = 400,
                 lsm_dz_lsm_modification = 0.5,
                 lsm_wind_enhancement = 1.5,
                 lsm_max_swe = 10000,

                 # online bias parameters
                 bias_bias_correction_filename = '""',
                 bias_rain_fraction_var = '""',

                 # experimental blocked flow parameters
                 block_blocking_contribution = 0.5,
                 block_smooth_froude_distance = 6000,
                 block_n_smoothing_passes = 3,
                 block_block_fr_max = 0.75,
                 block_block_fr_min = 0.5,
                 block_block_flow = False,

                 # radiation parameters
                 rad_update_interval_rrtmg = 1800,
                 rad_icloud = 3,
                 rad_use_simple_sw = True,

                 # convection parameters
                 cu_stochastic_cu = 0,
                 cu_tendency_fraction = 1.0,
                 cu_tend_qv_fraction = 1.0,
                 cu_tend_qc_fraction = 1.0,
                 cu_tend_th_fraction = 1.0,
                 cu_tend_qi_fraction = 1.0,

                 # parcels namelist
                 total_parcels = 0):


        # Open file, create namelist objects, then generate namelist
        f = open(filename, 'w')
        self.model_version = \
            ModelVersion(filename = f,
                         version = mod_model_version,
                         comment = mod_model_comment)

        self.output_list = \
            OutputList(filename = f,
                       names = output_vars,
                       outputinterval = output_interval,
                       output_file = output_file,
                       restartinterval = restart_interval,
                       restart_file = restart_file)

        self.physics_list = \
            PhysicsList(filename = f,
                        pbl = phys_opt_pbl,
                        lsm = phys_opt_lsm,
                        water = phys_opt_water,
                        mp = phys_opt_mp,
                        rad = phys_opt_rad,
                        conv = phys_opt_conv,
                        adv = phys_opt_adv,
                        wind = phys_opt_wind)

        self.files_list = \
            FilesList(filename = f,
                      init_conditions_file = init_conditions_file,
                      boundary_files = boundary_files,
                      forcing_file_list = forcing_file_list)

        self.z_info_list = \
            ZInfoList(filename = f,
                      dz_levels = zinfo_dz_levels,
                      space_varying = zinfo_space_varying,
                      flat_z_height = zinfo_flat_z_height ,
                      fixed_dz_advection = zinfo_fixed_dz_advection,
                      sleve = zinfo_sleve,
                      terrain_smooth_windowsize = zinfo_terrain_smooth_windowsize,
                      terrain_smooth_cycles = zinfo_terrain_smooth_cycles ,
                      decay_rate_L_topo = zinfo_decay_rate_L_topo,
                      decay_rate_S_topo = zinfo_decay_rate_S_topo,
                      sleve_n = zinfo_sleve_n,
                      use_terrain_difference = zinfo_use_terrain_difference)

        self.forcing_var_list = \
            ForcingVarList(filename = f,
                           uvar = forc_u_var,
                           vvar = forc_v_var,
                           pvar = forc_p_var,
                           tvar = forc_t_var,
                           qvvar = forc_qv_var,
                           hgtvar = forc_hgt_var,
                           zvar = forc_z_var,
                           latvar = forc_lat_var,
                           lonvar = forc_lon_var,
                           lat_hi = forc_lat_hi_var,
                           lon_hi = forc_lon_hi_var,
                           hgt_hi = forc_hgt_hi_var,
                           time_var = forc_time_var)

        self.parameters_list = \
            ParametersList(filename = f,
                           forcing_start_date = start_date,
                           end_date = end_date,
                           calendar = calendar,
                           inputinterval = input_interval,
                           dx = dx,
                           qv_is_relative_humidity = qv_is_relative_humidity,
                           ideal = ideal,
                           debug = debug,
                           readdz = readdz,
                           nz = nz,
                           z_is_geopotential = z_is_geopotential,
                           z_is_on_interface = z_is_on_interface,
                           t_is_potential = t_is_potential,
                           time_varying_z = time_varying_z,
                           use_agl_height = use_agl_height,
                           agl_cap = agl_cap,
                           smooth_wind_distance = smooth_wind_distance,
                           use_adv_options = par_use_adv_options,
                           use_lt_options = par_use_lt_options,
                           use_mp_options = par_use_mp_options,
                           use_lsm_options = par_use_lsm_options,
                           use_bias_correction = par_use_bias_correction,
                           use_block_options = par_use_block_options,
                           use_rad_options = par_use_rad_options,
                           use_cu_options = par_use_cu_options
        )

        self.linear_theory_list = \
            LinearTheoryList(filename = f,
                             buffer = lt_buffer,
                             stability_window_size = lt_stability_window_size,
                             vert_smooth = lt_vert_smooth,
                             smooth_nsq = lt_smooth_nsq,
                             max_stability = lt_max_stability,
                             min_stability = lt_min_stability,
                             N_squared = lt_N_squared,
                             variable_N = lt_variable_N,
                             linear_update_fraction = lt_linear_update_fraction,
                             linear_contribution = lt_linear_contribution,
                             spatial_linear_fields = lt_spatial_linear_fields,
                             dirmax = lt_dirmax,
                             dirmin = lt_dirmin,
                             n_dir_values = lt_n_dir_values,
                             spdmax = lt_spdmax,
                             spdmin = lt_spdmin,
                             n_spd_values = lt_n_spd_values,
                             nsqmax = lt_nsqmax,
                             nsqmin = lt_nsqmin,
                             n_nsq_values = lt_n_nsq_values,
                             minimum_layer_size = lt_minimum_layer_size,
                             read_LUT = lt_read_LUT,
                             write_LUT = lt_write_LUT,
                             LUT_filename = lt_LUT_filename)

        self.mp_parameters = \
            MpParameters(filename = f,
                             update_interval = mp_update_interval,
                             Nt_c = mp_Nt_c,
                             TNO = mp_TNO,
                             am_s = mp_am_s,
                             rho_g = mp_rho_g,
                             av_s = mp_av_s,
                             bv_s = mp_bv_s,
                             fv_s = mp_fv_s,
                             av_g = mp_av_g,
                             bv_g = mp_bv_g,
                             av_i = mp_av_i,
                             Ef_si = mp_Ef_si,
                             Ef_rs = mp_Ef_rs,
                             Ef_rg = mp_Ef_rg,
                             Ef_ri = mp_Ef_ri,
                             C_cubes = mp_C_cubes,
                             C_sqrd = mp_C_sqrd,
                             mu_r = mp_mu_r,
                             t_adjust= mp_t_adjust,
                             Ef_rw_l = mp_Ef_rw_l,
                             Ef_sw_l = mp_Ef_sw_l,
                             top_mp_level = mp_top_mp_level,
                             local_precip_fraction = mp_local_precip_fraction)

        self. lsm_parameters = \
            LsmParameters(filename = f,
                          update_interval = lsm_update_interval,
                          LU_Categories = lsm_LU_Categories,
                          monthly_albedo = lsm_monthly_albedo,
                          lh_feedback_fraction = lsm_lh_feedback_fraction,
                          sh_feedback_fraction = lsm_sh_feedback_fraction,
                          sfc_layer_thickness = lsm_sfc_layer_thickness,
                          dz_lsm_modification = lsm_dz_lsm_modification,
                          wind_enhancement = lsm_wind_enhancement,
                          max_swe = lsm_max_swe)

        self.bias_parameters = \
            BiasParameters(filename = f,
                           bias_correction_filename = bias_bias_correction_filename,
                           rain_fraction_var = bias_rain_fraction_var)

        self.adv_parameters = \
            AdvParameters(filename = f,
                          flux_corrected_transport = adv_flux_corrected_transport,
                          mpdata_order = adv_mpdata_order,
                          boundary_buffer = adv_boundary_buffer)

        self.block_parameters = \
            BlockParameters(filename = f,
                            blocking_contribution = block_blocking_contribution,
                            smooth_froude_distance = block_smooth_froude_distance,
                            n_smoothing_passes = block_n_smoothing_passes,
                            block_fr_max = block_block_fr_max,
                            block_fr_min = block_block_fr_min,
                            block_flow = block_block_flow)

        self.rad_parameters = \
            RadParameters(filename = f,
                          update_interval_rrtmg = rad_update_interval_rrtmg,
                          icloud = rad_icloud,
                          use_simple_sw = rad_use_simple_sw)

        self.cu_parameters = \
            CuParameters(filename = f,
                         stochastic_cu = cu_stochastic_cu,
                         tendency_fraction = cu_tendency_fraction,
                         tend_qv_fraction = cu_tend_qv_fraction,
                         tend_qc_fraction = cu_tend_qc_fraction,
                         tend_th_fraction = cu_tend_th_fraction,
                         tend_qi_fraction = cu_tend_qi_fraction)

        self.parcels_list = \
            ParcelsList(filename = f,
                        total_parcels = total_parcels)

        self.generate_all_namelists()
        f.close()


    def generate_all_namelists(self):
        self.model_version.gen()
        self.output_list.gen()
        self.physics_list.gen()
        self.files_list.gen()
        self.z_info_list.gen()
        self.forcing_var_list.gen()
        self.parameters_list.gen()
        self.adv_parameters.gen()
        self.linear_theory_list.gen()
        self.mp_parameters.gen()
        self.lsm_parameters.gen()
        self.bias_parameters.gen()
        self.block_parameters.gen()
        self.rad_parameters.gen()
        self.cu_parameters.gen()
        self.parcels_list.gen()



class Namelist:
    def __init__(self, kargs):
        self.filename = kargs['filename']
        del kargs['filename']
        self.nml = {}
        for name, val in kargs.items():
            self.nml[name] = val
        self.remove_empty_values()
    def remove_empty_values(self):
        delete = []
        for name, val in self.nml.items():
            if val in ['', ""]:
                delete.append(name)
        for name in delete:
                del self.nml[name]
    def gen(self):
        f = self.filename
        f.write("&"+self.nml['name'])
        del self.nml['name']
        i = 0
        for name, val in self.nml.items():
            if i != 0:
                f.write(', \n')
            else:
                f.write('\n')
                i += 1
            f.write(str(name)+'='+str(val))
        f.write('\n/\n\n')


class ModelVersion(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'model_version'

    def gen(self):
        for name, val in self.nml.items():
            if name == 'comment':
                self.nml[name] = '"' + val + '"'
        super().gen()


class OutputList(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'output_list'
    def gen(self):
        for name, val in self.nml.items():
            if name == 'names':
                self.nml[name] = '"' + '","'.join(val) + '"'
            if name in ['output_file', 'restart_file']:
                self.nml[name] = '"' + val + '"'
        super().gen()


class PhysicsList(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'physics'


class FilesList(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'files_list'
    def gen(self):
        for name, val in self.nml.items():
            if name == 'forcing_file_list':
                self.nml[name] = '"' + '","'.join(val) + '"'
            elif name != 'name':
                self.nml[name] = '"' + val + '"'
        super().gen()


class ZInfoList(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'z_info'
    def gen(self):
        for name, val in self.nml.items():
            if name == 'dz_levels':
                self.nml[name] = str(val)[1:-1]
        super().gen()


class ForcingVarList(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'var_list'

    def gen(self):
        for name, val in self.nml.items():
            if name == 'name':
                continue
            self.nml[name] = '"' + val + '"'
        super().gen()


class ParametersList(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'parameters'

    def gen(self):
        for name, val in self.nml.items():
            if name in ['forcing_start_date', 'end_date', 'calendar']:
                self.nml[name] = '"' + val + '"'
        super().gen()


class LinearTheoryList(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'lt_parameters'

class MpParameters(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'mp_parameters'

class LsmParameters(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'lsm_parameters'

class BiasParameters(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'bias_parameters'

class BlockParameters(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'block_parameters'

class RadParameters(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'rad_parameters'

class CuParameters(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'cu_parameters'

class AdvParameters(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'adv_parameters'

class ParcelsList(Namelist):
    def __init__(self, **kargs):
        Namelist.__init__(self, kargs)
        self.nml['name'] = 'parcel_parameters'
