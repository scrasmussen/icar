SRC = $(wildcard src/*/*.f90)
OBJ = $(patsubst %.f90, %.o, $(SRC))
# OBJ_FULLPATH = $(patsubst %.f90, %.o, $(SRC))
# OBJ = $(patsubst '/','_', $(OBJ_FULLPATH))
OBJ_FULLPATH=$(SRC:%.f90=%.o)
files_tmp=$(subst /,_,$(SRC))
files_tmp2=$(subst .f90,.f90.o,$(files_tmp))
files=$(filter-out src_tests_%,$(files_tmp2))
fullpath=$(addprefix build/caf_*/icar/,$(files))


.PHONY: print build
all: $(fullpath) build
	echo "DONE"

build:
	bash scripts/build-icar.sh

build/caf_*/icar/src_utilities_vinterp.f90.o:src/utilities/vinterp.f90
	rm -f $(@)
build/caf_*/icar/src_utilities_time_obj.f90.o:src/utilities/time_obj.f90
	rm $(@)
build/caf_*/icar/src_utilities_time_io.f90.o:src/utilities/time_io.f90
	rm $(@)
build/caf_*/icar/src_utilities_time_h.f90.o:src/utilities/time_h.f90
	rm $(@)
build/caf_*/icar/src_utilities_time_delta_obj.f90.o:src/utilities/time_delta_obj.f90
	rm $(@)
build/caf_*/icar/src_utilities_time.f90.o:src/utilities/time.f90
	rm $(@)
build/caf_*/icar/src_utilities_string.f90.o:src/utilities/string.f90
	rm $(@)
build/caf_*/icar/src_utilities_geo_reader.f90.o:src/utilities/geo_reader.f90
	rm $(@)
build/caf_*/icar/src_utilities_fftw.f90.o:src/utilities/fftw.f90
	rm $(@)
build/caf_*/icar/src_utilities_fftshift.f90.o:src/utilities/fftshift.f90
	rm $(@)
build/caf_*/icar/src_utilities_debug_utils.f90.o:src/utilities/debug_utils.f90
	rm $(@)
build/caf_*/icar/src_utilities_co_utilities.f90.o:src/utilities/co_utilities.f90
	rm $(@)
build/caf_*/icar/src_utilities_atm_utilities.f90.o:src/utilities/atm_utilities.f90
	rm $(@)
build/caf_*/icar/src_utilities_array_utilities.f90.o:src/utilities/array_utilities.f90
	rm $(@)
build/caf_*/icar/src_physics_wind.f90.o:src/physics/wind.f90
	rm $(@)
build/caf_*/icar/src_physics_water_simple.f90.o:src/physics/water_simple.f90
	rm $(@)
build/caf_*/icar/src_physics_ra_simple.f90.o:src/physics/ra_simple.f90
	rm $(@)
build/caf_*/icar/src_physics_ra_driver.f90.o:src/physics/ra_driver.f90
	rm $(@)
build/caf_*/icar/src_physics_pbl_ysu.f90.o:src/physics/pbl_ysu.f90
	rm $(@)
build/caf_*/icar/src_physics_pbl_simple.f90.o:src/physics/pbl_simple.f90
	rm $(@)
build/caf_*/icar/src_physics_pbl_driver.f90.o:src/physics/pbl_driver.f90
	rm $(@)
build/caf_*/icar/src_physics_mp_wsm6.f90.o:src/physics/mp_wsm6.f90
	rm $(@)
build/caf_*/icar/src_physics_mp_thompson_aer.f90.o:src/physics/mp_thompson_aer.f90
	rm $(@)
build/caf_*/icar/src_physics_mp_thompson.f90.o:src/physics/mp_thompson.f90
	rm $(@)
build/caf_*/icar/src_physics_mp_simple.f90.o:src/physics/mp_simple.f90
	rm $(@)
build/caf_*/icar/src_physics_mp_morrison.f90.o:src/physics/mp_morrison.f90
	rm $(@)
build/caf_*/icar/src_physics_mp_driver.f90.o:src/physics/mp_driver.f90
	rm $(@)
build/caf_*/icar/src_physics_lsm_noahlsm.f90.o:src/physics/lsm_noahlsm.f90
	rm $(@)
build/caf_*/icar/src_physics_lsm_noahdrv.f90.o:src/physics/lsm_noahdrv.f90
	rm $(@)
build/caf_*/icar/src_physics_lsm_driver.f90.o:src/physics/lsm_driver.f90
	rm $(@)
build/caf_*/icar/src_physics_linear_winds.f90.o:src/physics/linear_winds.f90
	rm $(@)
build/caf_*/icar/src_physics_cu_tiedtke.f90.o:src/physics/cu_tiedtke.f90
	rm $(@)
build/caf_*/icar/src_physics_cu_kf.f90.o:src/physics/cu_kf.f90
	rm $(@)
build/caf_*/icar/src_physics_cu_driver.f90.o:src/physics/cu_driver.f90
	rm $(@)
build/caf_*/icar/src_physics_advection_driver.f90.o:src/physics/advection_driver.f90
	rm $(@)
build/caf_*/icar/src_physics_advect.f90.o:src/physics/advect.f90
	rm $(@)
build/caf_*/icar/src_physics_adv_mpdata.f90.o:src/physics/adv_mpdata.f90
	rm $(@)
build/caf_*/icar/src_objects_variable_obj.f90.o:src/objects/variable_obj.f90
	rm $(@)
build/caf_*/icar/src_objects_variable_h.f90.o:src/objects/variable_h.f90
	rm $(@)
build/caf_*/icar/src_objects_variable_dict_obj.f90.o:src/objects/variable_dict_obj.f90
	rm $(@)
build/caf_*/icar/src_objects_variable_dict_h.f90.o:src/objects/variable_dict_h.f90
	rm $(@)
build/caf_*/icar/src_objects_timer_obj.f90.o:src/objects/timer_obj.f90
	rm $(@)
build/caf_*/icar/src_objects_timer_h.f90.o:src/objects/timer_h.f90
	rm $(@)
build/caf_*/icar/src_objects_options_obj.f90.o:src/objects/options_obj.f90
	rm $(@)
build/caf_*/icar/src_objects_options_h.f90.o:src/objects/options_h.f90
	rm $(@)
build/caf_*/icar/src_objects_opt_types.f90.o:src/objects/opt_types.f90
	rm $(@)
build/caf_*/icar/src_objects_meta_data_obj.f90.o:src/objects/meta_data_obj.f90
	rm $(@)
build/caf_*/icar/src_objects_meta_data_h.f90.o:src/objects/meta_data_h.f90
	rm $(@)
build/caf_*/icar/src_objects_grid_obj.f90.o:src/objects/grid_obj.f90
	rm $(@)
build/caf_*/icar/src_objects_grid_h.f90.o:src/objects/grid_h.f90
	rm $(@)
build/caf_*/icar/src_objects_exchangeable_obj.f90.o:src/objects/exchangeable_obj.f90
	rm $(@)
build/caf_*/icar/src_objects_exchangeable_h.f90.o:src/objects/exchangeable_h.f90
	rm $(@)
build/caf_*/icar/src_objects_domain_obj.f90.o:src/objects/domain_obj.f90
	rm $(@)
build/caf_*/icar/src_objects_domain_h.f90.o:src/objects/domain_h.f90
	rm $(@)
build/caf_*/icar/src_objects_boundary_obj.f90.o:src/objects/boundary_obj.f90
	rm $(@)
build/caf_*/icar/src_objects_boundary_h.f90.o:src/objects/boundary_h.f90
	rm $(@)
build/caf_*/icar/src_main_time_step.f90.o:src/main/time_step.f90
	rm $(@)
build/caf_*/icar/src_main_model_tracking.f90.o:src/main/model_tracking.f90
	rm $(@)
build/caf_*/icar/src_main_init.f90.o:src/main/init.f90
	rm $(@)
build/caf_*/icar/src_main_data_structures.f90.o:src/main/data_structures.f90
	rm $(@)
build/caf_*/icar/src_io_restart_h.f90.o:src/io/restart_h.f90
	rm $(@)
build/caf_*/icar/src_io_restart.f90.o:src/io/restart.f90
	rm $(@)
build/caf_*/icar/src_io_output_obj.f90.o:src/io/output_obj.f90
	rm $(@)
build/caf_*/icar/src_io_output_h.f90.o:src/io/output_h.f90
	rm $(@)
build/caf_*/icar/src_io_lt_lut_io.f90.o:src/io/lt_lut_io.f90
	rm $(@)
build/caf_*/icar/src_io_io_routines.f90.o:src/io/io_routines.f90
	rm $(@)
build/caf_*/icar/src_io_default_output_metadata.f90.o:src/io/default_output_metadata.f90
	rm $(@)
build/caf_*/icar/src_constants_wrf_constants.f90.o:src/constants/wrf_constants.f90
	rm $(@)
build/caf_*/icar/src_constants_icar_constants.f90.o:src/constants/icar_constants.f90
	rm $(@)
