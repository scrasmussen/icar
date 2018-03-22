submodule(domain_interface) domain_implementation
    use assertions_mod,       only : assert, assertions
    use grid_interface,       only : grid_t
    use options_interface,    only : options_t
    use mod_atm_utilities,    only : exner_function, update_pressure
    use icar_constants,       only : kVARS
    use string,               only : str
    use microphysics,         only : mp_simple_var_request
    use co_util,              only : broadcast
    use io_routines,          only : io_read, io_write
    use geo,                  only : geo_lut, geo_interp, geo_interp2d, standardize_coordinates
    use array_utilities,      only : array_offset_x, array_offset_y, smooth_array
    use vertical_interpolation,only : vinterp, vLUT

    implicit none

    interface setup
        module procedure setup_var, setup_exch
    end interface

    ! primary public routines : init, get_initial_conditions, halo_send, halo_retrieve, or halo_exchange
contains


    !> -------------------------------
    !! Initialize the size of the domain
    !!
    !! -------------------------------
    module subroutine init(this, options)
        class(domain_t), intent(inout) :: this
        class(options_t),intent(inout) :: options

        this%dx = options%parameters%dx

        call this%var_request(options)

        call read_domain_shape(this, options)

        call create_variables(this, options)

        call initialize_variables(this, options)

        call setup_meta_data(this)

    end subroutine


    !> -------------------------------
    !! Set up the initial conditions for the domain
    !!
    !! This includes setting up all of the geographic interpolation now that we have the forcing grid
    !! and interpolating the first time step of forcing data on to the high res domain grids
    !!
    !! -------------------------------
    module subroutine get_initial_conditions(this, forcing, options)
      implicit none
      class(domain_t),  intent(inout) :: this
      class(boundary_t),intent(inout) :: forcing
      class(options_t), intent(in)    :: options

      ! create geographic lookup table for domain
      call setup_geo_interpolation(this, forcing)

      ! for all variables with a forcing_var /= "", get forcing, interpolate to local domain
      call this%interpolate_forcing(forcing)

      this%model_time = forcing%current_time

    end subroutine


    !> -------------------------------
    !! Send the halos from all exchangable objects to their neighbors
    !!
    !! -------------------------------
    module subroutine halo_send(this)
      class(domain_t), intent(inout) :: this
      if (associated(this%water_vapor%data_3d))           call this%water_vapor%send()
      if (associated(this%potential_temperature%data_3d)) call this%potential_temperature%send()
      if (associated(this%cloud_water_mass%data_3d))      call this%cloud_water_mass%send()
      if (associated(this%cloud_ice_mass%data_3d))        call this%cloud_ice_mass%send()
      if (associated(this%cloud_ice_number%data_3d))      call this%cloud_ice_number%send()
      if (associated(this%rain_mass%data_3d))             call this%rain_mass%send()
      if (associated(this%rain_number%data_3d))           call this%rain_number%send()
      if (associated(this%snow_mass%data_3d))             call this%snow_mass%send()
      if (associated(this%graupel_mass%data_3d))          call this%graupel_mass%send()
    end subroutine

    !> -------------------------------
    !! Get the halos from all exchangable objects from their neighbors
    !!
    !! -------------------------------
    module subroutine halo_retrieve(this)
      class(domain_t), intent(inout) :: this
      if (associated(this%water_vapor%data_3d))           call this%water_vapor%retrieve() ! the retrieve call will sync all
      if (associated(this%potential_temperature%data_3d)) call this%potential_temperature%retrieve(no_sync=.True.)
      if (associated(this%cloud_water_mass%data_3d))      call this%cloud_water_mass%retrieve(no_sync=.True.)
      if (associated(this%cloud_ice_mass%data_3d))        call this%cloud_ice_mass%retrieve(no_sync=.True.)
      if (associated(this%cloud_ice_number%data_3d))      call this%cloud_ice_number%retrieve(no_sync=.True.)
      if (associated(this%rain_mass%data_3d))             call this%rain_mass%retrieve(no_sync=.True.)
      if (associated(this%rain_number%data_3d))           call this%rain_number%retrieve(no_sync=.True.)
      if (associated(this%snow_mass%data_3d))             call this%snow_mass%retrieve(no_sync=.True.)
      if (associated(this%graupel_mass%data_3d))          call this%graupel_mass%retrieve(no_sync=.True.)
    end subroutine

    !> -------------------------------
    !! Send and get the halos from all exchangable objects to/from their neighbors
    !!
    !! -------------------------------
    module subroutine halo_exchange(this)
      class(domain_t), intent(inout) :: this
      call this%halo_send()

      call this%halo_retrieve()
    end subroutine


    !> -------------------------------
    !! Allocate and or initialize all domain variables if they have been requested
    !!
    !! -------------------------------
    subroutine create_variables(this, opt)
        class(domain_t), intent(inout)  :: this
        class(options_t),intent(in)     :: opt
        integer :: i,j

        integer :: ims, ime, jms, jme

        ims = this%grid%ims
        ime = this%grid%ime
        jms = this%grid%jms
        jme = this%grid%jme

        if (this_image()==1) print *,"  Initializing variables"

        if (0<opt%vars_to_allocate( kVARS%u) )                          call setup(this%u,                        this%u_grid,   forcing_var=opt%parameters%uvar,       list=this%variables_to_force, force_boundaries=.False.)
        if (0<opt%vars_to_allocate( kVARS%v) )                          call setup(this%v,                        this%v_grid,   forcing_var=opt%parameters%vvar,       list=this%variables_to_force, force_boundaries=.False.)
        if (0<opt%vars_to_allocate( kVARS%w) )                          call setup(this%w,                        this%grid )
        if (0<opt%vars_to_allocate( kVARS%water_vapor) )                call setup(this%water_vapor,              this%grid,     forcing_var=opt%parameters%qvvar,      list=this%variables_to_force, force_boundaries=.True.)
        if (0<opt%vars_to_allocate( kVARS%potential_temperature) )      call setup(this%potential_temperature,    this%grid,     forcing_var=opt%parameters%tvar,       list=this%variables_to_force, force_boundaries=.True.)
        if (0<opt%vars_to_allocate( kVARS%cloud_water) )                call setup(this%cloud_water_mass,         this%grid,     forcing_var=opt%parameters%qcvar,      list=this%variables_to_force, force_boundaries=.True.)
        if (0<opt%vars_to_allocate( kVARS%cloud_number_concentration))  call setup(this%cloud_number,             this%grid )
        if (0<opt%vars_to_allocate( kVARS%cloud_ice) )                  call setup(this%cloud_ice_mass,           this%grid,     forcing_var=opt%parameters%qivar,      list=this%variables_to_force, force_boundaries=.True.)
        if (0<opt%vars_to_allocate( kVARS%ice_number_concentration))    call setup(this%cloud_ice_number,         this%grid )
        if (0<opt%vars_to_allocate( kVARS%rain_in_air) )                call setup(this%rain_mass,                this%grid,     forcing_var=opt%parameters%qrvar,      list=this%variables_to_force, force_boundaries=.True.)
        if (0<opt%vars_to_allocate( kVARS%rain_number_concentration))   call setup(this%rain_number,              this%grid )
        if (0<opt%vars_to_allocate( kVARS%snow_in_air) )                call setup(this%snow_mass,                this%grid,     forcing_var=opt%parameters%qsvar,      list=this%variables_to_force, force_boundaries=.True.)
        if (0<opt%vars_to_allocate( kVARS%snow_number_concentration) )  call setup(this%snow_number,              this%grid )
        if (0<opt%vars_to_allocate( kVARS%graupel_in_air) )             call setup(this%graupel_mass,             this%grid,     forcing_var=opt%parameters%qgvar,      list=this%variables_to_force, force_boundaries=.True.)
        if (0<opt%vars_to_allocate( kVARS%graupel_number_concentration))call setup(this%graupel_number,           this%grid )
        if (0<opt%vars_to_allocate( kVARS%precipitation) )              call setup(this%accumulated_precipitation,this%grid2d )
        if (0<opt%vars_to_allocate( kVARS%snowfall) )                   call setup(this%accumulated_snowfall,     this%grid2d )
        if (0<opt%vars_to_allocate( kVARS%pressure) )                   call setup(this%pressure,                 this%grid,     forcing_var=opt%parameters%pvar,       list=this%variables_to_force, force_boundaries=.False.)
        if (0<opt%vars_to_allocate( kVARS%temperature) )                call setup(this%temperature,              this%grid )
        if (0<opt%vars_to_allocate( kVARS%exner) )                      call setup(this%exner,                    this%grid )
        if (0<opt%vars_to_allocate( kVARS%z) )                          call setup(this%z,                        this%grid )
        if (0<opt%vars_to_allocate( kVARS%dz_interface) )               call setup(this%dz_interface,             this%grid )
        if (0<opt%vars_to_allocate( kVARS%z_interface) )                call setup(this%z_interface,              this%grid )
        if (0<opt%vars_to_allocate( kVARS%dz) )                         call setup(this%dz_mass,                  this%grid )
        if (0<opt%vars_to_allocate( kVARS%density) )                    call setup(this%density,                  this%grid )
        if (0<opt%vars_to_allocate( kVARS%pressure_interface) )         call setup(this%pressure_interface,       this%grid )
        if (0<opt%vars_to_allocate( kVARS%graupel) )                    call setup(this%graupel,                  this%grid2d )
        if (0<opt%vars_to_allocate( kVARS%shortwave) )                  call setup(this%shortwave,                this%grid2d,   forcing_var=opt%parameters%swdown_var,  list=this%variables_to_force)
        if (0<opt%vars_to_allocate( kVARS%longwave) )                   call setup(this%longwave,                 this%grid2d,   forcing_var=opt%parameters%lwdown_var,  list=this%variables_to_force)
        if (0<opt%vars_to_allocate( kVARS%vegetation_fraction) )        call setup(this%vegetation_fraction,      this%grid_monthly )
        if (0<opt%vars_to_allocate( kVARS%lai) )                        call setup(this%lai,                      this%grid2d )
        if (0<opt%vars_to_allocate( kVARS%canopy_water) )               call setup(this%canopy_water,             this%grid2d )
        if (0<opt%vars_to_allocate( kVARS%snow_water_equivalent) )      call setup(this%snow_water_equivalent,    this%grid2d )
        if (0<opt%vars_to_allocate( kVARS%skin_temperature) )           call setup(this%skin_temperature,         this%grid2d,   forcing_var=opt%parameters%sst_var,     list=this%variables_to_force)
        if (0<opt%vars_to_allocate( kVARS%soil_water_content) )         call setup(this%soil_water_content,       this%grid_soil,forcing_var=opt%parameters%soil_t_var,  list=this%variables_to_force)
        if (0<opt%vars_to_allocate( kVARS%soil_temperature) )           call setup(this%soil_temperature,         this%grid_soil,forcing_var=opt%parameters%soil_vwc_var,list=this%variables_to_force)
        if (0<opt%vars_to_allocate( kVARS%latitude) )                   call setup(this%latitude,                 this%grid2d)
        if (0<opt%vars_to_allocate( kVARS%longitude) )                  call setup(this%longitude,                this%grid2d)
        if (0<opt%vars_to_allocate( kVARS%u_latitude) )                 call setup(this%u_latitude,               this%u_grid2d)
        if (0<opt%vars_to_allocate( kVARS%u_longitude) )                call setup(this%u_longitude,              this%u_grid2d)
        if (0<opt%vars_to_allocate( kVARS%v_latitude) )                 call setup(this%v_latitude,               this%v_grid2d)
        if (0<opt%vars_to_allocate( kVARS%v_longitude) )                call setup(this%v_longitude,              this%v_grid2d)
        if (0<opt%vars_to_allocate( kVARS%terrain) )                    call setup(this%terrain,                  this%grid2d)

        ! integer variable_t types aren't available yet...
        if (0<opt%vars_to_allocate( kVARS%precipitation) ) allocate(this%precipitation_bucket     (ims:ime, jms:jme),          source=0)
        if (0<opt%vars_to_allocate( kVARS%snowfall) )      allocate(this%snowfall_bucket          (ims:ime, jms:jme),          source=0)
        if (0<opt%vars_to_allocate( kVARS%land_cover) )    allocate(this%land_cover_type          (ims:ime, jms:jme),          source=0 )

        ! call setup_forcing_variable

    end subroutine

    !> -------------------------------
    !! Setup a regular variable.
    !!
    !! Initializes the variable
    !! including the forcing_variable if it was set
    !! and adds that variable to the list of variables that has forcing data if the list is supplied
    !! and the forcing_var is both present and not blank ("")
    !!
    !! -------------------------------
    subroutine setup_var(var, grid, forcing_var, list, force_boundaries)
        implicit none
        type(variable_t),   intent(inout) :: var
        type(grid_t),       intent(in)    :: grid
        character(len=*),   intent(in),   optional :: forcing_var
        type(var_dict_t),   intent(inout),optional :: list
        logical,            intent(in),   optional :: force_boundaries

        if (present(forcing_var)) then
            call var%initialize(grid, forcing_var=forcing_var)

            if (present(list)) then
                if (Len(Trim(forcing_var)) /= 0) then
                    if (present(force_boundaries)) var%force_boundaries = force_boundaries
                    call list%add_var(forcing_var, var)
                endif
            endif
        else

            call var%initialize(grid)
        endif

    end subroutine

    !> -------------------------------
    !! Setup an exchangeable variable.
    !!
    !! Initializes the variable
    !! including the forcing_variable if it was set
    !! and adds that variable to the list of variables that has forcing data if the list is supplied
    !! and the forcing_var is both present and not blank ("")
    !!
    !! -------------------------------
    subroutine setup_exch(var, grid, forcing_var, list, force_boundaries)
        implicit none
        type(exchangeable_t),   intent(inout) :: var
        type(grid_t),           intent(in)    :: grid
        character(len=*),       intent(in),   optional :: forcing_var
        type(var_dict_t),       intent(inout),optional :: list
        logical,                intent(in),   optional :: force_boundaries

        if (present(forcing_var)) then
            call var%initialize(grid, forcing_var=forcing_var)

            if (present(list)) then
                if (Len(Trim(forcing_var)) /= 0) then
                    if (present(force_boundaries)) var%meta_data%force_boundaries = force_boundaries
                    call list%add_var(forcing_var, var%meta_data)
                endif
            endif
        else

            call var%initialize(grid)
        endif

    end subroutine

    !> ---------------------------------
    !! Load the data in varname from filename into data_array
    !!
    !! The first / master image reads the file from the disk
    !! Other images get the data broadcast from the master image
    !!
    !! ---------------------------------
    subroutine load_data(filename, varname, data_array, grid)
        implicit none
        character(len=*),  intent(in)   :: filename, varname
        real, allocatable, intent(inout):: data_array(:,:)
        type(grid_t),      intent(in)   :: grid

        if (this_image()==1) then
            call io_read(filename, varname, data_array)
        else
            if (allocated(data_array)) deallocate(data_array)
            allocate(data_array(grid%nx_global, grid%ny_global))
        endif

        call broadcast(data_array, 1, 1, num_images(), .true.)

    end subroutine


    !> ---------------------------------
    !! Read the core model variables from disk
    !!
    !! Reads Terrain, lat, lon and u/v lat/lon on the high-res domain grid
    !! Passing data between images and disk is handled by load_data
    !!
    !! ---------------------------------
    subroutine read_core_variables(this, options)
        implicit none
        class(domain_t), intent(inout)  :: this
        class(options_t),intent(in)     :: options
        real, allocatable :: temporary_data(:,:), temp_offset(:,:)

        ! Read the terrain data
        call load_data(options%parameters%init_conditions_file,   &
                       options%parameters%hgt_hi,                 &
                       temporary_data, this%grid)
        this%terrain%data_2d = temporary_data(this%grid%ims:this%grid%ime, this%grid%jms:this%grid%jme)

        associate(g => this%u_grid2d_ext, geo => this%geo_u)
            call array_offset_x(temporary_data, temp_offset)
            if (allocated(geo%z)) deallocate(geo%z)
            allocate(geo%z(1:g%ime-g%ims+1, 1:this%u_grid%kme-this%u_grid%kms+1, 1:g%jme-g%jms+1))
            geo%z(:,1,:) = temp_offset(g%ims:g%ime, g%jms:g%jme)
        end associate

        associate(g => this%v_grid2d_ext, geo => this%geo_v)
            call array_offset_y(temporary_data, temp_offset)
            if (allocated(geo%z)) deallocate(geo%z)
            allocate(geo%z(1:g%ime-g%ims+1, 1:this%u_grid%kme-this%u_grid%kms+1, 1:g%jme-g%jms+1))
            geo%z(:,1,:) = temp_offset(g%ims:g%ime, g%jms:g%jme)
        end associate


        ! Read the latitude data
        call load_data(options%parameters%init_conditions_file,   &
                       options%parameters%lat_hi,                 &
                       temporary_data, this%grid)
        this%latitude%data_2d = temporary_data(this%grid%ims:this%grid%ime, this%grid%jms:this%grid%jme)

        ! Read the longitude data
        call load_data(options%parameters%init_conditions_file,   &
                       options%parameters%lon_hi,                 &
                       temporary_data, this%grid)
        this%longitude%data_2d = temporary_data(this%grid%ims:this%grid%ime, this%grid%jms:this%grid%jme)


        !-----------------------------------------
        !
        ! Handle staggered lat/lon grids, straightfoward if ulat/ulon are supplied
        ! If not, then read in mass grid lat/lon and stagger them
        !
        !-----------------------------------------
        ! Read the u-grid longitude data if specified, other wise interpolate from mass grid
        if (options%parameters%ulon_hi /= "") then
            call load_data(options%parameters%init_conditions_file,   &
                           options%parameters%ulon_hi,                &
                           temporary_data, this%u_grid)

            call subset_array(temporary_data, this%u_longitude%data_2d, this%u_grid)

            associate(g=>this%u_grid2d_ext, var=>this%geo_u%lon)
                allocate(this%geo_u%lon(1:g%ime-g%ims+1, 1:g%jme-g%jms+1))
                call subset_array(temporary_data, this%geo_u%lon, g)
            end associate
        else
            ! load the mass grid data again to get the full grid
            call load_data(options%parameters%init_conditions_file,   &
                           options%parameters%lon_hi,                 &
                           temporary_data, this%grid)

            call array_offset_x(temporary_data, temp_offset)
            call subset_array(temp_offset, this%u_longitude%data_2d, this%u_grid)
            associate(g=>this%u_grid2d_ext, var=>this%geo_u%lon)
                allocate(this%geo_u%lon(1:g%ime-g%ims+1, 1:g%jme-g%jms+1))
                call subset_array(temp_offset, this%geo_u%lon, g)
            end associate
        endif

        ! Read the u-grid latitude data if specified, other wise interpolate from mass grid
        if (options%parameters%ulat_hi /= "") then
            call load_data(options%parameters%init_conditions_file,   &
                           options%parameters%ulat_hi,                &
                           temporary_data, this%u_grid)

            call subset_array(temporary_data, this%u_latitude%data_2d, this%u_grid)
            associate(g=>this%u_grid2d_ext, var=>this%geo_u%lat)
                allocate(this%geo_u%lat(1:g%ime-g%ims+1, 1:g%jme-g%jms+1))
                call subset_array(temporary_data, this%geo_u%lat, g)
            end associate
        else
            ! load the mass grid data again to get the full grid
            call load_data(options%parameters%init_conditions_file,   &
                           options%parameters%lat_hi,                 &
                           temporary_data, this%grid)

            call array_offset_x(temporary_data, temp_offset)
            call subset_array(temp_offset, this%u_latitude%data_2d, this%u_grid)
            associate(g=>this%u_grid2d_ext, var=>this%geo_u%lat)
                allocate(this%geo_u%lat(1:g%ime-g%ims+1, 1:g%jme-g%jms+1))
                call subset_array(temp_offset, this%geo_u%lat, g)
            end associate

        endif

        ! Read the v-grid longitude data if specified, other wise interpolate from mass grid
        if (options%parameters%vlon_hi /= "") then
            call load_data(options%parameters%init_conditions_file,   &
                           options%parameters%vlon_hi,                &
                           temporary_data, this%v_grid)

            call subset_array(temporary_data, this%v_longitude%data_2d, this%v_grid)
            associate(g=>this%v_grid2d_ext, var=>this%geo_v%lon)
                allocate(this%geo_v%lon(1:g%ime-g%ims+1, 1:g%jme-g%jms+1))
                call subset_array(temporary_data, this%geo_v%lon, g)
            end associate
        else
            ! load the mass grid data again to get the full grid
            call load_data(options%parameters%init_conditions_file,   &
                           options%parameters%lon_hi,                 &
                           temporary_data, this%grid)

            call array_offset_y(temporary_data, temp_offset)
            call subset_array(temp_offset, this%v_longitude%data_2d, this%v_grid)
            associate(g=>this%v_grid2d_ext, var=>this%geo_v%lon)
                allocate(this%geo_v%lon(1:g%ime-g%ims+1, 1:g%jme-g%jms+1))
                call subset_array(temp_offset, this%geo_v%lon, g)
            end associate
        endif

        ! Read the v-grid latitude data if specified, other wise interpolate from mass grid
        if (options%parameters%vlat_hi /= "") then
            call load_data(options%parameters%init_conditions_file,   &
                           options%parameters%vlat_hi,                &
                           temporary_data, this%v_grid)

            call subset_array(temporary_data, this%v_latitude%data_2d, this%v_grid)
            associate(g=>this%v_grid2d_ext, var=>this%geo_v%lat)
                allocate(this%geo_v%lat(1:g%ime-g%ims+1, 1:g%jme-g%jms+1))
                call subset_array(temporary_data, this%geo_v%lat, g)
            end associate

        else
            ! load the mass grid data again to get the full grid
            call load_data(options%parameters%init_conditions_file,   &
                           options%parameters%lat_hi,                 &
                           temporary_data, this%grid)

            call array_offset_y(temporary_data, temp_offset)
            call subset_array(temp_offset, this%v_latitude%data_2d, this%v_grid)
            associate(g=>this%v_grid2d_ext, var=>this%geo_v%lat)
                allocate(this%geo_v%lat(1:g%ime-g%ims+1, 1:g%jme-g%jms+1))
                call subset_array(temp_offset, this%geo_v%lat, g)
            end associate
        endif

        if (this_image()==1) write(*,*) "  Finished reading core domain variables"

    end subroutine


    !> ---------------------------------
    !! Subset one array to the memory bounds defined by the grid
    !!
    !! If the input grid does not cover the entire subset, values
    !! are extrapolated outside of that subset region
    !!
    !! ---------------------------------
    subroutine extend_array(input, output, grid, extrapolate)
        implicit none
        real,         intent(in)    :: input(:,:,:)
        real,         intent(inout) :: output(:,:,:)
        type(grid_t), intent(in)    :: grid
        logical,      intent(in),   optional :: extrapolate

        ! loop counter
        integer :: i

        ! input array dimensions
        integer :: nx, ny
        ! output array dimensions
        integer :: nxo, nyo

        ! these will hold the actual indexes into the two arrays
        integer :: xs_in, xs_out, ys_in, ys_out
        integer :: xe_in, xe_out, ye_in, ye_out
        integer :: i_delta, j_delta ! offsets in x and y to store in case ims or jms is < 1

        logical :: do_extrapolate

        do_extrapolate = .True.
        if (present(extrapolate)) do_extrapolate = extrapolate

        ! Ideally, and most of the time, this is all it is doing
        ! output = input(grid%ims:grid%ime, grid%jms:grid%jme)
        ! However, it is possible that input does not cover the requested memory bounds of this data
        ! so we have to test.  If outside of bounds, extrapolate out from the boundary

        nx = size(input,1)
        ny = size(input,3)

        nxo = size(output,1)
        nyo = size(output,3)

        i_delta = (nxo - nx) / 2
        j_delta = (nyo - ny) / 2

        xs_in = 1
        xe_in = nx
        ys_in = 1
        ye_in = ny

        if (i_delta > 0) then
            xs_out = i_delta
            xe_out = nx + i_delta - 1
        endif

        if (j_delta > 0) then
            ys_out = j_delta
            ye_out = ny + j_delta - 1
        endif

        ! if (grid%ims < 1) then
        !     xs_out = 1 - grid%ims + 1
        !     xs_in  = 1
        !     i_delta = 1 - grid%ims
        ! else
        !     xs_out = 1
        !     xs_in  = grid%ims
        ! endif
        !
        ! if (grid%ime > nx) then
        !     xe_out = nx + i_delta
        !     xe_in  = nx
        ! else
        !     xe_out = grid%ime + i_delta
        !     xe_in  = grid%ime
        ! endif
        !
        ! if (grid%jms < 1) then
        !     ys_out = 1 - grid%jms + 1
        !     ys_in  = 1
        !     j_delta = 1 - grid%jms
        ! else
        !     ys_out = 1
        !     ys_in  = grid%jms
        ! endif
        !
        ! if (grid%jme > ny) then
        !     ye_out = ny + j_delta
        !     ye_in  = ny
        ! else
        !     ye_out = grid%jme + j_delta
        !     ye_in  = grid%jme
        ! endif
        !
        !----------------------------------------------------
        ! This is the area of overlap
        ! Note that this is the main and likely only assignment
        !----------------------------------------------------
        output(xs_out:xe_out, :, ys_out:ye_out) = input(xs_in:xe_in, :, ys_in:ye_in)

        ! outside of that overlap, extrapolate out from the boundary
        ! this should only be necessary for border images
        if (grid%ims < 1) then
            do i=1,xs_out-1
                if (do_extrapolate) then
                    output(i,:,:) = output(xs_out,:,:) + (output(xs_out,:,:) - output(xs_out+1,:,:)) * (xs_out - i)
                else
                    output(i,:,:) = output(xs_out,:,:)
                endif
            enddo
        endif

        if (grid%ime > nx) then
            do i=xe_out+1,nxo
                if (do_extrapolate) then
                    output(i,:,:) = output(xe_out,:,:) + (output(xe_out,:,:) - output(xe_out-1,:,:)) * (i - xe_out)
                else
                    output(i,:,:) = output(xe_out,:,:)
                endif
            enddo
        endif

        if (grid%jms < 1) then
            do i=1,ys_out-1
                if (do_extrapolate) then
                    output(:,:,i) = output(:,:,ys_out) + (output(:,:,ys_out) - output(:,:,ys_out+1)) * (ys_out - i)
                else
                    output(:,:,i) = output(:,:,ys_out)
                endif
            enddo
        endif

        if (grid%jme > ny) then
            do i=ye_out+1,nyo
                if (do_extrapolate) then
                    output(:,:,i) = output(:,:,ye_out) + (output(:,:,ye_out) - output(:,:,ye_out-1)) * (i - ye_out)
                else
                    output(:,:,i) = output(:,:,ye_out)
                endif
            enddo
        endif

    end subroutine extend_array


    !> ---------------------------------
    !! Subset one array to the memory bounds defined by the grid
    !!
    !! If the input grid does not cover the entire subset, values
    !! are extrapolated outside of that subset region
    !!
    !! ---------------------------------
    subroutine subset_array(input, output, grid, extrapolate)
        implicit none
        real,           intent(in)    :: input(:,:)
        real,           intent(inout) :: output(:,:)
        type(grid_t),   intent(in)    :: grid
        logical,        intent(in),   optional :: extrapolate

        ! loop counter
        integer :: i

        ! input array dimensions
        integer :: nx, ny
        ! output array dimensions
        integer :: nxo, nyo

        ! these will hold the actual indexes into the two arrays
        integer :: xs_in, xs_out, ys_in, ys_out
        integer :: xe_in, xe_out, ye_in, ye_out

        logical :: do_extrapolate

        do_extrapolate = .True.
        if (present(extrapolate)) do_extrapolate = extrapolate

        ! Ideally, and most of the time, this is all it is doing
        ! output = input(grid%ims:grid%ime, grid%jms:grid%jme)
        ! However, it is possible that input does not cover the requested memory bounds of this data
        ! so we have to test.  If outside of bounds, extrapolate out from the boundary

        nx = size(input,1)
        ny = size(input,2)

        nxo = size(output,1)
        nyo = size(output,2)

        xs_in=grid%ims; xs_out=1
        ys_in=grid%jms; ys_out=1
        xe_in=grid%ime; xe_out=nxo
        ye_in=grid%jme; ye_out=nyo

        ! if (grid%ims < 1) then
        !     xs_out = 1 - grid%ims + 1
        !     xs_in  = 1
        ! else
        !     xs_out = 1
        !     xs_in  = grid%ims
        ! endif
        !
        ! if (grid%ime > nx) then
        !     xe_out = nxo - (grid%ime - nx)
        !     xe_in  = nx
        ! else
        !     xe_out = nxo
        !     xe_in  = grid%ime
        ! endif
        !
        ! if (grid%jms < 1) then
        !     ys_out = 1 - grid%jms + 1
        !     ys_in  = 1
        ! else
        !     ys_out = 1
        !     ys_in  = grid%jms
        ! endif
        !
        ! if (grid%jme > ny) then
        !     ye_out = nyo - (grid%jme - ny)
        !     ye_in  = ny
        ! else
        !     ye_out = nyo
        !     ye_in  = grid%jme
        ! endif
        !
        !----------------------------------------------------
        ! This is the area of overlap
        ! Note that this is the main and likely only assignment
        !----------------------------------------------------

        output(xs_out:xe_out, ys_out:ye_out) = input(xs_in:xe_in, ys_in:ye_in)

        ! outside of that overlap, extrapolate out from the boundary
        ! this should only be necessary for border images
        if (grid%ims < 1) then
            do i=1,xs_out-1
                if (do_extrapolate) then
                    output(i,:) = output(xs_out,:) + (output(xs_out,:) - output(xs_out+1,:)) * (xs_out - i)
                else
                    output(i,:) = output(xs_out,:)
                endif
            enddo
        endif

        if (grid%ime > nx) then
            do i=xe_out+1,nxo
                if (do_extrapolate) then
                    output(i,:) = output(xe_out,:) + (output(xe_out,:) - output(xe_out-1,:)) * (i - xe_out)
                else
                    output(i,:) = output(xe_out,:)
                endif
            enddo
        endif

        if (grid%jms < 1) then
            do i=1,ys_out-1
                if (do_extrapolate) then
                    output(:,i) = output(:,ys_out) + (output(:,ys_out) - output(:,ys_out+1)) * (ys_out - i)
                else
                    output(:,i) = output(:,ys_out)
                endif
            enddo
        endif

        if (grid%jme > ny) then
            do i=ye_out+1,nyo
                if (do_extrapolate) then
                    output(:,i) = output(:,ye_out) + (output(:,ye_out) - output(:,ye_out-1)) * (i - ye_out)
                else
                    output(:,i) = output(:,ye_out)
                endif
            enddo
        endif

    end subroutine subset_array

    !> -------------------------------
    !! Setup a single Geographic structure given a latitude, longitude, and z array
    !!
    !! -------------------------------
    subroutine setup_geo(geo, latitude, longitude, z)
        implicit none
        class(interpolable_type), intent(inout) :: geo
        real,                     intent(in)    :: latitude(:,:)
        real,                     intent(in)    :: longitude(:,:)
        real,                     intent(in)    :: z(:,:,:)

        if (allocated(geo%lat)) deallocate(geo%lat)
        allocate( geo%lat, source=latitude)

        if (allocated(geo%lon)) deallocate(geo%lon)
        allocate( geo%lon, source=longitude)

        if (allocated(geo%z)) deallocate(geo%z)
        allocate( geo%z, source=z)

        ! This makes 2D variables out of lat/lon if they come in as 1D variables
        ! This also puts the longitudes onto a 0-360 if they are -180-180 (important for Alaska)
        ! Though if working in Europe the -180-180 grid is better ideally the optimal value should be checked.
        ! and good luck if you want to work over the poles...
        call standardize_coordinates(geo)

    end subroutine


    !> -------------------------------
    !! Initialize various domain variables, mostly z, dz, etc.
    !!
    !! -------------------------------
    subroutine initialize_variables(this, options)
        implicit none
        class(domain_t), intent(inout)  :: this
        class(options_t),intent(in)     :: options

        integer :: i

        call read_core_variables(this, options)

        associate(z                     => this%z%data_3d,                      &
                  z_u                   => this%geo_u%z,                        &
                  z_v                   => this%geo_v%z,                        &
                  z_interface           => this%z_interface%data_3d,            &
                  dz                    => options%parameters%dz_levels,        &
                  dz_mass               => this%dz_mass%data_3d,                &
                  dz_interface          => this%dz_interface%data_3d,           &
                  exner                 => this%exner%data_3d,                  &
                  pressure              => this%pressure%data_3d,               &
                  temperature           => this%temperature%data_3d,            &
                  potential_temperature => this%potential_temperature%data_3d,  &
                  terrain               => this%terrain%data_2d)

            exner = exner_function(pressure)

            i = this%grid%kms
            dz_mass(:,i,:)      = dz(i) / 2
            dz_interface(:,i,:) = dz(i)
            z(:,i,:)            = terrain + dz_mass(:,i,:)
            z_interface(:,i,:)  = terrain

            ! for the u and v grids, z(1) was already initialized with terrain.
            ! but the first level needs to be offset, and the rest of the levels need to be created
            z_u(:,i,:)          = z_u(:,i,:) + dz(i) / 2
            z_v(:,i,:)          = z_v(:,i,:) + dz(i) / 2

            do i = this%grid%kms+1, this%grid%kme
                dz_mass(:,i,:)     = (dz(i) + dz(i-1)) / 2
                dz_interface(:,i,:)= dz(i)
                z(:,i,:)           = z(:,i-1,:)           + dz_mass(:,i,:)
                z_interface(:,i,:) = z_interface(:,i-1,:) + dz_interface(:,i,:)

                z_u(:,i,:)         = z_u(:,i-1,:)         + ((dz(i) + dz(i-1)) / 2)
                z_v(:,i,:)         = z_v(:,i-1,:)         + ((dz(i) + dz(i-1)) / 2)

            enddo

        end associate

        call setup_geo(this%geo,   this%latitude%data_2d,   this%longitude%data_2d,   this%z%data_3d)

    end subroutine initialize_variables

    !> -------------------------------
    !! Populare the metadata structure in the domain for later output
    !!
    !! -------------------------------
    subroutine setup_meta_data(this)
        implicit none
        class(domain_t), intent(inout) :: this

        call this%info%add_attribute("ids",str(this%grid%ids))
        call this%info%add_attribute("ide",str(this%grid%ide))
        call this%info%add_attribute("jds",str(this%grid%jds))
        call this%info%add_attribute("jde",str(this%grid%jde))
        call this%info%add_attribute("kds",str(this%grid%kds))
        call this%info%add_attribute("kde",str(this%grid%kde))

        call this%info%add_attribute("ims",str(this%grid%ims))
        call this%info%add_attribute("ime",str(this%grid%ime))
        call this%info%add_attribute("jms",str(this%grid%jms))
        call this%info%add_attribute("jme",str(this%grid%jme))
        call this%info%add_attribute("kms",str(this%grid%kms))
        call this%info%add_attribute("kme",str(this%grid%kme))

        call this%info%add_attribute("its",str(this%grid%its))
        call this%info%add_attribute("ite",str(this%grid%ite))
        call this%info%add_attribute("jts",str(this%grid%jts))
        call this%info%add_attribute("jte",str(this%grid%jte))
        call this%info%add_attribute("kts",str(this%grid%kts))
        call this%info%add_attribute("kte",str(this%grid%kte))


    end subroutine setup_meta_data


    !> -------------------------------
    !! Add variables needed by all domains to the list of requested variables
    !!
    !! -------------------------------
    module subroutine var_request(this, options)
        class(domain_t), intent(inout) :: this
        class(options_t),intent(inout) :: options

        ! List the variables that are required to be allocated for any domain
        call options%alloc_vars(                                                    &
                     [kVARS%z,                      kVARS%z_interface,              &
                      kVARS%dz,                     kVARS%dz_interface,             &
                      kVARS%terrain,                kVARS%pressure,                 &
                      kVARS%exner,                  kVARS%potential_temperature,    &
                      kVARS%latitude,               kVARS%longitude,                &
                      kVARS%u_latitude,             kVARS%u_longitude,              &
                      kVARS%v_latitude,             kVARS%v_longitude               ])

        ! List the variables that are required for any restart
        call options%restart_vars(                                                  &
                     [kVARS%z,                                                      &
                      kVARS%terrain,                kVARS%potential_temperature,    &
                      kVARS%latitude,               kVARS%longitude,                &
                      kVARS%u_latitude,             kVARS%u_longitude,              &
                      kVARS%v_latitude,             kVARS%v_longitude               ])

        call options%advect_vars([kVARS%potential_temperature])

    end subroutine var_request

    !> -------------------------------
    !! Read in the shape of the domain required and setup the grid objects
    !!
    !! -------------------------------
    subroutine read_domain_shape(this, options)
        implicit none
        class(domain_t), intent(inout)  :: this
        class(options_t),intent(in)     :: options

        real, allocatable :: temporary_data(:,:)
        integer :: nx_global, ny_global, nz_global, nsmooth

        nsmooth = nint(options%parameters%smooth_wind_distance / options%parameters%dx)
        this%nsmooth = nsmooth

        ! This doesn't need to read in this variable, it could just request the dimensions
        ! but this is not a performance sensitive part of the code (for now)
        call io_read(options%parameters%init_conditions_file,   &
                     options%parameters%hgt_hi,                 &
                     temporary_data)

        nx_global = size(temporary_data,1)
        ny_global = size(temporary_data,2)
        nz_global = options%parameters%nz

        call this%grid%set_grid_dimensions(         nx_global, ny_global, nz_global)
        call this%u_grid%set_grid_dimensions(       nx_global, ny_global, nz_global, nx_extra = 1)
        call this%v_grid%set_grid_dimensions(       nx_global, ny_global, nz_global, ny_extra = 1)

        ! for 2D mass variables
        call this%grid2d%set_grid_dimensions(       nx_global, ny_global, 0)

        ! setup a 2D lat/lon grid extended by nsmooth grid cells so that smoothing can take place "across" images
        ! This just sets up the fields to interpolate u and v to so that the input data are handled on an extended
        ! grid.  They are then subset to the u_grid and v_grids above before actual use.
        call this%u_grid2d%set_grid_dimensions(     nx_global, ny_global, 0, nx_extra = 1)
        call this%u_grid2d_ext%set_grid_dimensions( nx_global, ny_global, 0, nx_extra = 1)
        ! extend by nsmooth, but bound to the domain grid
        this%u_grid2d_ext%ims = max(this%u_grid2d%ims - nsmooth, this%u_grid2d%ids)
        this%u_grid2d_ext%ime = min(this%u_grid2d%ime + nsmooth, this%u_grid2d%ide)
        this%u_grid2d_ext%jms = max(this%u_grid2d%jms - nsmooth, this%u_grid2d%jds)
        this%u_grid2d_ext%jme = min(this%u_grid2d%jme + nsmooth, this%u_grid2d%jde)

        ! handle the v-grid too
        call this%v_grid2d%set_grid_dimensions(     nx_global, ny_global, 0, ny_extra = 1)
        call this%v_grid2d_ext%set_grid_dimensions( nx_global, ny_global, 0, ny_extra = 1)
        ! extend by nsmooth, but bound to the domain grid
        this%v_grid2d_ext%ims = max(this%v_grid2d%ims - nsmooth, this%v_grid2d%ids)
        this%v_grid2d_ext%ime = min(this%v_grid2d%ime + nsmooth, this%v_grid2d%ide)
        this%v_grid2d_ext%jms = max(this%v_grid2d%jms - nsmooth, this%v_grid2d%jds)
        this%v_grid2d_ext%jme = min(this%v_grid2d%jme + nsmooth, this%v_grid2d%jde)


        call this%grid_soil%set_grid_dimensions(    nx_global, ny_global, 4)
        call this%grid_monthly%set_grid_dimensions( nx_global, ny_global, 12)

        deallocate(temporary_data)

    end subroutine

    !> -------------------------------
    !! Check that a set of variables is within realistic bounds (i.e. >0)
    !!
    !! Need to add more variables to the list
    !!
    !! -------------------------------
    module subroutine enforce_limits(this)
      class(domain_t), intent(inout) :: this
      if (associated(this%water_vapor%data_3d)           ) where(this%water_vapor%data_3d < 0)             this%water_vapor%data_3d = 0
      if (associated(this%potential_temperature%data_3d) ) where(this%potential_temperature%data_3d < 0)   this%potential_temperature%data_3d = 0
      if (associated(this%cloud_water_mass%data_3d)      ) where(this%cloud_water_mass%data_3d < 0)        this%cloud_water_mass%data_3d = 0
      if (associated(this%cloud_ice_mass%data_3d)        ) where(this%cloud_ice_mass%data_3d < 0)          this%cloud_ice_mass%data_3d = 0
      if (associated(this%cloud_ice_number%data_3d)      ) where(this%cloud_ice_number%data_3d < 0)        this%cloud_ice_number%data_3d = 0
      if (associated(this%rain_mass%data_3d)             ) where(this%rain_mass%data_3d < 0)               this%rain_mass%data_3d = 0
      if (associated(this%rain_number%data_3d)           ) where(this%rain_number%data_3d < 0)             this%rain_number%data_3d = 0
      if (associated(this%snow_mass%data_3d)             ) where(this%snow_mass%data_3d < 0)               this%snow_mass%data_3d = 0
      if (associated(this%graupel_mass%data_3d)          ) where(this%graupel_mass%data_3d < 0)            this%graupel_mass%data_3d = 0

    end subroutine


    !> -------------------------------
    !! Setup the Geographic look up tables for interpolating a given forcing data set to each of the grids
    !!
    !! -------------------------------
    subroutine setup_geo_interpolation(this, forcing)
        implicit none
        class(domain_t),  intent(inout) :: this
        class(boundary_t),intent(inout) :: forcing

        integer :: nx, ny, nz

        ! this%geo and forcing%geo have to be of class interpolable
        ! which means they must contain lat, lon, z, geolut, and vLUT components
        call geo_LUT(this%geo,   forcing%geo)
        call geo_LUT(this%geo_u, forcing%geo_u)
        call geo_LUT(this%geo_v, forcing%geo_v)

        nx = size(this%geo%z, 1)
        nz = size(forcing%z,  2)
        ny = size(this%geo%z, 3)

        allocate(forcing%geo%z(nx, nz, ny))
        call geo_interp(forcing%geo%z, forcing%z, forcing%geo%geolut)
        call vLUT(this%geo,   forcing%geo)

        allocate(forcing%geo_u%z, mold=this%geo_u%z)
        call geo_interp(forcing%geo_u%z, forcing%z, forcing%geo_u%geolut)
        call vLUT(this%geo_u, forcing%geo_u)

        allocate(forcing%geo_v%z, mold=this%geo_v%z)
        call geo_interp(forcing%geo_v%z, forcing%z, forcing%geo_v%geolut)
        call vLUT(this%geo_v, forcing%geo_v)

    end subroutine


    !> -------------------------------
    !! Update the dQdt fields for all forced variables
    !!
    !! This routine is the partner of apply_forcing below.
    !! update_delta_fields normalizes the difference by the time step of that difference field
    !! apply_forcing multiplies that /second value and multiplies it by the current time step before adding it
    !!
    !! -------------------------------
    module subroutine update_delta_fields(this, dt)
        implicit none
        class(domain_t),    intent(inout) :: this
        class(time_delta_t),intent(in)    :: dt

        ! temporary to hold the variable to be interpolated to
        type(variable_t) :: var_to_update

        ! make sure the dictionary is reset to point to the first variable
        call this%variables_to_force%reset_iterator()

        ! No iterate through the dictionary as long as there are more elements present
        do while (this%variables_to_force%has_more_elements())
            ! get the next variable
            var_to_update = this%variables_to_force%next()

            if (var_to_update%two_d) then
                var_to_update%dqdt_2d = (var_to_update%dqdt_2d - var_to_update%data_2d) / dt%seconds()

            else if (var_to_update%three_d) then
                var_to_update%dqdt_3d = (var_to_update%dqdt_3d - var_to_update%data_3d) / dt%seconds()
            endif

        enddo

        ! w has to be handled separately because it is the only variable that can be updated using the delta fields but is not
        ! actually read from disk. Note that if we move to balancing winds every timestep, then it doesn't matter.
        var_to_update = this%w%meta_data
        var_to_update%dqdt_3d = (var_to_update%dqdt_3d - var_to_update%data_3d) / dt%seconds()


    end subroutine


    !> -------------------------------
    !! Add the forcing update to boundaries and internal diagnosed fields
    !!
    !! This routine is the partner of update_delta_fields above.
    !! update_delta_fields normalizes the difference by the time step of that difference field
    !! apply forcing multiplies that /second value and multiplies it by the current time step before adding it
    !!
    !! -------------------------------
    module subroutine apply_forcing(this, dt)
        implicit none
        class(domain_t),    intent(inout) :: this
        class(time_delta_t),intent(in)    :: dt
        integer :: ims, ime, jms, jme

        ! temporary to hold the variable to be interpolated to
        type(variable_t) :: var_to_update

        ! make sure the dictionary is reset to point to the first variable
        call this%variables_to_force%reset_iterator()

        ! No iterate through the dictionary as long as there are more elements present
        do while (this%variables_to_force%has_more_elements())
            ! get the next variable
            var_to_update = this%variables_to_force%next()

            if (var_to_update%two_d) then
                ! apply forcing throughout the domain for 2D diagnosed variables (e.g. SST, SW)
                var_to_update%data_2d = var_to_update%data_2d + (var_to_update%dqdt_2d * dt%seconds())

            else if (var_to_update%three_d) then
                ! only apply forcing data on the boundaries for advected scalars (e.g. temperature, humidity)
                if (var_to_update%force_boundaries) then
                    ims = lbound(var_to_update%data_3d, 1)
                    ime = ubound(var_to_update%data_3d, 1)
                    jms = lbound(var_to_update%data_3d, 3)
                    jme = ubound(var_to_update%data_3d, 3)
                    var_to_update%data_3d(ims,:,:) = var_to_update%data_3d(ims,:,:) + (var_to_update%dqdt_3d(ims,:,:) * dt%seconds())
                    var_to_update%data_3d(ime,:,:) = var_to_update%data_3d(ime,:,:) + (var_to_update%dqdt_3d(ime,:,:) * dt%seconds())
                    var_to_update%data_3d(:,:,jms) = var_to_update%data_3d(:,:,jms) + (var_to_update%dqdt_3d(:,:,jms) * dt%seconds())
                    var_to_update%data_3d(:,:,jme) = var_to_update%data_3d(:,:,jme) + (var_to_update%dqdt_3d(:,:,jme) * dt%seconds())

                ! apply forcing throughout the domain for diagnosed variables (e.g. pressure, wind)
                else
                    var_to_update%data_3d = var_to_update%data_3d + (var_to_update%dqdt_3d * dt%seconds())
                endif
            endif

        enddo

        ! w has to be handled separately because it is the only variable that can be updated using the delta fields but is not
        ! actually read from disk. Note that if we move to balancing winds every timestep, then it doesn't matter.
        var_to_update = this%w%meta_data
        var_to_update%data_3d = var_to_update%data_3d + (var_to_update%dqdt_3d * dt%seconds())

    end subroutine


    !> -------------------------------
    !! Loop through all variables for which forcing data have been supplied and interpolate the forcing data to the domain
    !!
    !! -------------------------------
    module subroutine interpolate_forcing(this, forcing, update)
        implicit none
        class(domain_t),  intent(inout) :: this
        class(boundary_t),intent(in)    :: forcing
        logical,          intent(in),   optional :: update

        ! internal field always present for value of optional "update"
        logical :: update_only
        logical :: var_is_not_pressure
        ! temporary to hold the variable to be interpolated to
        type(variable_t) :: var_to_interpolate
        ! temporary to hold the forcing variable to be interpolated from
        type(variable_t) :: input_data
        ! number of layers has to be used when subsetting for update_pressure (for now)
        integer :: nz
        logical :: var_is_u, var_is_v

        update_only = .False.
        if (present(update)) update_only = update

        ! make sure the dictionary is reset to point to the first variable
        call this%variables_to_force%reset_iterator()

        ! No iterate through the dictionary as long as there are more elements present
        do while (this%variables_to_force%has_more_elements())
            ! get the next variable
            var_to_interpolate = this%variables_to_force%next()

            ! get the associated forcing data
            input_data = forcing%variables%get_var(var_to_interpolate%forcing_var)

            ! interpolate
            if (var_to_interpolate%two_d) then
                if (update_only) then
                    call geo_interp2d(var_to_interpolate%dqdt_2d, input_data%data_2d, forcing%geo%geolut)
                else
                    call geo_interp2d(var_to_interpolate%data_2d, input_data%data_2d, forcing%geo%geolut)
                endif

            else
                ! if this is the pressure variable, then don't perform vertical interpolation, adjust the pressure directly
                var_is_not_pressure = (trim(var_to_interpolate%forcing_var) /= trim(this%pressure%forcing_var))

                var_is_u = (trim(var_to_interpolate%forcing_var) == trim(this%u%meta_data%forcing_var))
                var_is_v = (trim(var_to_interpolate%forcing_var) == trim(this%v%meta_data%forcing_var))

                ! if just updating, use the dqdt variable otherwise use the 3D variable
                if (update_only) then

                    call interpolate_variable(var_to_interpolate%dqdt_3d, input_data, forcing, this, &
                                    vert_interp=var_is_not_pressure, var_is_u=var_is_u, var_is_v=var_is_v, nsmooth=this%nsmooth)
                    if (.not.var_is_not_pressure) then
                        nz = size(this%geo%z, 2)
                        call update_pressure(var_to_interpolate%dqdt_3d, forcing%geo%z(:,:nz,:), this%geo%z)
                    endif

                else
                    call interpolate_variable(var_to_interpolate%data_3d, input_data, forcing, this, &
                                    vert_interp=var_is_not_pressure, var_is_u=var_is_u, var_is_v=var_is_v, nsmooth=this%nsmooth)
                    if (.not.var_is_not_pressure) then
                        nz = size(this%geo%z, 2)
                        call update_pressure(var_to_interpolate%data_3d, forcing%geo%z(:,:nz,:), this%geo%z)
                    endif
                endif

            endif
        enddo

    end subroutine


    !> -------------------------------
    !! Interpolate one variable by requesting the forcing data from the boundary data structure then
    !! calling the appropriate interpolation routine (2D vs 3D) with the appropriate grid (mass, u, v)
    !!
    !! -------------------------------
    subroutine interpolate_variable(var_data, input_data, forcing, dom, vert_interp, var_is_u, var_is_v, nsmooth)
        implicit none
        real,               intent(inout) :: var_data(:,:,:)
        class(variable_t),  intent(in)    :: input_data
        class(boundary_t),  intent(in)    :: forcing
        class(domain_t),    intent(in)    :: dom
        logical,            intent(in),   optional :: vert_interp
        logical,            intent(in),   optional :: var_is_u, var_is_v
        integer,            intent(in),   optional :: nsmooth

        ! note that 3D variables have a different number of vertical levels, so they have to first be interpolated
        ! to the high res horizontal grid, then vertically interpolated to the actual icar domain
        real, allocatable :: temp_3d(:,:,:), pre_smooth(:,:,:)
        logical :: interpolate_vertically, uvar, vvar
        integer :: nx, ny, nz, windowsize

        interpolate_vertically=.True.
        if (present(vert_interp)) interpolate_vertically = vert_interp
        uvar = .False.
        if (present(var_is_u)) uvar = var_is_u
        vvar = .False.
        if (present(var_is_v)) vvar = var_is_v
        windowsize = 0
        if (present(nsmooth)) windowsize = nsmooth


        ! Sequence of if statements to test if this variable needs to be interpolated onto the staggared grids
        ! This could all be combined by passing in the geo data to use, along with a smoothing flag.

        ! Interpolate to the Mass grid
        if ((size(var_data,1) == size(forcing%geo%geolut%x,2)).and.(size(var_data,3) == size(forcing%geo%geolut%x,3))) then
            ! allocate a temporary variable to hold the horizontally interpolated data before vertical interpolation
            allocate(temp_3d(size(var_data,1), size(input_data%data_3d,2), size(var_data,3) ))

            call geo_interp(temp_3d, input_data%data_3d, forcing%geo%geolut)

            ! note that pressure (and possibly other variables?) should not be interpolated, it will be adjusted later
            ! really, it whould be interpolated, and the bottom layers (below the forcing model) should be adjusted separately...
            if (interpolate_vertically) then
                call vinterp(var_data, temp_3d, forcing%geo%vert_lut)
            else
                nz = size(var_data,2)
                var_data = temp_3d(:,:nz,:)
            endif

        ! Interpolate to the u staggered grid
        else if (uvar) then

            ! use the alternate allocate below to vertically interpolate to this first, then smooth, then subset to the actual variable
            allocate(temp_3d(size(forcing%geo_u%geolut%x,2), size(var_data,2), size(forcing%geo_u%geolut%x,3)))
            allocate(pre_smooth(size(forcing%geo_u%geolut%x,2), size(input_data%data_3d,2), size(forcing%geo_u%geolut%x,3) ))

            nx = size(forcing%geo_u%geolut%x,2)
            ny = size(forcing%geo_u%geolut%x,3)

            call geo_interp(pre_smooth, input_data%data_3d, forcing%geo_u%geolut)
            call vinterp(temp_3d, pre_smooth, forcing%geo_u%vert_lut)
            call smooth_array(temp_3d, windowsize=windowsize, ydim=3)

            var_data = temp_3d(dom%u_grid%ims-dom%u_grid2d_ext%ims+1 : dom%u_grid%ime-dom%u_grid2d_ext%ims+1,    &
                                :,   &
                               dom%u_grid%jms-dom%u_grid2d_ext%jms+1 : dom%u_grid%jme-dom%u_grid2d_ext%jms+1)

        ! Interpolate to the v staggered grid
        else if (vvar) then

            ! use the alternate allocate below to vertically interpolate to this first, then smooth, then subset to the actual variable
            allocate(temp_3d(size(forcing%geo_v%geolut%x,2), size(var_data,2), size(forcing%geo_v%geolut%x,3)))
            allocate(pre_smooth(size(forcing%geo_v%geolut%x,2), size(input_data%data_3d,2), size(forcing%geo_v%geolut%x,3) ))

            windowsize = (size(forcing%geo_v%geolut%x,2) - size(var_data,1)) / 2
            nx = size(forcing%geo_v%geolut%x,2)
            ny = size(forcing%geo_v%geolut%x,3)

            call geo_interp(pre_smooth, input_data%data_3d, forcing%geo_v%geolut)
            call vinterp(temp_3d, pre_smooth, forcing%geo_v%vert_lut)
            call smooth_array(temp_3d, windowsize=windowsize, ydim=3)

            var_data = temp_3d(dom%v_grid%ims-dom%u_grid2d_ext%ims+1 : dom%v_grid%ime-dom%u_grid2d_ext%ims+1,    &
                                :,   &
                               dom%v_grid%jms-dom%u_grid2d_ext%jms+1 : dom%v_grid%jme-dom%u_grid2d_ext%jms+1)
        endif

    end subroutine

    !> -------------------------------
    !! Used to interpolate an exchangeable type, just gets the meta_data structure from it and uses interpolate_variable
    !!
    !! This is not used presently since the meta_data structures are added directly to the variables_to_force dictionary
    !!
    !! -------------------------------
    ! subroutine interpolate_exchangeable(var, forcing)
    !     implicit none
    !     class(exchangeable_t), intent(inout) :: var
    !     class(boundary_t),     intent(in)    :: forcing
    !
    !     type(variable_t) :: input_data
    !
    !     input_data = forcing%variables%get_var(var%meta_data%forcing_var)
    !     ! exchangeables all have a meta_data variable_t component with a pointer to the 3D local data
    !     ! call interpolate_variable(var%meta_data%data_3d, input_data, forcing)
    !
    ! end subroutine


end submodule
