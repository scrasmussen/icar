module module_cu_parcel
  use variable_interface,     only : variable_t
  use options_types,          only : parcel_options_type
  use parcel_interface,       only : exchangeable_parcel, create_empty_parcel
  use parcel_type_interface,  only : parcel_t
  use exchangeable_interface, only : exchangeable_t
  use grid_interface,         only : grid_t
  use iso_fortran_env,        only : output_unit
  implicit none
  public :: cu_parcel, cu_parcel_physics, cu_parcels_init
  private
  interface cu_parcel
      module procedure cu_parcel_init, cu_parcel_physics
  end interface cu_parcel

  logical  :: debug = .false.
  type(parcel_options_type) :: parcel_options
  logical, parameter :: brunt_vaisala_data = .false.
  logical, parameter :: replacement = .false. ! parcel replacement
  logical, parameter :: replacement_message = .true.
  logical, parameter :: init_theta = .true.
  integer            :: local_buf_size

contains

  ! Initialize parcels' physics
  subroutine cu_parcels_init(parcels, grid, &
      z_interface, z_m, potential_temp, pressure, &
      u_in, v_in, w_in, dz_val, &
      water_vapor, cloud_water_mass, &
      temp_env, parcel_options_in, &
      input_buf_size, halo_width)
      class(exchangeable_parcel), intent(inout) :: parcels
      class(exchangeable_t), intent(in)    :: potential_temp
      class(exchangeable_t), intent(in)    :: u_in, v_in, w_in
      type(grid_t), intent(in)      :: grid
      type(variable_t), intent(in)  :: z_m
      type(variable_t), intent(in)  :: pressure
      type(variable_t), intent(in)  :: z_interface
      type(variable_t), intent(in)  :: dz_val
      type(exchangeable_t), intent(in)  :: water_vapor, cloud_water_mass
      type(variable_t), intent(in)  :: temp_env
      type(parcel_options_type), intent(in)  :: parcel_options_in
      integer, intent(in), optional :: input_buf_size
      integer, intent(in), optional :: halo_width

      integer :: me, create, seed(34)
      real :: random_start(3), x, z, y
      real :: z_meters, z_interface_val, theta_val
      real :: pressure_val, exner_val, temp_val, water_vapor_val
      real :: u_val, v_val, w_val
      integer :: x0, x1, z0, z1, y0, y1
      logical :: calc
      integer :: p_i

      me = this_image()
      parcel_options = parcel_options_in

      do p_i=1,parcels%image_parcel_count
          ! do p_i=1,parcels%image_num_parcels() ! FUND NOT WORKING
          if (debug .eqv. .true.) &
              print *, "===========i  =", p_i
          if (parcels%local(p_i)%exists .eqv. .TRUE.) then
              call init_parcel_physics(parcels%local(p_i), pressure, &
                  potential_temp, u_in, v_in, w_in, &
                  z_m, z_interface, water_vapor, cloud_water_mass, &
                  temp_env)
          end if
          ! print *, "PARCEL", parcels%local(p_i)
      end do
  end subroutine cu_parcels_init


  subroutine cu_parcel_init(parcel, grid, &
      z_interface, z_m, potential_temp, pressure, &
      u_in, v_in, w_in, dz_val, &
      water_vapor, cloud_water_mass, &
      temp_env, &
      input_buf_size, halo_width)
      class(parcel_t), intent(inout) :: parcel
      class(exchangeable_t), intent(in)    :: potential_temp
      class(exchangeable_t), intent(in)    :: u_in, v_in, w_in
      type(grid_t), intent(in)      :: grid
      type(variable_t), intent(in)  :: z_m
      type(variable_t), intent(in)  :: pressure
      type(variable_t), intent(in)  :: z_interface
      type(variable_t), intent(in)  :: dz_val
      type(exchangeable_t), intent(in)  :: water_vapor, cloud_water_mass
      type(variable_t), intent(in), optional  :: temp_env
      integer, intent(in), optional :: input_buf_size
      integer, intent(in), optional :: halo_width
      integer :: me, create, seed(34)
      real :: random_start(3), x, z, y
      real :: z_meters, z_interface_val, theta_val
      real :: pressure_val, exner_val, temp_val, water_vapor_val
      real :: u_val, v_val, w_val
      integer :: x0, x1, z0, z1, y0, y1
      logical :: calc
      integer :: p_i
      if (debug .eqv. .true.) &
          print *, me, ": CU_PARCEL_INIT"
      me = this_image()
      call init_parcel_physics(parcel, pressure, &
          potential_temp, u_in, v_in, w_in, &
          z_m, z_interface, water_vapor, cloud_water_mass)
  end subroutine cu_parcel_init


  ! Initialize a parcel's physics
  subroutine init_parcel_physics(parcel, pressure, potential_temp, u_in, &
      v_in, w_in, z_m, z_interface, &
      water_vapor, cloud_water_mass, &
      temp_env)
    type(parcel_t), intent(inout) :: parcel
    type(variable_t), intent(in) :: pressure
    class(exchangeable_t), intent(in) :: potential_temp
    class(exchangeable_t), intent(in) :: u_in, v_in, w_in
    type(variable_t), intent(in) :: z_m
    type(variable_t), intent(in) :: z_interface
    type(exchangeable_t), intent(in)  :: water_vapor, cloud_water_mass
    type(variable_t), intent(in), optional :: temp_env
    ! integer, intent(in), optional :: times_moved
    integer :: x0, z0, y0, x1, z1, y1
    real :: x, z, y, exner_val
    real :: parcel_rh
    real :: random_val

    ! REMOVE THIS, switch to parcel parameters?
    ! parcel%z = 3
    ! if (parcel%parcel_id .lt. 4)  parcel%z = 2

    ! default value if not passed by namelist
    parcel_rh = 0.99

    if (debug .eqv. .true.) &
        print *, "=== AIR_PARCEL_PHYSIC INIT for PARCEL", parcel%parcel_id
    x = parcel%x;
    z = parcel%z;
    y = parcel%y;


    x0 = floor(x); z0 = floor(z); y0 = floor(y);
    x1 = ceiling(x); z1 = ceiling(z); y1 = ceiling(y);

    associate (A => z_m%data_3d)
        parcel%z_meters = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate

    associate (A => potential_temp%data_3d)
        parcel%potential_temp = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate

    associate (A => pressure%data_3d)
        parcel%pressure = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate

    associate (A => z_interface%data_3d) ! TODO: look at this
        parcel%z_interface = bilinear_interpolation(x, x0, x1, y, y0, y1, &
            A(x0,1,y0), A(x0,1,y1), A(x1,1,y0), A(x1,1,y1))
    end associate
    ! associate (A => z_interface%data_3d(:,1,:)) ! TODO: look at this
    !     print *, "------------------------"
    !     print *, this_image(), ": x", x, "y", y
    !     print *, this_image(), ":", shape(A), "|L|", lbound(A), "|U|", ubound(A)
    !     print *, this_image(), ":", shape(z_interface%data_3d), "|L|", lbound(z_interface&
    !         &%data_3d), "|U|",ubound(z_interface%data_3d)
    !     sync all
    !     ! stop "ARTLESS 2"
    !     parcel%z_interface = bilinear_interpolation(x, x0, x1, y, y0, y1, &
    !         A(x0,y0), A(x0,y1), A(x1,y0), A(x1,y1))
    ! end associate

    exner_val = exner_function_local(parcel%pressure)
    parcel%temperature = exner_val * parcel%potential_temp
    parcel%temperature = parcel%temperature

    ! --- Add random value to potential temperature
    ! call random_number(rand)
    if (init_theta .eqv. .true.) then
        parcel%potential_temp = parcel%potential_temp * (1 + 0.0 / 100) ! non-random increase
        ! else
        !     theta_val = theta_val ! * (1 + 0.01) ! increase by 1%
    end if

    ! Wind
    associate (A => w_in%data_3d)
        parcel%w = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate
    parcel%velocity = parcel%w

    associate (A => u_in%data_3d)
        parcel%u = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate
    associate (A => v_in%data_3d)
        parcel%v = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate

    ! DEBUG: remove after debugging
    ! if (present(temp_env) .eqv. .true.) then
    !     associate (A => temp_env%data_3d)
    !         tmp_temp = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
    !             A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
    !             A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    !         if (abs(parcel%temperature - tmp_temp) > 2.0 ) then
    !             print *, "WARNING-----------", parcel%temperature , "vs", tmp_temp
    !         end if
    !     end associate
    ! end if

    associate (A => water_vapor%data_3d)
        parcel%water_vapor = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate
    ! print *, "ENV WV=", water_vapor%data_3d(4,:,4)

    ! print *, "WV", parcel%water_vapor
    ! stop "LESSART"
    associate (A => cloud_water_mass%data_3d)
        parcel%cloud_water = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate

    ! TODO : ARTLESS
    ! ! change parcel parameters based on namelist options
    if (parcel_options%environment_only .eqv. .false.) then
        if (int(parcel_options%velocity_init) .ne. -9999) &
             parcel%velocity = parcel_options%velocity_init
        parcel%velocity = parcel%velocity + parcel_options%velocity_offset
        if (int(parcel_options%velocity_prob_range) .ne. 0) then
           call random_number(random_val)
           random_val = random_val - 0.5
        else
           random_val = 0.0
        end if

        parcel%velocity = parcel%velocity + random_val * parcel_options%velocity_prob_range

        if (int(parcel_options%temp_init) .ne. -9999) &
            parcel%temperature = parcel_options%temp_init
        parcel%temperature = parcel%temperature + parcel_options%temp_offset
        if (int(parcel_options%velocity_prob_range) .eq. 1) then
           call random_number(random_val)
           random_val = random_val - 0.5
        else
           random_val = 0.0
        end if
        parcel%temperature = parcel%temperature + random_val * parcel_options%temp_prob_range

        parcel%potential_temp = parcel%potential_temp * (1 + 0.0 / 100)

        if (parcel_options%rh_init .ne. -1.0) &
             parcel_rh = parcel_options%rh_init
    end if


    ! parcel%relative_humidity = parcel%water_vapor  / sat_mr_local(parcel%temperature, parcel%pressure)
    !      sat_mr_local(parcel%temperature, parcel%pressure)
    parcel%relative_humidity = parcel_rh

    ! if (debug .eqv. .true.) then
        ! print *, "after ini parcel ="
        ! call parcel%print_parcel()
        ! print *, "parcel p", parcel%pressure, "at", parcel%z
    ! end if
  end subroutine init_parcel_physics


  subroutine cu_parcel_physics(this, grid, & !nx_global, ny_global, &
      z_interface, z_m, temperature, potential_temp, &
      pressure, u_in, v_in, w_in, &
      dt, dz, dx_val, water_vapor, cloud_water_mass, timestep)
    implicit none
    class(exchangeable_parcel), intent(inout) :: this
    type(grid_t), intent(in) :: grid
    type(variable_t), intent(in) :: temperature
    class(exchangeable_t), intent(in) :: potential_temp
    type(variable_t), intent(in) :: pressure
    type(variable_t), intent(in) :: z_interface
    type(variable_t), intent(in) :: z_m
    class(exchangeable_t), intent(in), optional :: u_in, v_in, w_in
    type(exchangeable_t), intent(in)  :: water_vapor, cloud_water_mass
    type(variable_t), intent(in) :: dz
    real, intent(in)    :: dx_val
    real, intent(in)    :: dt
    integer, intent(in), optional :: timestep
    ! local variables
    real, parameter :: gravity = 9.80665
    type(parcel_t) :: new_parcel
    real :: Gamma
    real :: a_prime, z_displacement, t, t_prime, buoyancy
    real :: ws, wind_correction, delta_z, z_interface_val, z_wind_change
    integer :: i,j,k, l_bound(1), dif(1), new_ijk(3), me, iter
    real :: new_pressure, R_s, p0, exponent, alt_pressure, alt_pressure2
    real :: alt_pressure3, alt_pressure4, mixing_ratio, sat_mr_val
    real :: vapor_p, sat_p, T_C, T_K, mr, T_squared, T_original, tmp
    real :: xx, yy, z_0, z_1
    real :: rate_of_temp_change, bv(local_buf_size)
    integer :: x0, x1, z0, z1, y0, y1, bv_i, image, parcel_id(local_buf_size)
    integer :: u_bound(3)
    logical :: calc, calc_x, calc_y, exist
    real :: x, z, y, foo, dz_val
    if (debug .eqv. .true.) print*, "--- start parcel processing ---"


    me = this_image()
    bv_i = 1
    bv = 0
    parcel_id = 0

    do i=1,ubound(this%local,1)
        associate (parcel=>this%local(i))
    if (parcel%exists .eqv. .true.) then
        call this%parcel_bounds_check(parcel, grid)
    else
        cycle
    end if

    parcel%lifetime = parcel%lifetime + 1
    if (debug .eqv. .true.) then
        delta_z = -999
        print*, "~249"
        call parcel%print_parcel()
    end if

    !-----------------------------------------------------------------
    ! Handle Buoyancy
    !-----------------------------------------------------------------
    ! Buoyancy (B), Temperature (T), Surrounding Tmperature (T')
    ! B = (T - T') / T'                                         (4.12)
    ! acceleration'_z = B * g                                    (11)
    !-----------------------------------------------------------------
    ! acceleration in terms of potential temperature             (13)
    ! theta = T(p_0 / p') ^ (R_d / c_p)
    ! acceleration'_z = B * g = (theta' - theta) / theta * gravity
    !-----------------------------------------------------------------
    ! displacement (s), initial velocity (u), final velocity (v)
    ! v = u + a * t
    ! s = u*t + 1/2 a*t^2
    !-----------------------------------------------------------------
    ! new: properites of environment are prime
    T = parcel%temperature

    x = parcel%x; z = parcel%z; y = parcel%y
    x0 = floor(x); z0 = floor(z); y0 = floor(y);
    x1 = ceiling(x); z1 = ceiling(z); y1 = ceiling(y);
    associate (A => temperature%data_3d)
        T_prime = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0,y1,&
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate

    buoyancy = (T - T_prime) / T_prime
    parcel%buoyancy = buoyancy
    a_prime = buoyancy * gravity ! Acceleration
    ! d = v_0 * t + 1/2 * a * t^2
    z_displacement = parcel%velocity * dt + 0.5 * a_prime * dt * dt

    ! print *, "------- dif", T-T_prime, "v", parcel%velocity, "a'", a_prime

    ! z_displacement = parcel%velocity + 0.5 * a_prime
    if (debug .eqv. .true.) then
        print*, "~287"
        print *, me, ": PARCEL_id =", parcel%parcel_id
        print *, "T", T, "T_prime", T_prime, "buoyancy", buoyancy
        print *, "a_prime", a_prime, "velocity", parcel%velocity
        print *, "dt", dt, "z_displacement", z_displacement
    end if

    associate (A => temperature%data_3d)
        T_prime = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0,y1,&
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate

    associate (A => dz%data_3d) ! was always 500
        dz_val = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate

    if (z_displacement /= z_displacement) then
        print *, me, ":: ---------NAN ERROR---------"
        print *, "po",p0, "z_dis",z_displacement, "T", T, "T'", T_prime
        call parcel%print_parcel()
        stop "NAN ERROR from variable z_displacement"
    end if


    ! print *, "WARNING:: buoyancy turned off"
    ! ARTLESS TURNING THIS OFF ONLY WIND
    delta_z = (z_displacement) / dz_val
    parcel%z = parcel%z + delta_z

    if (debug .eqv. .true.) then
        print*, "~318"
        print *, "delta_z =", delta_z, "z_displacement", z_displacement, &
            "dz_val", dz_val
    end if

    ! if (delta_z /= delta_z) then
    !     print *, me, ":: ________NAN ERROR________"
    !     print *, "z_displacement",z_displacement, "dz_val", dz_val
    !     call parcel%print_parcel()
    !     stop "NAN ERROR from variable delta_z"
    ! end if


    if (debug .eqv. .true.) then
        print *, "OLD VECOLITY IS ", z_displacement, "NEW VELOCITY IS", &
            parcel%velocity + buoyancy * dt
    end if
    ! v_f = v_0 + a*t
    parcel%velocity = parcel%velocity + buoyancy * dt


    !-----------------------------------------------------------------
    ! Orographic lift and Wind
    ! Find dz change from change in environment
    !-----------------------------------------------------------------
    associate (A => w_in%data_3d)
        parcel%w = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate

    associate (A => u_in%data_3d)
        parcel%u = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate
    associate (A => v_in%data_3d)
        parcel%v = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate

    ! --- wind ---
    ! u: zonal velocity, wind towards the east
    ! v: meridional velocity, wind towards north
    ! print *, "xzy", parcel%x, parcel%z, parcel%y
    wind_correction = (dt*1.0 / (dz_val ))
    ! parcel%z = parcel%z + (parcel%w * wind_correction)
    wind_correction = (dt*1.0 / (dx_val ))
    if (me .eq. 1 .and. parcel%parcel_id .eq. 0 .and. parcel%lifetime .eq. 0) &
        print *, "add back x and y and z-wind"
    ! parcel%x = parcel%x + (parcel%u * wind_correction)
    ! parcel%y = parcel%y + (parcel%v * wind_correction)

    ! FROM MEETING NOTES: windspeed = speed / dx * dt, so added dt
    ! print *, "_____________grid nx nz ny =",  grid%nx ,grid%nz ,grid%ny
    ! if (me .eq. 1 .and. parcel%parcel_id .eq. 0) then
    ! print *,me, ":", parcel%parcel_id,"xzy", parcel%x, parcel%z, parcel%y
    ! end if

    ! TODO ::  data_3d check
    x = parcel%x; z = parcel%z; y = parcel%y
    x0 = floor(x); z0 = floor(z); y0 = floor(y);
    x1 = ceiling(x); z1 = ceiling(z); y1 = ceiling(y);
    associate (A => z_interface%data_3d)
        z_interface_val = bilinear_interpolation(x, x0, x1, y, y0, y1, &
            A(x0,1,y0), A(x0,1,y1), A(x1,1,y0), A(x1,1,y1))
    end associate
    ! associate (A => z_interface%data_3d(:,1,:))
    !     z_interface_val = bilinear_interpolation(x, x0, x1, y, y0, y1, &
    !         A(x0,y0), A(x0,y1), A(x1,y0), A(x1,y1))
    ! end associate
    ! Interface change from land change
    z_wind_change = z_interface_val - parcel%z_interface  ! currently 0, debugging
    parcel%z_interface = z_interface_val
    z_displacement = z_displacement + z_wind_change

    !-----------------------------------------------------------------
    ! Move parcel, remove parcel if beyond the z axis
    !-----------------------------------------------------------------
    !-----------------------------------------------------------------
    z_0 = parcel%z_meters
    z_1 = parcel%z_meters + z_displacement
    parcel%z_meters = z_1
    ! print *, "z",parcel%z, "z_meters", parcel%z_meters, "mult",parcel%z * dz_val ,"z_wind_change", z_wind_change
    ! print *, "z z_meters dif = ", (parcel%z_meters) - parcel%z*dz_val
    if (debug .eqv. .true.) then
        print*, "~364"
        print *, "PARCEL MOVEMENT: parcel id =", parcel%parcel_id, "z =", parcel%z
    end if

    if ((parcel%z .lt. grid%kts) .or. (parcel%z .gt. grid%kte)) then
        print *, "PARCEL WENT OFF"
        print *, "Should be", grid%kts, "<", parcel%z, "<", grid%kte
        print *, "shape(z_interface)", shape(z_interface%data_3d)
        print *, "z_interface", z_interface%data_3d(x0,:,y0)
        ! stop "artless debug"

        call parcel%print_parcel()
        ! ---- replacement code ----
        parcel = create_empty_parcel(parcel%parcel_id, grid)
        if (replacement .eqv. .false.) then
            parcel%exists = .false.
            parcel%x = 0; parcel%z = 0; parcel%y = 0
        else
            call cu_parcel_init(parcel, grid, &
                z_interface, z_m, potential_temp, pressure, &
                u_in, v_in, w_in, dz, water_vapor, cloud_water_mass)
        end if
        if (replacement_message .eqv. .true.) &
            call print_replacement_message(parcel%parcel_id, parcel%z, grid%kts)
        ! stop "ARTLESS DEBUGGING"
        cycle
    end if

    ! print *, "REMOVE THIS CYCLE"
    ! cycle

    if (brunt_vaisala_data .eqv. .true.) then
        associate (A => potential_temp%data_3d, x => parcel%x, &
            z => parcel%z , y => parcel%y)
            x0 = floor(x); z0 = floor(z); y0 = floor(y);
            x1 = ceiling(x); z1 = ceiling(z); y1 = ceiling(y);

            bv(bv_i) = trilinear_interpolation( &
                x, x0, x1, z, z0, z1, y, y0,y1, &
                A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
                A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
            parcel_id(bv_i) = parcel%parcel_id
            bv_i = bv_i + 1
        end associate
    end if

    !-----------------------------------------------------------------
    ! Dry Lapse Rate
    !-----------------------------------------------------------------
    ! p = p_0 * e^( ((9.81/287.058)*dz) / t_mean )
    !-----------------------------------------------------------------
    ! Method one: physics
    !        a) change pressure         b) update temperature
    !-----------------------------------------------------------------
    !-----------------------------------------------------------------
    ! Relative Humidity and physics of Moist Adiabatic Lapse Rate
    !-----------------------------------------------------------------
    ! | Mixing Ratio |
    ! saturate = sat_mr(t,p)
    ! if (water_vapor > saturated)
    !   condensate = water_vapor - satured
    !   water_vapor -= condensate
    !   clouds += condensate
    !   Q_heat  = specific_latent_heat * condensate
    !   delta_T = Q_heat / c_p   ! c_p is specific heat capacity
    !   temperature += delta_T
    !-----------------------------------------------------------------
    block ! Moist Lapse Rate
    use icar_constants, only : LH_vaporization, cp
    real :: saturate, condensate, vapor, vapor_needed, RH
    ! specific latent heat values, calculating using formula
    real, parameter :: condensation_lh = 2600!000 ! 2.5 x 10^6 J/kg
    real, parameter :: vaporization_lh = -condensation_lh
    ! specific heat of water vapor at constant volume
    ! real, parameter :: C_vv = 1.0 / 1390.0
    real :: C_vv, c_p
    real :: specific_latent_heat, Q_heat, temp_c, delta_t, T0, T1, &
        p0, p1, q_dry, q_wet, potential_temp0, q_new_dry, q_dif
    real :: water_vapor0,  water_vapor1
    real :: cloud_water0,  cloud_water1
    real :: potential_T0, potential_T1
    integer :: repeat
    real :: divide_by

    if (debug .eqv. .true.) then
        call parcel%print_parcel()
    end if
    sync all


    T0 = parcel%temperature
    p0 = parcel%pressure
    potential_T0 = parcel%potential_temp
    call dry_lapse_rate(parcel%pressure, parcel%temperature, &
        parcel%potential_temp, z_displacement)
    T1 = parcel%temperature
    p1 = parcel%pressure
    q_dry = 1004 * 1 * (abs(t1-t0))  ! q = c_p x m x delta_T

    do iter = 1,5
        saturate = sat_mr_local(parcel%temperature, parcel%pressure)
        RH = parcel%water_vapor / saturate
        potential_T1 = parcel%potential_temp
        water_vapor0 = parcel%water_vapor
        cloud_water0 = parcel%cloud_water
        ! print *, iter, ": water_vapor", water_vapor0, " / saturate", saturate, "= RH", RH, "& cloudwater=", cloud_water0

        parcel%relative_humidity = RH
        ! https://en.wikipedia.org/wiki/Latent_heat#Specific_latent_heat
        ! specific latent heat for condensation and evaporation
        specific_latent_heat = 2.5 * 10**6 ! J kg^-1
        ! get this from ICAR module ARTLESS

        ! Parcel is falling, using evaporation of cloud water to keep
        ! the relative humidity at 1 if possible
        vapor_needed = saturate - parcel%water_vapor   ! mixing_ratio_devicit
        if (iter .ge. 1) divide_by = 4
        if (iter .ge. 3) divide_by = 3
        if (iter .ge. 5) divide_by = 1.5
        ! TODO: look up numerical theory for time steps

        if ((parcel%cloud_water .gt. 0.0) .and. (RH .lt. 1.0) .and. (vapor_needed .gt. 0.0000001)) then
            if (debug .eqv. .true.) print*, "==== cloud_water .gt. 0, rh .lt. 1, wet ===="
            if (vapor_needed > parcel%cloud_water) then
                vapor = parcel%cloud_water
                vapor = vapor / divide_by!(8-iter)                ! Saturated
                parcel%cloud_water = 0
            else
                vapor = vapor_needed
                vapor = vapor / divide_by!(8-iter)                ! Saturated
                parcel%cloud_water = parcel%cloud_water - vapor
            end if
            ! note: should probably rename vapor to more accurate term, amount of mass related
            parcel%water_vapor = parcel%water_vapor + vapor

            ! heat required by phase change
            Q_heat = specific_latent_heat * vapor ! kJ
            q_wet = Q_heat ! debug statement
            ! c_p = 1004 ! specific heat of dry air at 0C
            ! c_p = (1004 * (1 + 1.84 * vapor)) ! 3.3
            c_p = cp ! 1012 from icar_constants.f90

            delta_t = Q_heat / c_p   ! 3.2c
            parcel%temperature = T1 - delta_t

            if (debug .eqv. .true.) potential_temp0 = parcel%potential_temp

            ! update potential temperature, assumming pressure is constant
            parcel%potential_temp = parcel%temperature / exner_function_local(parcel%pressure)
        if (parcel%water_vapor < 0.0) then
            print *, "ERROR: WATER VAPOR = ", parcel%water_vapor
            stop "ERROR: WATER VAPOR = "
        end if


            ! Parcel is raising and condensation is occurring
        else if (RH .gt. 1.0) then
            ! ==== rh .gt. 1 ====
            ! water_vapor 5.271018017E-4 saturate 5.271018017E-4 cloud water 0.
            ! condensate 0. new water_vapor 5.271018017E-4 new cloud water 0.
            ! Q_heat 0.
            ! ============= process done ===============

            if (debug .eqv. .true.) print*, "==== rh .gt. 1, wet ===="
            ! TODO: ADD ITERATION, use same name
            ! saturate = sa

            condensate = parcel%water_vapor - saturate
            condensate = condensate / divide_by
            ! condensate = condensate ! / (6-iter) ! Saturated
            parcel%water_vapor = parcel%water_vapor - condensate
            parcel%cloud_water = parcel%cloud_water + condensate

            !--------------------------------------------------------------
            ! a different way to calculate specific latent heat
            !--------------------------------------------------------------
            ! if (T1 .lt. 248.15) then
            !   specific_latent_heat = 2600
            ! else if (T1 .gt. 314.15) then
            !   specific_latent_heat = 2400
            ! else
            !   T_C = T1 - 273.15
            !   specific_latent_heat = 2500.8 - 2.36*T_C + 0.0016 * T_C**2 - &
            !       0.00006 * T_C**3
            ! end if

            ! heat from phase change
            Q_heat = specific_latent_heat * condensate ! kJ
            q_wet = Q_heat

            !--------------------------------------------------------------
            ! calculate change in heat using specific heat (c_p)
            !--------------------------------------------------------------
            ! Stull: Practical Meteorology
            ! c_p = (1004 * (1 + 1.84 * condensate)) ! 3.3
            ! c_p = 1004 ! specific heat of dry air at 0C
            c_p = cp ! 1012
            delta_T = Q_heat / c_p   ! 3.2c
            parcel%temperature = T1 + delta_T
            ! update potential temperature, assumming pressure is constant
            ! parcel%potential_temp = exner_function(parcel%pressure) / &
            !      parcel%temperature
            parcel%potential_temp = parcel%temperature / exner_function_local(parcel%pressure)
            ! call dry_lapse_rate(parcel%pressure, parcel%temperature, &
            !      parcel%potential_temp, z_displacement)

            ! stop
            ! -- is pressure constant during this process?
            ! using Poisson's equation
            ! parcel%pressure = p0/ ((T0/parcel%temperature)**(1/0.286))
            if (parcel%water_vapor < 0.0) then
                print *, "ERROR: WATER VAPOR = ", parcel%water_vapor
                stop "ERROR: WATER VAPOR < 0.0 "
            end if

        else if (iter .eq. 1) then
            if (debug .eqv. .true.) print*, "==== was dry process ===="
            exit
        end if
        if (parcel%water_vapor < 0.0) then
            print *, "ERROR: WATER VAPOR = ", parcel%water_vapor
            stop "ERROR: WATER VAPOR < 0.0 "
        end if


        if (debug .eqv. .true.) then
            print *, "     pressure  |  temp      |   ~heat    | potential"
            print *, "pre ", p0, t0, ", -none-       ,", potential_t0

            q_new_dry = 1004 * (1) * (abs(parcel%temperature-t0))
            q_dif = abs(q_dry-(q_new_dry+q_wet))
            print *, q_dry, q_new_dry, q_wet, q_dif, 1004 * (1) * q_dif
        end if

    end do ! for saturated parcel iteration, iter = 1,5
    parcel%cloud_water = parcel%cloud_water * 0.99

    ! saturate = sat_mr(parcel%temperature, parcel%pressure)
    ! RH = parcel%water_vapor / saturate
    ! parcel%relative_humidity = RH
    end block ! saturated parcel block

    call this%move_if_needed(parcel, grid)

    if (debug .eqv. .true.) then
       call parcel%print_parcel()
       print *, "END OF PARCELS PHYSICS"
    end if
    debug = .false.
    end associate ! associate (parcel=>this%local(i))
    end do ! end of iterating through parcels

    ! todo
    ! print change in temp, water vapor, other stuff
    ! more indentation ! add end do statements, limit of 120
    ! add notes
    ! print statements with 1 parcel to figure out the weirdness

    if (brunt_vaisala_data .eqv. .true.) &
        call this%write_bv_data(bv, bv_i, parcel_id, timestep, local_buf_size)
    if (debug .eqv. .true.) &
        print *, "============= process done ==============="
    if (debug .eqv. .true.) then
    print *, "- AIR_PARCEL PHYSICS :: ENDING -"
    print *, ""
    print *, ""
    print *, ""
    end if
    end subroutine cu_parcel_physics

    ! subroutine inv_trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0 , y1, &
    !     c)
    !     real :: xd, zd, yd
    !     ! tri = trilinear_interpolation(args)
    !     ! addition = new_water - tri
    !     !
    !     xd = (x - x0) / (x1 - x0)
    !     zd = (z - z0) / (z1 - z0)
    !     yd = (y - y0) / (y1 - y0)


    ! end subroutine inv_trilinear_interpolation

    pure function trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
        c000, c001, c010, c100, c011, c101, c110, c111) result(c)
        real, intent(in) :: x, z, y
        real, intent(in) :: c000, c001, c010, c100, c011, c101, c110, c111
        integer, intent(in) :: x0, x1, z0, z1, y0, y1
        real :: xd, zd, yd, c00, c01, c10, c11, c0, c1, c
        if (x0 .eq. x1) then
            c = bilinear_interpolation(z, z0, z1, y, y0, y1, &
                c000, c001, c010, c011)
        else if (y0 .eq. y1) then
            c = bilinear_interpolation(x, x0, x1, z, z0, z1, &
                c000, c010, c100, c110)
        else if (z0 .eq. z1 ) then
            c = bilinear_interpolation(x, x0, x1, y, y0, y1, &
                c000, c001, c100, c101)
        else
            xd = (x - x0) / (x1 - x0)
            zd = (z - z0) / (z1 - z0)
            yd = (y - y0) / (y1 - y0)

            c00 = c000 * (1 - xd) + c100 * xd
            c01 = c001 * (1 - xd) + c101 * xd
            c10 = c010 * (1 - xd) + c110 * xd
            c11 = c011 * (1 - xd) + c111 * xd

            c0 = c00 * (1 - zd) + c10 * zd
            c1 = c01 * (1 - zd) + c11 * zd

            c = c0 * (1 - yd) + c1 * yd
        end if

    end function trilinear_interpolation


    pure function bilinear_interpolation(x, x0, x1, y, y0, y1, &
        c00, c01, c10, c11) result(c)
        real, intent(in) :: x, y
        real, intent(in) :: c00, c01, c10, c11
        integer, intent(in) :: x0, x1, y0, y1
        real :: xd, yd, c0, c1, c
        if (x0 .eq. x1) then
            if (y0 .eq. y1) then
                c = c00
                return
            end if
            c = c00 + (c11-c00) * (y-y0) / (y1-y0)
        else if (y0 .eq. y1) then
            c = c00 + (c11-c00) * (x-x0) / (x1-x0)
        else
            c0 = ((x1 - x)/(x1 - x0)) * c00 + ((x - x0)/(x1 - x0)) * c10
            c1 = ((x1 - x)/(x1 - x0)) * c01 + ((x - x0)/(x1 - x0)) * c11
            c = ((y1 - y)/(y1 - y0)) * c0 + ((y - y0)/(y1 - y0)) * c1
        end if
    end function bilinear_interpolation


    ! copied from src/unitilities/atm_utilities.f90
    elemental module function exner_function_local(pressure) result(exner)
        use icar_constants, only : Rd, cp
        real, intent(in) :: pressure
        real :: exner

        associate(po=>100000)!, Rd=>287.058, cp=>1003.5)
            exner = (pressure / po) ** (Rd/cp)
        end associate
    end function exner_function_local


    ! copied from src/unitilities/atm_utilities.f90
    elemental module function sat_mr_local(temperature,pressure)
        ! Calculate the saturated mixing ratio at a temperature (K), pressure (Pa)
        implicit none
        real,intent(in) :: temperature,pressure
        real :: e_s,a,b
        real :: sat_mr_local

        if (temperature < 273.15) then
            a = 21.8745584
            b = 7.66
        else
            a = 17.2693882
            b = 35.86
        endif

        e_s = 610.78 * exp(a * (temperature - 273.16) / (temperature - b)) !(Pa)
        if ((pressure - e_s) <= 0) then
            e_s = pressure * 0.99999
        endif
        ! from : http://www.srh.noaa.gov/images/epz/wxcalc/mixingRatio.pdf
        sat_mr_local = 0.6219907 * e_s / (pressure - e_s) !(kg/kg)
    end function sat_mr_local


    !-----------------------------------------------------------------
    ! p = p_0 * e^( ((9.81/287.058)*dz) / t_mean )
    !-----------------------------------------------------------------
    ! Method one: physics
    !        a) change pressure         b) update temperature
    !-----------------------------------------------------------------
    module subroutine dry_lapse_rate(&
        pressure, temperature, potential_temp, z_displacement)
    real, intent(inout) :: pressure, temperature, potential_temp
    real, intent(in) :: z_displacement

    real, parameter :: gravity = 9.80665
    real ::  p0, T_original

    p0 = pressure
    pressure = p0 - z_displacement * gravity / (287.05 * temperature) * p0
    ! if (pressure < 0.0) then
    ! "ARTLESS REMOVE"

    ! print *,""
    ! print *,""
    ! print *, "pressure ,p0 ,z_displacement ,temperature"
    ! print *, pressure ,p0 ,z_displacement ,temperature
        ! stop "ARTLESS DRY_LAPSE_RATE"
    ! end if
    T_original = temperature
    temperature = exner_function_local(pressure) * potential_temp
    end subroutine dry_lapse_rate

    module subroutine print_replacement_message(parcel_id, z, kts)
    integer, intent(in) :: parcel_id, kts
    real, intent(in) :: z
    if (z .lt. kts) then
        write(*,'(I4AI10A)', advance='no') this_image(), ": ", parcel_id, &
            " hit the ground"
    else
        write(*,'(I4AI10A)', advance='no') this_image(), ": ", parcel_id, &
            " went off the top"
    end if
    if (replacement .eqv. .true.) then
        print *, " and was replaced"
    else
        print *, " and was not replaced"
    end if
    ! stop "ARTLESS DEBUG"
    end subroutine print_replacement_message
end module module_cu_parcel
