module module_cu_parcel
  use variable_interface,     only : variable_t
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

  logical, parameter :: debug = .true.
  logical, parameter :: brunt_vaisala_data = .false.
  logical, parameter :: replacement = .true. ! parcel replacement
  logical, parameter :: replacement_message = .true.
  integer            :: local_buf_size

contains

  ! Initialize parcels' physics
  subroutine cu_parcels_init(parcels, grid, &
      z_interface, z_m, potential_temp, pressure, &
      u_in, v_in, w_in, dz_val, &
      input_buf_size, halo_width)
      class(exchangeable_parcel), intent(inout) :: parcels
      class(exchangeable_t), intent(in)    :: potential_temp
      class(exchangeable_t), intent(in)    :: u_in, v_in, w_in
      type(grid_t), intent(in)      :: grid
      type(variable_t), intent(in)  :: z_m
      type(variable_t), intent(in)  :: pressure
      type(variable_t), intent(in)  :: z_interface
      type(variable_t), intent(in)  :: dz_val
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

      do p_i=1,parcels%image_num_parcels()
          if (parcels%local(p_i)%exists .eqv. .TRUE.) then
              call init_parcel_physics(parcels%local(p_i), pressure, &
                  potential_temp, u_in, v_in, w_in, &
                  z_m, z_interface)
          end if
      end do
  end subroutine cu_parcels_init


  subroutine cu_parcel_init(parcel, grid, &
      z_interface, z_m, potential_temp, pressure, &
      u_in, v_in, w_in, dz_val, &
      input_buf_size, halo_width)
      class(parcel_t), intent(inout) :: parcel
      class(exchangeable_t), intent(in)    :: potential_temp
      class(exchangeable_t), intent(in)    :: u_in, v_in, w_in
      type(grid_t), intent(in)      :: grid
      type(variable_t), intent(in)  :: z_m
      type(variable_t), intent(in)  :: pressure
      type(variable_t), intent(in)  :: z_interface
      type(variable_t), intent(in)  :: dz_val
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
      print *, me, ": CU_PARCEL_INIT"
      me = this_image()
      call init_parcel_physics(parcel, pressure, &
          potential_temp, u_in, v_in, w_in, &
          z_m, z_interface)
  end subroutine cu_parcel_init


  ! Initialize a parcel's physics
  subroutine init_parcel_physics(parcel, pressure, potential_temp, u_in, &
      v_in, w_in, z_m, z_interface, &
      times_moved)
    type(parcel_t), intent(inout) :: parcel
    type(variable_t), intent(in) :: pressure
    class(exchangeable_t), intent(in) :: potential_temp
    class(exchangeable_t), intent(in) :: u_in, v_in, w_in
    type(variable_t), intent(in) :: z_m
    type(variable_t), intent(in) :: z_interface
    integer, intent(in), optional :: times_moved
    integer :: x0, z0, y0, x1, z1, y1
    real :: x, z, y, exner_val

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

    associate (A => z_interface%data_3d(:,1,:)) ! TODO: look at this
        parcel%z_interface = bilinear_interpolation(x, x0, x1, y, y0, y1, &
            A(x0,y0), A(x0,y1), A(x1,y0), A(x1,y1))
    end associate

    exner_val = exner_function_local(parcel%pressure)

    ! call random_number(rand)
    ! if (init_theta .eqv. .true.) then
    !     parcel%potential_temp = parcel%potential_temp * (1 + 1.0 / 100) ! random 0-1% change
    !     ! else
    !     !     theta_val = theta_val ! * (1 + 0.01) ! increase by 1%
    ! end if

    associate (A => w_in%data_3d)
        parcel%velocity = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
            A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
            A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
        parcel%w = parcel%velocity
    end associate

    ! Wind
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

    parcel%temperature = exner_val * parcel%potential_temp

    ! parcel%relative_humidity = 0
    parcel%relative_humidity = &
        sat_mr_local(parcel%temperature, parcel%pressure) *  1.0 !0.99

    ! print *, "after ini parcel ="
    ! call parcel%print_parcel()
  end subroutine init_parcel_physics


  subroutine cu_parcel_physics(this, grid, & !nx_global, ny_global, &
      z_interface, z_m, temperature, potential_temp, &
      pressure, u_in, v_in, w_in, &
      dt, dz, dx_val, timestep)
    implicit none
    class(exchangeable_parcel), intent(inout) :: this
    type(grid_t), intent(in) :: grid
    type(variable_t), intent(in) :: temperature
    class(exchangeable_t), intent(in) :: potential_temp
    type(variable_t), intent(in) :: pressure
    type(variable_t), intent(in) :: z_interface
    type(variable_t), intent(in) :: z_m
    class(exchangeable_t), intent(in), optional :: u_in, v_in, w_in
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
    if (debug .eqv. .true.) print*, "start parcel processing"

    me = this_image()
    bv_i = 1
    bv = 0
    parcel_id = 0

    do i=1,ubound(this%local,1)
        associate (parcel=>this%local(i))
    ! print *, "___________________ i =", i, "_________________________"
    if (parcel%exists .eqv. .true.) then
        call this%parcel_bounds_check(parcel, grid)
    else
        cycle
    end if


    if (debug .eqv. .true.) &
        call parcel%print_parcel()

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
    a_prime = buoyancy * gravity
    ! d = v_0 * t + 1/2 * a * t^2
    z_displacement = parcel%velocity * dt + 0.5 * a_prime * dt * dt
    ! z_displacement = parcel%velocity + 0.5 * a_prime
    if (debug .eqv. .true.) then
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

    delta_z = z_displacement / dz_val
    if (z_displacement /= z_displacement) then
        print *, me, ":: ---------NAN ERROR---------", &
            parcel%parcel_id, T, T_prime
        parcel%z = -1
        print *, p0, z_displacement, gravity, parcel%temperature
        stop "NAN ERROR from variable z_displaceme"
    else
        parcel%z = parcel%z + delta_z
        parcel%velocity = z_displacement
    end if

    if (debug .eqv. .true.) then
        print *, "delta_z =", delta_z, "z_displacement", z_displacement, &
            "dz_val", dz_val
    end if

    !-----------------------------------------------------------------
    ! Orographic lift
    ! Find dz change from change in environment
    !-----------------------------------------------------------------

    ! print *, "wind", wind_correction

    ! u: zonal velocity, wind towards the east
    ! v: meridional velocity, wind towards north
    print *, "xzy", parcel%x, parcel%z, parcel%y
    wind_correction = (1.0 / dz_val)
    parcel%z = parcel%z + (parcel%w * wind_correction)
    wind_correction = (1.0 / dx_val)
    parcel%x = parcel%x + (parcel%u * wind_correction)
    parcel%y = parcel%y + (parcel%v * wind_correction)

    ! print *, "_____________grid nx nz ny =",  grid%nx ,grid%nz ,grid%ny
    ! print *, "xzy", parcel%x, parcel%z, parcel%y
    ! stop "HOFFF"

    ! TODO ::  data_3d check
    x = parcel%x; z = parcel%z; y = parcel%y
    x0 = floor(x); z0 = floor(z); y0 = floor(y);
    x1 = ceiling(x); z1 = ceiling(z); y1 = ceiling(y);
    associate (A => z_interface%data_3d(:,1,:))
        z_interface_val = bilinear_interpolation(x, x0, x1, y, y0, y1, &
            A(x0,y0), A(x0,y1), A(x1,y0), A(x1,y1))
    end associate
    z_wind_change = z_interface_val - parcel%z_interface
    parcel%z_interface = z_interface_val
    z_displacement = z_displacement + z_wind_change

    !-----------------------------------------------------------------
    ! Move parcel, remove parcel if beyond the z axis
    !-----------------------------------------------------------------
    !-----------------------------------------------------------------
    z_0 = parcel%z_meters
    z_1 = parcel%z_meters + z_displacement
    parcel%z_meters = z_1
    print *, "parcel id =", parcel%parcel_id, "z =", parcel%z

    if ((parcel%z .lt. grid%kts) .or. (parcel%z .gt. grid%kte)) then
        ! ---- replacement code ----
        parcel = create_empty_parcel(parcel%parcel_id, grid)
        call cu_parcel_init(parcel, grid, &
            z_interface, z_m, potential_temp, pressure, &
            u_in, v_in, w_in, dz)
        if (replacement_message .eqv. .true.) then
            if (parcel%z .lt. grid%kts) then
                print *, me,":",parcel%parcel_id, "hit the ground"
            else
                print *, me,":",parcel%parcel_id, "went off the top"
            end if
        end if
        cycle
    end if

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
    block
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
    logical :: only_dry

    T0 = parcel%temperature
    p0 = parcel%pressure
    potential_T0 = parcel%potential_temp
    call dry_lapse_rate(parcel%pressure, parcel%temperature, &
        parcel%potential_temp, z_displacement)
    only_dry = .true.
    T1 = parcel%temperature
    p1 = parcel%pressure
    q_dry = 1004 * 1 * (abs(t1-t0))  ! q = c_p x m x delta_T

    do iter = 1,4
        saturate = sat_mr_local(parcel%temperature, parcel%pressure)
        RH = parcel%water_vapor / saturate
        potential_T1 = parcel%potential_temp
        water_vapor0 = parcel%water_vapor
        cloud_water0 = parcel%cloud_water

        parcel%relative_humidity = RH
        ! https://en.wikipedia.org/wiki/Latent_heat#Specific_latent_heat
        ! specific latent heat for condensation and evaporation
        specific_latent_heat = 2.5 * 10**6 ! J kg^-1

        ! Parcel is falling, using evaporation of cloud water to keep
        ! the relative humidity at 1 if possible
        vapor_needed = saturate - parcel%water_vapor

        if (parcel%cloud_water .gt. 0.0 .and. RH .lt. 1.0 &
            .and. vapor_needed .gt. 0.0000001) then
            if (debug .eqv. .true.) &
                print*, "==== cloud_water .gt. 0, rh .lt. 1, wet ===="



            only_dry = .false.
            if (vapor_needed > parcel%cloud_water) then
                vapor = parcel%cloud_water
                vapor = vapor / (6-iter)                ! Saturated
                parcel%cloud_water = 0
            else
                vapor = vapor_needed
                vapor = vapor / (6-iter)                ! Saturated
                parcel%cloud_water = parcel%cloud_water - vapor
            end if


            parcel%water_vapor = parcel%water_vapor + vapor

            ! heat required by phase change
            Q_heat = specific_latent_heat * vapor ! kJ
            q_wet = Q_heat
            c_p = (1004 * (1 + 1.84 * vapor)) ! 3.3
            ! c_p = 1004 ! specific heat of dry air at 0C
            delta_t = Q_heat / c_p   ! 3.2c
            parcel%temperature = T1 - delta_t

            if (debug .eqv. .true.) &
                potential_temp0 = parcel%potential_temp


            ! update potential temperature, assumming pressure is constant
            parcel%potential_temp = parcel%temperature / exner_function_local(parcel%pressure)


            ! Parcel is raising and condensation is occurring
        else if (RH .gt. 1.0) then
            ! ==== rh .gt. 1 ====
            ! water_vapor 5.271018017E-4 saturate 5.271018017E-4 cloud water 0.
            ! condensate 0. new water_vapor 5.271018017E-4 new cloud water 0.
            ! Q_heat 0.
            ! ============= process done ===============

            if (debug .eqv. .true.) print*, "==== rh .gt. 1, wet ===="

            only_dry = .false.
            condensate = parcel%water_vapor - saturate
            condensate = condensate / (6-iter) ! Saturated
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
            ! print *, "Q_heat", Q_heat
            ! exit
            !--------------------------------------------------------------
            ! calculate change in heat using specific heat (c_p)
            !--------------------------------------------------------------
            ! Stull: Practical Meteorology
            c_p = (1004 * (1 + 1.84 * condensate)) ! 3.3
            ! c_p = 1004 ! specific heat of dry air at 0C
            ! print *, "c_p", c_p
            delta_T = Q_heat / c_p   ! 3.2c
            ! print *, "delta_T", delta_t
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

        else if (iter .eq. 1) then
            if (debug .eqv. .true.) then
                print*, "==== was dry process ===="
            end if
            exit
        end if

        if (debug .eqv. .true.) then
            print *, "     pressure  |  temp      |   ~heat    | potential"
            print *, "pre ", p0, t0, ", -none-       ,", potential_t0

            q_new_dry = &
                1004 * (1) * (abs(parcel%temperature-t0))
            q_dif = abs(q_dry-(q_new_dry+q_wet))
            print *, q_dry, q_new_dry, q_wet, q_dif, &
                1004 * (1) * q_dif
        end if
        ! if ((debug .eqv. .true.) .and. (only_dry .eqv. .false.)) then
        !    print *, "post", p1, t1, q_dry, potential_t1
        !    print *, "new ", &
        !         parcel%pressure, parcel%temperature, &
        !         q_wet,  parcel%potential_temp


        ! print *, "heat: q_dry = q_new_dry + q_wet"
        ! print *, " ", q_dry, "=", q_new_dry, "+", q_wet
        ! print *, "dif         =", q_dif, "which is ~ temp diff", &
        !      1004 * (1) * q_dif

        ! print *, "--test--"
        ! print *, " t? ", t0 - (q_new_dry / (1004 * (1)) )

        ! print *, "vapor_needed", vapor_needed, &
        !      "new water_vapor", parcel%water_vapor, &
        !      "new cloud water", parcel%cloud_water, &
        !      "water_vapor0", water_vapor0, &
        !      "cloud water0", cloud_water0
        ! end if

    end do
    ! saturate = sat_mr(parcel%temperature, parcel%pressure)
    ! RH = parcel%water_vapor / saturate
    ! parcel%relative_humidity = RH

    end block
    call this%move_if_needed(parcel, grid)
    ! print *, "POST MOVE IF NEEDED"
    ! call parcel%print_parcel()
    ! print *, "END OF PARCELS PHYSICS"
    end associate
    end do

    if (brunt_vaisala_data .eqv. .true.) &
        call this%write_bv_data(bv, bv_i, parcel_id, timestep, local_buf_size)
    if (debug .eqv. .true.) &
        print *, "============= process done ==============="
    print *, "- AIR_PARCEL PHYSICS :: ENDING -"
    print *, ""
    print *, ""
    print *, ""
    end subroutine cu_parcel_physics


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
    pure module subroutine dry_lapse_rate(&
        pressure, temperature, potential_temp, z_displacement)
    real, intent(inout) :: pressure, temperature, potential_temp
    real, intent(in) :: z_displacement

    real, parameter :: gravity = 9.80665
    real ::  p0, T_original

    p0 = pressure
    pressure = p0 - z_displacement * gravity / (287.05 * temperature) * p0
    T_original = temperature
    temperature = exner_function_local(pressure) * potential_temp
    end subroutine dry_lapse_rate
end module module_cu_parcel
