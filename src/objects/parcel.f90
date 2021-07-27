submodule(parcel_interface) &
    parcel_implementation
  use assertions_mod, only : assert, assertions
  ! use exchangeable_interface, only : exchangeable_t
  use mod_atm_utilities, only : pressure_at_elevation, exner_function, sat_mr

  ! use grid_interface, only : grid_t
  implicit none

  ! ----- PARAMETERS TO TUNE CONVECTION MODEL -----
  logical, parameter :: debug = .false.
  logical, parameter :: wrap_neighbors = .true.
  logical, parameter :: convection = .true.
  logical, parameter :: wind = .true.
  logical, parameter :: fake_wind_correction = .false.
  logical, parameter :: use_input_wind = .true.
  logical, parameter :: caf_comm_message = .false.
  logical, parameter :: parcel_create_message = .false.
  logical, parameter :: brunt_vaisala_data = .false.
  logical, parameter :: replacement = .true.
  logical, parameter :: replacement_message = .false.
  logical, parameter :: init_theta = .false.
  logical, parameter :: init_velocity = .true.
  logical, parameter :: count_p_comm = .false.
  integer, save      :: parcels_communicated[*]
  integer, save      :: parcels_per_image
  integer, save      :: local_buf_size
  real,    save      :: input_wind
  logical, save      :: dry_air_parcels
  ! integer, parameter :: parcels_per_image=1
  ! integer, parameter :: local_buf_size=4*parcels_per_image
  ! logical, parameter :: dry_air_parcels=.true.
  ! -----------------------------------------------

  integer, parameter :: default_buf_size=1
  integer, parameter :: default_halo_size=1
  integer, save, allocatable :: neighbors(:)
  integer, save :: north_con_neighbor, south_con_neighbor, buf_size, halo_size
  integer, save :: east_con_neighbor, west_con_neighbor
  integer, save :: northeast_con_neighbor, northwest_con_neighbor
  integer, save :: southeast_con_neighbor, southwest_con_neighbor

contains
  module procedure convect_const
    integer :: me, create, seed(34)
    real :: random_start(3), x, z, y
    real :: z_meters, z_interface_val, theta_val
    real :: pressure_val, exner_val, temp_val, water_vapor_val
    real :: u_val, v_val, w_val
    integer :: x0, x1, z0, z1, y0, y1
    logical :: calc

    me = this_image()
    call initialize_from_file()
    parcels_communicated = 0
    if (parcels_per_image .eq. 0) then
       if (me .eq. 1) print *, "No air parcels used"
       return
    end if

    me = this_image()
    ! if (present(input_buf_size)) then
    !   buf_size = input_buf_size
    !   print *, 'using input_buf_size'
    ! else
    !   buf_size = default_buf_size
    !   print *, 'using default_buf_size'
    ! end if
    buf_size = parcels_per_image

    if (present(halo_width)) then
      halo_size = halo_width
    else
      halo_size = default_halo_size
    end if

    ! if (me .eq. 1) print *, "Buf size is", buf_size


    if (allocated(this%local)) deallocate(this%local)
    this%north_boundary = (grid%yimg == grid%yimages)
    this%south_boundary = (grid%yimg == 1)
    this%east_boundary  = (grid%ximg == grid%ximages)
    this%west_boundary  = (grid%ximg == 1)

    allocate(this%local(parcels_per_image * 4))
    if (parcel_create_message .eqv. .true.) then
      if (me == 1) then
        print*, "Creating", parcels_per_image, "parcels per image"
      end if
    end if

    seed = -1
    call random_seed(PUT=seed)
    call random_init(.true.,.true.)

    do create=1,parcels_per_image
      call this%create_parcel_id()
      this%local(create) = create_parcel(this%parcel_id_count, &
          its, ite, kts, kte, jts, jte, ims, ime, kms, kme, jms, jme, &
          z_m, potential_temp, z_interface, pressure, u_in, v_in, w_in)

      ! this%local(create) = parcel_t(this%parcel_id_count, .true., &
      !     .false., x, y, z, u_val, v_val, w_val, z_meters, z_interface_val, &
      !     pressure_val, temp_val, theta_val, 0, water_vapor_val, 0)
    end do


    if (parcel_create_message .eqv. .true.) then
      print *, "ALLOCATING BUFFERS OF SIZE", buf_size
    end if
    allocate( this%buf_north_in(buf_size)[*])
    allocate( this%buf_south_in(buf_size)[*])
    allocate( this%buf_east_in(buf_size)[*])
    allocate( this%buf_west_in(buf_size)[*])
    allocate( this%buf_northeast_in(buf_size)[*])
    allocate( this%buf_northwest_in(buf_size)[*])
    allocate( this%buf_southeast_in(buf_size)[*])
    allocate( this%buf_southwest_in(buf_size)[*])

    call this%setup_neighbors(grid)
  end procedure convect_const

  module procedure create_parcel
    real :: relative_humidity_in
    real :: z_meters, z_interface_val, theta_val
    real :: random_start(3), x, z, y
    integer :: x0, x1, z0, z1, y0, y1, times_moved_val
    real :: pressure_val, exner_val, temp_val, water_vapor_val
    real :: u_val, v_val, w_val, velocity, cloud_water
    call random_number(random_start)

    x = its + (random_start(1) * (ite-its))
    z = kts + (random_start(3) * (kte-kts))
    y = jts + (random_start(2) * (jte-jts))

    if (x .lt. its .or. &
        x .gt. ite .or. &
        z .lt. kts .or. &
        z .gt. kte .or. &
        y .lt. jts .or. &
        y .gt. jte) then
      print *, "x:", its, "<", x, "<", ite
      print *, "z:", kts, "<", z, "<", kte
      print *, "y:", jts, "<", y, "<", jte
      stop "x,y,z is out of bounds"
    end if

    x0 = floor(x); z0 = floor(z); y0 = floor(y);
    x1 = ceiling(x); z1 = ceiling(z); y1 = ceiling(y);

    associate (A => z_m)
      z_meters = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
          A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
          A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate

    associate (A => potential_temp%data_3d)
      theta_val = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
          A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
          A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate

    associate (A => pressure)
      pressure_val = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
          A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
          A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
    end associate

    ! print *, "val and func", pressure_val, pressure_at_elevation(100000.0, z_meters)
    ! call exit

    associate (A => z_interface)
      z_interface_val = bilinear_interpolation(x, x0, x1, y, y0, y1, &
          A(x0,y0), A(x0,y1), A(x1,y0), A(x1,y1))
    end associate

    ! print *, "z meters =" , z_meters, "z =", z, "zm =", z_m(x0,z0,y0)
    ! print *, "z_interface_val =", z_interface_val, "try =", z*dz_val + z_interface_val -250
    ! print *, "----"

    ! call flush()
    ! sync all
    ! call exit

    ! sealevel_pressure => 100000.0
    ! pressure_val = pressure_at_elevation(100000.0, z_meters)
    exner_val = exner_function(pressure_val)

    ! call random_number(rand)
    if (init_theta .eqv. .true.) then
      theta_val = theta_val * (1 + 1.0 / 100) ! random 0-1% change
    else
      theta_val = theta_val ! * (1 + 0.01) ! increase by 1%
   end if

   if (init_velocity .eqv. .true.) then
      velocity = 5
   else
      velocity = 0
   end if


    temp_val = exner_val * theta_val

    if (dry_air_parcels .eqv. .true.) then
       water_vapor_val = 0
    else
       water_vapor_val = sat_mr(temp_val, pressure_val) *  1.0 !0.99
    end if
    relative_humidity_in = water_vapor_val

    ! When testing, set to 0
    ! relative_humidity_in = 0

    ! wind is constant in this system, ignoring w wind (aka z-direction)
    u_val = u_in%data_3d(x0, z0, y0)
    v_val = v_in%data_3d(x0, z0, y0)
    w_val = 0


    cloud_water = 0
    if (present(times_moved) .eqv. .true.) then
       times_moved_val = times_moved
    else
       times_moved_val = 0
    end if


    parcel = parcel_t(parcel_id, .true., times_moved_val, &
        x, y, z, u_val, v_val, w_val, z_meters, z_interface_val, &
        pressure_val, temp_val, theta_val, velocity, water_vapor_val, &
        cloud_water, relative_humidity_in)
  end procedure


  module procedure process
    implicit none
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
    character(len=32) :: filename

    if (debug .eqv. .true.) print*, "start parcel processing"

    me = this_image()
    bv_i = 1
    bv = 0
    parcel_id = 0

    do i=1,ubound(this%local,1)
      associate (parcel=>this%local(i))
        if (parcel%exists .eqv. .true.) then
          if (parcel%x .lt. its-1 .or. &
              parcel%z .lt. kts-1 .or. &
              parcel%y .lt. jts-1 .or. &
              parcel%x .gt. ite+1 .or. &
              parcel%z .gt. kte+1 .or. &
              parcel%y .gt. jte+1) then
            print *, "parcel", parcel%parcel_id, "on image", me
            print *, "x:", its, "<", parcel%x, "<", ite, "with halo 2"
            print *, "z:", kts, "<", parcel%z, "<", kte, "with halo 2"
            print *, "y:", jts, "<", parcel%y, "<", jte, "with halo 2"
            stop "x,y,z is out of bounds"
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

          associate (A => temperature, x => parcel%x, z => parcel%z , &
              y => parcel%y)
            x0 = floor(x); z0 = floor(z); y0 = floor(y);
            x1 = ceiling(x); z1 = ceiling(z); y1 = ceiling(y);

            T_prime = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0,y1,&
                A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
                A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
          end associate


          if (convection .eqv. .true.) then
            buoyancy = (T - T_prime) / T_prime
            a_prime = buoyancy * gravity
            ! d = v_0 * t + 1/2 * a * t^2
            z_displacement = parcel%velocity * dt + 0.5 * a_prime * dt * dt
            ! z_displacement = parcel%velocity + 0.5 * a_prime


            ! number from dz_interface, currently always 500
            delta_z = z_displacement / dz
            if (z_displacement /= z_displacement) then
              print *, me, ":: ---------NAN ERROR---------", &
                  parcel%parcel_id, T, T_prime
              parcel%z = -1
              print *, p0, z_displacement, gravity, parcel%temperature
              call exit
            else
              parcel%z = parcel%z + delta_z
              parcel%velocity = z_displacement
            end if
          else
            z_displacement = 0.0
          end if


          !-----------------------------------------------------------------
          ! Orographic lift
          ! Find dz change from change in environment
          !-----------------------------------------------------------------
          if (wind .eqv. .true.) then
            wind_correction = (1.0 / dz) ! real correction
            ! print *, "wind", wind_correction
            ! wind_correction = (1.0 / 10) ! real correction
            if (fake_wind_correction .eqv. .true.) then
                wind_correction = wind_correction * 0.0
            end if

            ! u: zonal velocity, wind towards the east
            ! v: meridional velocity, wind towards north
            if (use_input_wind .eqv. .true.) then
               parcel%x = parcel%x + (input_wind * wind_correction)
               parcel%y = parcel%y + (input_wind * wind_correction)
            else
               parcel%x = parcel%x + (parcel%u * wind_correction)
               parcel%y = parcel%y + (parcel%v * wind_correction)
            end if

            associate (A => z_interface, x=> parcel%x, y=>parcel%y)
              x0 = floor(x); x1 = ceiling(x)
              y0 = floor(y); y1 = ceiling(y)

              z_interface_val = bilinear_interpolation(x, x0, x1, y, y0, y1, &
                  A(x0,y0), A(x0,y1), A(x1,y0), A(x1,y1))
            end associate
            z_wind_change = z_interface_val - parcel%z_interface
            parcel%z_interface = z_interface_val
            z_displacement = z_displacement + z_wind_change
          end if

          !-----------------------------------------------------------------
          ! Move parcel, remove parcel if beyond the z axis
          !-----------------------------------------------------------------
          z_0 = parcel%z_meters
          z_1 = parcel%z_meters + z_displacement
          parcel%z_meters = z_1
          if (parcel%z .lt. kts) then
            ! ---- hits the ground and stops  ----
            ! z_displacement = z_displacement + dz * (1-parcel%z)
            ! parcel%z = 1
            ! parcel%z_meters = z_interface_val + dz/2
            ! parcel%velocity = 0

            ! ---- replacement code ----
            parcel%exists = .false.
            ! print *, "-replacing??!!-", parcel%parcel_id
            if (replacement .eqv. .true.) then
               parcel = create_parcel(parcel%parcel_id, &
                  its, ite, kts, kte, jts, jte, ims, ime, kms, kme, jms, jme, &
                  z_m, potential_temp, z_interface, pressure, u_in, v_in, w_in,&
                  parcel%moved)
              if (replacement_message .eqv. .true.) then
                print *, me,":",parcel%parcel_id, "hit the ground"
              end if
            end if


          else if (parcel%z .gt. kte) then
            parcel%exists = .false.
            if (replacement .eqv. .true.) then
               parcel = create_parcel(parcel%parcel_id, &
                  its, ite, kts, kte, jts, jte, ims, ime, kms, kme, jms, jme, &
                  z_m, potential_temp, z_interface, pressure, u_in, v_in, w_in,&
                  parcel%moved)
              if (replacement_message .eqv. .true.) then
                print *, me,":",parcel%parcel_id, "went off the top"
              end if
            end if
            ! call exit
            ! cycle
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
          if (dry_air_parcels .eqv. .true.) then
             if (debug .eqv. .true.) print *, "-- only dry air parcels --"
             call dry_lapse_rate(parcel%pressure, parcel%temperature, &
                  parcel%potential_temp, z_displacement)
          else
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
              saturate = sat_mr(parcel%temperature, parcel%pressure)
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
                parcel%potential_temp = parcel%temperature / exner_function(parcel%pressure)


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
                parcel%potential_temp = parcel%temperature / exner_function(parcel%pressure)
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
          end if ! ---- end of relative humidity seciton ----


          !-----------------------------------------------------------------
          ! Move parcel if needed
          !-----------------------------------------------------------------
          associate (x => parcel%x, y => parcel%y, z => parcel%z)
            if (caf_comm_message .eqv. .true.) then
                if (  x .lt. its-1 .or. x .gt. ite+1 .or. &
                      z .lt. kms-1 .or. z .gt. kme   .or. &
                      y .lt. jts-1 .or. y .gt. jte+1 .or. &
                      x .lt. 1 .or. x .gt. nx_global .or. &
                      y .lt. 1 .or. y .gt. ny_global &
                    ) then
                    print *, "PUTTING", parcel%x, parcel%y, parcel%z_meters, &
                          "FROM", this_image(), "id:", parcel%parcel_id, &
                          "M", ims,ime, kms, kme, jms, jme, "T", its,ite,jts,jte
                end if
            end if

            xx = x
            yy = y
            ! If parcel is getting wrapped the x and y values need to be
            ! properly updated
            if (wrap_neighbors .eqv. .true.) then
              if (x > nx_global) then
                ! if (caf_comm_message .eqv. .true.) print *, "WRAPPED" &
                !     , parcel%parcel_id
                x = x - nx_global + 1
                xx = xx + 2
              else if (x < 1) then
                ! if (caf_comm_message .eqv. .true.) print *, "WRAPPED" &
                !     , parcel%parcel_id
                x = x + nx_global - 1
                xx = xx - 2
              end if

              if (y > ny_global) then
                ! if (caf_comm_message .eqv. .true.) print *, "WRAPPED" &
                !     , parcel%parcel_id
                y = y - ny_global + 1
                yy = yy + 2
              else if (y < 1) then
                ! if (caf_comm_message .eqv. .true.) print *, "WRAPPED" &
                !     , parcel%parcel_id
                y = y + ny_global - 1
                yy = yy - 2
              end if
            end if

            ! Check values to know where to send parcel
            if (yy .lt. jts-1) then      ! "jts  <   y    <  jte"
              if (xx .lt. its-1) then
                call this%put_southwest(parcel)
                if (count_p_comm .eqv. .true.) &
                     parcels_communicated = parcels_communicated + 1
              else if (xx .gt. ite+1) then
                call this%put_southeast(parcel)
                if (count_p_comm .eqv. .true.) &
                     parcels_communicated = parcels_communicated + 1
              else
                call this%put_south(parcel)
                if (count_p_comm .eqv. .true.) &
                     parcels_communicated = parcels_communicated + 1
              end if
            else if (yy .gt. jte+1) then ! jts will be 1
              if (xx .lt. its-1) then
                call this%put_northwest(parcel)
                if (count_p_comm .eqv. .true.) &
                     parcels_communicated = parcels_communicated + 1
              else if (xx .gt. ite+1) then
                call this%put_northeast(parcel)
                if (count_p_comm .eqv. .true.) &
                     parcels_communicated = parcels_communicated + 1
              else
                call this%put_north(parcel)
                if (count_p_comm .eqv. .true.) &
                     parcels_communicated = parcels_communicated + 1
              endif
            else if (xx .lt. its-1) then ! "its  <   x    <  ite"
              call this%put_west(parcel) ! need to double check this!
              if (count_p_comm .eqv. .true.) &
                   parcels_communicated = parcels_communicated + 1
            else if (xx .gt. ite+1) then
              call this%put_east(parcel)
              if (count_p_comm .eqv. .true.) &
                   parcels_communicated = parcels_communicated + 1
            end if
          end associate
        end if
      end associate
    end do

    if (brunt_vaisala_data .eqv. .true.) then
      do image=1,num_images()
      if (me .eq. image) then
         write (filename,"(A17)") "brunt_vaisala.txt"
         inquire(file=filename, exist=exist)
         if (exist) then
            open(unit=me, file=filename, status='old', position='append')
         else
            open(unit=me, file=filename, status='new')
         end if
         do i=1,bv_i-1
            write(me,*) me, timestep, parcel_id(i), bv(i)
         end do
         close(me)
      end if
      sync all
      end do

   end if
   if (debug .eqv. .true.) print *, "============= process done ==============="
  end procedure process

  module procedure send
    ! if (.not. this%north_boundary) call this%put_north
    ! if (.not. this%south_boundary) call this%put_south
    ! if (.not. this%east_boundary)  call this%put_east
    ! if (.not. this%west_boundary)  call this%put_west
  end procedure

  module procedure retrieve
    if (.not. present(no_sync)) then
      ! sync images (neighbors) ! sync neighbors currently broken
      sync all
    else
      if (.not. no_sync) then
        ! sync images (neighbors) ! sync neighbors currently broken
        sync all
      endif
    endif

    if (.not. this%north_boundary) call this%retrieve_buf(this%buf_north_in)
    if (.not. this%south_boundary) call this%retrieve_buf(this%buf_south_in)
    if (.not. this%east_boundary) call this%retrieve_buf(this%buf_east_in)
    if (.not. this%west_boundary) call this%retrieve_buf(this%buf_west_in)
    if (.not. this%northeast_boundary) &
        call this%retrieve_buf(this%buf_northeast_in)
    if (.not. this%northwest_boundary) &
        call this%retrieve_buf(this%buf_northwest_in)
    if (.not. this%southeast_boundary) &
        call this%retrieve_buf(this%buf_southeast_in)
    if (.not. this%southwest_boundary) &
        call this%retrieve_buf(this%buf_southwest_in)

    this%north_i = 1
    this%south_i = 1
    this%east_i  = 1
    this%west_i  = 1
    this%northeast_i = 1
    this%northwest_i = 1
    this%southeast_i = 1
    this%southwest_i = 1
    sync all
  end procedure

  module procedure exchange
    ! if (.not. this%north_boundary) call this%put_north
    ! if (.not. this%south_boundary) call this%put_south
    ! if (.not. this%east_boundary)  call this%put_east
    ! if (.not. this%west_boundary)  call this%put_west

    sync images( neighbors )

    if (.not. this%north_boundary) call this%retrieve_buf(this%buf_north_in)
    if (.not. this%south_boundary) call this%retrieve_buf(this%buf_south_in)
    if (.not. this%east_boundary) call this%retrieve_buf(this%buf_east_in)
    if (.not. this%west_boundary) call this%retrieve_buf(this%buf_west_in)
    if (.not. this%northeast_boundary) &
        call this%retrieve_buf(this%buf_northeast_in)
    if (.not. this%northwest_boundary) &
        call this%retrieve_buf(this%buf_northwest_in)
    if (.not. this%southeast_boundary) &
        call this%retrieve_buf(this%buf_southeast_in)
    if (.not. this%southwest_boundary) &
        call this%retrieve_buf(this%buf_southwest_in)

    this%north_i = 1
    this%south_i = 1
    this%east_i  = 1
    this%west_i  = 1
    this%northeast_i = 1
    this%northwest_i = 1
    this%southeast_i = 1
    this%southwest_i = 1
  end procedure

  module procedure retrieve_buf
    implicit none
    integer :: i, buf_n, local_i, local_n
    buf_n = ubound(buf, dim=1)
    local_n = ubound(this%local, dim=1)
    local_i = 1
    do i=1,buf_n
      associate (parcel=>buf(i))
        do while (parcel%exists .eqv. .true.)
          if (local_i .gt. local_n) then
            stop "retrieve_buf is out of bounds"
          end if
          if (this%local(local_i)%exists .eqv. .false.) then
            call parcel%move_to(this%local(local_i))
            local_i = local_i + 1
            exit
          end if
          local_i = local_i + 1
       end do
      end associate
    end do
  end procedure

  module procedure put_north
    if (caf_comm_message .eqv. .true.) then
       print*, "from", this_image(), "to", north_con_neighbor, parcel%parcel_id
    end if

    if (this%north_boundary) then
      parcel%exists = .false.
      return
    end if

    call check_buf_size(this%south_i)
    !dir$ pgas defer_sync
    this%buf_south_in(this%south_i)[north_con_neighbor] = parcel
    parcel%exists = .false.
    this%south_i = this%south_i + 1
  end procedure

  module procedure put_south
    if (caf_comm_message .eqv. .true.) then
       print*, "from", this_image(), "to", south_con_neighbor, parcel%parcel_id
    end if

    if (this%south_boundary) then
      parcel%exists = .false.
      return
    end if

    call check_buf_size(this%north_i)
    !dir$ pgas defer_sync
    this%buf_north_in(this%north_i)[south_con_neighbor] = parcel
    parcel%exists = .false.
    this%north_i = this%north_i + 1
  end procedure

  module procedure put_east
    if (caf_comm_message .eqv. .true.) then
       print*, "from", this_image(), "to", east_con_neighbor, parcel%parcel_id
    end if

    if (this%east_boundary) then
      parcel%exists = .false.
      return
    end if

    call check_buf_size(this%west_i)
    !dir$ pgas defer_sync
    this%buf_west_in(this%west_i)[east_con_neighbor] = parcel
    parcel%exists = .false.
    this%west_i = this%west_i + 1
  end procedure

  module procedure put_west
    if (caf_comm_message .eqv. .true.) then
       print*, "from", this_image(), "to", west_con_neighbor, parcel%parcel_id
    end if

    if (this%west_boundary) then
      parcel%exists = .false.
      return
    end if

    call check_buf_size(this%east_i)
    !dir$ pgas defer_sync
    this%buf_east_in(this%east_i)[west_con_neighbor] = parcel
    parcel%exists = .false.
    this%east_i = this%east_i + 1
  end procedure

  module procedure put_northeast
    if (caf_comm_message .eqv. .true.) then
       print*, "from", this_image(), "to", northeast_con_neighbor, parcel%parcel_id
    end if

    if (this%northeast_boundary) then
      parcel%exists = .false.
      return
    end if

    call check_buf_size(this%southwest_i)
    !dir$ pgas defer_sync
    this%buf_southwest_in(this%southwest_i)[northeast_con_neighbor] = parcel
    parcel%exists = .false.
    this%southwest_i = this%southwest_i + 1
  end procedure

  module procedure put_northwest
    if (caf_comm_message .eqv. .true.) then
       print*, "from", this_image(), "to", northwest_con_neighbor, parcel%parcel_id
    end if

    if (this%northwest_boundary) then
      parcel%exists = .false.
      return
    end if

    call check_buf_size(this%southeast_i)
    !dir$ pgas defer_sync
    this%buf_southeast_in(this%southeast_i)[northwest_con_neighbor] = parcel
    parcel%exists = .false.
    this%southeast_i = this%southeast_i + 1
  end procedure

  module procedure put_southeast
    if (caf_comm_message .eqv. .true.) then
       print*, "from", this_image(), "to", southeast_con_neighbor, parcel%parcel_id
    end if

    if (this%southeast_boundary) then
      parcel%exists = .false.
      return
    end if

    call check_buf_size(this%northwest_i)
    !dir$ pgas defer_sync
    this%buf_northwest_in(this%northwest_i)[southeast_con_neighbor] = parcel
    parcel%exists = .false.
    this%northwest_i = this%northwest_i + 1
  end procedure

  module procedure put_southwest
    if (caf_comm_message .eqv. .true.) then
       print*, "from", this_image(), "to", southwest_con_neighbor, parcel%parcel_id
    end if

    if (this%southwest_boundary) then
      parcel%exists = .false.
      return
    end if

    call check_buf_size(this%northeast_i)
    !dir$ pgas defer_sync
    this%buf_northeast_in(this%northeast_i)[southwest_con_neighbor] = parcel
    parcel%exists = .false.
    this%northeast_i = this%northeast_i + 1
  end procedure


  module procedure create_parcel_id
    use iso_fortran_env, only : int32
    implicit none
    integer :: id_range, h
    if (this%parcel_id_count .eq. -1) then
      id_range = huge(int32) / num_images()
      this%parcel_id_count = (this_image()-1) * id_range
    else
      this%parcel_id_count = this%parcel_id_count + 1
    end if
  end procedure

  module procedure setup_neighbors
    implicit none
    integer :: current, n_neighbors, n_images

    ! --- setup boundaries ---
    if (this%north_boundary) then
      this%northeast_boundary = .true.
      this%northwest_boundary = .true.
    end if
    if (this%south_boundary) then
      this%southeast_boundary = .true.
      this%southwest_boundary = .true.
    end if
    if (this%east_boundary) then
      this%northeast_boundary = .true.
      this%southeast_boundary = .true.
    end if
    if (this%west_boundary) then
      this%northwest_boundary = .true.
      this%southwest_boundary = .true.
    end if


    if (.not.allocated(neighbors)) then
      associate(me=>this_image())
        north_con_neighbor = me + grid%ximages
        south_con_neighbor = me - grid%ximages
        east_con_neighbor  = me + 1
        west_con_neighbor  = me - 1
        northeast_con_neighbor = me + grid%ximages + 1
        northwest_con_neighbor = me + grid%ximages - 1
        southeast_con_neighbor = me - grid%ximages + 1
        southwest_con_neighbor = me - grid%ximages - 1

        n_neighbors = &
             merge(0,1,this%south_boundary) &
            +merge(0,1,this%north_boundary) &
            +merge(0,1,this%east_boundary)  &
            +merge(0,1,this%west_boundary)  &
            +merge(0,1,this%northeast_boundary) &
            +merge(0,1,this%northwest_boundary) &
            +merge(0,1,this%southeast_boundary) &
            +merge(0,1,this%southwest_boundary)
        n_neighbors = max(1, n_neighbors)

        allocate(neighbors(n_neighbors))

        current = 1
        if (.not. this%south_boundary) then
          neighbors(current) = south_con_neighbor
          current = current+1
        endif
        if (.not. this%north_boundary) then
          neighbors(current) = north_con_neighbor
          current = current+1
        endif
        if (.not. this%east_boundary) then
          neighbors(current) = east_con_neighbor
          current = current+1
        endif
        if (.not. this%west_boundary) then
          neighbors(current) = west_con_neighbor
          current = current+1
        endif
        ! if current = 1 then all of the boundaries were set, just store ourself as our "neighbor"
        if (current == 1) then
          neighbors(current) = me
        endif

        ! Wrap boundaries: so parcels in the xy direction don't disappear
        associate(nx => grid%ximages, ny => grid%yimages, &
            nimages => grid%ximages * grid%yimages)
          if (wrap_neighbors .eqv. .true.) then
            n_images = num_images()
            ! --- handle diagonals
            if (this%north_boundary .eqv. .true.) then
              northeast_con_neighbor = modulo( (me+nx+1)-nimages, (nx+1))
              if (northeast_con_neighbor .eq. 0) northeast_con_neighbor = 1
              northwest_con_neighbor = (me+nx-1)-nimages
              if (northwest_con_neighbor .eq. 0) northwest_con_neighbor = nx
            else  if (this%east_boundary .eqv. .true.) then
              northeast_con_neighbor = me + 1
            else  if (this%west_boundary .eqv. .true.) then
              northwest_con_neighbor = me + nx * 2 - 1
            end if

            if (this%south_boundary .eqv. .true.) then
              southeast_con_neighbor = me + nimages - nx + 1
              if (southeast_con_neighbor > nimages) then
                southeast_con_neighbor = southeast_con_neighbor - nx
              end if
              southwest_con_neighbor = me + nimages - nx - 1
              if (modulo(southwest_con_neighbor,nx) == 0) then
                southwest_con_neighbor = nimages
              end if
              ! southwest_con_neighbor = - 2
            else  if (this%east_boundary .eqv. .true.) then
              southeast_con_neighbor = me - nx * 2 + 1
            else  if (this%west_boundary .eqv. .true.) then
              southwest_con_neighbor = me - 1
              ! southwest_con_neighbor = - 1
            end if
            this%northeast_boundary = .false.
            this%northwest_boundary = .false.
            this%southeast_boundary = .false.
            this%southwest_boundary = .false.

            ! --- handle up/down/left/right
            if (this%north_boundary .eqv. .true.) then
              north_con_neighbor = north_con_neighbor - n_images
              this%north_boundary = .false.
              this%wrapped_north = .true.
            end if
            if (this%south_boundary .eqv. .true.) then
              south_con_neighbor = south_con_neighbor + n_images
              this%south_boundary = .false.
              this%wrapped_south = .true.
            end if
            if (this%east_boundary .eqv. .true.) then
              east_con_neighbor = east_con_neighbor - grid%ximages
              this%east_boundary = .false.
              this%wrapped_east = .true.
            end if
            if (this%west_boundary .eqv. .true.) then
              west_con_neighbor = west_con_neighbor + grid%ximages
              this%west_boundary = .false.
              this%wrapped_west = .true.
            end if

            ! fix neighbors for when parcels are wrapped
            deallocate(neighbors)
            allocate(neighbors(8))
            neighbors = (/ &
                 north_con_neighbor, &
                 south_con_neighbor, &
                 east_con_neighbor, &
                 west_con_neighbor, &
                 northeast_con_neighbor, &
                 southeast_con_neighbor, &
                 northwest_con_neighbor, &
                 southwest_con_neighbor /)
          end if
        end associate
      end associate
    end if

  end procedure

  function bilinear_interpolation(x, x0, x1, y, y0, y1, &
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

  function trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
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

  module procedure total_num_parcels
    total_num_parcels = parcels_per_image * num_images()
  end procedure

  module procedure num_parcels_per_image
    num_parcels_per_image = parcels_per_image
  end procedure

  module procedure num_parcels_communicated
    if (count_p_comm .eqv. .true.) then
       call co_sum(parcels_communicated)
       num_parcels_communicated = parcels_communicated
    else
       num_parcels_communicated = -1
       print *, "WARNING: number of parcels communicated wasn't calculated"
    end if
  end procedure

  module procedure are_parcels_dry
    are_parcels_dry = dry_air_parcels
  end procedure

  module procedure get_wind_speed
    if (use_input_wind .eqv. .true.) then
      get_wind_speed = input_wind
    end if
  end procedure

  module procedure current_num_parcels
    integer :: buf_size, i
    buf_size = size(convection_obj%local)
    current_num_parcels = 0
    do i=1,buf_size
       if (convection_obj%local(i)%exists .eqv. .true.) &
            current_num_parcels = current_num_parcels + 1
    end do
  end procedure


  ! module procedure create_parcel
  !   integer :: index
  !   integer :: a
  !   a = -1
  !   ! print *, "hi!"
  ! end procedure create_parcel
  module procedure check_buf_size
    if (i .gt. parcels_per_image) then
       print *, this_image(), ": ERROR put buffer overflow"
       call exit
    end if
  end procedure check_buf_size


  module procedure initialize_from_file
    integer :: total_parcels
    logical :: parcel_is_dry
    real    :: wind_speed
    namelist/parcel_parameters/ total_parcels, parcel_is_dry, wind_speed

    character(len=*), parameter :: file = 'parcel-parameters.txt'
    integer :: unit, rc
    logical :: exists


    if (this_image() .eq. 1) &
         print *, "Initializing convection parcels from file"
    inquire(file=file, exist=exists)

    if (exists .neqv. .true.) then
       if (this_image() .eq. 1) &
            print*, trim(file), " does not exist, please create file"
       call exit
    end if

    unit = 10
    open(unit, file=file, action='read', status='old', iostat=rc)
    read(unit=unit, nml=parcel_parameters, iostat=rc)
    close(unit)

    parcels_per_image = nint(total_parcels / real(num_images()))
    local_buf_size = parcels_per_image * 4
    dry_air_parcels = parcel_is_dry
    input_wind = wind_speed
  end procedure

  !-----------------------------------------------------------------
  ! p = p_0 * e^( ((9.81/287.058)*dz) / t_mean )
  !-----------------------------------------------------------------
  ! Method one: physics
  !        a) change pressure         b) update temperature
  !-----------------------------------------------------------------
  module procedure dry_lapse_rate
  real, parameter :: gravity = 9.80665
  real ::  p0, T_original
  p0 = pressure
  pressure = p0 - z_displacement * &
       gravity / (287.05 * temperature) * p0

  T_original = temperature
  temperature = exner_function(pressure) * &
       potential_temp


  if (temperature /= temperature) then
     print *, this_image(), ":: ~~~~~~NAN ERROR~~~~~~", &
          p0, pressure, z_displacement, T_original, temperature, &
          exner_function(pressure), potential_temp
     call exit
  end if
  end procedure dry_lapse_rate
  !-----------------------------------------------------------------
  ! Method two: adiabatic lapse rate, not using
  !-----------------------------------------------------------------
  ! if (parcel%cloud_water .gt. 0.0) then
  !    gamma = 0.006 ! 6 celsius, not sure if this conversion is correct
  ! else
  !    gamma = 0.01 ! 10 celcius
  ! end if

end submodule
