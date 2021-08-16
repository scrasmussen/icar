submodule(parcel_interface) parcel_implementation
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
                its, ite, kts, kte, jts, jte, ims, ime, kms, kme, jms, jme)!, &
!                z_m, potential_temp, z_interface, pressure, u_in, v_in, w_in)

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
    end procedure

    module procedure create_parcel
        real :: random_start(3), x, z, y

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

        ! x0 = floor(x); z0 = floor(z); y0 = floor(y);
        ! x1 = ceiling(x); z1 = ceiling(z); y1 = ceiling(y);

        ! associate (A => z_m)
        !     z_meters = trilinear_interpolation(x, x0, x1, z, z0, z1, y, y0, y1, &
        !         A(x0,z0,y0), A(x0,z0,y1), A(x0,z1,y0), A(x1,z0,y0), &
        !         A(x0,z1,y1), A(x1,z0,y1), A(x1,z1,y0), A(x1,z1,y1))
        ! end associate


        parcel = parcel_t(parcel_id, .true., &
            0, x, y, z, &
            ! filler values, the rest are set later
            0.0, 0.0, 0.0, 0.0,  &
            0.0, 0.0, 0.0, 0.0,  &
            0.0, 0.0, 0.0, 0.0)
        ! u_val, v_val, w_val, z_meters, z_interface_val, &
        !     pressure_val, temp_val, theta_val, velocity, water_vapor_val, &
        !     cloud_water, relative_humidity_in)
    end procedure


    !         !-----------------------------------------------------------------
    !         ! Move parcel if needed
    !         !-----------------------------------------------------------------
    !         associate (x => parcel%x, y => parcel%y, z => parcel%z)
    !             if (caf_comm_message .eqv. .true.) then
    !                 if (  x .lt. its-1 .or. x .gt. ite+1 .or. &
    !                     z .lt. kms-1 .or. z .gt. kme   .or. &
    !                     y .lt. jts-1 .or. y .gt. jte+1 .or. &
    !                     x .lt. 1 .or. x .gt. nx_global .or. &
    !                     y .lt. 1 .or. y .gt. ny_global &
    !                     ) then
    !                     print *, "PUTTING", parcel%x, parcel%y, parcel%z_meters, &
    !                         "FROM", this_image(), "id:", parcel%parcel_id, &
    !                         "M", ims,ime, kms, kme, jms, jme, "T", its,ite,jts,jte
    !                 end if
    !             end if

    !             xx = x
    !             yy = y
    !             ! If parcel is getting wrapped the x and y values need to be
    !             ! properly updated
    !             if (wrap_neighbors .eqv. .true.) then
    !                 if (x > nx_global) then
    !                     ! if (caf_comm_message .eqv. .true.) print *, "WRAPPED" &
    !                     !     , parcel%parcel_id
    !                     x = x - nx_global + 1
    !                     xx = xx + 2
    !                 else if (x < 1) then
    !                     ! if (caf_comm_message .eqv. .true.) print *, "WRAPPED" &
    !                     !     , parcel%parcel_id
    !                     x = x + nx_global - 1
    !                     xx = xx - 2
    !                 end if

    !                 if (y > ny_global) then
    !                     ! if (caf_comm_message .eqv. .true.) print *, "WRAPPED" &
    !                     !     , parcel%parcel_id
    !                     y = y - ny_global + 1
    !                     yy = yy + 2
    !                 else if (y < 1) then
    !                     ! if (caf_comm_message .eqv. .true.) print *, "WRAPPED" &
    !                     !     , parcel%parcel_id
    !                     y = y + ny_global - 1
    !                     yy = yy - 2
    !                 end if
    !             end if

    !             ! Check values to know where to send parcel
    !             if (yy .lt. jts-1) then      ! "jts  <   y    <  jte"
    !                 if (xx .lt. its-1) then
    !                     call this%put_southwest(parcel)
    !                     if (count_p_comm .eqv. .true.) &
    !                         parcels_communicated = parcels_communicated + 1
    !                 else if (xx .gt. ite+1) then
    !                     call this%put_southeast(parcel)
    !                     if (count_p_comm .eqv. .true.) &
    !                         parcels_communicated = parcels_communicated + 1
    !                 else
    !                     call this%put_south(parcel)
    !                     if (count_p_comm .eqv. .true.) &
    !                         parcels_communicated = parcels_communicated + 1
    !                 end if
    !             else if (yy .gt. jte+1) then ! jts will be 1
    !                 if (xx .lt. its-1) then
    !                     call this%put_northwest(parcel)
    !                     if (count_p_comm .eqv. .true.) &
    !                         parcels_communicated = parcels_communicated + 1
    !                 else if (xx .gt. ite+1) then
    !                     call this%put_northeast(parcel)
    !                     if (count_p_comm .eqv. .true.) &
    !                         parcels_communicated = parcels_communicated + 1
    !                 else
    !                     call this%put_north(parcel)
    !                     if (count_p_comm .eqv. .true.) &
    !                         parcels_communicated = parcels_communicated + 1
    !                 endif
    !             else if (xx .lt. its-1) then ! "its  <   x    <  ite"
    !                 call this%put_west(parcel) ! need to double check this!
    !                 if (count_p_comm .eqv. .true.) &
    !                     parcels_communicated = parcels_communicated + 1
    !             else if (xx .gt. ite+1) then
    !                 call this%put_east(parcel)
    !                 if (count_p_comm .eqv. .true.) &
    !                     parcels_communicated = parcels_communicated + 1
    !             end if
    !         end associate
    !         end if
    !         end associate
    !     end do

    !     if (brunt_vaisala_data .eqv. .true.) then
    !         do image=1,num_images()
    !             if (me .eq. image) then
    !                 write (filename,"(A17)") "brunt_vaisala.txt"
    !                 inquire(file=filename, exist=exist)
    !                 if (exist) then
    !                     open(unit=me, file=filename, status='old', position='append')
    !                 else
    !                     open(unit=me, file=filename, status='new')
    !                 end if
    !                 do i=1,bv_i-1
    !                     write(me,*) me, timestep, parcel_id(i), bv(i)
    !                 end do
    !                 close(me)
    !             end if
    !             sync all
    !         end do

    !     end if
    !     if (debug .eqv. .true.) print *, "============= process done ==============="
    ! end procedure

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

end submodule
