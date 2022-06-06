submodule(parcel_interface) parcel_implementation
    use assertions_mod, only : assert, assertions
    ! use exchangeable_interface, only : exchangeable_t
    use mod_atm_utilities, only : pressure_at_elevation, exner_function, sat_mr

    ! use grid_interface, only : grid_t
    implicit none

    ! ----- PARAMETERS TO TUNE CONVECTION MODEL -----
    logical, parameter :: debug = .false.
    logical, parameter :: caf_comm_message = .true.
    logical, parameter :: parcel_create_message = .false.
    logical, parameter :: brunt_vaisala_data = .false.
    logical, parameter :: count_p_comm = .false.
    logical, parameter :: wrap_neighbors = .true.
    integer            :: parcels_communicated[*]
    integer            :: parcels_per_image
    integer            :: local_buf_size
    real               :: input_wind
    logical            :: dry_air_parcels
    integer            :: total_parcels
    ! integer, parameter :: local_buf_size=4*parcels_per_image
    ! -----------------------------------------------

    integer, parameter   :: default_halo_size=1
    integer, allocatable :: neighbors(:)
    integer       :: north_con_neighbor, south_con_neighbor, buf_size, halo_size
    integer       :: east_con_neighbor, west_con_neighbor
    integer       :: northeast_con_neighbor, northwest_con_neighbor
    integer       :: southeast_con_neighbor, southwest_con_neighbor

contains
    module procedure init_num_parcels
      ! compute number of parcels per image
      total_parcels = options%parcel_options%total_parcels
      this%total_parcel_count = total_parcels
      this%image_parcel_count = total_parcels / num_images()
      if (this_image() .le. mod(total_parcels, num_images())) then
          this%image_parcel_count = this%image_parcel_count + 1
      end if
      print *, this_image(), ": IMAGE PARCEL COUNT", this%image_parcel_count
      ! call backtrace()
    end procedure

    module procedure init_position
      integer :: i, seed(34)

      if (options%physics%convection /= 4) then
          return
      end if
      call this%init_num_parcels(options)
      print *, "===================== PARCEL INIT_POSITION ================="

      ! allocate boundary regions
      ! if (allocated(this%local)) deallocate(this%local)
      if (associated(this%local)) then
          deallocate(this%local)
          nullify(this%local)
      end if
      this%north_boundary = (grid%yimg == grid%yimages)
      this%south_boundary = (grid%yimg == 1)
      this%east_boundary  = (grid%ximg == grid%ximages)
      this%west_boundary  = (grid%ximg == 1)

      ! print *, " NEED TO FIX THIS HACK", this%image_parcel_count
      ! this%image_parcel_count = options%parcel_options%total_parcels
      print *, " ALLOCATING", this%image_parcel_count, "PARCELS"
      allocate(this%local(this%image_parcel_count * 4))

      ! setup random number generator for parcel location
      seed = -1
      call random_seed(PUT=seed)
      ! call random_init(.true.,.true.)

      do i=1,this%image_parcel_count
          call this%create_parcel_id()
          ! print *, " PARCEL ID COUNT", this%parcel_id_count
          this%local(i) = create_empty_parcel(this%parcel_id_count, grid)
      end do

      buf_size = this%image_parcel_count
      allocate( this%image_np[*])
      allocate( this%buf_north_in(buf_size)[*])
      allocate( this%buf_south_in(buf_size)[*])
      allocate( this%buf_east_in(buf_size)[*])
      allocate( this%buf_west_in(buf_size)[*])
      allocate( this%buf_northeast_in(buf_size)[*])
      allocate( this%buf_northwest_in(buf_size)[*])
      allocate( this%buf_southeast_in(buf_size)[*])
      allocate( this%buf_southwest_in(buf_size)[*])

      call this%setup_neighbors(grid)
      print *, "Total number of convected air parcels:", &
          options%parcel_options%total_parcels, "and", this_image(), "has",&
          this%image_parcel_count
    end procedure


    ! --- Create an empty parcel
    module procedure create_empty_parcel
    real :: random_start(3), x, z, y

    call random_number(random_start)
    x = grid%its + (random_start(1) * (grid%ite-grid%its))
    z = grid%kts + (random_start(3) * (grid%kte-grid%kts))
    y = grid%jts + (random_start(2) * (grid%jte-grid%jts))

    if (x .lt. grid%its .or. &
        x .gt. grid%ite .or. &
        z .lt. grid%kts .or. &
        z .gt. grid%kte .or. &
        y .lt. grid%jts .or. &
        y .gt. grid%jte) then
        print *, "x:", grid%its, "<", x, "<", grid%ite
        print *, "z:", grid%kts, "<", z, "<", grid%kte
        print *, "y:", grid%jts, "<", y, "<", grid%jte
        stop "PARCEL: x,y,z is out of bounds"
    end if

    ! filler values, the rest are set later
    parcel = parcel_t( &
        parcel_id, .true., 0, 0.0, &
        x, y, z, 0.0, &
        0.0, 0.0, 0.0, 0.0, &
        0.0, 0.0, 0.0, 0.0, &
        0.0, 0.0, 0.0, 0.0, &
        0.0)
    end procedure


    module procedure move_if_needed
    integer :: its, ite, kts, kte, jts, jte
    real :: x, y, z, xx, yy
    !-----------------------------------------------------------------
    ! Move parcel if needed
    !-----------------------------------------------------------------
    x = parcel%x; y = parcel%y; z = parcel%z
    its = grid%its; ite = grid%ite
    kts = grid%kts; kte = grid%kte
    jts = grid%jts; jte = grid%jte

    if (caf_comm_message .eqv. .true.) &
        call parcel%caf_comm_message(grid)

    xx = x
    yy = y
    ! If parcel is getting wrapped the x and y values need to be
    ! properly updated
    if (wrap_neighbors .eqv. .true.) then
        if (x > grid%nx_global) then
            ! if (caf_comm_message .eqv. .true.) print *, "WRAPPED" &
            !     , parcel%parcel_id
            x = x - grid%nx_global + 1
            xx = xx + 2
        else if (x < 1) then
            ! if (caf_comm_message .eqv. .true.) print *, "WRAPPED" &
            !     , parcel%parcel_id
            x = x + grid%nx_global - 1
            xx = xx - 2
        end if

        if (y > grid%ny_global) then
            ! if (caf_comm_message .eqv. .true.) print *, "WRAPPED" &
            !     , parcel%parcel_id
            y = y - grid%ny_global + 1
            yy = yy + 2
        else if (y < 1) then
            ! if (caf_comm_message .eqv. .true.) print *, "WRAPPED" &
            !     , parcel%parcel_id
            y = y + grid%ny_global - 1
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
    end procedure

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

    call this%check_buf_size(this%south_i)
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

    call this%check_buf_size(this%north_i)
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

    call this%check_buf_size(this%west_i)
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

    call this%check_buf_size(this%east_i)
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

    call this%check_buf_size(this%southwest_i)
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

    call this%check_buf_size(this%southeast_i)
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

    call this%check_buf_size(this%northwest_i)
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

    call this%check_buf_size(this%northeast_i)
    !dir$ pgas defer_sync
    this%buf_northeast_in(this%northeast_i)[southwest_con_neighbor] = parcel
    parcel%exists = .false.
    this%northeast_i = this%northeast_i + 1
  end procedure


  module procedure create_parcel_id
    use iso_fortran_env, only : int32
    integer :: id_range, h
    if (this%parcel_id_count .eq. -1) then
      id_range = huge(int32) / num_images()
      this%parcel_id_count = (this_image()-1) * id_range
    else
      this%parcel_id_count = this%parcel_id_count + 1
    end if
  end procedure

  module procedure setup_neighbors
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
    integer :: buf_size_l, i
    ! allocate(image_np)
    buf_size_l = size(this%local)
    do i=1,buf_size_l
        if (this%local(i)%exists .eqv. .true.) &
            this%image_np = this%image_np + 1
    end do

    if (this_image() .eq. 1) then
        total_num_parcels = this%image_np
        do i=2,num_images()
            total_num_parcels = total_num_parcels + this%image_np[i]
        end do
    else
        total_num_parcels = -1
    end if
    sync all
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


  ! return current images number of parcels
  module procedure image_num_parcels
    integer :: buf_size_l, i
    buf_size_l = size(this%local)
    do i=1,buf_size_l
       if (this%local(i)%exists .eqv. .true.) &
            image_num_parcels = image_num_parcels + 1
    end do
  end procedure image_num_parcels


  module procedure check_buf_size
    if (i .gt. this%image_parcel_count) then
       print *, this_image(), ": ERROR put buffer overflow"
       call exit
    end if
  end procedure check_buf_size

  ! Check if parcel is out of the image's boundary
  module procedure parcel_bounds_check
  if (parcel%x .lt. grid%its-1 .or. parcel%z .lt. grid%kts-1 .or. &
      parcel%y .lt. grid%jts-1 .or. parcel%x .gt. grid%ite+1 .or. &
      parcel%z .gt. grid%kte+1 .or. parcel%y .gt. grid%jte+1) then
      print *, "parcel", parcel%parcel_id, "on image", this_image()
      print *, "x:", grid%its, "<", parcel%x, "<", grid%ite, "with halo 2"
      print *, "z:", grid%kts, "<", parcel%z, "<", grid%kte, "with halo 2"
      print *, "y:", grid%jts, "<", parcel%y, "<", grid%jte, "with halo 2"
      stop "CU_PARCEL: x,y,z is out of bounds"
  end if
  end procedure parcel_bounds_check


  ! Write Brunt-Vaisala Frequency data to file
  module procedure write_bv_data
  integer :: image, me, i
  logical :: exist
  character(len=32) :: filename
  me = this_image()
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
  end procedure write_bv_data

  ! module subroutine set_outputdata(this, metadata)
  !   implicit none
  !   class(exchangeable_t), intent(inout)  :: this
  !   type(variable_t),      intent(in),    :: metadata

  !   if (present(metadata)) then
  !       this%meta_data = metadata
  !   endif

  !   this%meta_data%data_3d => this%data_3d
  !   this%meta_data%three_d = .True.

  !   if (.not.allocated(this%meta_data%dim_len)) allocate(this%meta_data%dim_len(3))
  !   this%meta_data%dim_len(1) = size(this%data_3d,1)
  !   this%meta_data%dim_len(2) = size(this%data_3d,2)
  !   this%meta_data%dim_len(3) = size(this%data_3d,3)

  ! end subroutine


end submodule ! parcel_implementation
