module parcel_type_interface
implicit none
private
public :: parcel_t

type parcel_t
    integer :: parcel_id
    logical :: exists = .false.
    integer :: moved
    real :: x, y, z
    real :: u, v, w
    real :: z_meters, z_interface
    real :: pressure, temperature, potential_temp
    real :: velocity, water_vapor, cloud_water
    real :: relative_humidity
    contains
    procedure :: move_to
    procedure :: print_parcel
    procedure :: caf_comm_message
end type parcel_t
contains
  subroutine move_to(from,to)
    class(parcel_t), intent(inout) :: from
    type(parcel_t), intent(inout)  :: to
    ! handle the from
    from%exists = .false.
    to%moved  = from%moved +1
    from%moved  = 0
    ! handle the to
    to%exists = .true.
    to%parcel_id = from%parcel_id
    to%x = from%x; to%y = from%y; to%z = from%z
    to%u = from%u; to%v = from%v; to%w = from%w
    to%z_meters = from%z_meters
    to%z_interface = from%z_interface
    to%pressure = from%pressure
    to%temperature = from%temperature
    to%potential_temp = from%potential_temp
    to%velocity = from%velocity
    to%water_vapor = from%water_vapor
    to%cloud_water = from%cloud_water
    to%relative_humidity = from%relative_humidity
  end subroutine move_to

  subroutine print_parcel(this)
    class(parcel_t), intent(inout) :: this
    print *, "--- parcel", this%parcel_id, "---"
    print *, "exists", this%exists
    print *, "moved", this%moved
    print *, "x,y,z", this%x, this%y, this%z
    print *, "u,v,w", this%u, this%v, this%w
    print *, "z_meters", this%z_meters
    print *, "z_interface", this%z_interface
    print *, "pressure", this%pressure
    print *, "temperature", this%temperature
    print *, "potential_temp", this%potential_temp
    print *, "velocity", this%velocity
    print *, "water_vapor", this%water_vapor
    print *, "cloud_water", this%cloud_water
    print *, "relative_humidity", this%relative_humidity
    print *, "------"
  end subroutine print_parcel

  subroutine caf_comm_message(parcel, grid)
    use grid_interface, only : grid_t
    class(parcel_t), intent(in) :: parcel
    type(grid_t), intent(in) :: grid
    if (parcel%x .lt. grid%its-1 .or. parcel%x .gt. grid%ite+1 .or. &
        parcel%z .lt. grid%kms-1 .or. parcel%z .gt. grid%kme   .or. &
        parcel%y .lt. grid%jts-1 .or. parcel%y .gt. grid%jte+1 .or. &
        parcel%x .lt. 1 .or. parcel%x .gt. grid%nx_global .or. &
        parcel%y .lt. 1 .or. parcel%y .gt. grid%ny_global &
        ) then
    print *, "PUTTING", parcel%x, parcel%y, parcel%z_meters, &
        "FROM", this_image(), "id:", parcel%parcel_id, &
        "M", grid%ims, grid%ime, grid%kms, grid%kme, grid%jms, grid%jme, &
        "T", grid%its, grid%ite, grid%jts, grid%jte
    end if
  end subroutine caf_comm_message

  ! not being used right now
  function constructor() result(this)
      type(parcel_t) :: this
  end function constructor
end module parcel_type_interface



module parcel_interface
  use grid_interface, only : grid_t
  use options_interface, only : options_t
  use iso_c_binding, only : c_int !
  use parcel_type_interface, only : parcel_t
  use exchangeable_interface, only : exchangeable_t

  implicit none

  private
  public :: exchangeable_parcel, &
       total_num_parcels, are_parcels_dry, &
       num_parcels_communicated, get_wind_speed, check_buf_size, &
       current_num_parcels, create_empty_parcel, get_image_parcel_count, &
       write_bv_data
  type exchangeable_parcel
     private
     ! type(parcel_t), allocatable, public :: data_1d(:) => null()
     integer                             :: parcel_id_count = -1
     integer, public                     :: max_parcel_count
     type(parcel_t), allocatable, public :: local(:)
     type(parcel_t), allocatable :: buf_north_in(:)[:]
     type(parcel_t), allocatable :: buf_south_in(:)[:]
     type(parcel_t), allocatable :: buf_east_in(:)[:]
     type(parcel_t), allocatable :: buf_west_in(:)[:]
     type(parcel_t), allocatable :: buf_northeast_in(:)[:]
     type(parcel_t), allocatable :: buf_northwest_in(:)[:]
     type(parcel_t), allocatable :: buf_southeast_in(:)[:]
     type(parcel_t), allocatable :: buf_southwest_in(:)[:]

     logical :: north_boundary=.false.
     logical :: south_boundary=.false.
     logical :: east_boundary=.false.
     logical :: west_boundary=.false.
     logical :: northeast_boundary=.false.
     logical :: northwest_boundary=.false.
     logical :: southeast_boundary=.false.
     logical :: southwest_boundary=.false.
     logical :: wrapped_north=.false.
     logical :: wrapped_south=.false.
     logical :: wrapped_east=.false.
     logical :: wrapped_west=.false.

     integer :: north_i=1
     integer :: south_i=1
     integer :: east_i=1
     integer :: west_i=1
     integer :: northeast_i=1
     integer :: northwest_i=1
     integer :: southeast_i=1
     integer :: southwest_i=1


   contains
     private
     procedure, public :: send
     procedure, public :: retrieve
     procedure, public :: exchange
     ! procedure, public :: process  ! see physics
     ! procedure, public :: parcels_init
     ! procedure, public :: convect_const
     ! generic,   public :: initialize=>convect_const
     procedure, public :: init_position
     procedure, public :: get_image_parcel_count
     procedure, public :: move_if_needed
     procedure, public :: parcel_bounds_check
     procedure, public :: write_bv_data

     procedure :: put_north
     procedure :: put_south
     procedure :: put_east
     procedure :: put_west
     procedure :: put_northeast
     procedure :: put_northwest
     procedure :: put_southeast
     procedure :: put_southwest
     procedure :: retrieve_buf
     procedure :: create_parcel_id
     procedure :: setup_neighbors
  end type exchangeable_parcel


  interface
     ! module subroutine parcels_init(domain, options)
     !   type(domain_t),  intent(inout) :: domain
     !   type(options_t), intent(in) :: options
     ! end module subroutine

     ! module subroutine process(this, nx_global, ny_global, &
     !     ims, ime, kms, kme, jms, jme, dt, dz, temperature, z_interface, &
     !     its, ite, kts, kte, jts, jte, z_m, potential_temp, pressure, u_in, &
     !     v_in, w_in, timestep)
     !   implicit none
     !   class(exchangeable_parcel), intent(inout) :: this
     !   integer, intent(in) :: nx_global, ny_global
     !   real, intent(in)    :: dt, dz
     !   integer, intent(in) :: ims, ime, kms, kme, jms, jme
     !   integer, intent(in) :: its, ite, kts, kte, jts, jte
     !   real, intent(in) :: temperature(ims:ime,kms:kme,jms:jme)
     !   real, intent(in) :: z_interface(ims:ime,jms:jme)
     !   real, intent(in) :: pressure(ims:ime,kms:kme,jms:jme)
     !   real, intent(in) :: z_m(ims:ime,kms:kme,jms:jme)
     !   class(exchangeable_t), intent(in) :: potential_temp
     !   class(exchangeable_t), intent(in), optional :: u_in, v_in, w_in
     !   integer, intent(in), optional :: timestep
     ! end subroutine

     module subroutine init_position(this, options, grid)
       class(exchangeable_parcel), intent(inout) :: this
       type(options_t), intent(in) :: options
       type(grid_t),    intent(in) :: grid
     end subroutine

     ! module subroutine convect_const(this, potential_temp, u_in, v_in, w_in, grid, z_m, &
     !     z_interface, ims, ime, kms, kme, jms, jme, dz_val, &
     !     its, ite, kts, kte, jts, jte, pressure, input_buf_size, halo_width)
     !   class(exchangeable_parcel), intent(inout) :: this
     !   class(exchangeable_t), intent(in)    :: potential_temp
     !   class(exchangeable_t), intent(in)    :: u_in, v_in, w_in
     !   type(grid_t), intent(in)      :: grid
     !   real, intent(in)              :: z_m(ims:ime,kms:kme,jms:jme)
     !   real, intent(in)              :: pressure(ims:ime,kms:kme,jms:jme)
     !   real, intent(in)              :: z_interface(ims:ime,jms:jme)
     !   integer, intent(in)           :: ims, ime, kms, kme, jms, jme
     !   integer, intent(in)           :: its, ite, kts, kte, jts, jte
     !   real, intent(in)              :: dz_val
     !   integer, intent(in), optional :: input_buf_size
     !   integer, intent(in), optional :: halo_width
     ! end subroutine

     module subroutine move_if_needed(this, parcel, grid)
       class(exchangeable_parcel), intent(inout) :: this
       type(parcel_t), intent(inout) :: parcel
       type(grid_t), intent(in) :: grid
     end subroutine

     module subroutine send(this)
       class(exchangeable_parcel), intent(inout) :: this
     end subroutine

     module subroutine load_buf(this, parcel)
       class(exchangeable_parcel), intent(inout) :: this
       type(parcel_t), intent(inout) :: parcel
     end subroutine

     module subroutine retrieve_buf(this, buf)
       class(exchangeable_parcel), intent(inout) :: this
       type(parcel_t), intent(inout) :: buf(:)[*]
     end subroutine

     module subroutine retrieve(this, no_sync)
       class(exchangeable_parcel), intent(inout) :: this
       logical,               intent(in),   optional :: no_sync
     end subroutine

     module subroutine exchange(this)
       class(exchangeable_parcel), intent(inout) :: this
     end subroutine

     module subroutine put_north(this, parcel)
       class(exchangeable_parcel), intent(inout) :: this
       type(parcel_t), intent(inout) :: parcel
     end subroutine

     module subroutine put_south(this, parcel)
       class(exchangeable_parcel), intent(inout) :: this
       type(parcel_t), intent(inout) :: parcel
     end subroutine

     module subroutine put_east(this, parcel)
       class(exchangeable_parcel), intent(inout) :: this
       type(parcel_t), intent(inout) :: parcel
     end subroutine

     module subroutine put_west(this, parcel)
       class(exchangeable_parcel), intent(inout) :: this
       type(parcel_t), intent(inout) :: parcel
     end subroutine

     module subroutine put_northeast(this, parcel)
       class(exchangeable_parcel), intent(inout) :: this
       type(parcel_t), intent(inout) :: parcel
     end subroutine

     module subroutine put_northwest(this, parcel)
       class(exchangeable_parcel), intent(inout) :: this
       type(parcel_t), intent(inout) :: parcel
     end subroutine

     module subroutine put_southeast(this, parcel)
       class(exchangeable_parcel), intent(inout) :: this
       type(parcel_t), intent(inout) :: parcel
     end subroutine

     module subroutine put_southwest(this, parcel)
       class(exchangeable_parcel), intent(inout) :: this
       type(parcel_t), intent(inout) :: parcel
     end subroutine

     module subroutine create_parcel_id(this)
       class(exchangeable_parcel), intent(inout) :: this
     end subroutine

     module subroutine setup_neighbors(this,grid)
       class(exchangeable_parcel), intent(inout) :: this
       type(grid_t), intent(in) :: grid
     end subroutine

     module subroutine parcel_bounds_check(this, parcel, grid)
       class(exchangeable_parcel), intent(in) :: this
       type(parcel_t), intent(in) :: parcel
       type(grid_t), intent(in) :: grid
     end subroutine parcel_bounds_check

     module function total_num_parcels()
       integer :: total_num_parcels
     end function total_num_parcels

     module function get_image_parcel_count(this)
       class(exchangeable_parcel), intent(inout) :: this
       integer :: get_image_parcel_count
     end function get_image_parcel_count

     module function are_parcels_dry()
       logical :: are_parcels_dry
     end function are_parcels_dry

     module function num_parcels_communicated()
       integer :: num_parcels_communicated
     end function num_parcels_communicated

     module function get_wind_speed()
       real :: get_wind_speed
     end function get_wind_speed

     module subroutine check_buf_size(i)
       integer, intent(in) :: i
     end subroutine check_buf_size

     module function current_num_parcels(convection_obj)
       integer :: current_num_parcels
       type(exchangeable_parcel),intent(in) :: convection_obj
     end function current_num_parcels

     module function create_empty_parcel(parcel_id, grid)  result(parcel)
     ! its, ite, kts, kte, jts, jte,&
     !     ims, ime, kms, kme, jms, jme)
     ! , z_m, potential_temp, z_interface, &
     !     pressure, u_in, v_in, w_in, times_moved)
       integer :: parcel_id
       type(parcel_t) :: parcel
       type(grid_t)   :: grid
       ! integer, intent(in)           :: its, ite, kts, kte, jts, jte
       ! integer, intent(in)           :: ims, ime, kms, kme, jms, jme
       ! real, intent(in)              :: z_m(ims:ime,kms:kme,jms:jme)
       ! real, intent(in)              :: pressure(ims:ime,kms:kme,jms:jme)
       ! class(exchangeable_t), intent(in) :: potential_temp
       ! real, intent(in)              :: z_interface(ims:ime,jms:jme)
       ! class(exchangeable_t), intent(in)    :: u_in, v_in, w_in
       ! integer, intent(in), optional :: times_moved
     end function

     module subroutine initialize_from_file()
     end subroutine

     module subroutine write_bv_data(this, bv, bv_i, parcel_id, &
         timestep, buf_size)
       class(exchangeable_parcel), intent(in) :: this
       real, intent(in) :: bv(buf_size)
       integer, intent(in) :: parcel_id(buf_size)
       integer, intent(in) :: bv_i, timestep, buf_size
     end subroutine

     module subroutine dry_lapse_rate(pressure, temperature, potential_temp, &
          z_displacement)
       real, intent(inout) :: pressure, temperature, potential_temp
       real, intent(in) :: z_displacement
     end subroutine
 end interface
end module parcel_interface
