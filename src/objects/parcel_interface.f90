! --- Module Parcel Interface: defines parcel memory structure and movement
module parcel_interface
  use grid_interface, only : grid_t
  use options_interface, only : options_t
  use iso_c_binding, only : c_int !
  use parcel_type_interface, only : parcel_t
  use variable_interface,    only : variable_t

  implicit none

  private
  public :: exchangeable_parcel, &
       total_num_parcels,  &
       num_parcels_communicated, check_buf_size, &
       image_num_parcels, create_empty_parcel, write_bv_data
  type exchangeable_parcel
     private
     ! type(parcel_t), allocatable, public :: data_1d(:) => null()
     integer                             :: parcel_id_count = -1
     integer, public                     :: max_parcel_count
     integer, public                     :: image_parcel_count
     integer, public                     :: total_parcel_count

     integer, public, allocatable        :: image_np[:]
     ! type(parcel_t), allocatable, public, target :: local(:)
     type(parcel_t), pointer, public :: local(:) => null()
     type(variable_t),  public :: meta_data
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
     procedure, public :: init_num_parcels
     procedure, public :: image_num_parcels
     procedure, public :: move_if_needed
     procedure, public :: parcel_bounds_check
     procedure, public :: write_bv_data
     procedure, public :: check_buf_size

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

     module subroutine init_num_parcels(this, options)
       class(exchangeable_parcel), intent(inout) :: this
       type(options_t), intent(in) :: options
     end subroutine

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

     module function total_num_parcels(this)
       class(exchangeable_parcel), intent(inout) :: this
       integer :: total_num_parcels
     end function total_num_parcels

     module function image_num_parcels(this)
       class(exchangeable_parcel), intent(in) :: this
       integer :: image_num_parcels
     end function image_num_parcels

     module function are_parcels_dry()
       logical :: are_parcels_dry
     end function are_parcels_dry

     module function num_parcels_communicated()
       integer :: num_parcels_communicated
     end function num_parcels_communicated

     module function get_wind_speed()
       real :: get_wind_speed
     end function get_wind_speed

     module subroutine check_buf_size(this,i)
       class(exchangeable_parcel), intent(in) :: this
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
         timestep, buf_size_i)
       class(exchangeable_parcel), intent(in) :: this
       integer, intent(in) :: bv_i, timestep, buf_size_i
       real, intent(in) :: bv(buf_size_i)
       integer, intent(in) :: parcel_id(buf_size_i)
     end subroutine

     module subroutine dry_lapse_rate(pressure, temperature, potential_temp, &
          z_displacement)
       real, intent(inout) :: pressure, temperature, potential_temp
       real, intent(in) :: z_displacement
     end subroutine
 end interface
end module parcel_interface
