module parcel_type_interface
implicit none
private
public :: parcel_t, REPLACE_PARCEL_T

integer, parameter :: REPLACE_PARCEL_T = -2
type parcel_t
    integer :: parcel_id = -1
    logical :: exists = .false.
    logical :: replace = .false.
    real :: lifetime
    real :: x, y, z
    real :: u, v, w
    real :: z_meters, z_interface
    real :: pressure, temperature, potential_temp
    real :: velocity, water_vapor, cloud_water
    real :: relative_humidity
    real :: buoyancy, buoyancy_multiplier
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
    to%replace  = from%replace
    from%replace  = .false.
    ! handle the to
    to%exists = .true.
    to%parcel_id = from%parcel_id
    from%parcel_id = -1
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
    print *, "replace", this%replace
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
    print *, "buoyancy", this%buoyancy
    print *, "buoyancy multiplier", this%buoyancy_multiplier
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
