module module_cu_parcel
    use variable_interface,     only : variable_t
    use parcel_interface,       only : exchangeable_parcel
    use exchangeable_interface, only : exchangeable_t
    use grid_interface,         only : grid_t
    implicit none
    public :: cu_parcel, cu_parcel_physics, cu_parcel_init
    private
    interface cu_parcel
        module procedure cu_parcel_init, cu_parcel_physics
    end interface cu_parcel
contains

    subroutine cu_parcel_init(this, grid, &
        ims, ime, kms, kme, &
        jms, jme, its, ite, &
        kts, kte, jts, jte, &
        z_interface, z_m, potential_temp, pressure, &
        u_in, v_in, w_in, dz_val, &
        input_buf_size, halo_width)
       class(exchangeable_parcel), intent(inout) :: this
       class(exchangeable_t), intent(in)    :: potential_temp
       class(exchangeable_t), intent(in)    :: u_in, v_in, w_in
       type(grid_t), intent(in)      :: grid
       type(variable_t), intent(in)  :: z_m
       type(variable_t), intent(in)  :: pressure
       type(variable_t), intent(in)  :: z_interface
       integer, intent(in)           :: ims, ime, kms, kme, jms, jme
       integer, intent(in)           :: its, ite, kts, kte, jts, jte
       type(variable_t), intent(in)  :: dz_val
       integer, intent(in), optional :: input_buf_size
       integer, intent(in), optional :: halo_width

        print *, "ARTLESS AIR_PARCEL INIT"

    end subroutine cu_parcel_init


    ! subroutine cu_parcel_physics(foo)
    ! old `process`
    subroutine cu_parcel_physics(this, nx_global, ny_global, &
        ims, ime, kms, kme, &
        jms, jme, its, ite, &
        kts, kte, jts, jte, &
        z_interface, z_m, temperature, potential_temp, &
        pressure, u_in, v_in, w_in, &
        dt, dz, timestep)
    implicit none
    class(exchangeable_parcel), intent(inout) :: this
    integer, intent(in) :: nx_global, ny_global
    integer, intent(in) :: ims, ime, kms, kme, jms, jme
    integer, intent(in) :: its, ite, kts, kte, jts, jte
    type(variable_t), intent(in) :: temperature
    class(exchangeable_t), intent(in) :: potential_temp
    type(variable_t), intent(in) :: pressure
    type(variable_t), intent(in) :: z_interface
    type(variable_t), intent(in) :: z_m
    class(exchangeable_t), intent(in), optional :: u_in, v_in, w_in
    real, intent(in)    :: dt, dz
    integer, intent(in), optional :: timestep

    integer :: foo
        print *, "ARTLESS AIR_PARCEL PHYSICS :: ENTERING"
    end subroutine cu_parcel_physics

end module module_cu_parcel
