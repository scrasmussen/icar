program colorado_benchmark_thompson
  !! Integration test with Thompson microphysics
  use icar_m, only : icar
  use assert_m, only : assert
  use domain_interface, only : domain_t
  implicit none
  integer exit_status

  type(domain_t) domain

  character(len=*), parameter :: restart_file="icar_gcm_test/restart/icar_rst_000001_2000-10-02_00-00-00.nc"

  call icar(domain)

  if (this_image()==1) then

    call execute_command_line( &
      command = "wget https://www.dropbox.com/s/xrhzec09yyaiui0/colorado_benchmark_thompson_restart.nc.zip"  , &
      wait = .true., &
      exitstat = exit_status &
    )
    call assert(exit_status==0,"colorado_benchmark_thompson: wget command")
      
    call execute_command_line( &
      command = "unzip colorado_benchmark_thompson_restart.nc.zip"  , &
      wait = .true., &
      exitstat = exit_status &
    )
    call assert(exit_status==0,"colorado_benchmark_thompson: unzip command")
      
    call execute_command_line( &
      command = "check.py " // restart_file // "colorado_benchmark_thompson_restart.nc"  , &
      wait = .true., &
      exitstat = exit_status &
    )
    call assert(exit_status==0, "colorado_benchmark_thompson: check.py")

  end if

end program
