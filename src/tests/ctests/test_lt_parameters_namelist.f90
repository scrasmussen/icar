program test_lt_parameters_namelist
  ! use options_implementation !, only : lt_parameters_namelist
  ! use options_interface, only : options_t, options_implementation   !lt_parameters_namelist, options_t
  use options_interface, only : options_t, call_lt_parameters_namelist
  implicit none

  character(len=:), allocatable :: filename
  type(options_t) :: options

  filename = "lt_options.nml"

  print *, "opening ", filename

  ! call options_interface%options_implementation%lt_parameters_namelist(filename, options)
  ! call options_implementation%lt_parameters_namelist(filename, options)
  call call_lt_parameters_namelist(filename, options)


  print *, "fin"
end program test_lt_parameters_namelist
