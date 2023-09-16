program test_lt_parameters_namelist
  use options_interface, only : options_t, call_lt_parameters_namelist
  implicit none

  character(len=1024) :: filename
  type(options_t) :: opt

  call get_command_argument(1, filename)

  ! call options_interface%options_implementation%lt_parameters_namelist(filename, opt)
  ! call options_implementation%lt_parameters_namelist(filename, opt)

  opt%parameters%use_lt_options = .true.
  call call_lt_parameters_namelist(filename, opt)

  if (opt%lt_options%read_LUT .neqv. .true.) then
     error stop "lt_option%read_LUT is false, should be true"
  end if

  if (opt%lt_options%write_LUT .neqv. .true.) then
     error stop "lt_option%write_LUT is false, should be true"
  end if
end program test_lt_parameters_namelist
