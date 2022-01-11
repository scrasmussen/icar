program colorado_benchmark_thompson
  !! Integration test with Thompson microphysics
  use icar_m, only : icar
  use assert_m, only : assert
  use domain_interface, only : domain_t
  implicit none
  integer exit_status

  type(domain_t) domain

  call icar(domain)
  
end program
