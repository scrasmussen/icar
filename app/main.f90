program main
  use icar_m, only : icar
  use domain_interface, only : domain_t
  implicit none

  type(domain_t) domain

  call icar(domain)
  
end program main
