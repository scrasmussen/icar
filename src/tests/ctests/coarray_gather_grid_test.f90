program coarray_gather_grid_test
  use array_utilities, only: gather_coarray_3d_grid
  use grid_interface, only: grid_t
  implicit none
  integer, parameter :: n=2
  real, allocatable :: A(:,:,:)[:], B(:)
  integer :: i, j, k, m, image, gather_image
  type(grid_t) :: grid

  m = num_images()

  allocate(A(n,n,m)[*])
  allocate(B(n*n*m))

  A = 0
  B = 0

  image = this_image()
  do concurrent (i=1:n, j=1:n, k=image:image)
     A(i,j,k) = (i-1) + (j-1) * n + (k-1) * n * n
  end do
  do i=1,n*n*m
     B(i) = i-1
  end do

  do i=1,num_images()
     if (i == this_image()) then
        print *, image, ": PRE =", A
     end if
     sync all
  end do

  grid%ims = 1
  grid%ime = n
  grid%kms = 1
  grid%kme = n
  grid%jms = image
  grid%jme = image

  gather_image = 1
  call gather_coarray_3d_grid(A, grid, main_image_arg=gather_image)
  sync all

  if (image == gather_image) then
     if (.not. all(pack(A, .true.) == B)) then
        error stop "A != B on gather image"
     end if
  end if
end program coarray_gather_grid_test
