program coarray_scatter_index_test
  use array_utilities, only: scatter_coarray_3d_grid
  use grid_interface, only: grid_t
  implicit none
  integer, parameter :: n=2
  real, allocatable :: A(:,:,:)[:], B(:,:,:)
  integer :: i, j, k, m, image, scatter_image
  type(grid_t) :: grid

  m = num_images()

  allocate(A(n,n,m)[*])
  allocate(B(n,n,m))

  A = 0
  B = 0

  scatter_image = 1
  image = this_image()
  if (image == scatter_image) then
     do concurrent (i=1:n, j=1:n, k=1:m)
        A(i,j,k) = (i-1) + (j-1) * n + (k-1) * n * n
     end do
  end if
  do concurrent (i=1:n, j=1:n, k=image:image)
     B(i,j,k) = (i-1) + (j-1) * n + (k-1) * n * n
  end do

  do i=1,m
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

  call scatter_coarray_3d_grid(A, grid, main_image_arg=scatter_image)
  sync all

  do i=1,m
     if (i == this_image()) then
        print *, image, ": POST =", A
     end if
     sync all
  end do

  if (image /= scatter_image) then
     if (.not. all(A == B)) then
        print *, "A != B on an image", image
        error stop "A != B on an image"
     end if
  end if

end program coarray_scatter_index_test
