module foo_writer_mod
use netcdf
implicit none

interface foo_writer
    procedure :: foo_writer_2d
    procedure :: foo_writer_3d
end interface foo_writer

contains
  subroutine foo_check(status, where)
    implicit none
    integer, intent ( in) :: status, where

    if(status /= nf90_noerr) then
       print *, "==== AT", where, "===="
       print *, trim(nf90_strerror(status))
       stop "Stopped"
    end if
  end subroutine foo_check

  subroutine foo_writer_2d(A, nx_images, stop_l)
    use netcdf
    implicit none
    real, intent(in), dimension(:,:) :: A
    integer, intent(in) :: nx_images
    logical, intent(in), optional :: stop_l
    integer :: ncid, me, np, dims(2)
    integer :: x_dimid, y_dimid
    integer :: dim_ids(2), a_id
    integer :: i, j
    character(len=100) :: f, format_s
    me = this_image()
    np = num_images()
    dims = shape(A)

    ! if (me == 1) print*, A(1:dims(1),1)

    ! format for file name
    if (np < 10) then
       format_s = "(A4,I1,A1,I1,A3)"
       write (f, format_s) "foo_", me, "_", np, ".nc"
    else
       if (me < 10) then
          format_s = "(A5,I1,A1,I2,A3)"
          write (f, format_s) "foo_0", me, "_", np, ".nc"
       else
          format_s = "(A4,I2,A1,I2,A3)"
          write (f, format_s) "foo_", me, "_", np, ".nc"
       end if
    end if

    ! if (me == 1) print *, "dims = ", dims
    ! print *, "SHAPE A", shape(A)
    ! print *, "CHECK A", A(dims(1), dims(2))

    call foo_check( nf90_create(f, NF90_CLOBBER, ncid) , 1)
    call foo_check( nf90_def_dim(ncid, "x", dims(1), x_dimid), 2 )
    call foo_check( nf90_def_dim(ncid, "y", dims(2), y_dimid), 3 )
    dim_ids = [x_dimid, y_dimid]
    call foo_check( nf90_def_var(ncid, "A", NF90_FLOAT, dim_ids, a_id), 4 )
    call foo_check( nf90_put_att(ncid, NF90_GLOBAL, "nx_images", nx_images), 6)
    call foo_check( nf90_enddef(ncid), 7 )
    call foo_check( nf90_put_var(ncid, a_id, A), 80 )

    call foo_check( nf90_close(ncid), 10 )

    if (present(stop_l)) then
        if (stop_l .eqv. .true.) then
            sync all
            stop "DONE WRITING"
        end if
    else
        stop "DONE WRITING"
    end if
  end subroutine foo_writer_2d

  subroutine foo_writer_3d(A, nx_images, stop_l)
    use netcdf
    implicit none
    real, intent(in), dimension(:,:,:) :: A
    integer, intent(in) :: nx_images
    logical, intent(in), optional :: stop_l
    integer :: ncid, me, np, dims(3)
    integer :: x_dimid, y_dimid, z_dimid
    integer :: dim_ids(3), a_id
    integer :: i, j, k
    character(len=100) :: f, format_s
    me = this_image()
    np = num_images()
    dims = shape(A)

    ! if (me == 1) print*, A(1:dims(1),1)

    ! format for file name
    if (np < 10) then
       format_s = "(A4,I1,A1,I1,A3)"
       write (f, format_s) "foo_", me, "_", np, ".nc"
    else
       if (me < 10) then
          format_s = "(A5,I1,A1,I2,A3)"
          write (f, format_s) "foo_0", me, "_", np, ".nc"
       else
          format_s = "(A4,I2,A1,I2,A3)"
          write (f, format_s) "foo_", me, "_", np, ".nc"
       end if
    end if

    call foo_check( nf90_create(f, NF90_CLOBBER, ncid) , 1)
    call foo_check( nf90_def_dim(ncid, "x", dims(1), x_dimid), 2 )
    call foo_check( nf90_def_dim(ncid, "z", dims(2), z_dimid), 22 )
    call foo_check( nf90_def_dim(ncid, "y", dims(3), y_dimid), 3 )
    dim_ids = [x_dimid, z_dimid, y_dimid]
    call foo_check( nf90_def_var(ncid, "A", NF90_FLOAT, dim_ids, a_id), 4 )
    call foo_check( nf90_put_att(ncid, NF90_GLOBAL, "nx_images", nx_images), 6)
    call foo_check( nf90_enddef(ncid), 7 )
    call foo_check( nf90_put_var(ncid, a_id, A), 80 )

    call foo_check( nf90_close(ncid), 10 )

    if (present(stop_l)) then
        if (stop_l .eqv. .true.) then
            sync all
            stop "DONE WRITING"
        end if
    else
        stop "DONE WRITING"
    end if
  end subroutine foo_writer_3d
end module foo_writer_mod
