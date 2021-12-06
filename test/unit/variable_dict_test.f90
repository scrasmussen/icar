module variable_dict_test
  !! Variable dictionary unit tests

  use variable_dict_interface, only : var_dict_t
  use variable_interface,      only : variable_t
  use grid_interface,          only : grid_t
  use vegetables, only: &
      result_t, test_item_t, describe, it, assert_that, fail, succeed
  implicit none

  private
  public :: test_variable_dict

contains

  function test_variable_dict() result(tests)
    type(test_item_t) :: tests

    tests = describe("The variabe_dict class", &
       [ it("gives the correct value associated with one of three added keys", check_initializations) &
      ])

  end function

  function check_initializations() result(result_)
    type(result_t) result_

    type(var_dict_t) var_collection
    type(variable_t) var1, var2, var3, output_var
    type(grid_t)     grid

    call grid%set_grid_dimensions(nx=10,ny=10,nz=5) ! define a grid for a test variable
    call var1%initialize(grid) ! initialize a test variable with that grid

    call grid%set_grid_dimensions(nx=2,ny=15,nz=5) ! initialize a test variable with that grid
    call var2%initialize(grid)

    call var3%initialize( [2,5] ) ! initialize a test variable with a shape spec instead of a grid spec

    ! add the test variable to the dictionary for the key "data"
    call var_collection%add_var("data_first", var1)
    call var_collection%add_var("data", var2)
    call var_collection%add_var("data_third", var3)

    output_var = var_collection%get_var("data")
      
    result_ = assert_that( &
      (output_var%three_d .eqv. var2%three_d) .and. &
      (output_var%n_dimensions == var2%n_dimensions) .and. &
      all(output_var%dim_len == var2%dim_len) &
    )

  end function

end module variable_dict_test
