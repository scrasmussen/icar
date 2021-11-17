module array_utilities_test
  !! Array utilities unit tests
  use array_utilities, only: linear_space
  use vegetables, only: &
    result_t, test_item_t, describe, it, assert_that, assert_equals, assert_equals_within_absolute, succeed
  implicit none

  private
  public :: test_array_utilities

  integer, parameter ::  n = 100
  integer, parameter :: db = kind(1.D0)
  real, parameter :: vmin = 1e-4, vmax = 3
  real, parameter :: dv = (vmax-vmin) / (n-1)
  real, parameter :: tolerance = 1e-6
  real, allocatable :: test_array(:)

contains

  function test_array_utilities() result(tests)
    type(test_item_t) :: tests

    call linear_space(test_array, vmin, vmax, n)

    tests = describe("The array utilities", &
       [ it("allocates a test array", check_array_allocation) &
        ,it("allocates a correctly-sized test array", check_allocated_array_size) &
        ,it("initializes a test array first element within tolerance", check_array_starts_within_tolerance) &
        ,it("initializes a test array last element within tolerance", check_array_ends_within_tolerance) &
        ,it("initializes a test array with an in-tolerance step size between elements", check_step_size_within_tolerance) &
      ])
  end function

  function check_array_allocation() result(result_)
    type(result_t) result_
    result_ = assert_that(allocated(test_array))
  end function

  function check_allocated_array_size() result(result_)
    type(result_t) result_
    result_ = assert_equals(n, size(test_array))
  end function

  function check_array_starts_within_tolerance() result(result_)
    type(result_t) result_
    result_ = assert_equals_within_absolute(real(vmin,db), real(test_array(1),db), real(tolerance,db))
  end function

  function check_array_ends_within_tolerance() result(result_)
    type(result_t) result_
    result_ = assert_equals_within_absolute(real(vmax,db), real(test_array(n),db), real(tolerance,db))
  end function

  function check_step_size_within_tolerance() result(result_)
    type(result_t) result_

    associate(running_dv => test_array(2:n)-test_array(1:n-1))
      result_ = assert_that(all(abs(running_dv - dv) <= tolerance))
    end associate
  end function

end module array_utilities_test
