module string_test
  !! String class unit tests
  use string, only : get_double, get_real, get_integer, str
  use iso_fortran_env, only : real32, real64, int32
  use vegetables, only: &
      result_t, test_item_t, assert_equals, describe, it, assert_that, fail, succeed
  implicit none
  private
  public :: test_string

  integer, parameter :: max_num_len=32 !! longest allowable character representation of a number

contains

  function test_string() result(tests)
    type(test_item_t) :: tests

    tests = describe("The strings class", &
      [ it("converts a real(real64) datum to string", check_real64_conversion) &
       ,it("converts a real(real32) datum to string", check_real32_conversion) &
       ,it("converts an integer datum to string", check_integer_conversion) &
      ])

  end function

  function check_real64_conversion() result(result_)
    type(result_t) result_
    character(len=max_num_len) real64_string
    real(real64), parameter :: real64_datum=0.3_real64

    write(real64_string,*) real64_datum
    ! adjustl(real64_string)      == str(real64_datum)
    result_ = assert_that(real64_datum == get_double(real64_string))
  end function

  function check_real32_conversion() result(result_)
    type(result_t) result_
    character(len=max_num_len) real32_string
    real(real32), parameter :: real32_datum=0.3_real32

    write(real32_string,*) real32_datum
    ! adjustl(real32_string)      == str(real32_datum)  
    result_ = assert_that(real32_datum == get_double(real32_string))
  end function

  function check_integer_conversion() result(result_)
    type(result_t) result_
    character(len=max_num_len) integer_string
    integer,      parameter :: integer_datum=1234567

    write(integer_string,*) integer_datum
    ! adjustl(integer_string)     == str(integer_datum) 
    result_ = assert_that(integer_datum == get_double(integer_string))
  end function

end module string_test
