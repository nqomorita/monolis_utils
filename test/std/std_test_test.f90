!> std test test モジュール
module mod_monolis_utils_std_test_test
  use mod_monolis_utils_std_test
  use mod_monolis_utils_prm
  implicit none

contains

  !> main test subroutine
  subroutine monolis_utils_std_test_test()
    implicit none

    call monolis_test_check_eq_I_main_test()
    call monolis_test_check_eq_R_main_test()
    call monolis_test_check_eq_C_main_test()
  end subroutine monolis_utils_std_test_test

  !> unit test
  subroutine monolis_test_check_eq_I_main_test()
    implicit none
    integer(kint) :: a
    integer(kint) :: b
    logical :: is_eq

    a = 2
    b = 2

    call monolis_test_check_eq_I_main(a, b, is_eq)

    if(is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_I_main_test case 1")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_I_main_test case 1", "")
    endif

    a = 4
    b = 2

    call monolis_test_check_eq_I_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_I_main_test case 2")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_I_main_test case 2", "")
    endif
  end subroutine monolis_test_check_eq_I_main_test

  subroutine monolis_test_check_eq_R_main_test()
    implicit none
    real(kdouble) :: a
    real(kdouble) :: b
    logical :: is_eq

    a = 2.0d0
    b = 2.0d0

    call monolis_test_check_eq_R_main(a, b, is_eq)

    if(is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_R_main_test case 1")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_R_main_test case 1", "")
    endif

    a = 4.0d0
    b = 2.0d0

    call monolis_test_check_eq_R_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_R_main_test case 2")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_R_main_test case 2", "")
    endif

    a = 0.0d0
    b = 2.0d0

    call monolis_test_check_eq_R_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_R_main_test case 3")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_R_main_test case 3", "")
    endif
  end subroutine monolis_test_check_eq_R_main_test

  subroutine monolis_test_check_eq_C_main_test()
    implicit none
    complex(kdouble) :: a
    complex(kdouble) :: b
    logical :: is_eq

    a = (2.0d0, 1.0d0)
    b = (2.0d0, 1.0d0)

    call monolis_test_check_eq_C_main(a, b, is_eq)

    if(is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_C_main_test case 1")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_C_main_test case 1", "")
    endif

    a = (4.0d0, 2.0d0)
    b = (2.0d0, 1.0d0)

    call monolis_test_check_eq_C_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_C_main_test case 2")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_C_main_test case 2", "")
    endif

    a = (0.0d0, 2.0d0)
    b = (2.0d0, 1.0d0)

    call monolis_test_check_eq_C_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_C_main_test case 3")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_C_main_test case 3", "")
    endif

    a = (4.0d0, 0.0d0)
    b = (2.0d0, 1.0d0)

    call monolis_test_check_eq_C_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_C_main_test case 4")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_C_main_test case 4", "")
    endif

    a = (0.0d0, 0.0d0)
    b = (2.0d0, 1.0d0)

    call monolis_test_check_eq_C_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_C_main_test case 5")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_C_main_test case 5", "")
    endif
  end subroutine monolis_test_check_eq_C_main_test
end module mod_monolis_utils_std_test_test
