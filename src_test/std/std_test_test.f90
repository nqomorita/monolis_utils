!> std test テストモジュール
module mod_monolis_utils_std_test_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_utils_std_test_test()
    implicit none

    call monolis_test_check_eq_I_main_test()
    call monolis_test_check_eq_R_main_test()
    call monolis_test_check_eq_C_main_test()
    call monolis_test_check_eq_L_main_test()

    call monolis_std_global_log_string("monolis_test_check_eq_I1")
    call monolis_std_global_log_string("monolis_test_check_eq_R1")
    call monolis_std_global_log_string("monolis_test_check_eq_C1")
    call monolis_std_global_log_string("monolis_test_check_eq_L1")
    call monolis_std_global_log_string("monolis_test_check_eq_I")
    call monolis_std_global_log_string("monolis_test_check_eq_R")
    call monolis_std_global_log_string("monolis_test_check_eq_C")
    call monolis_std_global_log_string("monolis_test_check_eq_L")
    call monolis_std_global_log_string("monolis_test_assert_pass")
    call monolis_std_global_log_string("monolis_test_assert_fail")
  end subroutine monolis_utils_std_test_test

  !> unit test
  subroutine monolis_test_check_eq_I_main_test()
    implicit none
    integer(kint) :: a
    integer(kint) :: b
    logical :: is_eq

    call monolis_std_global_log_string("monolis_test_check_eq_I_main")

    !> case 1
    a = 2
    b = 2

    call monolis_test_check_eq_I_main(a, b, is_eq)

    if(is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_I_main case 1")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_I_main case 1", "")
    endif

    !> case 2
    a = 4
    b = 2

    call monolis_test_check_eq_I_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_I_main case 2")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_I_main case 2", "")
    endif
  end subroutine monolis_test_check_eq_I_main_test

  subroutine monolis_test_check_eq_R_main_test()
    implicit none
    real(kdouble) :: a
    real(kdouble) :: b
    logical :: is_eq

    call monolis_std_global_log_string("monolis_test_check_eq_R_main")

    !> case 1
    a = 2.0d0
    b = 2.0d0

    call monolis_test_check_eq_R_main(a, b, is_eq)

    if(is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_R_main case 1")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_R_main case 1", "")
    endif

    !> case 2
    a = 4.0d0
    b = 2.0d0

    call monolis_test_check_eq_R_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_R_main case 2")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_R_main case 2", "")
    endif

    !> case 3
    a = 0.0d0
    b = 2.0d0

    call monolis_test_check_eq_R_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_R_main case 3")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_R_main case 3", "")
    endif

    !> case 4
    a = 2.0d0
    b =-2.0d0

    call monolis_test_check_eq_R_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_R_main case 4")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_R_main case 4", "")
    endif
  end subroutine monolis_test_check_eq_R_main_test

  subroutine monolis_test_check_eq_C_main_test()
    implicit none
    complex(kdouble) :: a
    complex(kdouble) :: b
    logical :: is_eq

    call monolis_std_global_log_string("monolis_test_check_eq_C_main")

    !> case 1
    a = (2.0d0, 1.0d0)
    b = (2.0d0, 1.0d0)

    call monolis_test_check_eq_C_main(a, b, is_eq)

    if(is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_C_main case 1")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_C_main case 1", "")
    endif

    !> case 2
    a = (4.0d0, 2.0d0)
    b = (2.0d0, 1.0d0)

    call monolis_test_check_eq_C_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_C_main case 2")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_C_main case 2", "")
    endif

    !> case 3
    a = (0.0d0, 2.0d0)
    b = (2.0d0, 1.0d0)

    call monolis_test_check_eq_C_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_C_main case 3")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_C_main case 3", "")
    endif

    !> case 4
    a = (4.0d0, 0.0d0)
    b = (2.0d0, 1.0d0)

    call monolis_test_check_eq_C_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_C_main case 4")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_C_main case 4", "")
    endif

    !> case 5
    a = (0.0d0, 0.0d0)
    b = (2.0d0, 1.0d0)

    call monolis_test_check_eq_C_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_C_main case 5")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_C_main case 5", "")
    endif

    !> case 6
    a = (-2.0d0,-1.0d0)
    b = ( 2.0d0, 1.0d0)

    call monolis_test_check_eq_C_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_C_main case 6")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_C_main case 6", "")
    endif
  end subroutine monolis_test_check_eq_C_main_test

  subroutine monolis_test_check_eq_L_main_test()
    implicit none
    logical :: a
    logical :: b
    logical :: is_eq

    call monolis_std_global_log_string("monolis_test_check_eq_L_main")

    !> case 1
    a = .true.
    b = .true.

    call monolis_test_check_eq_L_main(a, b, is_eq)

    if(is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_L_main case 1")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_L_main case 1", "")
    endif

    !> case 2
    a = .true.
    b = .false.

    call monolis_test_check_eq_L_main(a, b, is_eq)

    if(.not. is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_L_main case 2")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_L_main case 2", "")
    endif

    !> case 2
    a = .false.
    b = .false.

    call monolis_test_check_eq_L_main(a, b, is_eq)

    if(is_eq)then
      call monolis_test_assert_pass("monolis_test_check_eq_L_main case 3")
    else
      call monolis_test_assert_fail("monolis_test_check_eq_L_main case 3", "")
    endif
  end subroutine monolis_test_check_eq_L_main_test
end module mod_monolis_utils_std_test_test
