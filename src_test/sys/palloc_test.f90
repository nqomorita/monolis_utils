!> palloc テストモジュール
module mod_monolis_utils_palloc_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_utils_palloc_test()
    implicit none
    call monolis_palloc_I_1d_test()
    call monolis_pdealloc_I_1d_test()
    call monolis_palloc_R_1d_test()
    call monolis_pdealloc_R_1d_test()
    call monolis_palloc_C_1d_test()
    call monolis_pdealloc_C_1d_test()
  end subroutine monolis_utils_palloc_test

  !> unit test
  subroutine monolis_palloc_I_1d_test()
    implicit none
    integer(kint) :: b(5)
    integer(kint), pointer :: a(:) => null()

    call monolis_std_global_log_string("monolis_palloc_I_1d")

    call monolis_palloc_I_1d(a, 5)

    if(.not. associated(a))then
      call monolis_test_assert_fail("monolis_palloc_I_1d", "")
    endif

    b(1) = 0
    b(2) = 0
    b(3) = 0
    b(4) = 0
    b(5) = 0

    call monolis_test_check_eq_I("monolis_palloc_I_1d", a, b)
  end subroutine monolis_palloc_I_1d_test

  subroutine monolis_pdealloc_I_1d_test()
    implicit none
    integer(kint), pointer :: a(:) => null()

    call monolis_std_global_log_string("monolis_pdealloc_I_1d")

    call monolis_palloc_I_1d(a, 5)

    call monolis_pdealloc_I_1d(a)

    if(associated(a))then
      call monolis_test_assert_fail("monolis_pdealloc_I_1d", "")
    else
      call monolis_test_assert_pass("monolis_pdealloc_I_1d")
    endif
  end subroutine monolis_pdealloc_I_1d_test

  subroutine monolis_palloc_R_1d_test()
    implicit none
    real(kdouble) :: b(5)
    real(kdouble), pointer :: a(:) => null()

    call monolis_std_global_log_string("monolis_palloc_R_1d")

    call monolis_palloc_R_1d(a, 5)

    if(.not. associated(a))then
      call monolis_test_assert_fail("monolis_palloc_R_1d", "")
    endif

    b(1) = 0.0d0
    b(2) = 0.0d0
    b(3) = 0.0d0
    b(4) = 0.0d0
    b(5) = 0.0d0

    call monolis_test_check_eq_R("monolis_palloc_R_1d", a, b)
  end subroutine monolis_palloc_R_1d_test

  subroutine monolis_pdealloc_R_1d_test()
    implicit none
    real(kdouble), pointer :: a(:) => null()

    call monolis_std_global_log_string("monolis_pdealloc_R_1d")

    call monolis_palloc_R_1d(a, 5)

    call monolis_pdealloc_R_1d(a)

    if(associated(a))then
      call monolis_test_assert_fail("monolis_pdealloc_R_1d", "")
    else
      call monolis_test_assert_pass("monolis_pdealloc_R_1d")
    endif
  end subroutine monolis_pdealloc_R_1d_test

  subroutine monolis_palloc_C_1d_test()
    implicit none
    complex(kdouble) :: b(5)
    complex(kdouble), pointer :: a(:) => null()

    call monolis_std_global_log_string("monolis_palloc_C_1d")

    call monolis_palloc_C_1d(a, 5)

    if(.not. associated(a))then
      call monolis_test_assert_fail("monolis_palloc_C_1d", "")
    endif

    b(1) = 0.0d0
    b(2) = 0.0d0
    b(3) = 0.0d0
    b(4) = 0.0d0
    b(5) = 0.0d0

    call monolis_test_check_eq_C("monolis_palloc_C_1d", a, b)
  end subroutine monolis_palloc_C_1d_test

  subroutine monolis_pdealloc_C_1d_test()
    implicit none
    complex(kdouble), pointer :: a(:) => null()

    call monolis_std_global_log_string("monolis_pdealloc_C_1d")

    call monolis_palloc_C_1d(a, 5)

    call monolis_pdealloc_C_1d(a)

    if(associated(a))then
      call monolis_test_assert_fail("monolis_pdealloc_C_1d", "")
    else
      call monolis_test_assert_pass("monolis_pdealloc_C_1d")
    endif
  end subroutine monolis_pdealloc_C_1d_test

end module mod_monolis_utils_palloc_test
