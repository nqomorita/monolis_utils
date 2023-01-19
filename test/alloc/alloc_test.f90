!> alloc test モジュール
module mod_monolis_utils_alloc_test
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  use mod_monolis_utils_std_test
  implicit none

contains

  !> main test subroutine
  subroutine monolis_utils_alloc_test()
    implicit none
    call monolis_alloc_I_1d_test()
    call monolis_dealloc_I_1d_test()
    call monolis_realloc_I_1d_test()
    call monolis_append_I_1d_test()
    call monolis_alloc_I_2d_test()
    call monolis_dealloc_I_2d_test()
    call monolis_alloc_R_1d_test()
    call monolis_dealloc_R_1d_test()
    call monolis_alloc_R_2d_test()
    call monolis_dealloc_R_2d_test()
    call monolis_alloc_C_1d_test()
    call monolis_dealloc_C_1d_test()
    call monolis_alloc_C_2d_test()
    call monolis_dealloc_C_2d_test()
    call monolis_alloc_L_1d_test()
    call monolis_dealloc_L_1d_test()
    call monolis_alloc_L_2d_test()
    call monolis_dealloc_L_2d_test()
  end subroutine monolis_utils_alloc_test

  !> unit test
  subroutine monolis_alloc_I_1d_test()
    implicit none
    integer(kint) :: b(5)
    integer(kint), allocatable :: a(:)

    call monolis_alloc_I_1d(a, 5)

    if(.not. allocated(a))then
      call monolis_test_assert_fail("monolis_alloc_I_1d_test", "")
    endif

    b(1) = 0
    b(2) = 0
    b(3) = 0
    b(4) = 0
    b(5) = 0

    call monolis_test_check_eq_I("monolis_alloc_I_1d_test", a, b)
  end subroutine monolis_alloc_I_1d_test

  subroutine monolis_dealloc_I_1d_test()
    implicit none
    integer(kint), allocatable :: a(:)

    call monolis_alloc_I_1d(a, 5)

    call monolis_dealloc_I_1d(a)

    if(allocated(a))then
      call monolis_test_assert_fail("monolis_dealloc_I_1d_test", "")
    else
      call monolis_test_assert_pass("monolis_dealloc_I_1d_test")
    endif
  end subroutine monolis_dealloc_I_1d_test

  subroutine monolis_realloc_I_1d_test()
    implicit none
    integer(kint) :: b(5)
    integer(kint), allocatable :: a(:)

    call monolis_alloc_I_1d(a, 3)

    a(1) = 1
    a(2) = 1
    a(3) = 1

    call monolis_realloc_I_1d(a, 5)

    b(1) = 1
    b(2) = 1
    b(3) = 1
    b(4) = 0
    b(5) = 0

    call monolis_test_check_eq_I("monolis_realloc_I_1d_test", a, b)
  end subroutine monolis_realloc_I_1d_test

  subroutine monolis_append_I_1d_test()
    implicit none
    integer(kint) :: b(5), c(2)
    integer(kint), allocatable :: a(:)

    call monolis_alloc_I_1d(a, 3)

    a(1) = 1
    a(2) = 2
    a(3) = 3
    c(1) = 4
    c(2) = 5

    call monolis_append_I_1d(a, 2, c)

    b(1) = 1
    b(2) = 2
    b(3) = 3
    b(4) = 4
    b(5) = 5

    call monolis_test_check_eq_I("monolis_append_I_1d_test", a, b)
  end subroutine monolis_append_I_1d_test

  subroutine monolis_alloc_I_2d_test()
    implicit none
    integer(kint) :: b(3,2)
    integer(kint), allocatable :: a(:,:)

    call monolis_alloc_I_2d(a, 3, 2)

    if(.not. allocated(a))then
      call monolis_test_assert_fail("monolis_alloc_I_2d_test", "")
    endif

    b(1,1) = 0; b(1,2) = 0
    b(2,1) = 0; b(2,2) = 0
    b(3,1) = 0; b(3,2) = 0

    call monolis_test_check_eq_I("monolis_alloc_I_2d_test", a(:,1), b(:,1))
    call monolis_test_check_eq_I("monolis_alloc_I_2d_test", a(:,2), b(:,2))
  end subroutine monolis_alloc_I_2d_test

  subroutine monolis_dealloc_I_2d_test()
    implicit none
    integer(kint), allocatable :: a(:,:)

    call monolis_alloc_I_2d(a, 3, 2)

    call monolis_dealloc_I_2d(a)

    if(allocated(a))then
      call monolis_test_assert_fail("monolis_dealloc_I_2d_test", "")
    else
      call monolis_test_assert_pass("monolis_dealloc_I_2d_test")
    endif
  end subroutine monolis_dealloc_I_2d_test

  subroutine monolis_alloc_R_1d_test()
    implicit none
    real(kdouble) :: b(5)
    real(kdouble), allocatable :: a(:)

    call monolis_alloc_R_1d(a, 5)

    if(.not. allocated(a))then
      call monolis_test_assert_fail("monolis_alloc_R_1d_test", "")
    endif

    b(1) = 0.0d0
    b(2) = 0.0d0
    b(3) = 0.0d0
    b(4) = 0.0d0
    b(5) = 0.0d0

    call monolis_test_check_eq_R("monolis_alloc_R_1d_test", a, b)
  end subroutine monolis_alloc_R_1d_test

  subroutine monolis_dealloc_R_1d_test()
    implicit none
    real(kdouble), allocatable :: a(:)

    call monolis_alloc_R_1d(a, 5)

    call monolis_dealloc_R_1d(a)

    if(allocated(a))then
      call monolis_test_assert_fail("monolis_dealloc_R_1d_test", "")
    else
      call monolis_test_assert_pass("monolis_dealloc_R_1d_test")
    endif
  end subroutine monolis_dealloc_R_1d_test

  subroutine monolis_alloc_R_2d_test()
    implicit none
    real(kdouble) :: b(3,2)
    real(kdouble), allocatable :: a(:,:)

    call monolis_alloc_R_2d(a, 3, 2)

    if(.not. allocated(a))then
      call monolis_test_assert_fail("monolis_alloc_R_2d_test", "")
    endif

    b(1,1) = 0; b(1,2) = 0
    b(2,1) = 0; b(2,2) = 0
    b(3,1) = 0; b(3,2) = 0

    call monolis_test_check_eq_R("monolis_alloc_R_2d_test", a(:,1), b(:,1))
    call monolis_test_check_eq_R("monolis_alloc_R_2d_test", a(:,2), b(:,2))
  end subroutine monolis_alloc_R_2d_test

  subroutine monolis_dealloc_R_2d_test()
    implicit none
    real(kdouble), allocatable :: a(:,:)

    call monolis_alloc_R_2d(a, 3, 2)

    call monolis_dealloc_R_2d(a)

    if(allocated(a))then
      call monolis_test_assert_fail("monolis_dealloc_R_2d_test", "")
    else
      call monolis_test_assert_pass("monolis_dealloc_R_2d_test")
    endif
  end subroutine monolis_dealloc_R_2d_test

  subroutine monolis_alloc_C_1d_test()
    implicit none
    complex(kdouble) :: b(5)
    complex(kdouble), allocatable :: a(:)

    call monolis_alloc_C_1d(a, 5)

    if(.not. allocated(a))then
      call monolis_test_assert_fail("monolis_alloc_C_1d_test", "")
    endif

    b(1) = 0.0d0
    b(2) = 0.0d0
    b(3) = 0.0d0
    b(4) = 0.0d0
    b(5) = 0.0d0

    call monolis_test_check_eq_C("monolis_alloc_C_1d_test", a, b)
  end subroutine monolis_alloc_C_1d_test

  subroutine monolis_dealloc_C_1d_test()
    implicit none
    complex(kdouble), allocatable :: a(:)

    call monolis_alloc_C_1d(a, 5)

    call monolis_dealloc_C_1d(a)

    if(allocated(a))then
      call monolis_test_assert_fail("monolis_dealloc_C_1d_test", "")
    else
      call monolis_test_assert_pass("monolis_dealloc_C_1d_test")
    endif
  end subroutine monolis_dealloc_C_1d_test

  subroutine monolis_alloc_C_2d_test()
    implicit none
  end subroutine monolis_alloc_C_2d_test

  subroutine monolis_dealloc_C_2d_test()
    implicit none
    complex(kdouble), allocatable :: a(:,:)

    call monolis_alloc_C_2d(a, 3, 2)

    call monolis_dealloc_C_2d(a)

    if(allocated(a))then
      call monolis_test_assert_fail("monolis_dealloc_C_2d_test", "")
    else
      call monolis_test_assert_pass("monolis_dealloc_C_2d_test")
    endif
  end subroutine monolis_dealloc_C_2d_test

  subroutine monolis_alloc_L_1d_test()
    implicit none
    logical :: b(5)
    logical, allocatable :: a(:)

    call monolis_alloc_L_1d(a, 5)

    if(.not. allocated(a))then
      call monolis_test_assert_fail("monolis_alloc_L_1d_test", "")
    endif

    b(1) = .false.
    b(2) = .false.
    b(3) = .false.
    b(4) = .false.
    b(5) = .false.

    call monolis_test_check_eq_L("monolis_alloc_L_1d_test", a, b)
  end subroutine monolis_alloc_L_1d_test

  subroutine monolis_dealloc_L_1d_test()
    implicit none
    logical, allocatable :: a(:)

    call monolis_alloc_L_1d(a, 5)

    call monolis_dealloc_L_1d(a)

    if(allocated(a))then
      call monolis_test_assert_fail("monolis_dealloc_L_1d_test", "")
    else
      call monolis_test_assert_pass("monolis_dealloc_L_1d_test")
    endif
  end subroutine monolis_dealloc_L_1d_test

  subroutine monolis_alloc_L_2d_test()
    implicit none
    logical :: b(3,2)
    logical, allocatable :: a(:,:)

    call monolis_alloc_L_2d(a, 3, 2)

    if(.not. allocated(a))then
      call monolis_test_assert_fail("monolis_alloc_L_2d_test", "")
    endif

    b(1,1) = .false.; b(1,2) = .false.
    b(2,1) = .false.; b(2,2) = .false.
    b(3,1) = .false.; b(3,2) = .false.

    call monolis_test_check_eq_L("monolis_alloc_L_2d_test", a(:,1), b(:,1))
    call monolis_test_check_eq_L("monolis_alloc_L_2d_test", a(:,2), b(:,2))
  end subroutine monolis_alloc_L_2d_test

  subroutine monolis_dealloc_L_2d_test()
    implicit none
    logical, allocatable :: a(:,:)

    call monolis_alloc_L_2d(a, 3, 2)

    call monolis_dealloc_L_2d(a)

    if(allocated(a))then
      call monolis_test_assert_fail("monolis_dealloc_L_2d_test", "")
    else
      call monolis_test_assert_pass("monolis_dealloc_L_2d_test")
    endif
  end subroutine monolis_dealloc_L_2d_test
end module mod_monolis_utils_alloc_test
