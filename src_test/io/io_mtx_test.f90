!> IO 行列モジュール
module mod_monolis_io_mtx_test
  use mod_monolis_utils
  implicit none

contains

  subroutine monolis_io_mtx_test()
    implicit none

    call monolis_input_mtx_R_test()
    call monolis_input_mtx_C_test()
  end subroutine monolis_io_mtx_test

  subroutine monolis_input_mtx_R_test()
    implicit none
    integer(kint) :: N, NZ, i_ans(2)
    real(kdouble) :: r_ans(9)
    integer(kint), allocatable :: elem(:,:)
    real(kdouble), allocatable :: coef(:)

    call monolis_std_global_log_string("monolis_input_mtx_R")

    call monolis_input_mtx_R("io/input/mtx.r.txt", N, NZ, elem, coef)

    call monolis_test_check_eq_I1("monolis_input_mtx_R_test 1", N, 3)

    call monolis_test_check_eq_I1("monolis_input_mtx_R_test 2", NZ, 9)

    i_ans(1) = 1
    i_ans(2) = 1
    call monolis_test_check_eq_I ("monolis_input_mtx_R_test 3", elem(:,1), i_ans)

    i_ans(1) = 1
    i_ans(2) = 2
    call monolis_test_check_eq_I ("monolis_input_mtx_R_test 4", elem(:,2), i_ans)

    i_ans(1) = 1
    i_ans(2) = 3
    call monolis_test_check_eq_I ("monolis_input_mtx_R_test 5", elem(:,3), i_ans)

    i_ans(1) = 2
    i_ans(2) = 1
    call monolis_test_check_eq_I ("monolis_input_mtx_R_test 6", elem(:,4), i_ans)

    i_ans(1) = 2
    i_ans(2) = 2
    call monolis_test_check_eq_I ("monolis_input_mtx_R_test 7", elem(:,5), i_ans)

    i_ans(1) = 2
    i_ans(2) = 3
    call monolis_test_check_eq_I ("monolis_input_mtx_R_test 8", elem(:,6), i_ans)

    i_ans(1) = 3
    i_ans(2) = 1
    call monolis_test_check_eq_I ("monolis_input_mtx_R_test 9", elem(:,7), i_ans)

    i_ans(1) = 3
    i_ans(2) = 2
    call monolis_test_check_eq_I ("monolis_input_mtx_R_test 10", elem(:,8), i_ans)

    i_ans(1) = 3
    i_ans(2) = 3
    call monolis_test_check_eq_I ("monolis_input_mtx_R_test 11", elem(:,9), i_ans)

    r_ans(1) = 1.0d0
    r_ans(2) = 2.0d0
    r_ans(3) = 3.0d0
    r_ans(4) = 4.0d0
    r_ans(5) = 5.0d0
    r_ans(6) = 6.0d0
    r_ans(7) = 7.0d0
    r_ans(8) = 8.0d0
    r_ans(9) = 9.0d0
    call monolis_test_check_eq_R ("monolis_input_mtx_R_test 11", coef, r_ans)
  end subroutine monolis_input_mtx_R_test

  subroutine monolis_input_mtx_C_test()
    implicit none
    integer(kint) :: N, NZ, i_ans(2)
    complex(kdouble) :: c_ans(9)
    integer(kint), allocatable :: elem(:,:)
    complex(kdouble), allocatable :: coef(:)

    call monolis_std_global_log_string("monolis_input_mtx_C")

    call monolis_input_mtx_C("io/input/mtx.c.txt", N, NZ, elem, coef)

    call monolis_test_check_eq_I1("monolis_input_mtx_C_test 1", N, 3)

    call monolis_test_check_eq_I1("monolis_input_mtx_C_test 2", NZ, 9)

    i_ans(1) = 1
    i_ans(2) = 1
    call monolis_test_check_eq_I ("monolis_input_mtx_C_test 3", elem(:,1), i_ans)

    i_ans(1) = 1
    i_ans(2) = 2
    call monolis_test_check_eq_I ("monolis_input_mtx_C_test 4", elem(:,2), i_ans)

    i_ans(1) = 1
    i_ans(2) = 3
    call monolis_test_check_eq_I ("monolis_input_mtx_C_test 5", elem(:,3), i_ans)

    i_ans(1) = 2
    i_ans(2) = 1
    call monolis_test_check_eq_I ("monolis_input_mtx_C_test 6", elem(:,4), i_ans)

    i_ans(1) = 2
    i_ans(2) = 2
    call monolis_test_check_eq_I ("monolis_input_mtx_C_test 7", elem(:,5), i_ans)

    i_ans(1) = 2
    i_ans(2) = 3
    call monolis_test_check_eq_I ("monolis_input_mtx_C_test 8", elem(:,6), i_ans)

    i_ans(1) = 3
    i_ans(2) = 1
    call monolis_test_check_eq_I ("monolis_input_mtx_C_test 9", elem(:,7), i_ans)

    i_ans(1) = 3
    i_ans(2) = 2
    call monolis_test_check_eq_I ("monolis_input_mtx_C_test 10", elem(:,8), i_ans)

    i_ans(1) = 3
    i_ans(2) = 3
    call monolis_test_check_eq_I ("monolis_input_mtx_C_test 11", elem(:,9), i_ans)

    c_ans(1) = (1.0d0, 1.0d0)
    c_ans(2) = (2.0d0, 2.0d0)
    c_ans(3) = (3.0d0, 3.0d0)
    c_ans(4) = (4.0d0, 4.0d0)
    c_ans(5) = (5.0d0, 5.0d0)
    c_ans(6) = (6.0d0, 6.0d0)
    c_ans(7) = (7.0d0, 7.0d0)
    c_ans(8) = (8.0d0, 8.0d0)
    c_ans(9) = (9.0d0, 9.0d0)
    call monolis_test_check_eq_C ("monolis_input_mtx_C_test 11", coef, c_ans)
  end subroutine monolis_input_mtx_C_test
end module mod_monolis_io_mtx_test
