!> パラメータテストモジュール
module mod_monolis_utils_define_R_N128_test
  use mod_monolis_utils
  implicit none

contains

  subroutine monolis_utils_define_R_N128_test()
    implicit none
    type(monolis_R_N128) :: a, b, c
    type(monolis_R_N128) :: e(2), f(2)
    real(kdouble) :: x, y, z
    integer(kint) :: len, data_type

    call monolis_std_global_log_string("monolis_utils_define_R_N128_test")

    !> monolis_add_R_N128
    call monolis_std_global_log_string("monolis_add_R_N128")

    a = monolis_conv_R_to_R_N128(1.0d0)
    b = monolis_conv_R_to_R_N128(1.0d-20)
    call monolis_add_R_N128(a, b, c)

    call monolis_test_check_eq_R1("monolis_add_R_N128 test 1", c%hi, 1.0d0)
    call monolis_test_check_eq_R1("monolis_add_R_N128 test 2", c%lo, 1.0d-20)

    a = monolis_conv_R_to_R_N128(1.23456789012345d0)
    b = monolis_conv_R_to_R_N128(1.0d-20)
    call monolis_add_R_N128(a, b, c)
    a = monolis_conv_R_to_R_N128(-1.23456789012345d0)
    b = monolis_conv_R_to_R_N128(0.0d0)
    call monolis_add_R_N128(c, a, b)

    call monolis_test_check_eq_R1("monolis_add_R_N128 test 3", b%hi, 1.0d-20)
    call monolis_test_check_eq_R1("monolis_add_R_N128 test 4", b%lo, 0.0d0)


    !> monolis_add_R_N128_MPI
    call monolis_std_global_log_string("monolis_add_R_N128_MPI")

    len = 2
    e(1) = monolis_conv_R_to_R_N128(1.0d0)
    e(2) = monolis_conv_R_to_R_N128(1.0d-20)
    f(1) = monolis_conv_R_to_R_N128(2.0d0)
    f(2) = monolis_conv_R_to_R_N128(2.0d-20)
    call monolis_add_R_N128_MPI(e, f, len, data_type)

    call monolis_test_check_eq_R1("monolis_add_R_N128_MPI test 1", f(1)%hi, 3.0d0)
    call monolis_test_check_eq_R1("monolis_add_R_N128_MPI test 2", f(1)%lo, 0.0d0)
    call monolis_test_check_eq_R1("monolis_add_R_N128_MPI test 1", f(2)%hi, 3.0d-20)
    call monolis_test_check_eq_R1("monolis_add_R_N128_MPI test 2", f(2)%lo, 0.0d0)


    !> monolis_conv_R_to_R_N128
    call monolis_std_global_log_string("monolis_conv_R_to_R_N128")

    x = 1.0d0

    a = monolis_conv_R_to_R_N128(x)

    call monolis_test_check_eq_R1("monolis_conv_R_to_R_N128 test 1", a%hi, 1.0d0)
    call monolis_test_check_eq_R1("monolis_conv_R_to_R_N128 test 2", a%lo, 0.0d0)


    !> monolis_conv_R_N128_to_R
    call monolis_std_global_log_string("monolis_conv_R_N128_to_R")

    a%hi = 1.0d0
    a%lo = 2.0d0

    x = monolis_conv_R_N128_to_R(a)

    call monolis_test_check_eq_R1("monolis_conv_R_N128_to_R test 1", x, 3.0d0)


    !> monolis_copy_R_N128
    call monolis_std_global_log_string("monolis_copy_R_N128")

    a%hi = 1.0d0
    a%lo = 2.0d0

    b = monolis_copy_R_N128(a)

    call monolis_test_check_eq_R1("monolis_copy_R_N128 test 1", b%hi, 1.0d0)
    call monolis_test_check_eq_R1("monolis_copy_R_N128 test 2", b%lo, 2.0d0)

  end subroutine monolis_utils_define_R_N128_test
end module mod_monolis_utils_define_R_N128_test
