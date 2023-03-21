module mod_monolis_driver_util_test
  use mod_monolis_utils
  use mod_monolis_driver_util
  implicit none

contains

  !> main test subroutine
  subroutine monolis_driver_util_test()
    implicit none
    call monolis_driver_get_arg_dbc_all_test()
  end subroutine monolis_driver_util_test

  subroutine monolis_driver_get_arg_dbc_all_test()
    implicit none
    integer(kint) :: n_dof
    real(kdouble) :: r_ans(2)
    real(kdouble), allocatable :: val(:)
    logical :: is_get

    call monolis_std_log_string("monolis_driver_get_arg_dbc_all_test")

    call monolis_driver_get_arg_dbc_all(n_dof, val, is_get)

    call monolis_test_check_eq_L1("monolis_driver_get_arg_dbc_all_test 1", is_get, .true.)

    call monolis_test_check_eq_I1("monolis_driver_get_arg_dbc_all_test 2", n_dof, 2)

    r_ans(1) = 10.0d0
    r_ans(2) = 20.0d0

    call monolis_test_check_eq_R ("monolis_driver_get_arg_dbc_all_test 3", val, r_ans)
  end subroutine monolis_driver_get_arg_dbc_all_test
end module mod_monolis_driver_util_test
