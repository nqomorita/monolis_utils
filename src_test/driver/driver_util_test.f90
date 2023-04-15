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
    complex(kdouble) :: c_ans(2)
    real(kdouble), allocatable :: rval(:)
    complex(kdouble), allocatable :: cval(:)
    logical :: is_get

    call monolis_std_global_log_string("monolis_driver_get_arg_dbc_all_R")

    call monolis_driver_get_arg_dbc_all_R(n_dof, rval, is_get)

    call monolis_test_check_eq_L1("monolis_driver_get_arg_dbc_all_R 1", is_get, .true.)

    call monolis_test_check_eq_I1("monolis_driver_get_arg_dbc_all_R 2", n_dof, 2)

    r_ans(1) = 10.0d0
    r_ans(2) = 20.0d0

    call monolis_test_check_eq_R ("monolis_driver_get_arg_dbc_all_R 3", rval, r_ans)



    call monolis_std_global_log_string("monolis_driver_get_arg_dbc_all_C")

    call monolis_driver_get_arg_dbc_all_C(n_dof, cval, is_get)

    call monolis_test_check_eq_L1("monolis_driver_get_arg_dbc_all_C 1", is_get, .true.)

    call monolis_test_check_eq_I1("monolis_driver_get_arg_dbc_all_C 2", n_dof, 2)

    c_ans(1) = (10.0d0, 20.0d0)
    c_ans(2) = (30.0d0, 40.0d0)

    call monolis_test_check_eq_C ("monolis_driver_get_arg_dbc_all_C 3", cval, c_ans)
  end subroutine monolis_driver_get_arg_dbc_all_test
end module mod_monolis_driver_util_test
