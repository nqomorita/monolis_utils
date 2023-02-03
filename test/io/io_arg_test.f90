!> IO 引数モジュール
module mod_monolis_io_arg_test
  use mod_monolis_utils_error
  use mod_monolis_utils_std_test
  use mod_monolis_io_arg
  implicit none

contains

  subroutine monolis_io_arg_test()
    implicit none
    call monolis_get_arg_input_I_test()
    call monolis_get_arg_input_R_test()
    call monolis_get_arg_input_S_test()
  end subroutine monolis_io_arg_test

  subroutine monolis_get_arg_input_I_test()
    implicit none
    character(monolis_charlen) :: tag
    integer(kint) :: var
    logical :: is_get

    call monolis_std_log_string("monolis_get_arg_input_I_test")

    !> case 1
    tag = "-test_i2"
    call monolis_get_arg_input_I(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_I_test case 1", is_get, .false.)

    !> case 2
    tag = "-test_i"
    call monolis_get_arg_input_I(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_I_test case 2", is_get, .true.)
  end subroutine monolis_get_arg_input_I_test

  subroutine monolis_get_arg_input_R_test()
    implicit none
    character(monolis_charlen) :: tag
    real(kdouble) :: var
    logical :: is_get

    call monolis_std_log_string("monolis_get_arg_input_R_test")

    !> case 1
    tag = "-test_r2"
    call monolis_get_arg_input_R(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_R case 1", is_get, .false.)

    !> case 2
    tag = "-test_r"
    call monolis_get_arg_input_R(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_R case 2", is_get, .true.)
  end subroutine monolis_get_arg_input_R_test

  subroutine monolis_get_arg_input_S_test()
    implicit none
    character(monolis_charlen) :: tag
    character(monolis_charlen) :: var
    logical :: is_get

    call monolis_std_log_string("monolis_get_arg_input_S_test")

    !> case 1
    tag = "-test_s2"
    call monolis_get_arg_input_S(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_S_test case 1", is_get, .false.)

    !> case 2
    tag = "-test_s"
    call monolis_get_arg_input_S(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_S_test case 2", is_get, .true.)
  end subroutine monolis_get_arg_input_S_test

end module mod_monolis_io_arg_test
