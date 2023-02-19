!> IO 引数モジュール
module mod_monolis_io_arg_test
  use mod_monolis_utils
  implicit none

contains

  subroutine monolis_io_arg_test()
    implicit none
    call monolis_check_arg_input_test()
    call monolis_get_arg_input_I_test()
    call monolis_get_arg_input_R_test()
    call monolis_get_arg_input_S_test()
    call monolis_get_arg_input_i_tag_test()
    call monolis_get_arg_input_o_tag_test()
    call monolis_get_arg_input_in_tag_test()
    call monolis_get_arg_input_ie_tag_test()
  end subroutine monolis_io_arg_test

  subroutine monolis_check_arg_input_test()
    implicit none
    character(monolis_charlen) :: tag
    logical :: is_get

    call monolis_std_log_string("monolis_check_arg_input_test")

    !> case 1
    tag = "-test_i2"
    call monolis_check_arg_input(tag, is_get)
    call monolis_test_check_eq_L1("monolis_check_arg_input_test 1", is_get, .false.)

    !> case 2
    tag = "-test_i"
    call monolis_check_arg_input(tag, is_get)
    call monolis_test_check_eq_L1("monolis_check_arg_input_test 2", is_get, .true.)
  end subroutine monolis_check_arg_input_test

  subroutine monolis_get_arg_input_I_test()
    implicit none
    character(monolis_charlen) :: tag
    integer(kint) :: var
    logical :: is_get

    call monolis_std_log_string("monolis_get_arg_input_I_test")

    !> case 1
    tag = "-test_i2"
    call monolis_get_arg_input_I(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_I_test 1", is_get, .false.)

    !> case 2
    tag = "-test_i"
    call monolis_get_arg_input_I(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_I_test 2", is_get, .true.)
    call monolis_test_check_eq_I1("monolis_get_arg_input_I_test 3", var, 1)
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
    call monolis_test_check_eq_L1("monolis_get_arg_input_R_test 1", is_get, .false.)

    !> case 2
    tag = "-test_r"
    call monolis_get_arg_input_R(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_R_test 2", is_get, .true.)
    call monolis_test_check_eq_R1("monolis_get_arg_input_R_test 3", var, 2.0d0)
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
    call monolis_test_check_eq_L1("monolis_get_arg_input_S_test 1", is_get, .false.)

    !> case 2
    tag = "-test_s"
    call monolis_get_arg_input_S(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_S_test 2", is_get, .true.)

    if(trim(var) == "test_string")then
      call monolis_test_assert_pass("monolis_get_arg_input_S_test 3")
    else
      call monolis_test_assert_fail("monolis_get_arg_input_S_test 3", "")
    endif
  end subroutine monolis_get_arg_input_S_test

  subroutine monolis_get_arg_input_i_tag_test()
    implicit none
    character(monolis_charlen) :: tag
    character(monolis_charlen) :: var
    logical :: is_get

    call monolis_std_log_string("monolis_get_arg_input_i_tag_test")

    !> case 1
    tag = "-i"
    call monolis_get_arg_input_S(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_i_tag_test 1", is_get, .true.)

    if(trim(var) == "a.txt")then
      call monolis_test_assert_pass("monolis_get_arg_input_i_tag_test 2")
    else
      call monolis_test_assert_fail("monolis_get_arg_input_i_tag_test 2", "")
    endif
  end subroutine monolis_get_arg_input_i_tag_test

  subroutine monolis_get_arg_input_in_tag_test()
    implicit none
    character(monolis_charlen) :: tag
    character(monolis_charlen) :: var
    logical :: is_get

    call monolis_std_log_string("monolis_get_arg_input_in_tag_test")

    !> case 2
    tag = "-in"
    call monolis_get_arg_input_S(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_in_tag_test 1", is_get, .true.)

    if(trim(var) == "c.txt")then
      call monolis_test_assert_pass("monolis_get_arg_input_in_tag_test 2")
    else
      call monolis_test_assert_fail("monolis_get_arg_input_in_tag_test 2", "")
    endif
  end subroutine monolis_get_arg_input_in_tag_test

  subroutine monolis_get_arg_input_ie_tag_test()
    implicit none
    character(monolis_charlen) :: tag
    character(monolis_charlen) :: var
    logical :: is_get

    call monolis_std_log_string("monolis_get_arg_input_ie_tag_test")

    !> case 1
    tag = "-ie"
    call monolis_get_arg_input_S(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_ie_tag_test 1", is_get, .true.)

    if(trim(var) == "d.txt")then
      call monolis_test_assert_pass("monolis_get_arg_input_ie_tag_test 2")
    else
      call monolis_test_assert_fail("monolis_get_arg_input_ie_tag_test 2", "")
    endif
  end subroutine monolis_get_arg_input_ie_tag_test

  subroutine monolis_get_arg_input_o_tag_test()
    implicit none
    character(monolis_charlen) :: tag
    character(monolis_charlen) :: var
    logical :: is_get

    call monolis_std_log_string("monolis_get_arg_input_o_tag_test")

    !> case 1
    tag = "-o"
    call monolis_get_arg_input_S(tag, var, is_get)
    call monolis_test_check_eq_L1("monolis_get_arg_input_o_tag_test 1", is_get, .true.)

    if(trim(var) == "b.txt")then
      call monolis_test_assert_pass("monolis_get_arg_input_o_tag_test 2")
    else
      call monolis_test_assert_fail("monolis_get_arg_input_o_tag_test 2", "")
    endif
  end subroutine monolis_get_arg_input_o_tag_test
end module mod_monolis_io_arg_test
