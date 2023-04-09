!> システムテストモジュール
module mod_monolis_utils_sys_test
  use mod_monolis_utils
  implicit none

contains

  subroutine monolis_utils_sys_test()
    implicit none

    call monolis_conv_I2L_test()
    call monolis_conv_L2I_test()
  end subroutine monolis_utils_sys_test

  subroutine monolis_conv_I2L_test
    implicit none

    call monolis_std_global_log_string("monolis_conv_I2L")

    call monolis_test_check_eq_L1("monolis_conv_I2L_test", monolis_conv_I2L(0), .false.)

    call monolis_test_check_eq_L1("monolis_conv_I2L_test", monolis_conv_I2L(1), .true.)
  end subroutine monolis_conv_I2L_test

  subroutine monolis_conv_L2I_test
    implicit none

    call monolis_std_global_log_string("monolis_conv_L2I")

    call monolis_test_check_eq_I1("monolis_conv_L2I_test", monolis_conv_L2I(.false.), 0)

    call monolis_test_check_eq_I1("monolis_conv_L2I_test", monolis_conv_L2I(.true.), 1)
  end subroutine monolis_conv_L2I_test
end module mod_monolis_utils_sys_test
