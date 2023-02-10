!> 通信データテストモジュール
module mod_monolis_comm_par_util_test
  use mod_monolis_utils
  use mod_monolis_utils_define_com
  use mod_monolis_comm_par_util
  implicit none

contains

  subroutine monolis_comm_par_util_test()
    implicit none

    call monolis_std_log_string("monolis_comm_par_util_test")
    call monolis_test_assert_pass("no test defined")
  end subroutine monolis_comm_par_util_test
end module mod_monolis_comm_par_util_test
