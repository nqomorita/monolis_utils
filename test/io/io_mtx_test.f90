!> IO 行列モジュール
module mod_monolis_io_mtx_test
  use mod_monolis_utils_error
  use mod_monolis_utils_std_test
  implicit none

contains

  subroutine monolis_io_mtx_test()
    implicit none
    call monolis_std_log_string("monolis_io_mtx_test")
    call monolis_test_assert_pass("no test defined")
  end subroutine monolis_io_mtx_test
end module mod_monolis_io_mtx_test