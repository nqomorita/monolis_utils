!> エラーテストモジュール
module mod_monolis_utils_error_test
  use mod_monolis_utils
  implicit none

contains

  subroutine monolis_utils_error_test()
    implicit none

    call monolis_std_global_log_string("monolis_std_debug_log_string")
    call monolis_std_global_log_string("monolis_std_debug_log_I1")
    call monolis_std_global_log_string("monolis_std_debug_log_R1")
    call monolis_std_global_log_string("monolis_std_debug_log_L1")

    call monolis_std_global_log_string("monolis_std_debug_log_flag")
    call monolis_std_global_log_string("monolis_std_debug_log_header")

    call monolis_std_global_log_string("monolis_std_global_log_string")
    call monolis_std_global_log_string("monolis_std_log_I1")
    call monolis_std_global_log_string("monolis_std_log_string")
    call monolis_std_global_log_string("monolis_std_log_string2")
    call monolis_std_global_log_string("monolis_std_make_dir")
    call monolis_std_global_log_string("monolis_std_warning_string")

    call monolis_std_global_log_string("monolis_std_error_stop")
    call monolis_std_global_log_string("monolis_std_error_string")

  end subroutine monolis_utils_error_test
end module mod_monolis_utils_error_test
