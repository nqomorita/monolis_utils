!> std エラーモジュール
module mod_monolis_utils_std_error
  use mod_monolis_utils_define_prm
  implicit none

contains

  !> @ingroup dev_error
  !> 通常ログ出力関数
  subroutine monolis_std_log_string(string)
    implicit none
    !> [in] 出力ログ
    character(*) :: string
    write(*,"(a,a)")"** MONOLIS: ", trim(string)
  end subroutine monolis_std_log_string

  !> @ingroup dev_error
  !> エラーストップ関数
  subroutine monolis_std_error_stop()
    implicit none
    error stop monolis_fail
  end subroutine monolis_std_error_stop

  !> @ingroup dev_error
  !> エラー出力関数（ERROR）
  subroutine monolis_std_error_string(string)
    implicit none
    !> [in] 出力ログ
    character(*) :: string
    write(*,"(a,a)")"** MONOLIS ERROR: ", trim(string)
  end subroutine monolis_std_error_string

  !> @ingroup dev_error
  !> 警告出力関数（WARNING）
  subroutine monolis_std_warning_string(string)
    implicit none
    !> [in] 出力ログ
    character(*) :: string
    write(*,"(a,a)")"** MONOLIS WARNING: ", trim(string)
  end subroutine monolis_std_warning_string
end module mod_monolis_utils_std_error
