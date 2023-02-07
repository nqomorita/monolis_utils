!> エラーモジュール
module mod_monolis_utils_error
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
  !> 通常ログ出力関数
  subroutine monolis_std_log_string2(header, string)
    implicit none
    !> [in] 出力ログ
    character(*) :: header
    !> [in] 出力ログ
    character(*) :: string
    write(*,"(a,a,x,a)")"** MONOLIS: ", trim(header), trim(string)
  end subroutine monolis_std_log_string2

  !> @ingroup dev_error
  !> デバッグ出力関数フラグの設定
  subroutine monolis_std_debug_log_flag(flag)
    implicit none
    !> [in] フラグ設定
    logical :: flag
    monolis_debug_log_write = .true.
  end subroutine monolis_std_debug_log_flag

  !> @ingroup dev_error
  !> デバッグログ出力
  subroutine monolis_std_debug_log_header(string)
    implicit none
    !> [in] 出力ログ
    character(*) :: string
    if(.not. monolis_debug_log_write) return
    write(*,"(a,a)")"** MONOLIS DEBUG: ", trim(string)
  end subroutine monolis_std_debug_log_header

  !> @ingroup dev_error
  !> デバッグログ出力（整数型）
  subroutine monolis_std_debug_log_I1(header, n)
    implicit none
    !> [in] 出力ログ
    character(*) :: header
    !> [in] 出力ログ
    integer(kint) :: n
    if(.not. monolis_debug_log_write) return
    write(*,"(a,a,x,i12)")"** MONOLIS DEBUG: ", trim(header), n
  end subroutine monolis_std_debug_log_I1

  !> @ingroup dev_error
  !> デバッグログ出力（実数型）
  subroutine monolis_std_debug_log_R1(header, var)
    implicit none
    !> [in] 出力ログ
    character(*) :: header
    !> [in] 出力ログ
    real(kdouble) :: var
    if(.not. monolis_debug_log_write) return
    write(*,"(a,a,x,1pe12.4)")"** MONOLIS DEBUG: ", trim(header), var
  end subroutine monolis_std_debug_log_R1

  !> @ingroup dev_error
  !> デバッグログ出力（文字列型）
  subroutine monolis_std_debug_log_C(header, char)
    implicit none
    !> [in] 出力ログ
    character(*) :: header
    !> [in] 出力ログ
    character(*) :: char
    if(.not. monolis_debug_log_write) return
    write(*,"(a,a,x,a)")"** MONOLIS DEBUG: ", trim(header), char
  end subroutine monolis_std_debug_log_C

  !> @ingroup dev_error
  !> デバッグログ出力（論理型）
  subroutine monolis_std_debug_log_L1(header, l)
    implicit none
    !> [in] 出力ログ
    character(*) :: header
    !> [in] 出力ログ
    logical :: l
    if(.not. monolis_debug_log_write) return
    write(*,"(a,a,x,l)")"** MONOLIS DEBUG: ", trim(header), l
  end subroutine monolis_std_debug_log_L1

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
    character :: esc*1 = char(27)

    write(*,"(a,a)")esc//"[31m"//"** MONOLIS ERROR: "//esc//"[0m", trim(string)
  end subroutine monolis_std_error_string

  !> @ingroup dev_error
  !> 警告出力関数（WARNING）
  subroutine monolis_std_warning_string(string)
    implicit none
    !> [in] 出力ログ
    character(*) :: string
    character :: esc*1 = char(27)

    write(*,"(a,a)")esc//"[31m"//"** MONOLIS WARNING: "//esc//"[0m", trim(string)
  end subroutine monolis_std_warning_string
end module mod_monolis_utils_error
