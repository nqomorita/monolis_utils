module mod_monolis_driver_util
  use mod_monolis_utils
  implicit none

contains

  !> @ingroup dev_driver
  !> 入力節点ファイル名を取得
  subroutine monolis_driver_get_arg_in(fnname)
    implicit none
    !> 入力節点ファイル名
    character(monolis_charlen) :: fnname
    logical :: is_get
    fnname = "node.dat"
    call monolis_get_arg_input_S("-in", fnname, is_get)
    call monolis_std_log_string2("[input node file]", fnname)
  end subroutine monolis_driver_get_arg_in

  !> @ingroup dev_driver
  !> 入力要素ファイル名を取得
  subroutine monolis_driver_get_arg_ie(fename)
    implicit none
    !> 入力要素ファイル名
    character(monolis_charlen) :: fename
    logical :: is_get
    fename = "elem.dat"
    call monolis_get_arg_input_S("-ie", fename, is_get)
    call monolis_std_log_string2("[input elem file]", fename)
  end subroutine monolis_driver_get_arg_ie

  !> @ingroup dev_driver
  !> 出力ファイル名を取得
  subroutine monolis_driver_get_arg_o(foname)
    implicit none
    !> 出力ファイル名
    character(monolis_charlen) :: foname
    logical :: is_get
    call monolis_get_arg_input_S("-o", foname, is_get)
    call monolis_std_log_string2("[output file]", foname)
  end subroutine monolis_driver_get_arg_o

  !> @ingroup dev_driver
  !> 出力境界条件値を取得
  subroutine monolis_driver_get_arg_dbc_all(n_dof, val)
    implicit none
    !> 出力ファイル名
    integer(kint) :: n_dof
    real(kdouble), allocatable :: val(:)
    !call monolis_get_arg_input_S("-o", foname, is_get)
    !call monolis_std_log_string2("[output file]", foname)
  end subroutine monolis_driver_get_arg_dbc_all
end module mod_monolis_driver_util
