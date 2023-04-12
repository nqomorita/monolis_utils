!> システムモジュール
module mod_monolis_utils_sys
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_error
  implicit none

contains

  !> ディレクトリの作成
  subroutine monolis_std_make_dir(dirname)
    implicit none
    !> [in] ディレクトリ名
    character(*), intent(in) :: dirname

    call system('if [ ! -d '//trim(dirname)//' ]; then (echo "** MONOLIS: create '// &
      & trim(dirname)//'"; mkdir -p '//trim(dirname)//'); fi')
  end subroutine monolis_std_make_dir

  !> 整数型を論理型に変換
  function monolis_conv_I2L(val)
    implicit none
    integer(kint), intent(in) :: val
    logical :: monolis_conv_I2L

    monolis_conv_I2L = .false.

    if(val == 0)then
      monolis_conv_I2L = .false.
    elseif(val == 1)then
      monolis_conv_I2L = .true.
    else
      call monolis_std_error_string("monolis_conv_I2L")
      call monolis_std_error_string("input arg. is not 0 or 1")
      call monolis_std_error_stop()
    endif
  end function monolis_conv_I2L

  !> 論理型を整数型に変換
  function monolis_conv_L2I(val)
    implicit none
    logical, intent(in) :: val
    integer(kint) :: monolis_conv_L2I

    if(val)then
      monolis_conv_L2I = 1
    else
      monolis_conv_L2I = 0
    endif
  end function monolis_conv_L2I
end module mod_monolis_utils_sys
