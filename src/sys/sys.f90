!> システムモジュール
module mod_monolis_utils_sys
  implicit none

contains

  subroutine monolis_std_make_dir(dirname)
    implicit none
    !> [in] ディレクトリ名
    character(*) :: dirname

    call system('if [ ! -d '//trim(dirname)//' ]; then (echo "** MONOLIS: create '// &
      & trim(dirname)//'"; mkdir -p '//trim(dirname)//'); fi')
  end subroutine monolis_std_make_dir

end module mod_monolis_utils_sys
