!> std ソートモジュール（整数型）
module mod_monolis_utils_std_sort_I_wrap
  use mod_monolis_utils_std_sort_I
  use iso_c_binding
  implicit none

contains

  subroutine monolis_qsort_I_1d_c(array, iS, iE) &
    & bind(c, name = "monolis_qsort_I_1d")
    implicit none
    integer(c_int), target :: array(1:iE-iS+1)
    integer(c_int), value :: iS, iE

    call monolis_qsort_I_1d(array, iS, iE)
  end subroutine monolis_qsort_I_1d_c

end module mod_monolis_utils_std_sort_I_wrap
