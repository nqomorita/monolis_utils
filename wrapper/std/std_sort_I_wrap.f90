!> std ソートモジュール（整数型）
module mod_monolis_utils_std_sort_I_wrap
  use mod_monolis_utils_std_sort_I
  use iso_c_binding
  implicit none

contains

  subroutine monolis_qsort_I_1d_c(array, n, iS, iE) &
    & bind(c, name = "monolis_qsort_I_1d")
    implicit none
    integer(kint_c), target :: array(1:n)
    integer(kint_c), value :: n, iS, iE

    iS = iS + 1
    iE = iE + 1
    call monolis_qsort_I_1d(array, iS, iE)
  end subroutine monolis_qsort_I_1d_c

  subroutine monolis_qsort_I_2d_c(array1, array2, n, iS, iE) &
    & bind(c, name = "monolis_qsort_I_2d")
    implicit none
    integer(kint_c), target :: array1(1:n)
    integer(kint_c), target :: array2(1:n)
    integer(kint_c), value :: n, iS, iE

    iS = iS + 1
    iE = iE + 1
    call monolis_qsort_I_2d(array1, array2, iS, iE)
  end subroutine monolis_qsort_I_2d_c

  subroutine monolis_bsearch_I_c(array, n, iS, iE, val, idx) &
    & bind(c, name = "monolis_bsearch_I")
    implicit none
    integer(kint_c), target :: array(1:n)
    integer(kint_c), value :: n, iS, iE, val
    integer(kint_c), target :: idx

    iS = iS + 1
    iE = iE + 1
    call monolis_bsearch_I(array, iS, iE, val, idx)
    if(idx > 0) idx = idx - 1
  end subroutine monolis_bsearch_I_c

  subroutine monolis_get_sequence_array_I_c(array, n, origin, diff) &
    & bind(c, name = "monolis_get_sequence_array_I")
    implicit none
    integer(kint_c), target :: array(1:n)
    integer(kint_c), value :: n, origin, diff

    call monolis_get_sequence_array_I(array, n, origin, diff)
  end subroutine monolis_get_sequence_array_I_c

  subroutine monolis_get_uniq_array_I_c(array, len, newlen) &
    & bind(c, name = "monolis_get_uniq_array_I")
    implicit none
    integer(kint_c), target :: array(1:len)
    integer(kint_c), value :: len
    integer(kint_c), target :: newlen

    call monolis_get_uniq_array_I(array, len, newlen)
  end subroutine monolis_get_uniq_array_I_c
end module mod_monolis_utils_std_sort_I_wrap
