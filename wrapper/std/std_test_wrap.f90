!> std テストモジュール
module mod_monolis_utils_std_test_wrap
  use mod_monolis_utils_std_test
  use mod_monolis_utils_error
  use iso_c_binding
  implicit none

contains

  subroutine monolis_std_log_string_c(array) &
    & bind(c, name = "monolis_std_log_string")
    implicit none
    character :: array(*)
    character(monolis_charlen) :: header

    header = monolis_string_c2f(array)
    call monolis_std_log_string(header)
  end subroutine monolis_std_log_string_c

  subroutine monolis_std_global_log_string_c(array) &
    & bind(c, name = "monolis_std_global_log_string")
    implicit none
    character :: array(*)
    character(monolis_charlen) :: header

    header = monolis_string_c2f(array)
    call monolis_std_global_log_string(header)
  end subroutine monolis_std_global_log_string_c

  subroutine monolis_test_assert_pass_c(array) &
    & bind(c, name = "monolis_test_assert_pass")
    implicit none
    character :: array(*)
    character(monolis_charlen) :: header

    header = monolis_string_c2f(array)
    call monolis_test_assert_pass(header)
  end subroutine monolis_test_assert_pass_c

  subroutine monolis_test_assert_fail_c(array1, array2) &
    & bind(c, name = "monolis_test_assert_fail")
    implicit none
    character :: array1(*)
    character :: array2(*)
    character(monolis_charlen) :: header1
    character(monolis_charlen) :: header2

    header1 = monolis_string_c2f(array1)
    header2 = monolis_string_c2f(array2)
    call monolis_test_assert_fail(header1, header2)
  end subroutine monolis_test_assert_fail_c

  subroutine monolis_test_check_eq_I1_c(array, a, b) &
    & bind(c, name = "monolis_test_check_eq_I1")
    implicit none
    character :: array(*)
    character(monolis_charlen) :: header
    integer(c_int), value :: a, b

    header = monolis_string_c2f(array)
    call monolis_test_check_eq_I1(header, a, b)
  end subroutine monolis_test_check_eq_I1_c

  subroutine monolis_test_check_eq_R1_c(array, a, b) &
    & bind(c, name = "monolis_test_check_eq_R1")
    implicit none
    character :: array(*)
    character(monolis_charlen) :: header
    real(c_double), value :: a, b

    header = monolis_string_c2f(array)
    call monolis_test_check_eq_R1(header, a, b)
  end subroutine monolis_test_check_eq_R1_c

  subroutine monolis_test_check_eq_C1_c(array, a, b) &
    & bind(c, name = "monolis_test_check_eq_C1")
    implicit none
    character :: array(*)
    character(monolis_charlen) :: header
    complex(c_double), value :: a, b

    header = monolis_string_c2f(array)
    call monolis_test_check_eq_C1(header, a, b)
  end subroutine monolis_test_check_eq_C1_c

  function monolis_string_c2f(array)
    character :: array(*)
    character(monolis_charlen) :: monolis_string_c2f
    integer(kint) :: i, len

    len = 1
    do while (array(len) /= c_null_char)
      len = len + 1
    enddo

    len = len - 1

    do i = 1, min(len, monolis_charlen)
      monolis_string_c2f(i:i) = array(i)
    enddo

    do i = len + 1, monolis_charlen
      monolis_string_c2f(i:i) = ""
    enddo
  end function
end module mod_monolis_utils_std_test_wrap
