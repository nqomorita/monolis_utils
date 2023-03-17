!> std テストモジュール
module mod_monolis_utils_std_test_wrap
  use mod_monolis_utils_std_test
  use iso_c_binding
  implicit none

contains

  subroutine monolis_test_check_eq_I1_c() &
    & bind(c, name = "monolis_test_check_eq_I1")
    implicit none
    !character :: array(*)
    !character(monolis_charlen) :: header
    !integer(c_int), value :: a, b
    integer(kint) :: c, d
!write(*,*)a, b
!write(*,*)1, 2
    c = 1
    d = 1
    !header = monolis_string_c2f(array)
    call monolis_test_check_eq_I1("test", c, d)
  end subroutine monolis_test_check_eq_I1_c

  !function monolis_string_c2f(array)
  !  character :: array(*)
  !  character(monolis_charlen) :: monolis_string_c2f
  !  integer(kint) :: i, len

  !  len = 1
  !  do while (array(len) /= c_null_char)
  !    len = len + 1
  !  enddo

  !  len = len - 1

  !  do i = 1, min(len, monolis_charlen)
  !    monolis_string_c2f(i:i) = array(i)
  !  enddo

  !  do i = len + 1, monolis_charlen
  !    monolis_string_c2f(i:i) = ""
  !  enddo
  !end function
end module mod_monolis_utils_std_test_wrap
