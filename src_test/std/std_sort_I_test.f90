!> std sort テストモジュール
module mod_monolis_utils_std_sort_I_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_utils_std_sort_I_test()
    implicit none

    call monolis_qsort_I_1d_test()
    call monolis_qsort_I_2d_test()
    call monolis_bsearch_I_test()
    call monolis_get_sequence_array_I_test()
    call monolis_get_uniq_array_I_test()
    call monolis_perm_array_I_test()
  end subroutine monolis_utils_std_sort_I_test

  !> unit test
  subroutine monolis_qsort_I_1d_test()
    implicit none
    integer(kint) :: a(5), b(5)

    call monolis_std_global_log_string("monolis_qsort_I_1d")

    !> case 1
    a(1) = 5
    a(2) = 1
    a(3) = 4
    a(4) = 3
    a(5) = 2

    call monolis_qsort_I_1d(a, 1, 5)

    b(1) = 1
    b(2) = 2
    b(3) = 3
    b(4) = 4
    b(5) = 5

    call monolis_test_check_eq_I("monolis_qsort_I_1d case 1", a, b)

    !> case 2
    a(1) = 5
    a(2) = 1
    a(3) = 4
    a(4) = 3
    a(5) = 2

    call monolis_qsort_I_1d(a, 1, 3)

    b(1) = 1
    b(2) = 4
    b(3) = 5
    b(4) = 3
    b(5) = 2

    call monolis_test_check_eq_I("monolis_qsort_I_1d case 2", a, b)

    !> case 3
    a(1) = 5
    a(2) = 5
    a(3) = 4
    a(4) = 4
    a(5) = 2

    call monolis_qsort_I_1d(a, 1, 5)

    b(1) = 2
    b(2) = 4
    b(3) = 4
    b(4) = 5
    b(5) = 5

    call monolis_test_check_eq_I("monolis_qsort_I_1d case 3", a, b)
  end subroutine monolis_qsort_I_1d_test

  subroutine monolis_qsort_I_2d_test()
    implicit none
    integer(kint) :: a(5), b(5), c(5), d(5)

    call monolis_std_global_log_string("monolis_qsort_I_2d")

    !> case 1
    a(1) = 5; b(1) = 50
    a(2) = 1; b(2) = 10
    a(3) = 4; b(3) = 40
    a(4) = 3; b(4) = 30
    a(5) = 2; b(5) = 20

    call monolis_qsort_I_2d(a, b, 1, 5)

    c(1) = 1; d(1) = 10
    c(2) = 2; d(2) = 20
    c(3) = 3; d(3) = 30
    c(4) = 4; d(4) = 40
    c(5) = 5; d(5) = 50

    call monolis_test_check_eq_I("monolis_qsort_I_2d case 1", a, c)
    call monolis_test_check_eq_I("monolis_qsort_I_2d case 1", b, d)

    !> case 2
    a(1) = 5; b(1) = 50
    a(2) = 1; b(2) = 10
    a(3) = 4; b(3) = 40
    a(4) = 3; b(4) = 30
    a(5) = 2; b(5) = 20

    call monolis_qsort_I_2d(a, b, 1, 3)

    c(1) = 1; d(1) = 10
    c(2) = 4; d(2) = 40
    c(3) = 5; d(3) = 50
    c(4) = 3; d(4) = 30
    c(5) = 2; d(5) = 20

    call monolis_test_check_eq_I("monolis_qsort_I_2d case 2", a, c)
    call monolis_test_check_eq_I("monolis_qsort_I_2d case 2", b, d)

    !> case 3
    a(1) = 5; b(1) = 50
    a(2) = 5; b(2) = 50
    a(3) = 4; b(3) = 40
    a(4) = 4; b(4) = 40
    a(5) = 2; b(5) = 20

    call monolis_qsort_I_2d(a, b, 1, 5)

    c(1) = 2; d(1) = 20
    c(2) = 4; d(2) = 40
    c(3) = 4; d(3) = 40
    c(4) = 5; d(4) = 50
    c(5) = 5; d(5) = 50

    call monolis_test_check_eq_I("monolis_qsort_I_2d case 3", a, c)
    call monolis_test_check_eq_I("monolis_qsort_I_2d case 3", b, d)
  end subroutine monolis_qsort_I_2d_test

  subroutine monolis_bsearch_I_test()
    implicit none
    integer(kint) :: a(5), idx

    call monolis_std_global_log_string("monolis_bsearch_I")

    !> case 1
    a(1) = 10
    a(2) = 20
    a(3) = 30
    a(4) = 40
    a(5) = 50

    call monolis_bsearch_I(a, 1, 5, 20, idx)

    call monolis_test_check_eq_I1("monolis_bsearch_I case 1", idx, 2)

    call monolis_bsearch_I(a, 1, 5, 10, idx)

    call monolis_test_check_eq_I1("monolis_bsearch_I case 2", idx, 1)

    call monolis_bsearch_I(a, 1, 3, 40, idx)

    call monolis_test_check_eq_I1("monolis_bsearch_I case 3", idx, -1)
  end subroutine monolis_bsearch_I_test

  subroutine monolis_get_sequence_array_I_test()
    implicit none
    integer(kint) :: a(5), b(5)

    call monolis_std_global_log_string("monolis_get_sequence_array_I")

    !> case 1
    call monolis_get_sequence_array_I(a, 5, 1, 1)

    b(1) = 1
    b(2) = 2
    b(3) = 3
    b(4) = 4
    b(5) = 5

    call monolis_test_check_eq_I("monolis_get_sequence_array_I case 1", a, b)

    !> case 2
    call monolis_get_sequence_array_I(a, 5, 2, 2)

    b(1) = 2
    b(2) = 4
    b(3) = 6
    b(4) = 8
    b(5) = 10

    call monolis_test_check_eq_I("monolis_get_sequence_array_I case 2", a, b)
  end subroutine monolis_get_sequence_array_I_test

  subroutine monolis_get_uniq_array_I_test()
    implicit none
    integer(kint) :: a(5), b(3), newlen

    call monolis_std_global_log_string("monolis_get_uniq_array_I")

    !> case 1
    a(1) = 1
    a(2) = 2
    a(3) = 2
    a(4) = 4
    a(5) = 4

    call monolis_get_uniq_array_I(a, 5, newlen)

    b(1) = 1
    b(2) = 2
    b(3) = 4

    call monolis_test_check_eq_I ("monolis_get_uniq_array_I case 1", a(1:3), b)
    call monolis_test_check_eq_I1("monolis_get_uniq_array_I case 1", newlen, 3)
  end subroutine monolis_get_uniq_array_I_test

  subroutine monolis_perm_array_I_test()
    implicit none
    integer(kint) :: perm(5), a(5), b(5)

    call monolis_std_global_log_string("monolis_perm_array_I")

    a(1) = 1
    a(2) = 2
    a(3) = 3
    a(4) = 4
    a(5) = 5

    perm(1) = 5
    perm(2) = 4
    perm(3) = 3
    perm(4) = 2
    perm(5) = 1

    call monolis_perm_array_I(a, perm, 5)

    b(1) = 5
    b(2) = 4
    b(3) = 3
    b(4) = 2
    b(5) = 1

    call monolis_test_check_eq_I("monolis_perm_array_I case 1", a, b)
  end subroutine monolis_perm_array_I_test

end module mod_monolis_utils_std_sort_I_test
