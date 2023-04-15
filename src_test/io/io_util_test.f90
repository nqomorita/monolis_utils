!> IO util テストモジュール
module mod_monolis_io_util_test
  use mod_monolis_utils
  implicit none

contains

  subroutine monolis_io_util_test()
    implicit none

    call monolis_check_fortran_1_origin_elem_test()
    call monolis_check_fortran_1_origin_graph_test()
  end subroutine monolis_io_util_test

  subroutine monolis_check_fortran_1_origin_elem_test()
    implicit none
    integer(kint) :: elem(2,2)
    logical :: is_1_origin

    call monolis_std_global_log_string("monolis_check_fortran_1_origin_elem")

    elem(1,1) = 1; elem(2,1) = 2
    elem(1,2) = 2; elem(2,2) = 3

    call monolis_check_fortran_1_origin_elem(elem, is_1_origin)

    call monolis_test_check_eq_L1("monolis_check_fortran_1_origin_elem 1", is_1_origin, .true.)

    elem(1,1) = 0; elem(2,1) = 1
    elem(1,2) = 1; elem(2,2) = 2

    call monolis_check_fortran_1_origin_elem(elem, is_1_origin)

    call monolis_test_check_eq_L1("monolis_check_fortran_1_origin_elem 2", is_1_origin, .false.)
  end subroutine monolis_check_fortran_1_origin_elem_test

  subroutine monolis_check_fortran_1_origin_graph_test()
    implicit none
    integer(kint) :: item(3)
    logical :: is_1_origin

    call monolis_std_global_log_string("monolis_check_fortran_1_origin_graph")

    item(1) = 1
    item(2) = 2
    item(3) = 3

    call monolis_check_fortran_1_origin_graph(item, is_1_origin)

    call monolis_test_check_eq_L1("monolis_check_fortran_1_origin_graph 1", is_1_origin, .true.)

    item(1) = 0
    item(2) = 1
    item(3) = 2

    call monolis_check_fortran_1_origin_graph(item, is_1_origin)

    call monolis_test_check_eq_L1("monolis_check_fortran_1_origin_graph 1", is_1_origin, .false.)
  end subroutine monolis_check_fortran_1_origin_graph_test
end module mod_monolis_io_util_test
