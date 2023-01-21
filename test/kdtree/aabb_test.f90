!> バウンディングボックステストモジュール
module mod_monolis_utils_aabb_test
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_error
  use mod_monolis_utils_std_test
  use mod_monolis_utils_aabb
  implicit none

contains

  !> main test subroutine
  subroutine monolis_utils_aabb_test()
    implicit none

    call monolis_get_aabb_test()
    call monolis_check_inside_in_aabb_test()
  end subroutine monolis_utils_aabb_test

  !> unit test
  subroutine monolis_get_aabb_test()
    implicit none
    real(kdouble) :: coord(3,3)
    real(kdouble) :: BB(6), BB_ans(6)

    call monolis_std_log_string("monolis_get_aabb_test")

    coord(1,1) = 1.0d0; coord(2,1) = 10.0d0; coord(3,1) = 100.0d0;
    coord(1,2) = 2.0d0; coord(2,2) = 20.0d0; coord(3,2) = 200.0d0;
    coord(1,3) = 3.0d0; coord(2,3) = 30.0d0; coord(3,3) = 300.0d0;

    call monolis_get_aabb(coord, BB)

    BB_ans(1) = 1.0d0
    BB_ans(2) = 3.0d0
    BB_ans(3) = 10.0d0
    BB_ans(4) = 30.0d0
    BB_ans(5) = 100.0d0
    BB_ans(6) = 300.0d0

    call monolis_test_check_eq_R("monolis_get_aabb_test", BB, BB_ans)
  end subroutine monolis_get_aabb_test

  subroutine monolis_check_inside_in_aabb_test()
    implicit none
    real(kdouble) :: coord(3)
    real(kdouble) :: BB(6), ths
    logical :: is_inside

    call monolis_std_log_string("monolis_check_inside_in_aabb_test")

    BB(1) = 1.0d0
    BB(2) = 3.0d0
    BB(3) = 10.0d0
    BB(4) = 30.0d0
    BB(5) = 100.0d0
    BB(6) = 300.0d0

    ths = 0.0d0

    coord(1) = 2.0d0
    coord(2) = 20.0d0
    coord(3) = 200.0d0

    call monolis_check_inside_in_aabb(coord, BB, ths, is_inside)

    call monolis_test_check_eq_L1("monolis_check_inside_in_aabb_test 1", is_inside, .true.)

    coord(1) = 0.0d0
    coord(2) = 20.0d0
    coord(3) = 200.0d0

    call monolis_check_inside_in_aabb(coord, BB, ths, is_inside)

    call monolis_test_check_eq_L1("monolis_check_inside_in_aabb_test 2", is_inside, .false.)

    coord(1) = 2.0d0
    coord(2) = 0.0d0
    coord(3) = 200.0d0

    call monolis_check_inside_in_aabb(coord, BB, ths, is_inside)

    call monolis_test_check_eq_L1("monolis_check_inside_in_aabb_test 3", is_inside, .false.)


    coord(1) = 2.0d0
    coord(2) = 20.0d0
    coord(3) = 0.0d0

    call monolis_check_inside_in_aabb(coord, BB, ths, is_inside)

    call monolis_test_check_eq_L1("monolis_check_inside_in_aabb_test 4", is_inside, .false.)
  end subroutine monolis_check_inside_in_aabb_test
end module mod_monolis_utils_aabb_test
