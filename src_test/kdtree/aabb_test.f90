!> バウンディングボックステストモジュール
module mod_monolis_utils_aabb_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_utils_aabb_test()
    implicit none

    call monolis_get_aabb_from_coodinates_test()
    call monolis_get_aabb_from_BB_test()
    call monolis_check_inside_in_aabb_test()
  end subroutine monolis_utils_aabb_test

  !> unit test
  subroutine monolis_get_aabb_from_coodinates_test()
    implicit none
    real(kdouble) :: coord(3,3)
    real(kdouble) :: BB(6), BB_ans(6)

    call monolis_std_global_log_string("monolis_get_aabb_from_coodinates")

    coord(1,1) = 1.0d0; coord(2,1) = 10.0d0; coord(3,1) = 100.0d0;
    coord(1,2) = 2.0d0; coord(2,2) = 20.0d0; coord(3,2) = 200.0d0;
    coord(1,3) = 3.0d0; coord(2,3) = 30.0d0; coord(3,3) = 300.0d0;

    call monolis_get_aabb_from_coodinates(coord, BB)

    BB_ans(1) = 1.0d0
    BB_ans(2) = 3.0d0
    BB_ans(3) = 10.0d0
    BB_ans(4) = 30.0d0
    BB_ans(5) = 100.0d0
    BB_ans(6) = 300.0d0

    call monolis_test_check_eq_R("monolis_get_aabb_from_coodinates_test", BB, BB_ans)
  end subroutine monolis_get_aabb_from_coodinates_test

  subroutine monolis_get_aabb_from_BB_test()
    implicit none
    real(kdouble) :: BB_in(6,3)
    real(kdouble) :: BB(6), BB_ans(6)

    call monolis_std_global_log_string("monolis_get_aabb_from_BB")

    BB_in(1,1) = 1.0d0; BB_in(2,1) = 10.0d0; BB_in(3,1) = 4.0d0; BB_in(4,1) = 40.0d0; BB_in(5,1) = 7.0d0; BB_in(6,1) = 70.0d0;
    BB_in(1,2) = 2.0d0; BB_in(2,2) = 20.0d0; BB_in(3,2) = 5.0d0; BB_in(4,2) = 50.0d0; BB_in(5,2) = 8.0d0; BB_in(6,2) = 80.0d0;
    BB_in(1,3) = 3.0d0; BB_in(2,3) = 30.0d0; BB_in(3,3) = 6.0d0; BB_in(4,3) = 60.0d0; BB_in(5,3) = 9.0d0; BB_in(6,3) = 90.0d0;

    call monolis_get_aabb_from_BB(BB_in, BB)

    BB_ans(1) = 1.0d0
    BB_ans(2) = 30.0d0
    BB_ans(3) = 4.0d0
    BB_ans(4) = 60.0d0
    BB_ans(5) = 7.0d0
    BB_ans(6) = 90.0d0

    call monolis_test_check_eq_R("monolis_get_aabb_from_BB_test", BB, BB_ans)
  end subroutine monolis_get_aabb_from_BB_test

  subroutine monolis_check_inside_in_aabb_test()
    implicit none
    real(kdouble) :: coord(3)
    real(kdouble) :: BB(6), ths
    logical :: is_inside

    call monolis_std_global_log_string("monolis_check_inside_in_aabb")

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

    call monolis_test_check_eq_L1("monolis_check_inside_in_aabb 1", is_inside, .true.)

    coord(1) = 0.0d0
    coord(2) = 20.0d0
    coord(3) = 200.0d0

    call monolis_check_inside_in_aabb(coord, BB, ths, is_inside)

    call monolis_test_check_eq_L1("monolis_check_inside_in_aabb 2", is_inside, .false.)

    coord(1) = 2.0d0
    coord(2) = 0.0d0
    coord(3) = 200.0d0

    call monolis_check_inside_in_aabb(coord, BB, ths, is_inside)

    call monolis_test_check_eq_L1("monolis_check_inside_in_aabb 3", is_inside, .false.)


    coord(1) = 2.0d0
    coord(2) = 20.0d0
    coord(3) = 0.0d0

    call monolis_check_inside_in_aabb(coord, BB, ths, is_inside)

    call monolis_test_check_eq_L1("monolis_check_inside_in_aabb 4", is_inside, .false.)
  end subroutine monolis_check_inside_in_aabb_test
end module mod_monolis_utils_aabb_test
