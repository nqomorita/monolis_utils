module mod_monolis_shape_1d_line_1st_test
  use mod_monolis_utils
  implicit none

contains

  subroutine monolis_shape_1d_line_1st_test()
    implicit none
    integer(kint) :: i
    real(kdouble) :: r(1), func(2), func_d(2,1), pos(1), node(1,2)
    real(kdouble) :: dndx(2,1), det
    logical :: is_inside

    call monolis_std_log_string("monolis_shape_1d_line_1st_test")

    !> monolis_shape_1d_line_1st_num_gauss_point
    call monolis_test_check_eq_I1("monolis_shape_1d_line_1st_test 1", &
      & monolis_shape_1d_line_1st_num_gauss_point(), 1)

    !> monolis_shape_1d_line_1st_weight
    i = 1
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 2", &
      & monolis_shape_1d_line_1st_weight(i), 2.0d0)

    !> monolis_shape_1d_line_1st_integral_point
    i = 1
    call monolis_shape_1d_line_1st_integral_point(i, r)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 3", r(1), 0.0d0)

    !> monolis_shape_1d_line_1st_node_point
    i = 1
    call monolis_shape_1d_line_1st_node_point(i, r)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 4", r(1), -1.0d0)

    i = 2
    call monolis_shape_1d_line_1st_node_point(i, r)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 5", r(1), 1.0d0)

    !> monolis_shape_1d_line_1st_is_inside_domain
    r(1) = -2.0d0
    call monolis_shape_1d_line_1st_is_inside_domain(r, is_inside)
    call monolis_test_check_eq_L1("monolis_shape_1d_line_1st_test 6", is_inside, .false.)

    r(1) = -1.0d0
    call monolis_shape_1d_line_1st_is_inside_domain(r, is_inside)
    call monolis_test_check_eq_L1("monolis_shape_1d_line_1st_test 7", is_inside, .true.)

    r(1) = 0.0d0
    call monolis_shape_1d_line_1st_is_inside_domain(r, is_inside)
    call monolis_test_check_eq_L1("monolis_shape_1d_line_1st_test 8", is_inside, .true.)

    r(1) = 1.0d0
    call monolis_shape_1d_line_1st_is_inside_domain(r, is_inside)
    call monolis_test_check_eq_L1("monolis_shape_1d_line_1st_test 9", is_inside, .true.)

    r(1) = 2.0d0
    call monolis_shape_1d_line_1st_is_inside_domain(r, is_inside)
    call monolis_test_check_eq_L1("monolis_shape_1d_line_1st_test 10", is_inside, .false.)

    !> monolis_shape_1d_line_1st_shapefunc
    r(1) = -1.0d0
    call monolis_shape_1d_line_1st_shapefunc(r, func)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 11", func(1), 1.0d0)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 12", func(2), 0.0d0)

    r(1) = 0.0d0
    call monolis_shape_1d_line_1st_shapefunc(r, func)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 13", func(1), 0.5d0)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 14", func(2), 0.5d0)

    r(1) = 1.0d0
    call monolis_shape_1d_line_1st_shapefunc(r, func)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 15", func(1), 0.0d0)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 16", func(2), 1.0d0)

    !> monolis_shape_1d_line_1st_shapefunc_deriv
    r(1) = 0.0d0
    call monolis_shape_1d_line_1st_shapefunc_deriv(r, func_d)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 17", func_d(1,1), -0.5d0)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 18", func_d(2,1), 0.5d0)

    !> monolis_shape_1d_line_1st_get_global_position
    node(1,1) = 0.0d0
    node(1,2) = 1.0d0
    r(1) = -1.0d0
    call monolis_shape_1d_line_1st_get_global_position(node, r, pos)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 19", pos(1), 0.0d0)

    r(1) = 0.0d0
    call monolis_shape_1d_line_1st_get_global_position(node, r, pos)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 20", pos(1), 0.5d0)

    r(1) = 1.0d0
    call monolis_shape_1d_line_1st_get_global_position(node, r, pos)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 21", pos(1), 1.0d0)

    !> monolis_shape_1d_line_1st_get_global_deriv
    node(1,1) = 0.0d0
    node(1,2) = 1.0d0
    r(1) = 0.0d0
    call monolis_shape_1d_line_1st_get_global_deriv(node, r, dndx, det)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 22", dndx(1,1), -1.0d0)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 23", dndx(2,1), 1.0d0)
    call monolis_test_check_eq_R1("monolis_shape_1d_line_1st_test 24", det, 0.5d0)
  end subroutine monolis_shape_1d_line_1st_test

end module mod_monolis_shape_1d_line_1st_test