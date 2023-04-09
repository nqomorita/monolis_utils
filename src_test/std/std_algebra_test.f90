!> std algebra テストモジュール
module mod_monolis_utils_std_algebra_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_utils_std_algebra_test()
    implicit none

    call monolis_get_inverse_matrix_R_test()
    call monolis_get_inverse_matrix_R_2d_test()
    call monolis_get_inverse_matrix_R_3d_test()
    call monolis_normalize_cross_product_R_3d_test()
    call monolis_cross_product_R_3d_test()
    call monolis_normalize_vector_R_test()
    call monolis_get_l2_norm_R_test()
    call monolis_get_rundom_number_R_test()
  end subroutine monolis_utils_std_algebra_test

  !> unit test
  subroutine monolis_get_inverse_matrix_R_test()
    implicit none
    real(kdouble) :: a(3,3), b(3,3), c(3,3)

    call monolis_std_global_log_string("monolis_get_inverse_matrix_R")

    !> case 1
    a(1,1) = 3.0d0; a(1,2) = 3.0d0; a(1,3) = 2.0d0
    a(2,1) = 2.0d0; a(2,2) = 3.0d0; a(2,3) = 3.0d0
    a(3,1) = 1.0d0; a(3,2) = 2.0d0; a(3,3) = 1.0d0

    call monolis_get_inverse_matrix_R(3, a, b)

    c(1,1) = 0.75d0; c(1,2) =-0.25d0; c(1,3) =-0.75d0
    c(2,1) =-0.25d0; c(2,2) =-0.25d0; c(2,3) = 1.25d0
    c(3,1) =-0.25d0; c(3,2) = 0.75d0; c(3,3) =-0.75d0

    call monolis_test_check_eq_R("monolis_get_inverse_matrix_R 1", b(:,1), c(:,1))
    call monolis_test_check_eq_R("monolis_get_inverse_matrix_R 1", b(:,2), c(:,2))
    call monolis_test_check_eq_R("monolis_get_inverse_matrix_R 1", b(:,3), c(:,3))
  end subroutine monolis_get_inverse_matrix_R_test

  subroutine monolis_get_inverse_matrix_R_2d_test()
    implicit none
    real(kdouble) :: a(2,2), b(2,2), c(2,2), det

    call monolis_std_global_log_string("monolis_get_inverse_matrix_R_2d")

    !> case 1
    a(1,1) = 3.0d0; a(1,2) = 4.0d0
    a(2,1) = 2.0d0; a(2,2) = 3.0d0

    call monolis_get_inverse_matrix_R_2d(a, b, det)

    c(1,1) = 3.0d0; c(1,2) =-4.0d0
    c(2,1) =-2.0d0; c(2,2) = 3.0d0

    call monolis_test_check_eq_R ("monolis_get_inverse_matrix_R_2d 1", b(:,1), c(:,1))
    call monolis_test_check_eq_R ("monolis_get_inverse_matrix_R_2d 1", b(:,2), c(:,2))
    call monolis_test_check_eq_R1("monolis_get_inverse_matrix_R_2d 1", det, 1.0d0)
  end subroutine monolis_get_inverse_matrix_R_2d_test

  subroutine monolis_get_inverse_matrix_R_3d_test()
    implicit none
    real(kdouble) :: a(3,3), b(3,3), c(3,3), det
    logical :: is_fail

    call monolis_std_global_log_string("monolis_get_inverse_matrix_R_3d")

    !> case 1
    a(1,1) = 3.0d0; a(1,2) = 3.0d0; a(1,3) = 2.0d0
    a(2,1) = 2.0d0; a(2,2) = 3.0d0; a(2,3) = 3.0d0
    a(3,1) = 1.0d0; a(3,2) = 2.0d0; a(3,3) = 1.0d0

    call monolis_get_inverse_matrix_R_3d(a, b, det, is_fail)

    c(1,1) = 0.75d0; c(1,2) =-0.25d0; c(1,3) =-0.75d0
    c(2,1) =-0.25d0; c(2,2) =-0.25d0; c(2,3) = 1.25d0
    c(3,1) =-0.25d0; c(3,2) = 0.75d0; c(3,3) =-0.75d0

    call monolis_test_check_eq_R ("monolis_get_inverse_matrix_R_3d 1", b(:,1), c(:,1))
    call monolis_test_check_eq_R ("monolis_get_inverse_matrix_R_3d 1", b(:,2), c(:,2))
    call monolis_test_check_eq_R ("monolis_get_inverse_matrix_R_3d 1", b(:,3), c(:,3))
    call monolis_test_check_eq_L1("monolis_get_inverse_matrix_R_3d 1", is_fail, .true.)
  end subroutine monolis_get_inverse_matrix_R_3d_test

  subroutine monolis_normalize_cross_product_R_3d_test()
    implicit none
    real(kdouble) :: v1(3), v2(3), v3(3), v4(3)

    call monolis_std_global_log_string("monolis_normalize_cross_product_R_3d")

    !> case 1
    v1(1) = 2.0d0;
    v1(2) = 0.0d0;
    v1(3) = 0.0d0;

    v2(1) = 0.0d0;
    v2(2) = 2.0d0;
    v2(3) = 0.0d0;

    call monolis_normalize_cross_product_R_3d(v1, v2, v3)

    v4(1) = 0.0d0;
    v4(2) = 0.0d0;
    v4(3) = 1.0d0;

    call monolis_test_check_eq_R ("monolis_normalize_cross_product_R_3d 1", v3, v4)

    !> case 2
    v1(1) = 0.0d0;
    v1(2) = 2.0d0;
    v1(3) = 0.0d0;

    v2(1) = 0.0d0;
    v2(2) = 0.0d0;
    v2(3) = 2.0d0;

    call monolis_normalize_cross_product_R_3d(v1, v2, v3)

    v4(1) = 1.0d0;
    v4(2) = 0.0d0;
    v4(3) = 0.0d0;

    call monolis_test_check_eq_R ("monolis_normalize_cross_product_R_3d 2", v3, v4)

    !> case 3
    v1(1) = 0.0d0;
    v1(2) = 0.0d0;
    v1(3) = 2.0d0;

    v2(1) = 2.0d0;
    v2(2) = 0.0d0;
    v2(3) = 0.0d0;

    call monolis_normalize_cross_product_R_3d(v1, v2, v3)

    v4(1) = 0.0d0;
    v4(2) = 1.0d0;
    v4(3) = 0.0d0;

    call monolis_test_check_eq_R ("monolis_normalize_cross_product_R_3d 3", v3, v4)
  end subroutine monolis_normalize_cross_product_R_3d_test

  subroutine monolis_cross_product_R_3d_test()
    implicit none
    real(kdouble) :: v1(3), v2(3), v3(3), v4(3)

    call monolis_std_global_log_string("monolis_cross_product_R_3d")

    !> case 1
    v1(1) = 2.0d0;
    v1(2) = 0.0d0;
    v1(3) = 0.0d0;

    v2(1) = 0.0d0;
    v2(2) = 2.0d0;
    v2(3) = 0.0d0;

    call monolis_cross_product_R_3d(v1, v2, v3)

    v4(1) = 0.0d0;
    v4(2) = 0.0d0;
    v4(3) = 4.0d0;

    call monolis_test_check_eq_R ("monolis_cross_product_R_3d 1", v3, v4)

    !> case 2
    v1(1) = 0.0d0;
    v1(2) = 2.0d0;
    v1(3) = 0.0d0;

    v2(1) = 0.0d0;
    v2(2) = 0.0d0;
    v2(3) = 2.0d0;

    call monolis_cross_product_R_3d(v1, v2, v3)

    v4(1) = 4.0d0;
    v4(2) = 0.0d0;
    v4(3) = 0.0d0;

    call monolis_test_check_eq_R ("monolis_cross_product_R_3d 2", v3, v4)

    !> case 3
    v1(1) = 0.0d0;
    v1(2) = 0.0d0;
    v1(3) = 2.0d0;

    v2(1) = 2.0d0;
    v2(2) = 0.0d0;
    v2(3) = 0.0d0;

    call monolis_cross_product_R_3d(v1, v2, v3)

    v4(1) = 0.0d0;
    v4(2) = 4.0d0;
    v4(3) = 0.0d0;

    call monolis_test_check_eq_R ("monolis_cross_product_R_3d 3", v3, v4)
  end subroutine monolis_cross_product_R_3d_test

  subroutine monolis_normalize_vector_R_test()
    implicit none
    real(kdouble) :: v1(3), v2(3), v3(3)

    call monolis_std_global_log_string("monolis_normalize_vector_R")

    !> case 1
    v1(1) = 2.0d0;
    v1(2) = 3.0d0;
    v1(3) = 5.0d0;

    call monolis_normalize_vector_R(3, v1, v2)

    v3(1) = 2.0d0/sqrt(38.0d0);
    v3(2) = 3.0d0/sqrt(38.0d0);
    v3(3) = 5.0d0/sqrt(38.0d0);

    call monolis_test_check_eq_R ("monolis_normalize_vector_R 1", v2, v3)
  end subroutine monolis_normalize_vector_R_test

  subroutine monolis_get_l2_norm_R_test()
    implicit none
    real(kdouble) :: v1(3), norm

    call monolis_std_global_log_string("monolis_get_l2_norm_R")

    !> case 1
    v1(1) = 2.0d0;
    v1(2) = 3.0d0;
    v1(3) = 5.0d0;

    call monolis_get_l2_norm_R(3, v1, norm)

    call monolis_test_check_eq_R1 ("monolis_get_l2_norm_R 3", norm, sqrt(38.0d0))
  end subroutine monolis_get_l2_norm_R_test

  subroutine monolis_get_rundom_number_R_test()
    implicit none
    real(kdouble) :: v1(3), norm

    call monolis_std_global_log_string("monolis_get_rundom_number_R")

    call monolis_get_rundom_number_R(3, v1, 1)

    call monolis_test_check_eq_R1 ("monolis_get_rundom_number_R 1", v1(1), 0.92925207013993982d0)
    call monolis_test_check_eq_R1 ("monolis_get_rundom_number_R 2", v1(2), 0.26211879716503622d0)
    call monolis_test_check_eq_R1 ("monolis_get_rundom_number_R 3", v1(3), 0.35532631100852446d0)
  end subroutine monolis_get_rundom_number_R_test
end module mod_monolis_utils_std_algebra_test
