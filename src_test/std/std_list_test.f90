!> std list テストモジュール
module mod_monolis_utils_std_list_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_utils_std_list_test()
    implicit none
    call monolis_list_initialize_R_test()
    call monolis_list_initialize_I_test()
    call monolis_list_initialize_C_test()
    call monolis_list_finalize_R_test()
    call monolis_list_finalize_I_test()
    call monolis_list_finalize_C_test()
    call monolis_list_set_R_test()
    call monolis_list_set_I_test()
    call monolis_list_set_C_test()
    call monolis_list_get_R_test()
    call monolis_list_get_I_test()
    call monolis_list_get_C_test()
  end subroutine monolis_utils_std_list_test

  subroutine monolis_list_initialize_R_test()
    implicit none
    type(monolis_list_R) :: list_struct_R(1)

    call monolis_std_log_string("monolis_list_initialize_R")
    call monolis_list_initialize_R(list_struct_R, 1)
    call monolis_test_check_eq_I1("monolis_list_initialize_R", list_struct_R(1)%n, 0)
  end subroutine monolis_list_initialize_R_test

  subroutine monolis_list_initialize_I_test()
    implicit none
    type(monolis_list_I) :: list_struct_I(1)

    call monolis_std_log_string("monolis_list_initialize_I")
    call monolis_list_initialize_I(list_struct_I, 1)
    call monolis_test_check_eq_I1("monolis_list_initialize_I", list_struct_I(1)%n, 0)
  end subroutine monolis_list_initialize_I_test

  subroutine monolis_list_initialize_C_test()
    implicit none
    type(monolis_list_C) :: list_struct_C(1)

    call monolis_std_log_string("monolis_list_initialize_C")
    call monolis_list_initialize_C(list_struct_C, 1)
    call monolis_test_check_eq_I1("monolis_list_initialize_C", list_struct_C(1)%n, 0)
  end subroutine monolis_list_initialize_C_test

  subroutine monolis_list_finalize_R_test()
    implicit none
    type(monolis_list_R) :: list_struct_R(1)

    call monolis_std_log_string("monolis_list_finalize_R")
    list_struct_R(1)%n = 1
    call monolis_list_finalize_R(list_struct_R)
    call monolis_test_check_eq_I1("monolis_list_finalize_R", list_struct_R(1)%n, 0)
  end subroutine monolis_list_finalize_R_test

  subroutine monolis_list_finalize_I_test()
    implicit none
    type(monolis_list_I) :: list_struct_I(1)

    call monolis_std_log_string("monolis_list_finalize_I")
    list_struct_I(1)%n = 1
    call monolis_list_finalize_I(list_struct_I)
    call monolis_test_check_eq_I1("monolis_list_finalize_I", list_struct_I(1)%n, 0)
  end subroutine monolis_list_finalize_I_test

  subroutine monolis_list_finalize_C_test()
    implicit none
    type(monolis_list_C) :: list_struct_C(1)

    call monolis_std_log_string("monolis_list_finalize_C")
    list_struct_C(1)%n = 1
    call monolis_list_finalize_C(list_struct_C)
    call monolis_test_check_eq_I1("monolis_list_finalize_C", list_struct_C(1)%n, 0)
  end subroutine monolis_list_finalize_C_test

  subroutine monolis_list_set_R_test()
    implicit none
    type(monolis_list_R) :: list_struct_R(1)
    real(kdouble) :: array(1)

    call monolis_std_log_string("monolis_list_set_R")
    call monolis_list_initialize_R(list_struct_R, 1)
    array(1) = 1.0d0
    call monolis_list_set_R(list_struct_R, 1, 1, array)
    call monolis_test_check_eq_I1("monolis_list_set_R n", list_struct_R(1)%n, 1)
    call monolis_test_check_eq_R("monolis_list_set_R array", list_struct_R(1)%array, array)
  end subroutine monolis_list_set_R_test

  subroutine monolis_list_set_I_test()
    implicit none
    type(monolis_list_I) :: list_struct_I(1)
    integer(kint) :: array(1)

    call monolis_std_log_string("monolis_list_set_I")
    call monolis_list_initialize_I(list_struct_I, 1)
    array(1) = 1
    call monolis_list_set_I(list_struct_I, 1, 1, array)
    call monolis_test_check_eq_I1("monolis_list_set_I n", list_struct_I(1)%n, 1)
    call monolis_test_check_eq_I("monolis_list_set_I array", list_struct_I(1)%array, array)
  end subroutine monolis_list_set_I_test

  subroutine monolis_list_set_C_test()
    implicit none
    type(monolis_list_C) :: list_struct_C(1)
    complex(kdouble) :: array(1)

    call monolis_std_log_string("monolis_list_set_C")
    call monolis_list_initialize_C(list_struct_C, 1)
    array(1) = (1.0d0, 1.0d0)
    call monolis_list_set_C(list_struct_C, 1, 1, array)
    call monolis_test_check_eq_I1("monolis_list_set_C n", list_struct_C(1)%n, 1)
    call monolis_test_check_eq_C("monolis_list_set_C array", list_struct_C(1)%array, array)
  end subroutine monolis_list_set_C_test

  subroutine monolis_list_get_R_test()
    implicit none
    type(monolis_list_R) :: list_struct_R(1)
    real(kdouble), allocatable :: array(:)

    call monolis_std_log_string("monolis_list_get_R")

    list_struct_R(1)%n = 1
    call monolis_alloc_R_1d(list_struct_R(1)%array, 1)
    list_struct_R(1)%array = 1.0d0

    call monolis_list_get_R(list_struct_R, 1, array)
    call monolis_test_check_eq_R("monolis_list_get_R", list_struct_R(1)%array, array)
  end subroutine monolis_list_get_R_test

  subroutine monolis_list_get_I_test()
    implicit none
    type(monolis_list_I) :: list_struct_I(1)
    integer(kint), allocatable :: array(:)

    call monolis_std_log_string("monolis_list_get_I")

    list_struct_I(1)%n = 1
    call monolis_alloc_I_1d(list_struct_I(1)%array, 1)
    list_struct_I(1)%array = 1

    call monolis_list_get_I(list_struct_I, 1, array)
    call monolis_test_check_eq_I("monolis_list_get_I", list_struct_I(1)%array, array)
  end subroutine monolis_list_get_I_test

  subroutine monolis_list_get_C_test()
    implicit none
    type(monolis_list_C) :: list_struct_C(1)
    complex(kdouble), allocatable :: array(:)

    call monolis_std_log_string("monolis_list_get_C")

    list_struct_C(1)%n = 1
    call monolis_alloc_C_1d(list_struct_C(1)%array, 1)
    list_struct_C(1)%array = (1.0d0, 1.0d0)

    call monolis_list_get_C(list_struct_C, 1, array)
    call monolis_test_check_eq_C("monolis_list_get_C", list_struct_C(1)%array, array)
  end subroutine monolis_list_get_C_test

end module mod_monolis_utils_std_list_test
