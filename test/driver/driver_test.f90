module mod_monolis_driver_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_driver_test()
    implicit none

    call monolis_dbc_all_surf_hex_test()
    call monolis_dbc_all_surf_tet_test()
    call monolis_extract_all_surf_hex_test()
    call monolis_extract_all_surf_tet_test()
    call monolis_p_refiner_tet_test()
    call monolis_h_refiner_tet_test()
    call monolis_h_refiner_hex_test()
  end subroutine monolis_driver_test

  subroutine monolis_dbc_all_surf_hex_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: n_bc, n_dof, i
    integer(kint) :: i_bc_ans(26)
    integer(kint), allocatable :: i_bc(:,:)
    real(kdouble), allocatable :: r_bc(:)

    call monolis_std_log_string("monolis_dbc_all_surf_hex_test")

    fname = "driver/output/dbc.hex.dat"
    call monolis_input_bc(fname, n_bc, n_dof, i_bc, r_bc)

    call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test 1", n_bc, 52)
    call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test 2", n_dof, 2)

    i_bc_ans(1) = 1
    i_bc_ans(2) = 2
    i_bc_ans(3) = 3
    i_bc_ans(4) = 4
    i_bc_ans(5) = 5
    i_bc_ans(6) = 6
    i_bc_ans(7) = 7
    i_bc_ans(8) = 8
    i_bc_ans(9) = 9
    i_bc_ans(10) = 10
    i_bc_ans(11) = 11
    i_bc_ans(12) = 12
    i_bc_ans(13) = 13
    i_bc_ans(14) = 15
    i_bc_ans(15) = 16
    i_bc_ans(16) = 17
    i_bc_ans(17) = 18
    i_bc_ans(18) = 19
    i_bc_ans(19) = 20
    i_bc_ans(20) = 21
    i_bc_ans(21) = 22
    i_bc_ans(22) = 23
    i_bc_ans(23) = 24
    i_bc_ans(24) = 25
    i_bc_ans(25) = 26
    i_bc_ans(26) = 27

    do i = 1, 26
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test 3", i_bc(1,2*i-1), i_bc_ans(i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test 4", i_bc(2,2*i-1), 1)
      call monolis_test_check_eq_R1("monolis_dbc_all_surf_hex_test 5", r_bc(2*i-1), 1.0d0)
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test 3", i_bc(1,2*i  ), i_bc_ans(i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test 4", i_bc(2,2*i  ), 2)
      call monolis_test_check_eq_R1("monolis_dbc_all_surf_hex_test 5", r_bc(2*i  ), 2.0d0)
    enddo
  end subroutine monolis_dbc_all_surf_hex_test

  subroutine monolis_dbc_all_surf_tet_test()
    implicit none
    !character(monolis_charlen) :: fname
    !integer(kint) :: n_bc, n_dof, i
    !integer(kint) :: i_bc_ans(26)
    !integer(kint), allocatable :: i_bc(:,:)
    !real(kdouble), allocatable :: r_bc(:)

    call monolis_std_log_string("monolis_dbc_all_surf_tet_test")
  end subroutine monolis_dbc_all_surf_tet_test

  subroutine monolis_extract_all_surf_hex_test()
    implicit none

    call monolis_std_log_string("monolis_extract_all_surf_hex_test")
  end subroutine monolis_extract_all_surf_hex_test

  subroutine monolis_extract_all_surf_tet_test()
    implicit none

    call monolis_std_log_string("monolis_extract_all_surf_tet_test")
  end subroutine monolis_extract_all_surf_tet_test

  subroutine monolis_p_refiner_tet_test()
    implicit none

    call monolis_std_log_string("monolis_p_refiner_tet_test")
  end subroutine monolis_p_refiner_tet_test

  subroutine monolis_h_refiner_tet_test()
    implicit none

    call monolis_std_log_string("monolis_h_refiner_tet_test")
  end subroutine monolis_h_refiner_tet_test

  subroutine monolis_h_refiner_hex_test()
    implicit none

    call monolis_std_log_string("monolis_h_refiner_hex_test")
  end subroutine monolis_h_refiner_hex_test
end module mod_monolis_driver_test
