module mod_monolis_driver_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_driver_test()
    implicit none

    call monolis_dbc_all_surf_hex_R_test()
    call monolis_dbc_all_surf_tet_R_test()
    call monolis_dbc_all_surf_hex_C_test()
    call monolis_dbc_all_surf_tet_C_test()
    call monolis_extract_all_surf_hex_test()
    call monolis_extract_all_surf_tet_test()
    call monolis_p_refiner_tet_test()
    call monolis_h_refiner_tet_test()
    call monolis_h_refiner_hex_test()

    call monolis_std_global_log_string("monolis_h_refine_hex")
    call monolis_std_global_log_string("monolis_h_refine_tet")
    call monolis_std_global_log_string("monolis_p_refine_tet")

    call monolis_std_global_log_string("monolis_get_surf")
    call monolis_std_global_log_string("monolis_get_surf_main")
  end subroutine monolis_driver_test

  subroutine monolis_dbc_all_surf_hex_R_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: n_bc, n_dof, i
    integer(kint) :: i_bc_ans(26)
    integer(kint), allocatable :: i_bc(:,:)
    real(kdouble), allocatable :: r_bc(:)

    call monolis_std_global_log_string("monolis_dbc_all_surf_hex_R")

    fname = "driver/output.f/dbc.hex.R.dat"
    call monolis_input_bc_R(fname, n_bc, n_dof, i_bc, r_bc)

    call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test R 1", n_bc, 52)
    call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test R 2", n_dof, 2)

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
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test R 3", i_bc(1,2*i-1), i_bc_ans(i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test R 4", i_bc(2,2*i-1), 1)
      call monolis_test_check_eq_R1("monolis_dbc_all_surf_hex_test R 5", r_bc(2*i-1), 1.0d0)
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test R 3", i_bc(1,2*i  ), i_bc_ans(i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test R 4", i_bc(2,2*i  ), 2)
      call monolis_test_check_eq_R1("monolis_dbc_all_surf_hex_test R 5", r_bc(2*i  ), 2.0d0)
    enddo

    call monolis_dealloc_I_2d(i_bc)
    call monolis_dealloc_R_1d(r_bc)

    fname = "driver/output.c/dbc.hex.R.dat"
    call monolis_input_bc_R(fname, n_bc, n_dof, i_bc, r_bc)

    call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test R 1 Clang", n_bc, 52)
    call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test R 2 Clang", n_dof, 2)

    do i = 1, 26
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test R 3 Clang", i_bc(1,2*i-1), i_bc_ans(i) - 1)
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test R 4 Clang", i_bc(2,2*i-1), 1)
      call monolis_test_check_eq_R1("monolis_dbc_all_surf_hex_test R 5 Clang", r_bc(2*i-1), 1.0d0)
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test R 3 Clang", i_bc(1,2*i  ), i_bc_ans(i) - 1)
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test R 4 Clang", i_bc(2,2*i  ), 2)
      call monolis_test_check_eq_R1("monolis_dbc_all_surf_hex_test R 5 Clang", r_bc(2*i  ), 2.0d0)
    enddo
  end subroutine monolis_dbc_all_surf_hex_R_test

  subroutine monolis_dbc_all_surf_hex_C_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: n_bc, n_dof, i
    integer(kint) :: i_bc_ans(26)
    integer(kint), allocatable :: i_bc(:,:)
    complex(kdouble), allocatable :: c_bc(:)

    call monolis_std_global_log_string("monolis_dbc_all_surf_hex_C")

    fname = "driver/output.f/dbc.hex.C.dat"
    call monolis_input_bc_C(fname, n_bc, n_dof, i_bc, c_bc)

    call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test C 1", n_bc, 52)
    call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test C 2", n_dof, 2)

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
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test C 3", i_bc(1,2*i-1), i_bc_ans(i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test C 4", i_bc(2,2*i-1), 1)
      call monolis_test_check_eq_C1("monolis_dbc_all_surf_hex_test C 5", c_bc(2*i-1), (1.0d0, 2.0d0))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test C 3", i_bc(1,2*i  ), i_bc_ans(i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test C 4", i_bc(2,2*i  ), 2)
      call monolis_test_check_eq_C1("monolis_dbc_all_surf_hex_test C 5", c_bc(2*i  ), (3.0d0, 4.0d0))
    enddo

    call monolis_dealloc_I_2d(i_bc)
    call monolis_dealloc_C_1d(c_bc)

    fname = "driver/output.c/dbc.hex.C.dat"
    call monolis_input_bc_C(fname, n_bc, n_dof, i_bc, c_bc)

    call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test C 1 Clang", n_bc, 52)
    call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test C 2 Clang", n_dof, 2)

    do i = 1, 26
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test C 3 Clang", i_bc(1,2*i-1), i_bc_ans(i) - 1)
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test C 4 Clang", i_bc(2,2*i-1), 1)
      call monolis_test_check_eq_C1("monolis_dbc_all_surf_hex_test C 5 Clang", c_bc(2*i-1), (1.0d0, 2.0d0))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test C 3 Clang", i_bc(1,2*i  ), i_bc_ans(i) - 1)
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_hex_test C 4 Clang", i_bc(2,2*i  ), 2)
      call monolis_test_check_eq_C1("monolis_dbc_all_surf_hex_test C 5 Clang", c_bc(2*i  ), (3.0d0, 4.0d0))
    enddo
  end subroutine monolis_dbc_all_surf_hex_C_test

  subroutine monolis_dbc_all_surf_tet_R_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: n_bc, n_dof, i
    integer(kint) :: i_bc_ans(8)
    integer(kint), allocatable :: i_bc(:,:)
    real(kdouble), allocatable :: r_bc(:)

    call monolis_std_global_log_string("monolis_dbc_all_surf_tet_R")

    fname = "driver/output.f/dbc.tet.R.dat"
    call monolis_input_bc_R(fname, n_bc, n_dof, i_bc, r_bc)

    call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test R 1", n_bc, 16)
    call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test R 2", n_dof, 2)

    i_bc_ans(1) = 1
    i_bc_ans(2) = 2
    i_bc_ans(3) = 3
    i_bc_ans(4) = 4
    i_bc_ans(5) = 5
    i_bc_ans(6) = 6
    i_bc_ans(7) = 7
    i_bc_ans(8) = 8

    do i = 1, 8
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test R 3", i_bc(1,2*i-1), i_bc_ans(i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test R 4", i_bc(2,2*i-1), 1)
      call monolis_test_check_eq_R1("monolis_dbc_all_surf_tet_test R 5", r_bc(2*i-1), 1.0d0)
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test R 3", i_bc(1,2*i  ), i_bc_ans(i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test R 4", i_bc(2,2*i  ), 2)
      call monolis_test_check_eq_R1("monolis_dbc_all_surf_tet_test R 5", r_bc(2*i  ), 2.0d0)
    enddo

    call monolis_dealloc_I_2d(i_bc)
    call monolis_dealloc_R_1d(r_bc)

    fname = "driver/output.c/dbc.tet.R.dat"
    call monolis_input_bc_R(fname, n_bc, n_dof, i_bc, r_bc)

    call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test R 1 Clang", n_bc, 16)
    call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test R 2 Clang", n_dof, 2)

    do i = 1, 8
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test R 3 Clang", i_bc(1,2*i-1), i_bc_ans(i) - 1)
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test R 4 Clang", i_bc(2,2*i-1), 1)
      call monolis_test_check_eq_R1("monolis_dbc_all_surf_tet_test R 5 Clang", r_bc(2*i-1), 1.0d0)
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test R 3 Clang", i_bc(1,2*i  ), i_bc_ans(i) - 1)
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test R 4 Clang", i_bc(2,2*i  ), 2)
      call monolis_test_check_eq_R1("monolis_dbc_all_surf_tet_test R 5 Clang", r_bc(2*i  ), 2.0d0)
    enddo
  end subroutine monolis_dbc_all_surf_tet_R_test

  subroutine monolis_dbc_all_surf_tet_C_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: n_bc, n_dof, i
    integer(kint) :: i_bc_ans(8)
    integer(kint), allocatable :: i_bc(:,:)
    complex(kdouble), allocatable :: c_bc(:)

    call monolis_std_global_log_string("monolis_dbc_all_surf_tet_C")

    fname = "driver/output.f/dbc.tet.C.dat"
    call monolis_input_bc_C(fname, n_bc, n_dof, i_bc, c_bc)

    call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test C 1", n_bc, 16)
    call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test C 2", n_dof, 2)

    i_bc_ans(1) = 1
    i_bc_ans(2) = 2
    i_bc_ans(3) = 3
    i_bc_ans(4) = 4
    i_bc_ans(5) = 5
    i_bc_ans(6) = 6
    i_bc_ans(7) = 7
    i_bc_ans(8) = 8

    do i = 1, 8
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test C 3", i_bc(1,2*i-1), i_bc_ans(i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test C 4", i_bc(2,2*i-1), 1)
      call monolis_test_check_eq_C1("monolis_dbc_all_surf_tet_test C 5", c_bc(2*i-1), (1.0d0, 2.0d0))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test C 3", i_bc(1,2*i  ), i_bc_ans(i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test C 4", i_bc(2,2*i  ), 2)
      call monolis_test_check_eq_C1("monolis_dbc_all_surf_tet_test C 5", c_bc(2*i  ), (3.0d0, 4.0d0))
    enddo

    call monolis_dealloc_I_2d(i_bc)
    call monolis_dealloc_C_1d(c_bc)

    fname = "driver/output.c/dbc.tet.C.dat"
    call monolis_input_bc_C(fname, n_bc, n_dof, i_bc, c_bc)

    call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test C 1 Clang", n_bc, 16)
    call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test C 2 Clang", n_dof, 2)

    do i = 1, 8
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test C 3 Clang", i_bc(1,2*i-1), i_bc_ans(i) - 1)
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test C 4 Clang", i_bc(2,2*i-1), 1)
      call monolis_test_check_eq_C1("monolis_dbc_all_surf_tet_test C 5 Clang", c_bc(2*i-1), (1.0d0, 2.0d0))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test C 3 Clang", i_bc(1,2*i  ), i_bc_ans(i) - 1)
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test C 4 Clang", i_bc(2,2*i  ), 2)
      call monolis_test_check_eq_C1("monolis_dbc_all_surf_tet_test C 5 Clang", c_bc(2*i  ), (3.0d0, 4.0d0))
    enddo
  end subroutine monolis_dbc_all_surf_tet_C_test

  subroutine monolis_extract_all_surf_hex_test()
    character(monolis_charlen) :: fname
    integer(kint) :: n_elem, n_base, i
    integer(kint) :: ians(4,24)
    integer(kint), allocatable :: elem(:,:)

    call monolis_std_global_log_string("monolis_extract_all_surf_hex")

    fname = "driver/output.f/surf.hex.dat"
    call monolis_input_elem(fname, n_elem, n_base, elem)

    call monolis_test_check_eq_I1("monolis_extract_all_surf_hex_test 1", n_elem, 24)
    call monolis_test_check_eq_I1("monolis_extract_all_surf_hex_test 2", n_base, 4)

    ians(1, 1) = 4; ians(2, 1) = 5; ians(3, 1) = 2; ians(4, 1) = 1;
    ians(1, 2) = 1; ians(2, 2) = 2; ians(3, 2) =11; ians(4, 2) =10;
    ians(1, 3) = 4; ians(2, 3) = 1; ians(3, 3) =10; ians(4, 3) =13;
    ians(1, 4) = 5; ians(2, 4) = 6; ians(3, 4) = 3; ians(4, 4) = 2;
    ians(1, 5) = 2; ians(2, 5) = 3; ians(3, 5) =12; ians(4, 5) =11;
    ians(1, 6) = 3; ians(2, 6) = 6; ians(3, 6) =15; ians(4, 6) =12;
    ians(1, 7) = 7; ians(2, 7) = 8; ians(3, 7) = 5; ians(4, 7) = 4;
    ians(1, 8) = 8; ians(2, 8) = 7; ians(3, 8) =16; ians(4, 8) =17;
    ians(1, 9) = 7; ians(2, 9) = 4; ians(3, 9) =13; ians(4, 9) =16;
    ians(1,10) = 8; ians(2,10) = 9; ians(3,10) = 6; ians(4,10) = 5;
    ians(1,11) = 6; ians(2,11) = 9; ians(3,11) =18; ians(4,11) =15;
    ians(1,12) = 9; ians(2,12) = 8; ians(3,12) =17; ians(4,12) =18;
    ians(1,13) =19; ians(2,13) =20; ians(3,13) =23; ians(4,13) =22;
    ians(1,14) =10; ians(2,14) =11; ians(3,14) =20; ians(4,14) =19;
    ians(1,15) =13; ians(2,15) =10; ians(3,15) =19; ians(4,15) =22;
    ians(1,16) =20; ians(2,16) =21; ians(3,16) =24; ians(4,16) =23;
    ians(1,17) =11; ians(2,17) =12; ians(3,17) =21; ians(4,17) =20;
    ians(1,18) =12; ians(2,18) =15; ians(3,18) =24; ians(4,18) =21;
    ians(1,19) =22; ians(2,19) =23; ians(3,19) =26; ians(4,19) =25;
    ians(1,20) =17; ians(2,20) =16; ians(3,20) =25; ians(4,20) =26;
    ians(1,21) =16; ians(2,21) =13; ians(3,21) =22; ians(4,21) =25;
    ians(1,22) =23; ians(2,22) =24; ians(3,22) =27; ians(4,22) =26;
    ians(1,23) =15; ians(2,23) =18; ians(3,23) =27; ians(4,23) =24;
    ians(1,24) =18; ians(2,24) =17; ians(3,24) =26; ians(4,24) =27;

    do i = 1, 24
      call monolis_test_check_eq_I1("monolis_extract_all_surf_hex_test", ians(1,i), elem(1,i))
      call monolis_test_check_eq_I1("monolis_extract_all_surf_hex_test", ians(2,i), elem(2,i))
      call monolis_test_check_eq_I1("monolis_extract_all_surf_hex_test", ians(3,i), elem(3,i))
      call monolis_test_check_eq_I1("monolis_extract_all_surf_hex_test", ians(4,i), elem(4,i))
    enddo

    call monolis_dealloc_I_2d(elem)

    fname = "driver/output.c/surf.hex.dat"
    call monolis_input_elem(fname, n_elem, n_base, elem)

    call monolis_test_check_eq_I1("monolis_extract_all_surf_hex_test 1 Clang", n_elem, 24)
    call monolis_test_check_eq_I1("monolis_extract_all_surf_hex_test 2 Clang", n_base, 4)

    do i = 1, 24
      call monolis_test_check_eq_I1("monolis_extract_all_surf_hex_test Clang", ians(1,i) - 1, elem(1,i))
      call monolis_test_check_eq_I1("monolis_extract_all_surf_hex_test Clang", ians(2,i) - 1, elem(2,i))
      call monolis_test_check_eq_I1("monolis_extract_all_surf_hex_test Clang", ians(3,i) - 1, elem(3,i))
      call monolis_test_check_eq_I1("monolis_extract_all_surf_hex_test Clang", ians(4,i) - 1, elem(4,i))
    enddo
  end subroutine monolis_extract_all_surf_hex_test

  subroutine monolis_extract_all_surf_tet_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: n_elem, n_base, i
    integer(kint) :: ians(3,12)
    integer(kint), allocatable :: elem(:,:)

    call monolis_std_global_log_string("monolis_extract_all_surf_tet")

    fname = "driver/output.f/surf.tet.dat"
    call monolis_input_elem(fname, n_elem, n_base, elem)

    call monolis_test_check_eq_I1("monolis_extract_all_surf_tet_test 1", n_elem, 12)
    call monolis_test_check_eq_I1("monolis_extract_all_surf_tet_test 2", n_base, 3)

    ians(1, 1) = 3; ians(2, 1) = 2; ians(3, 1) = 1;
    ians(1, 2) = 4; ians(2, 2) = 3; ians(3, 2) = 1;
    ians(1, 3) = 5; ians(2, 3) = 6; ians(3, 3) = 7;
    ians(1, 4) = 5; ians(2, 4) = 7; ians(3, 4) = 8;
    ians(1, 5) = 5; ians(2, 5) = 1; ians(3, 5) = 2;
    ians(1, 6) = 6; ians(2, 6) = 5; ians(3, 6) = 2;
    ians(1, 7) = 6; ians(2, 7) = 2; ians(3, 7) = 3;
    ians(1, 8) = 7; ians(2, 8) = 6; ians(3, 8) = 3;
    ians(1, 9) = 7; ians(2, 9) = 3; ians(3, 9) = 4;
    ians(1,10) = 8; ians(2,10) = 7; ians(3,10) = 4;
    ians(1,11) = 8; ians(2,11) = 4; ians(3,11) = 1;
    ians(1,12) = 5; ians(2,12) = 8; ians(3,12) = 1;

    do i = 1, 12
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test", ians(1,i), elem(1,i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test", ians(2,i), elem(2,i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test", ians(3,i), elem(3,i))
    enddo

    call monolis_dealloc_I_2d(elem)

    fname = "driver/output.c/surf.tet.dat"
    call monolis_input_elem(fname, n_elem, n_base, elem)

    call monolis_test_check_eq_I1("monolis_extract_all_surf_tet_test 1 Clang", n_elem, 12)
    call monolis_test_check_eq_I1("monolis_extract_all_surf_tet_test 2 Clang", n_base, 3)

    do i = 1, 12
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test Clang", ians(1,i) - 1 , elem(1,i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test Clang", ians(2,i) - 1, elem(2,i))
      call monolis_test_check_eq_I1("monolis_dbc_all_surf_tet_test Clang", ians(3,i) - 1, elem(3,i))
    enddo
  end subroutine monolis_extract_all_surf_tet_test

  subroutine monolis_p_refiner_tet_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: n_node, n_elem, n_base, i, j
    integer(kint) :: ians(10,12)
    real(kdouble) :: rans(3,53)
    integer(kint), allocatable :: elem(:,:)
    real(kdouble), allocatable :: node(:,:)

    call monolis_std_global_log_string("monolis_p_refiner_tet")

    fname = "driver/output.f/node.ref.p.tet.dat"
    call monolis_input_node(fname, n_node, node)

    fname = "driver/output.f/elem.ref.p.tet.dat"
    call monolis_input_elem(fname, n_elem, n_base, elem)

    call monolis_test_check_eq_I1("monolis_p_refiner_tet_test 1", n_node, 53)
    call monolis_test_check_eq_I1("monolis_p_refiner_tet_test 2", n_elem, 12)
    call monolis_test_check_eq_I1("monolis_p_refiner_tet_test 3", n_base, 10)

    rans(1, 1) = 0.0d+00; rans(2, 1) = 0.0d+00; rans(3, 1) = 0.0d+00;
    rans(1, 2) = 2.0d+00; rans(2, 2) = 0.0d+00; rans(3, 2) = 0.0d+00;
    rans(1, 3) = 2.0d+00; rans(2, 3) = 2.0d+00; rans(3, 3) = 0.0d+00;
    rans(1, 4) = 0.0d+00; rans(2, 4) = 2.0d+00; rans(3, 4) = 0.0d+00;
    rans(1, 5) = 0.0d+00; rans(2, 5) = 0.0d+00; rans(3, 5) = 2.0d+00;
    rans(1, 6) = 2.0d+00; rans(2, 6) = 0.0d+00; rans(3, 6) = 2.0d+00;
    rans(1, 7) = 2.0d+00; rans(2, 7) = 2.0d+00; rans(3, 7) = 2.0d+00;
    rans(1, 8) = 0.0d+00; rans(2, 8) = 2.0d+00; rans(3, 8) = 2.0d+00;
    rans(1, 9) = 1.0d+00; rans(2, 9) = 1.0d+00; rans(3, 9) = 1.0d+00;
    rans(1,10) = 1.0d+00; rans(2,10) = 0.0d+00; rans(3,10) = 0.0d+00;
    rans(1,11) = 2.0d+00; rans(2,11) = 1.0d+00; rans(3,11) = 0.0d+00;
    rans(1,12) = 1.0d+00; rans(2,12) = 1.0d+00; rans(3,12) = 0.0d+00;
    rans(1,13) = 5.0d-01; rans(2,13) = 5.0d-01; rans(3,13) = 5.0d-01;
    rans(1,14) = 1.5d+00; rans(2,14) = 5.0d-01; rans(3,14) = 5.0d-01;
    rans(1,15) = 1.5d+00; rans(2,15) = 1.5d+00; rans(3,15) = 5.0d-01;
    rans(1,16) = 1.0d+00; rans(2,16) = 1.0d+00; rans(3,16) = 0.0d+00;
    rans(1,17) = 1.0d+00; rans(2,17) = 2.0d+00; rans(3,17) = 0.0d+00;
    rans(1,18) = 0.0d+00; rans(2,18) = 1.0d+00; rans(3,18) = 0.0d+00;
    rans(1,19) = 5.0d-01; rans(2,19) = 1.5d+00; rans(3,19) = 5.0d-01;
    rans(1,20) = 2.0d+00; rans(2,20) = 1.0d+00; rans(3,20) = 2.0d+00;
    rans(1,21) = 1.0d+00; rans(2,21) = 0.0d+00; rans(3,21) = 2.0d+00;
    rans(1,22) = 1.0d+00; rans(2,22) = 1.0d+00; rans(3,22) = 2.0d+00;
    rans(1,23) = 1.5d+00; rans(2,23) = 1.5d+00; rans(3,23) = 1.5d+00;
    rans(1,24) = 1.5d+00; rans(2,24) = 5.0d-01; rans(3,24) = 1.5d+00;
    rans(1,25) = 5.0d-01; rans(2,25) = 5.0d-01; rans(3,25) = 1.5d+00;
    rans(1,26) = 1.0d+00; rans(2,26) = 2.0d+00; rans(3,26) = 2.0d+00;
    rans(1,27) = 1.0d+00; rans(2,27) = 1.0d+00; rans(3,27) = 2.0d+00;
    rans(1,28) = 0.0d+00; rans(2,28) = 1.0d+00; rans(3,28) = 2.0d+00;
    rans(1,29) = 5.0d-01; rans(2,29) = 1.5d+00; rans(3,29) = 1.5d+00;
    rans(1,30) = 1.0d+00; rans(2,30) = 0.0d+00; rans(3,30) = 0.0d+00;
    rans(1,31) = 0.0d+00; rans(2,31) = 0.0d+00; rans(3,31) = 1.0d+00;
    rans(1,32) = 1.0d+00; rans(2,32) = 0.0d+00; rans(3,32) = 1.0d+00;
    rans(1,33) = 1.0d+00; rans(2,33) = 0.0d+00; rans(3,33) = 1.0d+00;
    rans(1,34) = 1.0d+00; rans(2,34) = 0.0d+00; rans(3,34) = 2.0d+00;
    rans(1,35) = 2.0d+00; rans(2,35) = 0.0d+00; rans(3,35) = 1.0d+00;
    rans(1,36) = 2.0d+00; rans(2,36) = 1.0d+00; rans(3,36) = 0.0d+00;
    rans(1,37) = 2.0d+00; rans(2,37) = 0.0d+00; rans(3,37) = 1.0d+00;
    rans(1,38) = 2.0d+00; rans(2,38) = 1.0d+00; rans(3,38) = 1.0d+00;
    rans(1,39) = 2.0d+00; rans(2,39) = 1.0d+00; rans(3,39) = 1.0d+00;
    rans(1,40) = 2.0d+00; rans(2,40) = 1.0d+00; rans(3,40) = 2.0d+00;
    rans(1,41) = 2.0d+00; rans(2,41) = 2.0d+00; rans(3,41) = 1.0d+00;
    rans(1,42) = 1.0d+00; rans(2,42) = 2.0d+00; rans(3,42) = 0.0d+00;
    rans(1,43) = 2.0d+00; rans(2,43) = 2.0d+00; rans(3,43) = 1.0d+00;
    rans(1,44) = 1.0d+00; rans(2,44) = 2.0d+00; rans(3,44) = 1.0d+00;
    rans(1,45) = 1.0d+00; rans(2,45) = 2.0d+00; rans(3,45) = 1.0d+00;
    rans(1,46) = 1.0d+00; rans(2,46) = 2.0d+00; rans(3,46) = 2.0d+00;
    rans(1,47) = 0.0d+00; rans(2,47) = 2.0d+00; rans(3,47) = 1.0d+00;
    rans(1,48) = 0.0d+00; rans(2,48) = 1.0d+00; rans(3,48) = 0.0d+00;
    rans(1,49) = 0.0d+00; rans(2,49) = 2.0d+00; rans(3,49) = 1.0d+00;
    rans(1,50) = 0.0d+00; rans(2,50) = 1.0d+00; rans(3,50) = 1.0d+00;
    rans(1,51) = 0.0d+00; rans(2,51) = 1.0d+00; rans(3,51) = 1.0d+00;
    rans(1,52) = 0.0d+00; rans(2,52) = 1.0d+00; rans(3,52) = 2.0d+00;
    rans(1,53) = 0.0d+00; rans(2,53) = 0.0d+00; rans(3,53) = 1.0d+00;

    do i = 1, 53
      call monolis_test_check_eq_R1("monolis_p_refiner_tet_test node", rans(1,i), node(1,i))
      call monolis_test_check_eq_R1("monolis_p_refiner_tet_test node", rans(2,i), node(2,i))
      call monolis_test_check_eq_R1("monolis_p_refiner_tet_test node", rans(3,i), node(3,i))
    enddo

    ians(1, 1) = 1; ians(2, 1) = 2; ians(3, 1) = 3; ians(4, 1) = 9; ians(5, 1) = 10;
    ians(6, 1) = 11; ians(7, 1) = 12; ians(8, 1) = 13; ians(9, 1) = 14; ians(10, 1) = 15;
    ians(1, 2) = 1; ians(2, 2) = 3; ians(3, 2) = 4; ians(4, 2) = 9; ians(5, 2) = 16;
    ians(6, 2) = 17; ians(7, 2) = 18; ians(8, 2) = 13; ians(9, 2) = 15; ians(10, 2) = 19;
    ians(1, 3) = 7; ians(2, 3) = 6; ians(3, 3) = 5; ians(4, 3) = 9; ians(5, 3) = 20;
    ians(6, 3) = 21; ians(7, 3) = 22; ians(8, 3) = 23; ians(9, 3) = 24; ians(10, 3) = 25;
    ians(1, 4) = 8; ians(2, 4) = 7; ians(3, 4) = 5; ians(4, 4) = 9; ians(5, 4) = 26;
    ians(6, 4) = 27; ians(7, 4) = 28; ians(8, 4) = 29; ians(9, 4) = 23; ians(10, 4) = 25;
    ians(1, 5) = 2; ians(2, 5) = 1; ians(3, 5) = 5; ians(4, 5) = 9; ians(5, 5) = 30;
    ians(6, 5) = 31; ians(7, 5) = 32; ians(8, 5) = 14; ians(9, 5) = 13; ians(10, 5) = 25;
    ians(1, 6) = 2; ians(2, 6) = 5; ians(3, 6) = 6; ians(4, 6) = 9; ians(5, 6) = 33;
    ians(6, 6) = 34; ians(7, 6) = 35; ians(8, 6) = 14; ians(9, 6) = 25; ians(10, 6) = 24;
    ians(1, 7) = 3; ians(2, 7) = 2; ians(3, 7) = 6; ians(4, 7) = 9; ians(5, 7) = 36;
    ians(6, 7) = 37; ians(7, 7) = 38; ians(8, 7) = 15; ians(9, 7) = 14; ians(10, 7) = 24;
    ians(1, 8) = 3; ians(2, 8) = 6; ians(3, 8) = 7; ians(4, 8) = 9; ians(5, 8) = 39;
    ians(6, 8) = 40; ians(7, 8) = 41; ians(8, 8) = 15; ians(9, 8) = 24; ians(10, 8) = 23;
    ians(1, 9) = 4; ians(2, 9) = 3; ians(3, 9) = 7; ians(4, 9) = 9; ians(5, 9) = 42;
    ians(6, 9) = 43; ians(7, 9) = 44; ians(8, 9) = 19; ians(9, 9) = 15; ians(10, 9) = 23;
    ians(1,10) = 4; ians(2,10) = 7; ians(3,10) = 8; ians(4,10) = 9; ians(5,10) = 45;
    ians(6,10) = 46; ians(7,10) = 47; ians(8,10) = 19; ians(9,10) = 23; ians(10,10) = 29;
    ians(1,11) = 1; ians(2,11) = 4; ians(3,11) = 8; ians(4,11) = 9; ians(5,11) = 48;
    ians(6,11) = 49; ians(7,11) = 50; ians(8,11) = 13; ians(9,11) = 19; ians(10,11) = 29;
    ians(1,12) = 1; ians(2,12) = 8; ians(3,12) = 5; ians(4,12) = 9; ians(5,12) = 51;
    ians(6,12) = 52; ians(7,12) = 53; ians(8,12) = 13; ians(9,12) = 29; ians(10,12) = 25;

    do i = 1, 12
    do j = 1, 10
      call monolis_test_check_eq_I1("monolis_p_refiner_tet_test elem", ians(j,i), elem(j,i))
    enddo
    enddo


    call monolis_dealloc_I_2d(elem)
    call monolis_dealloc_R_2d(node)

    fname = "driver/output.c/node.ref.p.tet.dat"
    call monolis_input_node(fname, n_node, node)

    fname = "driver/output.c/elem.ref.p.tet.dat"
    call monolis_input_elem(fname, n_elem, n_base, elem)

    call monolis_test_check_eq_I1("monolis_p_refiner_tet_test 1 Clang", n_node, 53)
    call monolis_test_check_eq_I1("monolis_p_refiner_tet_test 2 Clang", n_elem, 12)
    call monolis_test_check_eq_I1("monolis_p_refiner_tet_test 3 Clang", n_base, 10)

    do i = 1, 53
      call monolis_test_check_eq_R1("monolis_p_refiner_tet_test node Clang", rans(1,i), node(1,i))
      call monolis_test_check_eq_R1("monolis_p_refiner_tet_test node Clang", rans(2,i), node(2,i))
      call monolis_test_check_eq_R1("monolis_p_refiner_tet_test node Clang", rans(3,i), node(3,i))
    enddo

    do i = 1, 12
    do j = 1, 10
      call monolis_test_check_eq_I1("monolis_p_refiner_tet_test elem Clang", ians(j,i) - 1, elem(j,i))
    enddo
    enddo
  end subroutine monolis_p_refiner_tet_test

  subroutine monolis_h_refiner_tet_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: n_node, n_elem, n_base, i, j
    integer(kint), allocatable :: elem(:,:), elem_ans(:,:)
    real(kdouble), allocatable :: node(:,:), node_ans(:,:)

    call monolis_std_global_log_string("monolis_h_refiner_tet")

    fname = "driver/ans/node.ref.h.tet.dat"
    call monolis_input_node(fname, n_node, node_ans)

    fname = "driver/ans/elem.ref.h.tet.dat"
    call monolis_input_elem(fname, n_elem, n_base, elem_ans)

    fname = "driver/output.f/node.ref.h.tet.dat"
    call monolis_input_node(fname, n_node, node)

    fname = "driver/output.f/elem.ref.h.tet.dat"
    call monolis_input_elem(fname, n_elem, n_base, elem)

    call monolis_test_check_eq_I1("monolis_h_refiner_hex_test 1", n_node, 14)
    call monolis_test_check_eq_I1("monolis_h_refiner_hex_test 2", n_elem, 16)
    call monolis_test_check_eq_I1("monolis_h_refiner_hex_test 3", n_base, 4)

    do i = 1, n_node
      call monolis_test_check_eq_R1("monolis_h_refiner_hex_test node", node_ans(1,i), node(1,i))
      call monolis_test_check_eq_R1("monolis_h_refiner_hex_test node", node_ans(2,i), node(2,i))
      call monolis_test_check_eq_R1("monolis_h_refiner_hex_test node", node_ans(3,i), node(3,i))
    enddo

    do i = 1, n_elem
    do j = 1, n_base
      call monolis_test_check_eq_I1("monolis_h_refiner_hex_test elem", elem_ans(j,i), elem(j,i))
    enddo
    enddo


    call monolis_dealloc_I_2d(elem)
    call monolis_dealloc_R_2d(node)

    fname = "driver/output.c/node.ref.h.tet.dat"
    call monolis_input_node(fname, n_node, node)

    fname = "driver/output.c/elem.ref.h.tet.dat"
    call monolis_input_elem(fname, n_elem, n_base, elem)

    call monolis_test_check_eq_I1("monolis_h_refiner_hex_test 1 Clang", n_node, 14)
    call monolis_test_check_eq_I1("monolis_h_refiner_hex_test 2 Clang", n_elem, 16)
    call monolis_test_check_eq_I1("monolis_h_refiner_hex_test 3 Clang", n_base, 4)

    do i = 1, n_node
      call monolis_test_check_eq_R1("monolis_h_refiner_hex_test node Clang", node_ans(1,i), node(1,i))
      call monolis_test_check_eq_R1("monolis_h_refiner_hex_test node Clang", node_ans(2,i), node(2,i))
      call monolis_test_check_eq_R1("monolis_h_refiner_hex_test node Clang", node_ans(3,i), node(3,i))
    enddo

    do i = 1, n_elem
    do j = 1, n_base
      call monolis_test_check_eq_I1("monolis_h_refiner_hex_test elem Clang", elem_ans(j,i) - 1, elem(j,i))
    enddo
    enddo
  end subroutine monolis_h_refiner_tet_test

  subroutine monolis_h_refiner_hex_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: n_node, n_elem, n_base, i, j
    integer(kint), allocatable :: elem(:,:), elem_ans(:,:)
    real(kdouble), allocatable :: node(:,:), node_ans(:,:)

    call monolis_std_global_log_string("monolis_h_refiner_hex")

    fname = "driver/ans/node.ref.h.hex.dat"
    call monolis_input_node(fname, n_node, node_ans)

    fname = "driver/ans/elem.ref.h.hex.dat"
    call monolis_input_elem(fname, n_elem, n_base, elem_ans)

    fname = "driver/output.f/node.ref.h.hex.dat"
    call monolis_input_node(fname, n_node, node)

    fname = "driver/output.f/elem.ref.h.hex.dat"
    call monolis_input_elem(fname, n_elem, n_base, elem)

    call monolis_test_check_eq_I1("monolis_h_refiner_hex_test 1", n_node, 45)
    call monolis_test_check_eq_I1("monolis_h_refiner_hex_test 2", n_elem, 16)
    call monolis_test_check_eq_I1("monolis_h_refiner_hex_test 3", n_base, 8)

    do i = 1, n_node
      call monolis_test_check_eq_R1("monolis_h_refiner_hex_test node", node_ans(1,i), node(1,i))
      call monolis_test_check_eq_R1("monolis_h_refiner_hex_test node", node_ans(2,i), node(2,i))
      call monolis_test_check_eq_R1("monolis_h_refiner_hex_test node", node_ans(3,i), node(3,i))
    enddo

    do i = 1, n_elem
    do j = 1, n_base
      call monolis_test_check_eq_I1("monolis_h_refiner_hex_test elem", elem_ans(j,i), elem(j,i))
    enddo
    enddo



    call monolis_dealloc_I_2d(elem)
    call monolis_dealloc_R_2d(node)

    fname = "driver/output.c/node.ref.h.hex.dat"
    call monolis_input_node(fname, n_node, node)

    fname = "driver/output.c/elem.ref.h.hex.dat"
    call monolis_input_elem(fname, n_elem, n_base, elem)

    call monolis_test_check_eq_I1("monolis_h_refiner_hex_test 1 Clang", n_node, 45)
    call monolis_test_check_eq_I1("monolis_h_refiner_hex_test 2 Clang", n_elem, 16)
    call monolis_test_check_eq_I1("monolis_h_refiner_hex_test 3 Clang", n_base, 8)

    do i = 1, n_node
      call monolis_test_check_eq_R1("monolis_h_refiner_hex_test node Clang", node_ans(1,i), node(1,i))
      call monolis_test_check_eq_R1("monolis_h_refiner_hex_test node Clang", node_ans(2,i), node(2,i))
      call monolis_test_check_eq_R1("monolis_h_refiner_hex_test node Clang", node_ans(3,i), node(3,i))
    enddo

    do i = 1, n_elem
    do j = 1, n_base
      call monolis_test_check_eq_I1("monolis_h_refiner_hex_test elem Clang", elem_ans(j,i) - 1, elem(j,i))
    enddo
    enddo
  end subroutine monolis_h_refiner_hex_test
end module mod_monolis_driver_test
