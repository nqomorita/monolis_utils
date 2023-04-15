module mod_monolis_extract_util_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_extract_util_test()
    implicit none

    call monolis_get_surf_node_test()
    call get_key_surf_test()
  end subroutine monolis_extract_util_test

  subroutine monolis_get_surf_node_test()
    implicit none
    integer(kint) :: n_surf, n_base, n_node
    integer(kint) :: surf(2,4), i_ans(5)
    integer(kint), allocatable :: node_id(:)

    call monolis_std_global_log_string("monolis_get_surf_node")

    n_base = 2

    n_surf = 4

    surf(1,1) = 2; surf(2,1) = 4
    surf(1,2) = 4; surf(2,2) = 6
    surf(1,3) = 6; surf(2,3) = 8
    surf(1,4) = 8; surf(2,4) = 10

    call monolis_get_surf_node(n_base, n_surf, surf, n_node, node_id)

    call monolis_test_check_eq_I1("monolis_get_surf_node 1", n_node, 5)

    i_ans(1) = 2
    i_ans(2) = 4
    i_ans(3) = 6
    i_ans(4) = 8
    i_ans(5) = 10

    call monolis_test_check_eq_I("monolis_get_surf_node 2", node_id, i_ans)
  end subroutine monolis_get_surf_node_test

  subroutine get_key_surf_test()
    implicit none
    character :: key*27
    integer(kint) :: n_base
    integer(kint) :: conn(8)

    call monolis_std_global_log_string("get_key_surf")

    !> hex mesh

    n_base = 8

    conn(1) = 11
    conn(2) = 22
    conn(3) = 33
    conn(4) = 44
    conn(5) = 55
    conn(6) = 66
    conn(7) = 77
    conn(8) = 88

    key = get_key_surf(n_base, 1, conn)

    if(trim(key) == "000000011000000022000000033")then
      call monolis_test_assert_pass("get_key_surf 1")
    else
      call monolis_test_assert_fail("get_key_surf 1", "")
    endif

    key = get_key_surf(n_base, 2, conn)

    if(trim(key) == "000000055000000066000000077")then
      call monolis_test_assert_pass("get_key_surf 2")
    else
      call monolis_test_assert_fail("get_key_surf 2", "")
    endif

    key = get_key_surf(n_base, 3, conn)

    if(trim(key) == "000000011000000022000000055")then
      call monolis_test_assert_pass("get_key_surf 3")
    else
      call monolis_test_assert_fail("get_key_surf 3", "")
    endif

    key = get_key_surf(n_base, 4, conn)

    if(trim(key) == "000000022000000033000000066")then
      call monolis_test_assert_pass("get_key_surf 4")
    else
      call monolis_test_assert_fail("get_key_surf 4", "")
    endif

    key = get_key_surf(n_base, 5, conn)

    if(trim(key) == "000000033000000044000000077")then
      call monolis_test_assert_pass("get_key_surf 5")
    else
      call monolis_test_assert_fail("get_key_surf 5", "")
    endif

    key = get_key_surf(n_base, 6, conn)

    if(trim(key) == "000000011000000044000000055")then
      call monolis_test_assert_pass("get_key_surf 6")
    else
      call monolis_test_assert_fail("get_key_surf 6", "")
    endif

    !> tet mesh

    n_base = 4

    key = get_key_surf(n_base, 1, conn)

    if(trim(key) == "000000011000000022000000033")then
      call monolis_test_assert_pass("get_key_surf 7")
    else
      call monolis_test_assert_fail("get_key_surf 7", "")
    endif

    key = get_key_surf(n_base, 2, conn)

    if(trim(key) == "000000011000000022000000044")then
      call monolis_test_assert_pass("get_key_surf 8")
    else
      call monolis_test_assert_fail("get_key_surf 8", "")
    endif

    key = get_key_surf(n_base, 3, conn)

    if(trim(key) == "000000022000000033000000044")then
      call monolis_test_assert_pass("get_key_surf 9")
    else
      call monolis_test_assert_fail("get_key_surf 9", "")
    endif

    key = get_key_surf(n_base, 4, conn)

    if(trim(key) == "000000011000000033000000044")then
      call monolis_test_assert_pass("get_key_surf 10")
    else
      call monolis_test_assert_fail("get_key_surf 10", "")
    endif
  end subroutine get_key_surf_test
end module mod_monolis_extract_util_test
