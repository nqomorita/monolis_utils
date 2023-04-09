!> 通信データテストモジュール
module mod_monolis_comm_ser_util_test
  use mod_monolis_utils
  use mod_monolis_utils_define_com
  use mod_monolis_comm_ser_util
  use mod_monolis_comm_table
  implicit none

contains

  subroutine monolis_comm_ser_util_test()
    implicit none

    call monolis_comm_get_all_external_node_domain_id_serial_test()
    call monolis_comm_get_recv_serial_test()
    call monolis_comm_get_send_serial_test()
  end subroutine monolis_comm_ser_util_test

  subroutine monolis_comm_get_all_external_node_domain_id_serial_test()
    implicit none
    !> 節点の所属する領域番号（全節点数）
    integer(kint) :: vertex_domain_id(5)
    !> 領域分割数
    integer(kint) :: n_domain
    !> 全ての外部節点番号
    integer(kint) :: outer_node_id_all_global(2)
    !> 全ての外部節点が属する領域番号
    integer(kint), allocatable :: outer_domain_id_all(:)
    !> 全ての外部節点配列の各領域に属する節点数
    integer(kint) :: displs(3)

    call monolis_std_global_log_string("monolis_comm_get_all_external_node_domain_id_serial")

    vertex_domain_id(1) = 1
    vertex_domain_id(2) = 1
    vertex_domain_id(3) = 1
    vertex_domain_id(4) = 2
    vertex_domain_id(5) = 2

    outer_node_id_all_global(1) = 4
    outer_node_id_all_global(2) = 3

    displs(1) = 0
    displs(2) = 1
    displs(3) = 2

    n_domain = 2

    call monolis_comm_get_all_external_node_domain_id_serial(vertex_domain_id, n_domain, &
      & outer_node_id_all_global, outer_domain_id_all, displs)

    call monolis_test_check_eq_I1("monolis_comm_get_all_external_node_domain_id_serial 1", outer_domain_id_all(1), 2)
    call monolis_test_check_eq_I1("monolis_comm_get_all_external_node_domain_id_serial 1", outer_domain_id_all(2), 1)
  end subroutine monolis_comm_get_all_external_node_domain_id_serial_test

  subroutine monolis_comm_get_recv_serial_test()
    implicit none
    !> [in] 分割領域数
    integer(kint) :: n_domain
    !> [in] 領域番号
    integer(kint) :: domain_id
    !> [in] 領域の内部節点数
    integer(kint) :: n_internal_vertex
    !> 全ての外部節点番号（グローバル番号）
    integer(kint) :: outer_node_id_all_global(2)
    !> [in] 節点が属する領域番号（全節点）
    integer(kint) :: outer_domain_id_all(2)
    !> 全ての外部節点配列の各領域に属する節点数
    integer(kint) :: displs(3)
    !> [in] 分割領域に対応する com 構造体
    type(monolis_COM) :: com
    !> [in] recv 節点の情報
    type(monolis_comm_node_list) :: recv_list(2)

    call monolis_std_global_log_string("monolis_comm_get_recv_serial")

    n_domain = 2

    n_internal_vertex = 3

    outer_node_id_all_global(1) = 40
    outer_node_id_all_global(2) = 30

    outer_domain_id_all(1) = 1
    outer_domain_id_all(2) = 0

    displs(1) = 0
    displs(2) = 1
    displs(3) = 2

    domain_id = 0

    call monolis_comm_get_recv_serial(n_domain, domain_id, n_internal_vertex, &
      & outer_node_id_all_global, outer_domain_id_all, displs, com, recv_list)

    call monolis_test_check_eq_I1("monolis_comm_get_all_external_node_domain_id_serial_test a 1", com%recv_n_neib, 1)
    call monolis_test_check_eq_I1("monolis_comm_get_all_external_node_domain_id_serial_test a 2", com%recv_neib_pe(1), 1)
    call monolis_test_check_eq_I1("monolis_comm_get_all_external_node_domain_id_serial_test a 3", com%recv_index(1), 0)
    call monolis_test_check_eq_I1("monolis_comm_get_all_external_node_domain_id_serial_test a 4", com%recv_index(2), 1)
    call monolis_test_check_eq_I1("monolis_comm_get_all_external_node_domain_id_serial_test a 5", com%recv_item(1), 4)

    call monolis_com_finalize(com)

    domain_id = 1

    n_internal_vertex = 2

    call monolis_comm_get_recv_serial(n_domain, domain_id, n_internal_vertex, &
      & outer_node_id_all_global, outer_domain_id_all, displs, com, recv_list)

    call monolis_test_check_eq_I1("monolis_comm_get_all_external_node_domain_id_serial_test b 1", com%recv_n_neib, 1)
    call monolis_test_check_eq_I1("monolis_comm_get_all_external_node_domain_id_serial_test b 2", com%recv_neib_pe(1), 0)
    call monolis_test_check_eq_I1("monolis_comm_get_all_external_node_domain_id_serial_test b 3", com%recv_index(1), 0)
    call monolis_test_check_eq_I1("monolis_comm_get_all_external_node_domain_id_serial_test b 4", com%recv_index(2), 1)
    call monolis_test_check_eq_I1("monolis_comm_get_all_external_node_domain_id_serial_test b 5", com%recv_item(1), 3)
  end subroutine monolis_comm_get_recv_serial_test

  subroutine monolis_comm_get_send_serial_test()
    implicit none
    !> [in] 分割領域数
    integer(kint) :: n_domain
    !> [in] 領域番号
    integer(kint) :: domain_id
    !> [in] 節点数
    integer(kint) :: n_vertex
    !> [in] 節点 id
    integer(kint) :: vertex_id(4)
    !> [in] 分割領域に対応する com 構造体
    type(monolis_COM) :: com
    !> [in] recv 節点の情報
    type(monolis_comm_node_list) :: recv_list(2)
    integer(kint) :: n_internal_vertex, outer_node_id_all_global(2)
    integer(kint) :: outer_domain_id_all(2), displs(3)

    call monolis_std_global_log_string("monolis_comm_get_send_serial")

    n_domain = 2

    outer_node_id_all_global(1) = 40
    outer_node_id_all_global(2) = 30

    outer_domain_id_all(1) = 1
    outer_domain_id_all(2) = 0

    displs(1) = 0
    displs(2) = 1
    displs(3) = 2

    domain_id = 0

    n_internal_vertex = 3

    call monolis_comm_get_recv_serial(n_domain, domain_id, n_internal_vertex, &
      & outer_node_id_all_global, outer_domain_id_all, displs, com, recv_list)

    call monolis_com_finalize(com)

    domain_id = 1

    n_internal_vertex = 2

    call monolis_comm_get_recv_serial(n_domain, domain_id, n_internal_vertex, &
      & outer_node_id_all_global, outer_domain_id_all, displs, com, recv_list)

    n_vertex = 4

    vertex_id(1) = 10
    vertex_id(2) = 20
    vertex_id(3) = 30
    vertex_id(4) = 40

    call monolis_comm_get_send_serial(n_domain, n_vertex, vertex_id, com, recv_list(1))

    call monolis_test_check_eq_I1("monolis_comm_get_send_serial a 1", com%send_n_neib, 1)
    call monolis_test_check_eq_I1("monolis_comm_get_send_serial a 2", com%send_neib_pe(1), 1)
    call monolis_test_check_eq_I1("monolis_comm_get_send_serial a 3", com%send_index(1), 0)
    call monolis_test_check_eq_I1("monolis_comm_get_send_serial a 4", com%send_index(2), 1)
    call monolis_test_check_eq_I1("monolis_comm_get_send_serial a 5", com%send_item(1), 3)

    call monolis_com_finalize(com)

    domain_id = 1

    n_internal_vertex = 2

    n_vertex = 3

    vertex_id(1) = 40
    vertex_id(2) = 50
    vertex_id(3) = 30

    call monolis_comm_get_recv_serial(n_domain, domain_id, n_internal_vertex, &
      & outer_node_id_all_global, outer_domain_id_all, displs, com, recv_list)

    call monolis_comm_get_send_serial(n_domain, n_vertex, vertex_id, com, recv_list(2))

    call monolis_test_check_eq_I1("monolis_comm_get_send_serial b 1", com%send_n_neib, 1)
    call monolis_test_check_eq_I1("monolis_comm_get_send_serial b 2", com%send_neib_pe(1), 0)
    call monolis_test_check_eq_I1("monolis_comm_get_send_serial b 3", com%send_index(1), 0)
    call monolis_test_check_eq_I1("monolis_comm_get_send_serial b 4", com%send_index(2), 1)
    call monolis_test_check_eq_I1("monolis_comm_get_send_serial b 5", com%send_item(1), 1)
  end subroutine monolis_comm_get_send_serial_test

end module mod_monolis_comm_ser_util_test
