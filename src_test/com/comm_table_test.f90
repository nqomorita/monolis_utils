!> 通信データテストモジュール
module mod_monolis_comm_table_test
  use mod_monolis_utils
  use mod_monolis_utils_define_com
  use mod_monolis_comm_table
  implicit none

contains

  subroutine monolis_comm_table_test()
    implicit none

    call monolis_com_get_comm_table_parallel_test()
    call monolis_get_bool_list_of_internal_test()
  end subroutine monolis_comm_table_test

  subroutine monolis_get_bool_list_of_internal_test()
    implicit none
    type(monolis_COM) :: monoCOM
    integer(kint) :: n_node
    integer(kint) :: n_elem
    integer(kint) :: n_base
    integer(kint) :: comm
    integer(kint) :: n_internal_vertex
    integer(kint) :: elem(2,4)
    integer(kint) :: global_id(5)
    integer(kint) :: index(5)
    integer(kint) :: item(8)
    logical :: list(4)

    if(monolis_mpi_get_global_comm_size() == 1) return

    call monolis_std_global_log_string("monolis_get_bool_list_of_internal_simple_mesh")

    comm = monolis_mpi_get_global_comm()

    n_internal_vertex = 3

    n_node = 5

    if(monolis_mpi_get_global_my_rank() == 0)then
      global_id(1) = 10
      global_id(2) = 20
      global_id(3) = 30
      global_id(4) = 40
      global_id(5) = 50
    else
      global_id(1) = 40
      global_id(2) = 50
      global_id(3) = 60
      global_id(4) = 20
      global_id(5) = 30
    endif

    call monolis_com_initialize_by_global_id(monoCOM, comm, n_internal_vertex, n_node, global_id)

    n_elem = 4
    n_base = 2

    if(monolis_mpi_get_global_my_rank() == 0)then
      elem(1,1) = 1; elem(2,1) = 2;
      elem(1,2) = 2; elem(2,2) = 3;
      elem(1,3) = 3; elem(2,3) = 4;
      elem(1,4) = 4; elem(2,4) = 5;
    else
      elem(1,1) = 1; elem(2,1) = 2;
      elem(1,2) = 2; elem(2,2) = 3;
      elem(1,3) = 4; elem(2,3) = 5;
      elem(1,4) = 5; elem(2,4) = 1;
    endif

    monoCOM%n_internal_vertex = 3

    list = .false.

    call monolis_get_bool_list_of_internal_simple_mesh(monoCOM, n_node, n_elem, n_base, elem, list)

    if(monolis_mpi_get_global_my_rank() == 0)then
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list(1), .true.)
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list(2), .true.)
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list(3), .true.)
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list(4), .false.)
    else
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list(1), .true.)
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list(2), .true.)
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list(3), .false.)
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list(4), .false.)
    endif

    call monolis_com_finalize(monoCOM)


    call monolis_std_global_log_string("monolis_get_bool_list_of_internal_connetivity")

    list = .false.

    n_node = 5

    n_elem = 4

    monoCOM%n_internal_vertex = 3

    call monolis_com_initialize_by_global_id(monoCOM, comm, n_internal_vertex, n_node, global_id)

    index(1) = 0
    index(2) = 2
    index(3) = 4
    index(4) = 6
    index(5) = 8

    if(monolis_mpi_get_global_my_rank() == 0)then
      item(1) = 1; item(2) = 2;
      item(3) = 2; item(4) = 3;
      item(5) = 3; item(6) = 4;
      item(7) = 4; item(8) = 5;
    else
      item(1) = 1; item(2) = 2;
      item(3) = 2; item(4) = 3;
      item(5) = 4; item(6) = 5;
      item(7) = 5; item(8) = 1;
    endif

    call monolis_get_bool_list_of_internal_connetivity(monoCOM, n_node, n_elem, index, item, list)

    if(monolis_mpi_get_global_my_rank() == 0)then
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list(1), .true.)
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list(2), .true.)
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list(3), .true.)
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list(4), .false.)
    else
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list(1), .true.)
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list(2), .true.)
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list(3), .false.)
      call monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list(4), .false.)
    endif

    call monolis_com_finalize(monoCOM)
  end subroutine monolis_get_bool_list_of_internal_test

  subroutine monolis_com_get_comm_table_parallel_test()
    implicit none
    integer(kint) :: n_internal_vertex
    integer(kint) :: n_vertex
    integer(kint) :: vertex_id(5), i_ans(2)
    type(monolis_COM) :: com

    if(monolis_mpi_get_global_comm_size() == 1) return

    call monolis_std_global_log_string("monolis_com_get_comm_table_parallel")
    call monolis_std_global_log_string("monolis_comm_get_all_external_n_node_parallel")
    call monolis_std_global_log_string("monolis_comm_get_all_external_node_parallel")
    call monolis_std_global_log_string("monolis_comm_get_all_external_node_domain_id_parallel")
    call monolis_std_global_log_string("monolis_comm_get_recv_parallel")
    call monolis_std_global_log_string("monolis_comm_get_recv_parallel_n_neib")
    call monolis_std_global_log_string("monolis_comm_get_recv_parallel_neib_id")
    call monolis_std_global_log_string("monolis_comm_get_recv_parallel_index")
    call monolis_std_global_log_string("monolis_comm_get_recv_parallel_item")
    call monolis_std_global_log_string("monolis_comm_get_send_parallel")
    call monolis_std_global_log_string("monolis_comm_get_send_parallel_n_neib")
    call monolis_std_global_log_string("monolis_comm_get_send_parallel_neib_id")
    call monolis_std_global_log_string("monolis_comm_get_send_parallel_index")
    call monolis_std_global_log_string("monolis_comm_get_send_parallel_item")
    call monolis_std_global_log_string("monolis_comm_get_send_parallel_n_list")

    n_internal_vertex = 3

    n_vertex = 5

    call monolis_com_set_communicator(com, monolis_mpi_get_global_comm())

    if(monolis_mpi_get_global_my_rank() == 0)then
      vertex_id(1) = 10
      vertex_id(2) = 20
      vertex_id(3) = 30
      vertex_id(4) = 40
      vertex_id(5) = 50
    else
      vertex_id(1) = 40
      vertex_id(2) = 50
      vertex_id(3) = 60
      vertex_id(4) = 20
      vertex_id(5) = 30
    endif

    call monolis_com_get_comm_table_parallel &
      & (n_internal_vertex, n_vertex, vertex_id, com)

    if(monolis_mpi_get_global_my_rank() == 0)then
      i_ans(1) = 0
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 1", com%my_rank, 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 2", com%comm_size, 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 3", com%n_internal_vertex, 3)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 4", com%recv_n_neib, 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 5", com%recv_neib_pe(1), 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 6", com%recv_index(1), 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 7", com%recv_index(2), 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 8", com%recv_item(1), 4)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 9", com%recv_item(2), 5)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 10", com%send_n_neib, 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 11", com%send_neib_pe(1), 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 12", com%send_index(1), 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 13", com%send_index(2), 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 14", com%send_item(1), 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 15", com%send_item(2), 3)
    else
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 1", com%my_rank, 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 2", com%comm_size, 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 3", com%n_internal_vertex, 3)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 4", com%recv_n_neib, 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 5", com%recv_neib_pe(1), 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 6", com%recv_index(1), 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 7", com%recv_index(2), 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 8", com%recv_item(1), 4)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 9", com%recv_item(2), 5)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 10", com%send_n_neib, 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 11", com%send_neib_pe(1), 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 12", com%send_index(1), 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 13", com%send_index(2), 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 14", com%send_item(1), 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 15", com%send_item(2), 2)
    endif
    !call monolis_com_debug_write(com)
  end subroutine monolis_com_get_comm_table_parallel_test
end module mod_monolis_comm_table_test
