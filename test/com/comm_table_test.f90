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
  end subroutine monolis_comm_table_test

  subroutine monolis_com_get_comm_table_parallel_test()
    implicit none
    integer(kint) :: n_internal_vertex
    integer(kint) :: n_vertex
    integer(kint) :: vertex_id(5), i_ans(2)
    type(monolis_COM) :: com

    if(monolis_mpi_get_global_comm_size() == 1) return

    call monolis_std_log_string("monolis_com_get_comm_table_parallel_test")

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
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 1", com%my_rank, 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 2", com%comm_size, 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 3", com%n_internal_vertex, 3)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 4", com%recv_n_neib, 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 5", com%recv_neib_pe(1), 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 6", com%recv_index(1), 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 7", com%recv_index(2), 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 8", com%recv_item(1), 4)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 9", com%recv_item(2), 5)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 10", com%send_n_neib, 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 11", com%send_neib_pe(1), 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 12", com%send_index(1), 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 13", com%send_index(2), 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 14", com%send_item(1), 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 15", com%send_item(2), 3)
    else
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 1", com%my_rank, 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 2", com%comm_size, 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 3", com%n_internal_vertex, 3)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 4", com%recv_n_neib, 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 5", com%recv_neib_pe(1), 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 6", com%recv_index(1), 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 7", com%recv_index(2), 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 8", com%recv_item(1), 4)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 9", com%recv_item(2), 5)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 10", com%send_n_neib, 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 11", com%send_neib_pe(1), 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 12", com%send_index(1), 0)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 13", com%send_index(2), 2)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 14", com%send_item(1), 1)
      call monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel_test 15", com%send_item(2), 2)
    endif
    !call monolis_com_debug_write(com)
  end subroutine monolis_com_get_comm_table_parallel_test
end module mod_monolis_comm_table_test
