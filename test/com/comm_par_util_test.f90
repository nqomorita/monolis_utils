!> 通信データテストモジュール
module mod_monolis_comm_par_util_test
  use mod_monolis_utils
  use mod_monolis_utils_define_com
  use mod_monolis_comm_par_util
  use mod_monolis_comm_table
  implicit none

contains

  subroutine monolis_comm_par_util_test()
    implicit none

    call monolis_generate_global_vertex_id_test()
    call monolis_com_n_vertex_list_test()
  end subroutine monolis_comm_par_util_test

  subroutine monolis_generate_global_vertex_id_test()
    implicit none
    type(monolis_COM) :: com
    integer(kint) :: n_internal_vertex
    integer(kint) :: n_vertex, comm_size
    integer(kint) :: vertex_id(5), vertex_id_new(5), i_ans(5)

    if(monolis_mpi_global_comm_size() == 1) return

    call monolis_std_log_string("monolis_generate_global_vertex_id_test")

    n_internal_vertex = 3

    n_vertex = 5

    call monolis_com_set_communicator(com, monolis_mpi_global_comm())

    if(monolis_mpi_global_my_rank() == 0)then
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

    comm_size = monolis_mpi_global_comm_size() + 1

    call monolis_generate_global_vertex_id(n_internal_vertex, n_vertex, vertex_id_new, com)

    if(monolis_mpi_global_my_rank() == 0)then
      i_ans(1) = 1
      i_ans(2) = 2
      i_ans(3) = 3
      i_ans(4) = 4
      i_ans(5) = 5
    else
      i_ans(1) = 4
      i_ans(2) = 5
      i_ans(3) = 6
      i_ans(4) = 2
      i_ans(5) = 3
    endif

    call monolis_test_check_eq_I("monolis_generate_global_vertex_id_test 1", vertex_id_new, i_ans)
  end subroutine monolis_generate_global_vertex_id_test

  subroutine monolis_com_n_vertex_list_test()
    implicit none
    integer(kint) :: n_internal_vertex
    integer(kint) :: comm, i_ans(3)
    integer(kint), allocatable :: vtxdist(:)

    call monolis_std_log_string("monolis_com_n_vertex_list_test")

    comm = monolis_mpi_global_comm()

    n_internal_vertex = monolis_mpi_global_my_rank() + 1

    call monolis_com_n_vertex_list(n_internal_vertex, comm, vtxdist)

    i_ans(1) = 0
    i_ans(2) = 1
    i_ans(3) = 3

    if(monolis_mpi_global_comm_size() == 2)then
      call monolis_test_check_eq_I("monolis_com_n_vertex_list_test 1", vtxdist, i_ans)
    else
      call monolis_test_check_eq_I("monolis_com_n_vertex_list_test 1", vtxdist, i_ans(1:2))
    endif
  end subroutine monolis_com_n_vertex_list_test
end module mod_monolis_comm_par_util_test
