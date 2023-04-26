!> 通信データテストモジュール
module mod_monolis_utils_define_com_test
  use mod_monolis_utils
  implicit none

contains

  subroutine monolis_utils_define_com_test()
    implicit none

    call monolis_com_set_test()
    call monolis_com_copy_test()

    call monolis_std_global_log_string("monolis_com_debug_write")
  end subroutine monolis_utils_define_com_test

  subroutine monolis_com_copy_test()
    implicit none
    type(monolis_COM) :: COM1
    type(monolis_COM) :: COM2

    call monolis_std_global_log_string("monolis_com_copy")

    call monolis_com_initialize_by_self(COM1)

    COM1%comm = 1
    COM1%my_rank = 2
    COM1%comm_size = 3
    COM1%n_internal_vertex = 4

    COM1%recv_n_neib = 1

    call monolis_palloc_I_1d(COM1%recv_neib_pe, COM1%recv_n_neib)

    COM1%recv_neib_pe(1) = 10

    call monolis_palloc_I_1d(COM1%recv_index, COM1%recv_n_neib + 1)

    COM1%recv_index(1) = 0
    COM1%recv_index(2) = 2

    call monolis_palloc_I_1d(COM1%recv_item, 2)

    COM1%recv_item(1) = 40
    COM1%recv_item(2) = 50

    COM1%send_n_neib = 2

    call monolis_palloc_I_1d(COM1%send_neib_pe, COM1%send_n_neib)

    COM1%send_neib_pe(1) = 11
    COM1%send_neib_pe(2) = 21

    call monolis_palloc_I_1d(COM1%send_index, COM1%send_n_neib + 1)

    COM1%send_index(1) = 0
    COM1%send_index(2) = 1
    COM1%send_index(3) = 2

    call monolis_palloc_I_1d(COM1%send_item, 2)

    COM1%send_item(1) = 61
    COM1%send_item(2) = 71

    call monolis_com_copy(COM1, COM2)

    call monolis_test_check_eq_I1("monolis_com_copy 1", COM1%comm, COM2%comm)
    call monolis_test_check_eq_I1("monolis_com_copy 2", COM1%my_rank, COM2%my_rank)
    call monolis_test_check_eq_I1("monolis_com_copy 3", COM1%comm_size, COM2%comm_size)
    call monolis_test_check_eq_I1("monolis_com_copy 4", COM1%n_internal_vertex, COM2%n_internal_vertex)
    call monolis_test_check_eq_I1("monolis_com_copy 5", COM1%recv_n_neib, COM2%recv_n_neib)
    call monolis_test_check_eq_I1("monolis_com_copy 6", COM1%send_n_neib, COM2%send_n_neib)
    call monolis_test_check_eq_I1("monolis_com_copy 7", COM1%recv_neib_pe(1), COM2%recv_neib_pe(1))
    call monolis_test_check_eq_I1("monolis_com_copy 8", COM1%recv_index(1), COM2%recv_index(1))
    call monolis_test_check_eq_I1("monolis_com_copy 9", COM1%recv_index(2), COM2%recv_index(2))
    call monolis_test_check_eq_I1("monolis_com_copy 10", COM1%recv_item(1), COM2%recv_item(1))
    call monolis_test_check_eq_I1("monolis_com_copy 11", COM1%recv_item(2), COM2%recv_item(2))
    call monolis_test_check_eq_I1("monolis_com_copy 12", COM1%send_neib_pe(1), COM2%send_neib_pe(1))
    call monolis_test_check_eq_I1("monolis_com_copy 13", COM1%send_neib_pe(2), COM2%send_neib_pe(2))
    call monolis_test_check_eq_I1("monolis_com_copy 14", COM1%send_index(1), COM2%send_index(1))
    call monolis_test_check_eq_I1("monolis_com_copy 15", COM1%send_index(2), COM2%send_index(2))
    call monolis_test_check_eq_I1("monolis_com_copy 16", COM1%send_index(3), COM2%send_index(3))
    call monolis_test_check_eq_I1("monolis_com_copy 17", COM1%send_item(1), COM2%send_item(1))
    call monolis_test_check_eq_I1("monolis_com_copy 18", COM1%send_item(2), COM2%send_item(2))
  end subroutine monolis_com_copy_test

  subroutine monolis_com_set_test()
    implicit none
    type(monolis_COM) :: COM
    integer(kint) :: comm, comm_ans
    integer(kint) :: my_rank, my_rank_ans
    integer(kint) :: comm_size, comm_size_ans
    integer(kint) :: n_internal_vertex, n_internal_vertex_ans

    call monolis_std_global_log_string("monolis_com_finalize")

    call monolis_com_initialize_by_self(COM)

    !> case 1
    call monolis_std_global_log_string("monolis_com_set_communicator")
    call monolis_std_global_log_string("monolis_com_get_communicator")

    comm = 10
    call monolis_com_set_communicator(COM, comm)

    call monolis_com_get_communicator(COM, comm_ans)

    call monolis_test_check_eq_I1("monolis_com_set_test 1", comm, comm_ans)

    !> case 2
    call monolis_std_global_log_string("monolis_com_set_my_rank")
    call monolis_std_global_log_string("monolis_com_get_my_rank")

    my_rank = 20
    call monolis_com_set_my_rank(COM, my_rank)

    call monolis_com_get_my_rank(COM, my_rank_ans)

    call monolis_test_check_eq_I1("monolis_com_set_test 2", my_rank, my_rank_ans)

    !> case 3
    call monolis_std_global_log_string("monolis_com_set_comm_size")
    call monolis_std_global_log_string("monolis_com_get_comm_size")

    comm_size = 30
    call monolis_com_set_comm_size(COM, comm_size)

    call monolis_com_get_comm_size(COM, comm_size_ans)

    call monolis_test_check_eq_I1("monolis_com_set_test 3", comm_size, comm_size_ans)

    !> case 4
    call monolis_std_global_log_string("monolis_com_set_n_internal_vertex")
    call monolis_std_global_log_string("monolis_com_get_n_internal_vertex")
    n_internal_vertex = 40
    call monolis_com_set_n_internal_vertex(COM, n_internal_vertex)

    call monolis_com_get_n_internal_vertex(COM, n_internal_vertex_ans)

    call monolis_test_check_eq_I1("monolis_com_set_test 4", &
      & n_internal_vertex, n_internal_vertex_ans)

    call monolis_com_finalize(COM)
  end subroutine monolis_com_set_test
end module mod_monolis_utils_define_com_test
