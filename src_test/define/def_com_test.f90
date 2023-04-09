!> 通信データテストモジュール
module mod_monolis_utils_define_com_test
  use mod_monolis_utils
  use mod_monolis_utils_define_com
  implicit none

contains

  subroutine monolis_utils_define_com_test()
    implicit none

    call monolis_com_set_test()

    call monolis_std_global_log_string("monolis_com_debug_write")
  end subroutine monolis_utils_define_com_test

  subroutine monolis_com_set_test()
    implicit none
    type(monolis_COM) :: COM
    integer(kint) :: comm, comm_ans
    integer(kint) :: my_rank, my_rank_ans
    integer(kint) :: comm_size, comm_size_ans
    integer(kint) :: n_internal_vertex, n_internal_vertex_ans

    call monolis_std_global_log_string("monolis_com_initialize")
    call monolis_std_global_log_string("monolis_com_finalize")

    call monolis_com_initialize(COM)

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
