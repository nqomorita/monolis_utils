!> 通信データテストモジュール
module mod_monolis_utils_define_com_init_test
  use mod_monolis_utils
  use mod_monolis_utils_define_com
  implicit none

contains

  subroutine monolis_utils_define_com_init_test()
    implicit none
    type(monolis_com) :: COM
    integer(kint) :: n_internal_vertex
    integer(kint) :: n_vertex
    integer(kint) :: comm
    integer(kint) :: global_id(5)

    call monolis_std_global_log_string("monolis_com_input_comm_table")

    call monolis_std_global_log_string("monolis_com_initialize_by_parted_files")

    comm = monolis_mpi_get_global_comm()

    call monolis_com_initialize_by_parted_files(COM, comm, &
      & MONOLIS_DEFAULT_TOP_DIR, "parted.0.ans", MONOLIS_DEFAULT_FILE_NAME)

    call monolis_test_check_eq_I1("monolis_com_initialize_by_parted_files 1", COM%my_rank, monolis_mpi_get_global_my_rank())
    call monolis_test_check_eq_I1("monolis_com_initialize_by_parted_files 2", COM%comm_size, monolis_mpi_get_global_comm_size())

    call monolis_com_finalize(COM)

    call monolis_std_global_log_string("monolis_com_initialize_by_global_id")

    n_vertex = 5

    n_internal_vertex = 3

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

    call monolis_com_initialize_by_global_id(COM, comm, n_internal_vertex, n_vertex, global_id)

    call monolis_test_check_eq_I1("monolis_com_initialize_by_global_id 1", COM%my_rank, monolis_mpi_get_global_my_rank())
    call monolis_test_check_eq_I1("monolis_com_initialize_by_global_id 2", COM%comm_size, monolis_mpi_get_global_comm_size())

    if(monolis_mpi_get_local_comm_size(comm) == 1)then
      call monolis_test_check_eq_I1("monolis_com_initialize_by_global_id 3", COM%n_internal_vertex, 0)
    else
      call monolis_test_check_eq_I1("monolis_com_initialize_by_global_id 3", COM%n_internal_vertex, 3)
      endif

    call monolis_com_finalize(COM)

    call monolis_std_global_log_string("monolis_com_initialize_by_self")

    call monolis_com_initialize_by_self(COM)

    call monolis_test_check_eq_I1("monolis_com_initialize_by_self 1", COM%my_rank, 0)
    call monolis_test_check_eq_I1("monolis_com_initialize_by_self 2", COM%comm_size, 1)
    call monolis_test_check_eq_I1("monolis_com_initialize_by_self 3", COM%n_internal_vertex, 0)
    call monolis_test_check_eq_I1("monolis_com_initialize_by_self 4", COM%recv_n_neib, 0)
    call monolis_test_check_eq_I1("monolis_com_initialize_by_self 5", COM%send_n_neib, 0)

    call monolis_com_finalize(COM)
  end subroutine monolis_utils_define_com_init_test

end module mod_monolis_utils_define_com_init_test
