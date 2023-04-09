!> MPI util テストモジュール
module mod_monolis_mpi_util_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_mpi_util_test()
    implicit none

    if(monolis_mpi_get_global_comm_size() == 1) return

    call monolis_mpi_util_test_main()
    call monolis_mpi_split_comm_test()
  end subroutine monolis_mpi_util_test

  !> unit test
  subroutine monolis_mpi_util_test_main()
    implicit none

    call monolis_std_global_log_string("monolis_mpi_get_global_comm")
    call monolis_std_global_log_string("monolis_mpi_get_global_comm_size")
    call monolis_std_global_log_string("monolis_mpi_get_local_comm_size")

    if(monolis_mpi_get_global_comm() == MPI_COMM_WORLD)then
      call monolis_test_assert_pass("monolis_mpi_global_comm")
    else
      call monolis_test_assert_fail("monolis_mpi_global_comm", "")
    endif

    if(monolis_mpi_get_global_comm_size() == 2)then
      call monolis_test_assert_pass("monolis_mpi_global_comm_size")
    else
      call monolis_test_assert_fail("monolis_mpi_global_comm_size", "")
    endif

    if(monolis_mpi_get_local_comm_size(MPI_COMM_WORLD) == 2)then
      call monolis_test_assert_pass("monolis_mpi_local_comm_size")
    else
      call monolis_test_assert_fail("monolis_mpi_local_comm_size", "")
    endif
  end subroutine monolis_mpi_util_test_main

  subroutine monolis_mpi_split_comm_test
    implicit none
    integer(kint) :: comm
    integer(kint) :: group_id
    integer(kint) :: comm_split

    call monolis_std_global_log_string("monolis_mpi_split_comm")

    comm = monolis_mpi_get_global_comm()

    group_id = monolis_mpi_get_global_my_rank()

    call monolis_mpi_split_comm(comm, group_id, comm_split)

    call monolis_test_check_eq_I1("monolis_mpi_split_comm 1", monolis_mpi_get_local_comm_size(comm_split), 1)

    call monolis_test_check_eq_I1("monolis_mpi_split_comm 2", monolis_mpi_get_local_my_rank(comm_split), 0)
  end subroutine monolis_mpi_split_comm_test
end module mod_monolis_mpi_util_test
