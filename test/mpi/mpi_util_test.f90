!> MPI util テストモジュール
module mod_monolis_mpi_util_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_mpi_util_test()
    implicit none

    if(monolis_mpi_global_comm_size() == 1) return

    call monolis_mpi_util_test_main()
  end subroutine monolis_mpi_util_test

  !> unit test
  subroutine monolis_mpi_util_test_main()
    implicit none

    call monolis_std_log_string("monolis_mpi_util_test_main")

    if(monolis_mpi_global_comm() == MPI_COMM_WORLD)then
      call monolis_test_assert_pass("monolis_mpi_global_comm")
    else
      call monolis_test_assert_fail("monolis_mpi_global_comm", "")
    endif

    if(monolis_mpi_global_comm_size() == 2)then
      call monolis_test_assert_pass("monolis_mpi_global_comm_size")
    else
      call monolis_test_assert_fail("monolis_mpi_global_comm_size", "")
    endif

    if(monolis_mpi_local_comm_size(MPI_COMM_WORLD) == 2)then
      call monolis_test_assert_pass("monolis_mpi_local_comm_size")
    else
      call monolis_test_assert_fail("monolis_mpi_local_comm_size", "")
    endif
  end subroutine monolis_mpi_util_test_main
end module mod_monolis_mpi_util_test
