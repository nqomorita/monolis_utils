program monolis_utils_test
  use mod_monolis_utils
  use mod_monolis_utils_alloc_test
  use mod_monolis_utils_std_test_test
  use mod_monolis_utils_std_sort_I_test
  use mod_monolis_utils_std_sort_R_test
  use mod_monolis_utils_std_algebra_test
  use mod_monolis_utils_hash_test
  use mod_monolis_utils_aabb_test
  use mod_monolis_utils_kdtree_test
  use mod_monolis_mpi_test
  use mod_monolis_mpi_util_test
  use mod_monolis_mpi_util
  implicit none

  call monolis_mpi_initialize()

  if(monolis_mpi_global_comm_size() == 1)then
    !> std test for serial
    call monolis_utils_alloc_test()
    call monolis_utils_std_test_test()
    call monolis_utils_std_sort_I_test()
    call monolis_utils_std_sort_R_test()
    call monolis_utils_std_algebra_test()
    call monolis_utils_hash_test()
    call monolis_utils_aabb_test()
    call monolis_utils_kdtree_test()
  elseif(monolis_mpi_global_comm_size() == 2)then
    !> std test for paralell
    call check_mpi_num_process()
    call monolis_mpi_test()
    call monolis_mpi_util_test()
  else
    call monolis_test_assert_fail("num of MPI process", "MPI test program runs only in 'mpirun -np 2'")
  endif

  call monolis_mpi_finalize()

contains

  subroutine check_mpi_num_process()
    implicit none
  end subroutine check_mpi_num_process
end program monolis_utils_test
