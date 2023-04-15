program monolis_utils_test
  use mod_monolis_utils
  use mod_monolis_utils_error_test
  use mod_monolis_utils_alloc_test
  use mod_monolis_utils_palloc_test
  use mod_monolis_utils_std_test_test
  use mod_monolis_utils_std_sort_I_test
  use mod_monolis_utils_std_sort_R_test
  use mod_monolis_utils_std_algebra_test
  use mod_monolis_utils_hash_test
  use mod_monolis_utils_aabb_test
  use mod_monolis_utils_kdtree_test
  use mod_monolis_utils_define_com_test
  use mod_monolis_utils_sys_test
  use mod_monolis_utils_define_com_init_test
  use mod_monolis_mpi_test
  use mod_monolis_mpi_util_test
  use mod_monolis_mpi_util
  use mod_monolis_io_arg_test
  use mod_monolis_io_file_name_test
  use mod_monolis_io_com_test
  use mod_monolis_io_mtx_test
  use mod_monolis_io_util_test
  use mod_monolis_io_test
  use mod_monolis_comm_table_test
  use mod_monolis_comm_par_util_test
  use mod_monolis_comm_ser_util_test
  use mod_monolis_driver_util_test
  use mod_monolis_extract_util_test
  use mod_monolis_refiner_util_test
  use mod_monolis_driver_test
  implicit none

  call monolis_mpi_initialize()

  call monolis_utils_error_test()
  call monolis_utils_std_test_test()
  call monolis_utils_std_sort_I_test()
  call monolis_utils_std_sort_R_test()
  call monolis_utils_std_algebra_test()

  call monolis_utils_define_com_test()
  call monolis_utils_alloc_test()
  call monolis_utils_palloc_test()
  call monolis_utils_hash_test()
  call monolis_utils_aabb_test()
  call monolis_utils_kdtree_test()
  call monolis_utils_sys_test()

  call monolis_mpi_test()
  call monolis_mpi_util_test()
  call monolis_comm_table_test()
  call monolis_comm_par_util_test()
  call monolis_comm_ser_util_test()
  call monolis_utils_define_com_init_test()

  call monolis_driver_util_test()
  call monolis_extract_util_test()
  call monolis_refiner_util_test()

  if(monolis_mpi_get_global_comm_size() == 1)then
    call monolis_io_arg_test()
    call monolis_io_file_name_test()
    call monolis_io_com_test()
    call monolis_io_mtx_test()
    call monolis_io_util_test()
    call monolis_io_test()

    call monolis_driver_test()
  endif

  call monolis_mpi_finalize()
end program monolis_utils_test
