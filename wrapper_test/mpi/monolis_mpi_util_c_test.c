#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_utils.h"

void monolis_mpi_util_test()
{
  int comm;
  int id;
  int split;

  if(monolis_mpi_get_global_comm_size() == 1) return;

  monolis_std_global_log_string("monolis_mpi_get_global_comm");
  monolis_std_global_log_string("monolis_mpi_get_global_comm_size");
  monolis_std_global_log_string("monolis_mpi_get_local_comm_size");
  monolis_std_global_log_string("monolis_mpi_split_comm");

  if(monolis_mpi_get_global_comm() == 0){
    monolis_test_assert_pass("monolis_mpi_global_comm_c");
  } else {
    monolis_test_assert_fail("monolis_mpi_global_comm_c", "");
  }

  if(monolis_mpi_get_global_comm_size() == 2){
    monolis_test_assert_pass("monolis_mpi_get_global_comm_size_c");
  } else {
    monolis_test_assert_fail("monolis_mpi_get_global_comm_size_c", "");
  }

  comm = monolis_mpi_get_global_comm();

  if(monolis_mpi_get_local_comm_size(comm) == 2){
    monolis_test_assert_pass("monolis_mpi_get_local_comm_size_c");
  } else {
    monolis_test_assert_fail("monolis_mpi_get_local_comm_size_c", "");
  }

  id = monolis_mpi_get_global_my_rank();

  monolis_mpi_split_comm(comm, id, &split);

  monolis_test_check_eq_I1("monolis_mpi_split_comm_c", monolis_mpi_get_local_comm_size(split), 1);

  monolis_test_check_eq_I1("monolis_mpi_split_comm_c", monolis_mpi_get_local_my_rank(split), 0);
}
