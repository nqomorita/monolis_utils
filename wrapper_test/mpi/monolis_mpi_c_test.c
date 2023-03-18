#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>
#include "monolis_utils.h"

void monolis_allreduce_I_test()
{
  monolis_std_log_string("monolis_allreduce_I_test");

  //monolis_allreduce_I(n, val, tag, comm);
}

void monolis_allreduce_R_test()
{
  monolis_std_log_string("monolis_allreduce_R_test");

  //monolis_allreduce_R(n, val, tag, comm);
}

void monolis_allreduce_C_test()
{
  monolis_std_log_string("monolis_allreduce_C_test");

  //monolis_allreduce_C(n, val, tag, comm);
}

void monolis_mpi_update_I_test()
{
  monolis_std_log_string("monolis_mpi_update_I_test");

  //monolis_mpi_update_I(com, n, n_dof, x);
}

void monolis_mpi_update_R_test()
{
  monolis_std_log_string("monolis_mpi_update_R_test");

  //monolis_mpi_update_R(com, n, n_dof, x);
}

void monolis_mpi_update_C_test()
{
  monolis_std_log_string("monolis_mpi_update_C_test");

  //monolis_mpi_update_C(com, n, n_dof, x);
}

void monolis_mpi_test()
{
  monolis_allreduce_I_test();
  monolis_allreduce_R_test();
  monolis_allreduce_C_test();
  monolis_mpi_update_I_test();
  monolis_mpi_update_R_test();
  monolis_mpi_update_C_test();
}
