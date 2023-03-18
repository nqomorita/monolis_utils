#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>
#include "monolis_utils.h"

void monolis_allreduce_I_test()
{
  int comm;
  int i;

  monolis_std_log_string("monolis_allreduce_I_test");

  comm = monolis_mpi_get_global_comm();

  i = monolis_mpi_get_global_my_rank() + 1;

  monolis_allreduce_I(1, &i, MONOLIS_MPI_SUM, comm);

  if(monolis_mpi_get_global_comm_size() == 1){
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i, 1);
  } else {
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i, 3);
  }

  i = monolis_mpi_get_global_my_rank() + 1;

  monolis_allreduce_I(1, &i, MONOLIS_MPI_MAX, comm);

  if(monolis_mpi_get_global_comm_size() == 1){
    monolis_test_check_eq_I1("monolis_allreduce_I_test 2", i, 1);
  } else {
    monolis_test_check_eq_I1("monolis_allreduce_I_test 2", i, 2);
  }

  i = monolis_mpi_get_global_my_rank() + 1;

  monolis_allreduce_I(1, &i, MONOLIS_MPI_MIN, comm);

  monolis_test_check_eq_I1("monolis_allreduce_I_test 3", i, 1);
}

void monolis_allreduce_R_test()
{
  int comm;
  double i;

  monolis_std_log_string("monolis_allreduce_R_test");

  comm = monolis_mpi_get_global_comm();

  i = monolis_mpi_get_global_my_rank() + 1;

  monolis_allreduce_R(1, &i, MONOLIS_MPI_SUM, comm);

  if(monolis_mpi_get_global_comm_size() == 1){
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", i, 1.0);
  } else {
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", i, 3.0);
  }

  i = monolis_mpi_get_global_my_rank() + 1;

  monolis_allreduce_R(1, &i, MONOLIS_MPI_MAX, comm);

  if(monolis_mpi_get_global_comm_size() == 1){
    monolis_test_check_eq_R1("monolis_allreduce_R_test 2", i, 1.0);
  } else {
    monolis_test_check_eq_R1("monolis_allreduce_R_test 2", i, 2.0);
  }

  i = monolis_mpi_get_global_my_rank() + 1;

  monolis_allreduce_R(1, &i, MONOLIS_MPI_MIN, comm);

  monolis_test_check_eq_R1("monolis_allreduce_R_test 3", i, 1.0);
}

void monolis_allreduce_C_test()
{
  int comm;
  double a;
  double complex i;
  double complex j;

  monolis_std_log_string("monolis_allreduce_C_test");

  comm = monolis_mpi_get_global_comm();

  a = monolis_mpi_get_global_my_rank() + 1;
  i = a + a*I;

  monolis_allreduce_C(1, &i, MONOLIS_MPI_SUM, comm);

  if(monolis_mpi_get_global_comm_size() == 1){
    j = 1.0 + 1.0*I;
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", i, j);
  } else {
    j = 3.0 + 3.0*I;
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", i, j);
  }
}

void monolis_mpi_update_test()
{
  MONOLIS_COM monoCOM;
  int ndof;
  int i[8];
  double r[8];
  double complex c[8];

  monolis_std_log_string("monolis_mpi_update_test I");

  if(monolis_mpi_get_global_comm_size() == 1) return;

  ndof = 2;
  monoCOM.comm = monolis_mpi_get_global_comm();
  monoCOM.send_n_neib = 1;
  monoCOM.recv_n_neib = 1;

  monolis_alloc_I_1d(monoCOM.send_index, 2);
  monoCOM.send_index[0] = 0;
  monoCOM.send_index[1] = 2;

  monolis_alloc_I_1d(monoCOM.recv_index, 2);
  monoCOM.recv_index[0] = 0;
  monoCOM.recv_index[1] = 2;

  monolis_alloc_I_1d(monoCOM.send_item, 2);
  monoCOM.send_item[0] = 1;
  monoCOM.send_item[1] = 2;

  monolis_alloc_I_1d(monoCOM.recv_item, 2);
  monoCOM.recv_item[0] = 3;
  monoCOM.recv_item[1] = 4;

  monolis_alloc_I_1d(monoCOM.send_neib_pe, 1);
  monolis_alloc_I_1d(monoCOM.recv_neib_pe, 1);

  if(monolis_mpi_get_global_my_rank() == 0){
    monoCOM.send_neib_pe[0] = 1;
    monoCOM.recv_neib_pe[0] = 1;
  } else {
    monoCOM.send_neib_pe[0] = 0;
    monoCOM.recv_neib_pe[0] = 0;
  }

  if(monolis_mpi_get_global_my_rank() == 0){
    i[0] = 1;
    i[1] = 2;
    i[2] = 3;
    i[3] = 4;
  } else {
    i[0] = 5;
    i[1] = 6;
    i[2] = 7;
    i[3] = 8;
  }

  monolis_mpi_update_I(&monoCOM, 2, ndof, i);

  if(monolis_mpi_get_global_my_rank() == 0){
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[0], 1);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[1], 2);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[2], 3);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[3], 4);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[4], 5);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[5], 6);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[6], 7);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[7], 8);
  } else {
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[0], 5);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[1], 6);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[2], 7);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[3], 8);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[4], 1);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[5], 2);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[6], 3);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1", i[7], 4);
  }

  if(monolis_mpi_get_global_my_rank() == 0){
    r[0] = 1.0;
    r[1] = 2.0;
    r[2] = 3.0;
    r[3] = 4.0;
  } else {
    r[0] = 5.0;
    r[1] = 6.0;
    r[2] = 7.0;
    r[3] = 8.0;
  }

  monolis_mpi_update_R(&monoCOM, 2, ndof, r);

  if(monolis_mpi_get_global_my_rank() == 0){
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[0], 1.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[1], 2.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[2], 3.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[3], 4.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[4], 5.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[5], 6.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[6], 7.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[7], 8.0);
  } else {
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[0], 5.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[1], 6.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[2], 7.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[3], 8.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[4], 1.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[5], 2.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[6], 3.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1", r[7], 4.0);
  }

  if(monolis_mpi_get_global_my_rank() == 0){
    c[0] = 1.0 + 1.0*I;
    c[1] = 2.0 + 2.0*I;
    c[2] = 3.0 + 3.0*I;
    c[3] = 4.0 + 4.0*I;
  } else {
    c[0] = 5.0 + 5.0*I;
    c[1] = 6.0 + 6.0*I;
    c[2] = 7.0 + 7.0*I;
    c[3] = 8.0 + 8.0*I;
  }

  monolis_mpi_update_C(&monoCOM, 2, ndof, c);

  if(monolis_mpi_get_global_my_rank() == 0){
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[0], 1.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[1], 2.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[2], 3.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[3], 4.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[4], 5.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[5], 6.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[6], 7.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[7], 8.0);
  } else {
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[0], 5.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[1], 6.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[2], 7.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[3], 8.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[4], 1.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[5], 2.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[6], 3.0);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1", c[7], 4.0);
  }
}

void monolis_mpi_test()
{
  monolis_allreduce_I_test();
  monolis_allreduce_R_test();
  monolis_allreduce_C_test();
  monolis_mpi_update_test();
}
