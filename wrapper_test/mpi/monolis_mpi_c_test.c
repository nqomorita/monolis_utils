#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>
#include "monolis_utils.h"

void monolis_allreduce_I_test()
{
  int comm;
  int i;

  monolis_std_global_log_string("monolis_allreduce_I");

  comm = monolis_mpi_get_global_comm();

  i = monolis_mpi_get_global_my_rank() + 1;

  monolis_allreduce_I(1, &i, MONOLIS_MPI_SUM, comm);

  if(monolis_mpi_get_global_comm_size() == 1){
    monolis_test_check_eq_I1("monolis_allreduce_I 1_c", i, 1);
  } else {
    monolis_test_check_eq_I1("monolis_allreduce_I 1_c", i, 3);
  }

  i = monolis_mpi_get_global_my_rank() + 1;

  monolis_allreduce_I(1, &i, MONOLIS_MPI_MAX, comm);

  if(monolis_mpi_get_global_comm_size() == 1){
    monolis_test_check_eq_I1("monolis_allreduce_I 2_c", i, 1);
  } else {
    monolis_test_check_eq_I1("monolis_allreduce_I 2_c", i, 2);
  }

  i = monolis_mpi_get_global_my_rank() + 1;

  monolis_allreduce_I(1, &i, MONOLIS_MPI_MIN, comm);

  monolis_test_check_eq_I1("monolis_allreduce_I 3_c", i, 1);
}

void monolis_allreduce_R_test()
{
  int comm;
  double i;

  monolis_std_global_log_string("monolis_allreduce_R");

  comm = monolis_mpi_get_global_comm();

  i = monolis_mpi_get_global_my_rank() + 1;

  monolis_allreduce_R(1, &i, MONOLIS_MPI_SUM, comm);

  if(monolis_mpi_get_global_comm_size() == 1){
    monolis_test_check_eq_R1("monolis_allreduce_R 1_c", i, 1.0);
  } else {
    monolis_test_check_eq_R1("monolis_allreduce_R 1_c", i, 3.0);
  }

  i = monolis_mpi_get_global_my_rank() + 1;

  monolis_allreduce_R(1, &i, MONOLIS_MPI_MAX, comm);

  if(monolis_mpi_get_global_comm_size() == 1){
    monolis_test_check_eq_R1("monolis_allreduce_R 2_c", i, 1.0);
  } else {
    monolis_test_check_eq_R1("monolis_allreduce_R 2_c", i, 2.0);
  }

  i = monolis_mpi_get_global_my_rank() + 1;

  monolis_allreduce_R(1, &i, MONOLIS_MPI_MIN, comm);

  monolis_test_check_eq_R1("monolis_allreduce_R 3_c", i, 1.0);
}

void monolis_allreduce_C_test()
{
  int comm;
  double a;
  double _Complex i;
  double _Complex j;

  monolis_std_global_log_string("monolis_allreduce_C");

  comm = monolis_mpi_get_global_comm();

  a = monolis_mpi_get_global_my_rank() + 1;
  i = a + a*I;

  monolis_allreduce_C(1, &i, MONOLIS_MPI_SUM, comm);

  if(monolis_mpi_get_global_comm_size() == 1){
    j = 1.0 + 1.0*I;
    monolis_test_check_eq_C1("monolis_allreduce_C 1_c", i, j);
  } else {
    j = 3.0 + 3.0*I;
    monolis_test_check_eq_C1("monolis_allreduce_C 1_c", i, j);
  }
}

void monolis_mpi_update_test()
{
  MONOLIS_COM monoCOM;
  int ndof, nnode;
  int i[8];
  double r[8];
  double _Complex c[8];

  monolis_std_global_log_string("monolis_mpi_update_I I");
  monolis_std_global_log_string("monolis_mpi_update_R I");
  monolis_std_global_log_string("monolis_mpi_update_C I");

  if(monolis_mpi_get_global_comm_size() == 1) return;

  nnode = 4;
  ndof = 2;

  monoCOM.comm = monolis_mpi_get_global_comm();
  monoCOM.send_n_neib = 1;
  monoCOM.recv_n_neib = 1;

  monoCOM.send_index = monolis_alloc_I_1d(monoCOM.send_index, 2);
  monoCOM.send_index[0] = 0;
  monoCOM.send_index[1] = 2;

  monoCOM.recv_index = monolis_alloc_I_1d(monoCOM.recv_index, 2);
  monoCOM.recv_index[0] = 0;
  monoCOM.recv_index[1] = 2;

  monoCOM.send_item = monolis_alloc_I_1d(monoCOM.send_item, 2);
  monoCOM.send_item[0] = 0;
  monoCOM.send_item[1] = 1;

  monoCOM.recv_item = monolis_alloc_I_1d(monoCOM.recv_item, 2);
  monoCOM.recv_item[0] = 2;
  monoCOM.recv_item[1] = 3;

  monoCOM.send_neib_pe = monolis_alloc_I_1d(monoCOM.send_neib_pe, 1);
  monoCOM.recv_neib_pe = monolis_alloc_I_1d(monoCOM.recv_neib_pe, 1);

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

  monolis_mpi_update_I(&monoCOM, nnode, ndof, i);

  if(monolis_mpi_get_global_my_rank() == 0){
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1_c", i[0], 1);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1_c", i[1], 2);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1_c", i[2], 3);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1_c", i[3], 4);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1_c", i[4], 5);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1_c", i[5], 6);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1_c", i[6], 7);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 1_c", i[7], 8);
  } else {
    monolis_test_check_eq_I1("monolis_allreduce_I_test 2_c", i[0], 5);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 2_c", i[1], 6);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 2_c", i[2], 7);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 2_c", i[3], 8);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 2_c", i[4], 1);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 2_c", i[5], 2);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 2_c", i[6], 3);
    monolis_test_check_eq_I1("monolis_allreduce_I_test 2_c", i[7], 4);
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

  monolis_mpi_update_R(&monoCOM, nnode, ndof, r);

  if(monolis_mpi_get_global_my_rank() == 0){
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1_c", r[0], 1.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1_c", r[1], 2.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1_c", r[2], 3.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1_c", r[3], 4.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1_c", r[4], 5.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1_c", r[5], 6.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1_c", r[6], 7.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 1_c", r[7], 8.0);
  } else {
    monolis_test_check_eq_R1("monolis_allreduce_R_test 2_c", r[0], 5.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 2_c", r[1], 6.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 2_c", r[2], 7.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 2_c", r[3], 8.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 2_c", r[4], 1.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 2_c", r[5], 2.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 2_c", r[6], 3.0);
    monolis_test_check_eq_R1("monolis_allreduce_R_test 2_c", r[7], 4.0);
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

  monolis_mpi_update_C(&monoCOM, nnode, ndof, c);

  if(monolis_mpi_get_global_my_rank() == 0){
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1_c", c[0], 1.0 + 1.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1_c", c[1], 2.0 + 2.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1_c", c[2], 3.0 + 3.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1_c", c[3], 4.0 + 4.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1_c", c[4], 5.0 + 5.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1_c", c[5], 6.0 + 6.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1_c", c[6], 7.0 + 7.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 1_c", c[7], 8.0 + 8.0*I);
  } else {
    monolis_test_check_eq_C1("monolis_allreduce_C_test 2_c", c[0], 5.0 + 5.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 2_c", c[1], 6.0 + 6.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 2_c", c[2], 7.0 + 7.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 2_c", c[3], 8.0 + 8.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 2_c", c[4], 1.0 + 1.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 2_c", c[5], 2.0 + 2.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 2_c", c[6], 3.0 + 3.0*I);
    monolis_test_check_eq_C1("monolis_allreduce_C_test 2_c", c[7], 4.0 + 4.0*I);
  }
}

void monolis_mpi_test()
{
  monolis_allreduce_I_test();
  monolis_allreduce_R_test();
  monolis_allreduce_C_test();
  monolis_mpi_update_test();
}
