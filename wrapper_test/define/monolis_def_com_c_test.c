#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_utils.h"

void monolis_com_copy_test()
{
  MONOLIS_COM com1;
  MONOLIS_COM com2;

  monolis_std_global_log_string("monolis_com_copy");

  monolis_com_initialize_by_self(&com1);

  com1.comm = 1;
  com1.my_rank = 2;
  com1.comm_size = 3;
  com1.n_internal_vertex = 4;

  com1.recv_n_neib = 1;

  com1.recv_neib_pe = monolis_alloc_I_1d(com1.recv_neib_pe, com1.recv_n_neib);

  com1.recv_neib_pe[0] = 10;

  com1.recv_index = monolis_alloc_I_1d(com1.recv_index, com1.recv_n_neib + 1);

  com1.recv_index[0] = 20;
  com1.recv_index[1] = 30;

  com1.recv_item = monolis_alloc_I_1d(com1.recv_item, 2);

  com1.recv_item[0] = 40;
  com1.recv_item[1] = 50;

  com1.send_n_neib = 2;

  com1.send_neib_pe = monolis_alloc_I_1d(com1.send_neib_pe, com1.send_n_neib);

  com1.send_neib_pe[0] = 11;
  com1.send_neib_pe[1] = 21;

  com1.send_index = monolis_alloc_I_1d(com1.send_index, com1.send_n_neib + 1);

  com1.send_index[0] = 31;
  com1.send_index[1] = 41;
  com1.send_index[2] = 51;

  com1.send_item = monolis_alloc_I_1d(com1.send_item, 2);

  com1.send_item[0] = 61;
  com1.send_item[1] = 71;

  monolis_com_copy(&com1, &com2);

  monolis_test_check_eq_I1("monolis_com_copy 1", com1.comm, com2.comm);
  monolis_test_check_eq_I1("monolis_com_copy 2", com1.my_rank, com2.my_rank);
  monolis_test_check_eq_I1("monolis_com_copy 3", com1.comm_size, com2.comm_size);
  monolis_test_check_eq_I1("monolis_com_copy 4", com1.n_internal_vertex, com2.n_internal_vertex);
  monolis_test_check_eq_I1("monolis_com_copy 5", com1.recv_n_neib, com2.recv_n_neib);
  monolis_test_check_eq_I1("monolis_com_copy 6", com1.send_n_neib, com2.send_n_neib);
  monolis_test_check_eq_I1("monolis_com_copy 7", com1.recv_neib_pe[0], com2.recv_neib_pe[0]);
  monolis_test_check_eq_I1("monolis_com_copy 8", com1.recv_index[0], com2.recv_index[0]);
  monolis_test_check_eq_I1("monolis_com_copy 9", com1.recv_index[1], com2.recv_index[1]);
  monolis_test_check_eq_I1("monolis_com_copy 10", com1.recv_item[0], com2.recv_item[0]);
  monolis_test_check_eq_I1("monolis_com_copy 11", com1.recv_item[1], com2.recv_item[1]);
  monolis_test_check_eq_I1("monolis_com_copy 12", com1.send_neib_pe[0], com2.send_neib_pe[0]);
  monolis_test_check_eq_I1("monolis_com_copy 13", com1.send_neib_pe[1], com2.send_neib_pe[1]);
  monolis_test_check_eq_I1("monolis_com_copy 14", com1.send_index[0], com2.send_index[0]);
  monolis_test_check_eq_I1("monolis_com_copy 15", com1.send_index[1], com2.send_index[1]);
  monolis_test_check_eq_I1("monolis_com_copy 16", com1.send_index[2], com2.send_index[2]);
  monolis_test_check_eq_I1("monolis_com_copy 17", com1.send_item[0], com2.send_item[0]);
  monolis_test_check_eq_I1("monolis_com_copy 18", com1.send_item[1], com2.send_item[1]);
}


void monolis_com_set_test()
{
  MONOLIS_COM com;
  int         comm;
  int         comm_ans;
  int         my_rank;
  int         my_rank_ans;
  int         comm_size;
  int         comm_size_ans;
  int         n_internal_vertex;
  int         n_internal_vertex_ans;

  monolis_std_global_log_string("monolis_com_set_communicator");
  monolis_std_global_log_string("monolis_com_get_communicator");

  comm = 10;

  monolis_com_set_communicator(&com, comm);

  monolis_com_get_communicator(&com, &comm_ans);

  monolis_test_check_eq_I1("monolis_def_com_test 1_c", comm, comm_ans);

  my_rank = 20;

  monolis_std_global_log_string("monolis_com_set_my_rank");
  monolis_std_global_log_string("monolis_com_get_my_rank");

  monolis_com_set_my_rank(&com, my_rank);

  monolis_com_get_my_rank(&com, &my_rank_ans);

  monolis_test_check_eq_I1("monolis_def_com_test 2_c", my_rank, my_rank_ans);

  comm_size = 30;

  monolis_std_global_log_string("monolis_com_set_comm_size");
  monolis_std_global_log_string("monolis_com_get_comm_size");

  monolis_com_set_comm_size(&com, comm_size);

  monolis_com_get_comm_size(&com, &comm_size_ans);

  monolis_test_check_eq_I1("monolis_def_com_test 3_c", comm_size, comm_size_ans);

  n_internal_vertex = 40;

  monolis_std_global_log_string("monolis_com_set_n_internal_vertex");
  monolis_std_global_log_string("monolis_com_get_n_internal_vertex");

  monolis_com_set_n_internal_vertex(&com, n_internal_vertex);

  monolis_com_get_n_internal_vertex(&com, &n_internal_vertex_ans);

  monolis_test_check_eq_I1("monolis_def_com_test 4_c", n_internal_vertex, n_internal_vertex_ans);
}

void monolis_def_com_test()
{
  monolis_com_set_test();
  monolis_com_copy_test();
}