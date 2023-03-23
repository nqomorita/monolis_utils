#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_utils.h"

void monolis_comm_table_c_test()
{
  MONOLIS_COM com;
  int n_internal_vertex;
  int n_vertex;
  int vertex_id[5];

  if(monolis_mpi_get_global_comm_size() == 1) return;

  monolis_std_log_string("monolis_def_com_test");

  n_internal_vertex = 3;

  n_vertex = 5;

  monolis_com_set_communicator(&com, monolis_mpi_get_global_comm());

  if(monolis_mpi_get_global_my_rank() == 0){
    vertex_id[0] = 10;
    vertex_id[1] = 20;
    vertex_id[2] = 30;
    vertex_id[3] = 40;
    vertex_id[4] = 50;
  } else {
    vertex_id[0] = 40;
    vertex_id[1] = 50;
    vertex_id[2] = 60;
    vertex_id[3] = 20;
    vertex_id[4] = 30;
  }

  monolis_com_get_comm_table_parallel(n_internal_vertex, n_vertex, vertex_id, &com);

  if(monolis_mpi_get_global_my_rank() == 0){
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 1", com.my_rank, 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 2", com.comm_size, 2);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 3", com.n_internal_vertex, 3);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 4", com.recv_n_neib, 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 5", com.recv_neib_pe[0], 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 6", com.recv_index[0], 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 7", com.recv_index[1], 2);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 8", com.recv_item[0], 4);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 9", com.recv_item[1], 5);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 10", com.send_n_neib, 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 11", com.send_neib_pe[0], 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 12", com.send_index[0], 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 13", com.send_index[1], 2);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 14", com.send_item[0], 2);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 15", com.send_item[1], 3);
  } else {
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 1", com.my_rank, 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 2", com.comm_size, 2);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 3", com.n_internal_vertex, 3);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 4", com.recv_n_neib, 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 5", com.recv_neib_pe[0], 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 6", com.recv_index[0], 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 7", com.recv_index[1], 2);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 8", com.recv_item[0], 4);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 9", com.recv_item[1], 5);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 10", com.send_n_neib, 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 11", com.send_neib_pe[0], 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 12", com.send_index[0], 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 13", com.send_index[1], 2);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 14", com.send_item[0], 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 15", com.send_item[1], 2);
  }
}
