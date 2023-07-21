#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_utils.h"

void monolis_com_get_comm_table_parallel_c_test()
{
  MONOLIS_COM com;
  int n_internal_vertex;
  int n_vertex;
  int vertex_id[5];

  if(monolis_mpi_get_global_comm_size() == 1) return;

  monolis_std_global_log_string("monolis_com_set_communicator");

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

  monolis_std_global_log_string("monolis_com_get_comm_table_parallel");

  monolis_com_get_comm_table_parallel(n_internal_vertex, n_vertex, vertex_id, &com);

  if(monolis_mpi_get_global_my_rank() == 0){
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 1", com.my_rank, 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 2", com.comm_size, 2);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 3", com.n_internal_vertex, 3);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 4", com.recv_n_neib, 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 5", com.recv_neib_pe[0], 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 6", com.recv_index[0], 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 7", com.recv_index[1], 2);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 8", com.recv_item[0], 3);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 9", com.recv_item[1], 4);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 10", com.send_n_neib, 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 11", com.send_neib_pe[0], 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 12", com.send_index[0], 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 13", com.send_index[1], 2);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 14", com.send_item[0], 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 15", com.send_item[1], 2);
  } else {
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 1", com.my_rank, 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 2", com.comm_size, 2);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 3", com.n_internal_vertex, 3);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 4", com.recv_n_neib, 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 5", com.recv_neib_pe[0], 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 6", com.recv_index[0], 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 7", com.recv_index[1], 2);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 8", com.recv_item[0], 3);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 9", com.recv_item[1], 4);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 10", com.send_n_neib, 1);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 11", com.send_neib_pe[0], 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 12", com.send_index[0], 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 13", com.send_index[1], 2);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 14", com.send_item[0], 0);
    monolis_test_check_eq_I1("monolis_com_get_comm_table_parallel 15", com.send_item[1], 1);
  }
}

void monolis_get_bool_list_of_internal_c_test()
{
  MONOLIS_COM com;
  int i;
  int n_node;
  int n_elem;
  int n_base;
  int comm;
  int n_internal_vertex;
  int** elem;
  int global_id[5];
  int index[5];
  int item[8];
  bool list[4];

  if(monolis_mpi_get_global_comm_size() == 1) return;

  monolis_std_global_log_string("monolis_get_bool_list_of_internal_simple_mesh");

  comm = monolis_mpi_get_global_comm();

  n_internal_vertex = 3;

  n_node = 5;

  elem = monolis_alloc_I_2d(elem, 4, 2);

  if(monolis_mpi_get_global_my_rank() == 0){
    global_id[0] = 10;
    global_id[1] = 20;
    global_id[2] = 30;
    global_id[3] = 40;
    global_id[4] = 50;
  } else {
    global_id[0] = 40;
    global_id[1] = 50;
    global_id[2] = 60;
    global_id[3] = 20;
    global_id[4] = 30;
  }

  monolis_com_initialize_by_global_id(&com, comm, n_internal_vertex, n_node, global_id);

  n_elem = 4;

  n_base = 2;

  if(monolis_mpi_get_global_my_rank() == 0){
    elem[0][0] = 0; elem[0][1] = 1;
    elem[1][0] = 1; elem[1][1] = 2;
    elem[2][0] = 2; elem[2][1] = 3;
    elem[3][0] = 3; elem[3][1] = 4;
  } else {
    elem[0][0] = 0; elem[0][1] = 1;
    elem[1][0] = 1; elem[1][1] = 2;
    elem[2][0] = 3; elem[2][1] = 4;
    elem[3][0] = 4; elem[3][1] = 0;
  }

  com.n_internal_vertex = 3;

  monolis_get_bool_list_of_internal_simple_mesh(&com, n_node, n_elem, n_base, elem, list);

  if(monolis_mpi_get_global_my_rank() == 0){
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list[0], true);
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list[1], true);
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list[2], true);
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list[3], false);
  } else {
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list[0], true);
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list[1], true);
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list[2], false);
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_simple_mesh", list[3], false);
  }

  monolis_com_finalize(&com);

  monolis_std_global_log_string("monolis_get_bool_list_of_internal_connetivity");

  n_node = 5;

  n_elem = 4;

  com.n_internal_vertex = 3;

  monolis_com_initialize_by_global_id(&com, comm, n_internal_vertex, n_node, global_id);

  index[0] = 0;
  index[1] = 2;
  index[2] = 4;
  index[3] = 6;
  index[4] = 8;

  if(monolis_mpi_get_global_my_rank() == 0){
    item[0] = 0; item[1] = 1;
    item[2] = 1; item[3] = 2;
    item[4] = 2; item[5] = 3;
    item[6] = 3; item[7] = 4;
  } else {
    item[0] = 0; item[1] = 1;
    item[2] = 1; item[3] = 2;
    item[4] = 3; item[5] = 4;
    item[6] = 4; item[7] = 0;
  }

  monolis_get_bool_list_of_internal_connetivity(&com, n_node, n_elem, index, item, list);

  if(monolis_mpi_get_global_my_rank() == 0){
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list[0], true);
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list[1], true);
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list[2], true);
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list[3], false);
  } else {
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list[0], true);
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list[1], true);
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list[2], false);
    //monolis_test_check_eq_L1("monolis_get_bool_list_of_internal_connetivity", list[3], false);
  }

  monolis_com_finalize(&com);

}

void monolis_comm_table_c_test()
{
  monolis_com_get_comm_table_parallel_c_test();
  monolis_get_bool_list_of_internal_c_test();
}
