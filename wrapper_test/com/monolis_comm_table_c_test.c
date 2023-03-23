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
}
