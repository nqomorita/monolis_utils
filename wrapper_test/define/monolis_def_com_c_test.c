#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_utils.h"

void monolis_def_com_test()
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

  monolis_std_log_string("monolis_def_com_test");

  comm = 10;

  monolis_com_set_communicator(&com, comm);

  monolis_com_get_communicator(&com, &comm_ans);

  monolis_test_check_eq_I1("monolis_def_com_test 1_c", comm, comm_ans);

  my_rank = 20;

  monolis_com_set_my_rank(&com, my_rank);

  monolis_com_get_my_rank(&com, &my_rank_ans);

  monolis_test_check_eq_I1("monolis_def_com_test 2_c", my_rank, my_rank_ans);

  comm_size = 30;

  monolis_com_set_comm_size(&com, comm_size);

  monolis_com_get_comm_size(&com, &comm_size_ans);

  monolis_test_check_eq_I1("monolis_def_com_test 3_c", comm_size, comm_size_ans);

  n_internal_vertex = 40;

  monolis_com_set_n_internal_vertex(&com, n_internal_vertex);

  monolis_com_get_n_internal_vertex(&com, &n_internal_vertex_ans);

  monolis_test_check_eq_I1("monolis_def_com_test 4_c", n_internal_vertex, n_internal_vertex_ans);
}
