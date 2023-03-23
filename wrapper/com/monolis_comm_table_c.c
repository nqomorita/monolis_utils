#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>
#include "monolis_def_com_c.h"
#include "monolis_mpi_util_c.h"
#include "monolis_alloc_c.h"
#include "monolis_comm_par_util_c.h"

void monolis_comm_get_recv_parallel(
  int          n_vertex,
  int*         vertex_id,
  MONOLIS_COM* com,
  int*         outer_node_id_all,
  int*         outer_domain_id_all,
  int*         displs)
{
  int comm_size;
  int* is_neib;

  comm_size = monolis_mpi_get_local_comm_size(com->comm);

  is_neib = monolis_alloc_I_1d(is_neib, comm_size);

  monolis_comm_get_recv_parallel_n_neib(com->comm, outer_domain_id_all, displs, &com->recv_n_neib, is_neib);

  com->recv_neib_pe = monolis_alloc_I_1d(com->recv_neib_pe, com->recv_n_neib);

  monolis_comm_get_recv_parallel_neib_id(com->comm, is_neib, com->recv_neib_pe);

  com->recv_index = monolis_alloc_I_1d(com->recv_index, com->recv_n_neib + 1);

  monolis_comm_get_recv_parallel_index(com->comm, displs, outer_domain_id_all,
    com->recv_n_neib, com->recv_neib_pe, com->recv_index);

  com->recv_item = monolis_alloc_I_1d(com->recv_item, com->recv_index[com->recv_n_neib + 1]);

  monolis_comm_get_recv_parallel_item(n_vertex, vertex_id, com->comm,
    outer_node_id_all, outer_domain_id_all, displs, com->recv_n_neib, com->recv_neib_pe, com->recv_index, com->recv_item);
}

void monolis_comm_get_send_parallel(
  int          n_vertex,
  int*         vertex_id,
  MONOLIS_COM* com)
{
  int comm_size;
  int* send_n_list;

  comm_size = monolis_mpi_get_local_comm_size(com->comm);

  send_n_list = monolis_alloc_I_1d(send_n_list, comm_size);

  monolis_comm_get_send_parallel_n_list(com->comm, com->recv_n_neib, com->recv_neib_pe, com->recv_index, send_n_list);

  monolis_comm_get_send_parallel_n_neib(com->comm, send_n_list, &com->send_n_neib);

  com->send_neib_pe = monolis_alloc_I_1d(com->send_neib_pe, com->send_n_neib);

  monolis_comm_get_send_parallel_neib_id(com->comm, send_n_list, com->send_neib_pe);

  com->send_index = monolis_alloc_I_1d(com->send_index, com->send_n_neib + 1);

  monolis_comm_get_send_parallel_index(com->comm, send_n_list, com->send_n_neib, com->send_index);

  com->send_item = monolis_alloc_I_1d(com->send_item, com->send_index[com->send_n_neib + 1]);

  monolis_comm_get_send_parallel_item(com->comm, n_vertex, vertex_id,
    com->recv_n_neib, com->recv_neib_pe, com->recv_index, com->recv_item,
    com->send_n_neib, com->send_neib_pe, com->send_index, com->send_item);
}

void monolis_com_get_comm_table_parallel(
  int          n_internal_vertex,
  int          n_vertex,
  int*         vertex_id,
  MONOLIS_COM* com)
{
  int n_outer_node;
  int* outer_node_id_all_global;
  int* outer_domain_id_all;
  int* displs;

  com->my_rank = monolis_mpi_get_local_my_rank(com->comm);

  com->comm_size = monolis_mpi_get_local_comm_size(com->comm);

  com->n_internal_vertex = n_internal_vertex;

  monolis_comm_get_all_external_n_node_parallel(n_internal_vertex, n_vertex, com->comm, &n_outer_node);

  outer_node_id_all_global = monolis_alloc_I_1d(outer_node_id_all_global, n_outer_node);

  displs = monolis_alloc_I_1d(displs, com->comm_size + 1);

  monolis_comm_get_all_external_node_parallel(n_internal_vertex, n_vertex, vertex_id, com->comm, outer_node_id_all_global, displs);

  outer_domain_id_all = monolis_alloc_I_1d(outer_domain_id_all, n_outer_node);

  monolis_comm_get_all_external_node_domain_id_parallel(n_internal_vertex, vertex_id, com->comm, outer_node_id_all_global, outer_domain_id_all, displs);

  monolis_comm_get_recv_parallel(n_vertex, vertex_id, com, outer_node_id_all_global, outer_domain_id_all, displs);

  monolis_comm_get_send_parallel(n_vertex, vertex_id, com);
}