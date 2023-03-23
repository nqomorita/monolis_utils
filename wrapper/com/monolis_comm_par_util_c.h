/* monolis_comm_par_util_c.h */
#ifndef MONOLIS_COMM_PAR_UTIL_C_H
#define MONOLIS_COMM_PAR_UTIL_C_H

#ifdef __cplusplus
extern "C" {
#endif


void  monolis_comm_get_recv_parallel_n_neib(
  int  comm,
  int  n_vertex,
  int* outer_domain_id_all,
  int  comm_size,
  int* displs,
  int* recv_n_neib,
  int* is_neib);

void monolis_comm_get_recv_parallel_neib_id(
  int  comm,
  int  comm_size,
  int* is_neib,
  int* recv_neib_pe);

void  monolis_comm_get_recv_parallel_index(
  int  comm,
  int  comm_size,
  int* displs,
  int  n_outer_node,
  int* outer_domain_id_all,
  int  recv_n_neib,
  int* recv_neib_pe,
  int* recv_index);

void monolis_comm_get_recv_parallel_item(
  int  n_vertex,
  int* vertex_id,
  int  comm,
  int  comm_size,
  int  n_outer_node,
  int* outer_node_id_all,
  int* outer_domain_id_all,
  int* displs,
  int  recv_n_neib,
  int* recv_neib_pe,
  int* recv_index,
  int  nz,
  int* recv_item);

void monolis_comm_get_send_parallel_n_list(
  int  comm,
  int  comm_size,
  int  recv_n_neib,
  int* recv_neib_pe,
  int* recv_index,
  int* send_n_list);

void monolis_comm_get_send_parallel_n_neib(
  int  comm,
  int  comm_size,
  int* send_n_list,
  int* send_n_neib);

void monolis_comm_get_send_parallel_neib_id(
  int  comm,
  int  comm_size,
  int  n_neib_send,
  int* send_n_list,
  int* send_neib_pe);

void monolis_comm_get_send_parallel_index(
  int  comm,
  int  comm_size,
  int* send_n_list,
  int  send_n_neib,
  int* send_index);

void monolis_comm_get_send_parallel_item(
  int  comm,
  int  n_vertex,
  int* vertex_id,
  int  recv_n_neib,
  int* recv_neib_pe,
  int* recv_index,
  int  recv_nz,
  int* recv_item,
  int  send_n_neib,
  int* send_neib_pe,
  int* send_index,
  int  send_nz,
  int* send_item);

void monolis_comm_get_all_external_n_node_parallel(
  int  n_internal_vertex,
  int  n_vertex,
  int  comm,
  int* n_outer_node);

void monolis_comm_get_all_external_node_parallel(
  int  n_internal_vertex,
  int  n_vertex,
  int* vertex_id,
  int  comm,
  int  n_outer_node,
  int* outer_node_id_all_global,
  int  comm_size,
  int* displs);

void monolis_comm_get_all_external_node_domain_id_parallel(
  int  n_internal_vertex,
  int  n_vertex,
  int* vertex_id,
  int  comm,
  int  n_outer_node,
  int* outer_node_id_all_global,
  int* outer_domain_id_all,
  int  comm_size,
  int* displs);

#ifdef __cplusplus
}
#endif

#endif
