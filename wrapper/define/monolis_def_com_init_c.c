#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_def_prm_c.h"
#include "monolis_def_com_c.h"
#include "monolis_io_c.h"
#include "monolis_io_com_c.h"
#include "monolis_io_file_name_c.h"
#include "monolis_alloc_c.h"
#include "monolis_comm_table_c.h"
#include "monolis_mpi_util_c.h"

void monolis_com_input_comm_table(
  MONOLIS_COM* com,
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name)
{
  if(com->comm_size <= 1){
    com->comm_size = 1;
    com->send_n_neib = 0;
    com->recv_n_neib = 0;
    com->send_neib_pe = monolis_alloc_I_1d(com->send_neib_pe, 1);
    com->send_index = monolis_alloc_I_1d(com->send_index, 2);
    com->send_item = monolis_alloc_I_1d(com->send_item, 1);
    com->recv_neib_pe = monolis_alloc_I_1d(com->recv_neib_pe, 1);
    com->recv_index = monolis_alloc_I_1d(com->recv_index, 2);
    com->recv_item = monolis_alloc_I_1d(com->recv_item, 1);
    return;
  }

  const char* input_file_name;
  char* body;

  body = (char*)malloc(sizeof(char)*MONOLIS_CHARLEN);
  snprintf(body, MONOLIS_CHARLEN, "%s.recv", file_name);
  input_file_name = monolis_get_output_file_name_by_domain_id(top_dir_name, part_dir_name, body, com->my_rank);
  monolis_input_recv_com_table(input_file_name, com);

  snprintf(body, MONOLIS_CHARLEN, "%s.send", file_name);
  input_file_name = monolis_get_output_file_name_by_domain_id(top_dir_name, part_dir_name, body, com->my_rank);
  monolis_input_send_com_table(input_file_name, com);

  snprintf(body, MONOLIS_CHARLEN, "%s.n_internal", file_name);
  input_file_name = monolis_get_output_file_name_by_domain_id(top_dir_name, part_dir_name, body, com->my_rank);
  monolis_input_internal_vertex_number(input_file_name, &com->n_internal_vertex);
}

void monolis_com_initialize_by_parted_files(
  MONOLIS_COM* com,
  int          comm,
  const char*  top_dir_name,
  const char*  part_dir_name,
  const char*  file_name)
{
  com->comm = comm;
  com->comm_size = monolis_mpi_get_local_comm_size(comm);
  com->my_rank = monolis_mpi_get_local_my_rank(comm);
  com->n_internal_vertex = 0;

  com->recv_n_neib = 0;
  com->send_n_neib = 0;

  com->recv_neib_pe = NULL;
  com->recv_index = NULL;
  com->recv_item = NULL;
  com->send_neib_pe = NULL;
  com->send_index = NULL;
  com->send_item = NULL;

  monolis_com_input_comm_table(
    com,
    top_dir_name,
    part_dir_name,
    file_name);
}

void monolis_com_initialize_by_global_id(
  MONOLIS_COM* com,
  int          comm,
  int          n_internal_vertex,
  int          n_vertex,
  int*         global_id)
{
  com->comm = comm;
  com->comm_size = monolis_mpi_get_local_comm_size(comm);
  com->my_rank = monolis_mpi_get_local_my_rank(comm);
  com->n_internal_vertex = 0;

  com->recv_n_neib = 0;
  com->send_n_neib = 0;

  com->recv_neib_pe = NULL;
  com->recv_index = NULL;
  com->recv_item = NULL;
  com->send_neib_pe = NULL;
  com->send_index = NULL;
  com->send_item = NULL;

  monolis_com_get_comm_table_parallel(
    n_internal_vertex,
    n_vertex,
    global_id,
    com);
}

void monolis_com_initialize_by_self(
  MONOLIS_COM* com)
{
  com->comm = 0;
  com->comm_size = 1;
  com->my_rank = 0;
  com->n_internal_vertex = 0;

  com->recv_n_neib = 0;
  com->send_n_neib = 0;

  com->send_neib_pe = monolis_alloc_I_1d(com->send_neib_pe, 1);
  com->send_index = monolis_alloc_I_1d(com->send_index, 2);
  com->send_item = monolis_alloc_I_1d(com->send_item, 1);
  com->recv_neib_pe = monolis_alloc_I_1d(com->recv_neib_pe, 1);
  com->recv_index = monolis_alloc_I_1d(com->recv_index, 2);
  com->recv_item = monolis_alloc_I_1d(com->recv_item, 1);
}
