#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_def_prm_c.h"
#include "monolis_mpi_c.h"
#include "monolis_def_com_c.h"
#include "monolis_alloc_c.h"

void monolis_mpi_update_R(
  MONOLIS_COM* com,
  int          n,
  int          n_dof,
  double*      x)
{
  int recv_nitem = com->recv_index[com->recv_n_neib];
  int send_nitem = com->send_index[com->send_n_neib];

  monolis_mpi_update_R_c_main(
    n,
    n_dof,
    x,
    com->comm,
    com->recv_n_neib,
    recv_nitem,
    com->recv_neib_pe,
    com->recv_index,
    com->recv_item,
    com->send_n_neib,
    send_nitem,
    com->send_neib_pe,
    com->send_index,
    com->send_item);
}

void monolis_mpi_update_I(
  MONOLIS_COM* com,
  int          n,
  int          n_dof,
  int*         x)
{
  int recv_nitem = com->recv_index[com->recv_n_neib];
  int send_nitem = com->send_index[com->send_n_neib];

  monolis_mpi_update_I_c_main(
    n,
    n_dof,
    x,
    com->comm,
    com->recv_n_neib,
    recv_nitem,
    com->recv_neib_pe,
    com->recv_index,
    com->recv_item,
    com->send_n_neib,
    send_nitem,
    com->send_neib_pe,
    com->send_index,
    com->send_item);
}

void monolis_mpi_update_C(
  MONOLIS_COM*     com,
  int              n,
  int              n_dof,
  double _Complex* x)
{
  int recv_nitem = com->recv_index[com->recv_n_neib];
  int send_nitem = com->send_index[com->send_n_neib];

  monolis_mpi_update_C_c_main(
    n,
    n_dof,
    x,
    com->comm,
    com->recv_n_neib,
    recv_nitem,
    com->recv_neib_pe,
    com->recv_index,
    com->recv_item,
    com->send_n_neib,
    send_nitem,
    com->send_neib_pe,
    com->send_index,
    com->send_item);
}

void monolis_mpi_get_n_neib_vector(
  MONOLIS_COM* com,
  int          n_vec,
  int*         n_neib_vec)
{
  int recv_nitem = com->recv_index[com->recv_n_neib];
  int send_nitem = com->send_index[com->send_n_neib];

  monolis_mpi_get_n_neib_vector_c_main(
    n_vec,
    n_neib_vec,
    com->comm,
    com->recv_n_neib,
    com->recv_neib_pe);
}

void monolis_mpi_get_neib_vector_R(
  MONOLIS_COM* com,
  int          np,
  int          n_dof,
  int          n_vec,
  int          n_neib_vec,
  double**     my_vec,
  double**     neib_vec)
{
  int i, j;
  int recv_nitem = com->recv_index[com->recv_n_neib];
  int send_nitem = com->send_index[com->send_n_neib];
  double* my_vec_t;
  double* neib_vec_t;

  my_vec_t   = monolis_alloc_R_1d(my_vec_t,   np*n_dof*n_vec);
  neib_vec_t = monolis_alloc_R_1d(neib_vec_t, np*n_dof*n_neib_vec);

  for(i = 0; i < n_vec; ++i){
    for(j = 0; j < np*n_dof; ++j){
      my_vec_t[np*n_dof*i + j] = my_vec[j][i];
    }
  }

  monolis_mpi_get_neib_vector_R_c_main(
    com->n_internal_vertex,
    np,
    n_dof,
    n_vec,
    n_neib_vec,
    my_vec_t,
    neib_vec_t,
    com->comm,
    com->recv_n_neib,
    recv_nitem,
    com->recv_neib_pe,
    com->recv_index,
    com->recv_item,
    com->send_n_neib,
    send_nitem,
    com->send_neib_pe,
    com->send_index,
    com->send_item);

  for(i = 0; i < n_neib_vec; ++i){
    for(j = 0; j < np*n_dof; ++j){
      neib_vec[j][i] = neib_vec_t[np*n_dof*i + j];
    }
  }

  monolis_dealloc_R_1d(&my_vec_t);
  monolis_dealloc_R_1d(&neib_vec_t);
}
