#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_mpi_c.h"
#include "monolis_def_com_c.h"

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