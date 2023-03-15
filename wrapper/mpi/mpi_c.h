/* alloc_c.h */
#ifndef MONOLIS_ALLOC_C_H
#define MONOLIS_ALLOC_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../define/def_com_c.h"

void monolis_allreduce_I_c(
  int  n,
  int* val,
  int  tag,
  int  comm);

void monolis_allreduce_R_c(
  int     n,
  double* val,
  int     tag,
  int     comm);

void monolis_allreduce_C_c(
  int             n,
  complex double* val,
  int             tag,
  int             comm);

void monolis_mpi_update_R(
  MONOLIS_COM* com,
  int          n,
  int          n_dof,
  double*      x);

void monolis_mpi_update_I_c_main(
  int  n,
  int  n_dof,
  int* x,
  int  myrank,
  int  comm,
  int  commsize,
  int  recv_n_neib,
  int  recv_nitem,
  int* recv_neib_pe,
  int* recv_index,
  int* recv_item,
  int  send_n_neib,
  int  send_nitem,
  int* send_neib_pe,
  int* send_index,
  int* send_item);

void monolis_mpi_update_I(
  MONOLIS_COM* com,
  int          n,
  int          n_dof,
  int*         x);

void monolis_mpi_update_R_c_main(
  int     n,
  int     n_dof,
  double* x,
  int     myrank,
  int     comm,
  int     commsize,
  int     recv_n_neib,
  int     recv_nitem,
  int*    recv_neib_pe,
  int*    recv_index,
  int*    recv_item,
  int     send_n_neib,
  int     send_nitem,
  int*    send_neib_pe,
  int*    send_index,
  int*    send_item);

void monolis_mpi_update_C(
  MONOLIS_COM*    com,
  int             n,
  int             n_dof,
  double complex* x);

void monolis_mpi_update_C_c_main(
  int             n,
  int             n_dof,
  double complex* x,
  int             myrank,
  int             comm,
  int             commsize,
  int             recv_n_neib,
  int             recv_nitem,
  int*            recv_neib_pe,
  int*            recv_index,
  int*            recv_item,
  int             send_n_neib,
  int             send_nitem,
  int*            send_neib_pe,
  int*            send_index,
  int*            send_item);

#endif
