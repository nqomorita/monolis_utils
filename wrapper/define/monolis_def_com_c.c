#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_def_com_c.h"
#include "monolis_alloc_c.h"
#include "monolis_mpi_util_c.h"

/** COM 構造体の終了処理関数 */
void monolis_com_finalize(
  MONOLIS_COM* com)
{
  com->recv_n_neib = 0;
  com->send_n_neib = 0;
  com->n_internal_vertex = 0;
  com->my_rank = 0;
  com->comm = 0;
  com->comm_size = 0;

  monolis_dealloc_I_1d(&com->recv_neib_pe);
  monolis_dealloc_I_1d(&com->recv_index);
  monolis_dealloc_I_1d(&com->recv_item);
  monolis_dealloc_I_1d(&com->send_neib_pe);
  monolis_dealloc_I_1d(&com->send_index);
  monolis_dealloc_I_1d(&com->send_item);
}

/** COM 構造体の終了処理関数 */
void monolis_com_copy(
  MONOLIS_COM* in,
  MONOLIS_COM* out)
{
  int i;
  int nz;

  out->n_internal_vertex = in->n_internal_vertex;
  out->my_rank = in->my_rank;
  out->comm = in->comm;
  out->comm_size = in->comm_size;

  if(in->recv_n_neib > 0){
    out->recv_n_neib = in->recv_n_neib;
    nz = in->recv_index[in->recv_n_neib];

    out->recv_neib_pe = monolis_alloc_I_1d(out->recv_neib_pe, in->recv_n_neib);
    out->recv_index = monolis_alloc_I_1d(out->recv_index, in->recv_n_neib + 1);
    out->recv_item = monolis_alloc_I_1d(out->recv_item, nz);

    for (i = 0; i < in->recv_n_neib; ++i) {
      out->recv_neib_pe[i] = in->recv_neib_pe[i];
    }

    for (i = 0; i < in->recv_n_neib + 1; ++i) {
      out->recv_index[i] = in->recv_index[i];
    }

    for (i = 0; i < nz; ++i) {
      out->recv_item[i] = in->recv_item[i];
    }
  }

  if(in->send_n_neib > 0){
    out->send_n_neib = in->send_n_neib;
    nz = in->send_index[in->send_n_neib];

    out->send_neib_pe = monolis_alloc_I_1d(out->send_neib_pe, in->send_n_neib);
    out->send_index = monolis_alloc_I_1d(out->send_index, in->send_n_neib + 1);
    out->send_item = monolis_alloc_I_1d(out->send_item, nz);

    for (i = 0; i < in->send_n_neib; ++i) {
      out->send_neib_pe[i] = in->send_neib_pe[i];
    }

    for (i = 0; i < in->send_n_neib + 1; ++i) {
      out->send_index[i] = in->send_index[i];
    }

    for (i = 0; i < nz; ++i) {
      out->send_item[i] = in->send_item[i];
    }
  }
}

/** COM 構造体に MPI コミュニケータを設定 */
void monolis_com_set_communicator(
  MONOLIS_COM* com,
  int          comm)
{
  com->comm = comm;
}

/** COM 構造体から MPI コミュニケータを取得 */
void monolis_com_get_communicator(
  MONOLIS_COM* com,
  int*         comm)
{
  *comm = com->comm;
}

/** COM 構造体に MPI ランク番号を設定 */
void monolis_com_set_my_rank(
  MONOLIS_COM* com,
  int          my_rank)
{
  com->my_rank = my_rank;
}

/** COM 構造体に MPI ランク番号を取得 */
void monolis_com_get_my_rank(
  MONOLIS_COM* com,
  int*         my_rank)
{
  *my_rank = com->my_rank;
}

/** COM 構造体に MPI コミュニケータサイズを設定 */
void monolis_com_set_comm_size(
  MONOLIS_COM* com,
  int          comm_size)
{
  com->comm_size = comm_size;
}

/** COM 構造体に MPI コミュニケータサイズを取得 */
void monolis_com_get_comm_size(
  MONOLIS_COM* com,
  int*         comm_size)
{
  *comm_size = com->comm_size;
}

/** COM 構造体から内部領域に属する自由度数を設定 */
void monolis_com_set_n_internal_vertex(
  MONOLIS_COM* com,
  int          n_internal_vertex)
{
  com->n_internal_vertex = n_internal_vertex;
}

/** COM 構造体から内部領域に属する自由度数を取得 */
void monolis_com_get_n_internal_vertex(
  MONOLIS_COM* com,
  int*         n_internal_vertex)
{
  *n_internal_vertex = com->n_internal_vertex;
}

