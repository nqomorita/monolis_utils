#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>
#include "monolis_def_com_c.h"
#include "monolis_alloc_c.h"
#include "monolis_mpi_util_c.h"

/** COM 構造体の初期化関数 */
void monolis_com_initialize(
  MONOLIS_COM* com)
{
    com->recv_n_neib = 0;
    com->send_n_neib = 0;
    com->n_internal_vertex = 0;
    com->my_rank = monolis_mpi_get_global_my_rank();
    com->comm = monolis_mpi_get_global_comm();
    com->comm_size = monolis_mpi_get_global_comm_size();

    com->recv_neib_pe = NULL;
    com->recv_index = NULL;
    com->recv_item = NULL;
    com->send_neib_pe = NULL;
    com->send_index = NULL;
    com->send_item = NULL;
}

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
