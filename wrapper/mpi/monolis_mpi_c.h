/* monolis_mpi_c.h */
#ifndef MONOLIS_MPI_C_H
#define MONOLIS_MPI_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include "monolis_def_com_c.h"

static const int MONOLIS_MPI_SUM = 1;

static const int MONOLIS_MPI_MAX = 2;

static const int MONOLIS_MPI_MIN = 3;

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
void monolis_allreduce_I(
  int  n,
  int* val,
  int  tag,
  int  comm);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
void monolis_allreduce_R(
  int     n,
  double* val,
  int     tag,
  int     comm);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
void monolis_allreduce_C(
  int              n,
  double _Complex* val,
  int              tag,
  int              comm);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
void monolis_mpi_update_R(
  MONOLIS_COM* com,
  int          n,
  int          n_dof,
  double*      x);

void monolis_mpi_update_I_c_main(
  int  n,
  int  n_dof,
  int* x,
  int  comm,
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

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
void monolis_mpi_update_I(
  MONOLIS_COM* com,
  int          n,
  int          n_dof,
  int*         x);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
void monolis_mpi_update_R_c_main(
  int     n,
  int     n_dof,
  double* x,
  int     comm,
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

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
void monolis_mpi_update_C(
  MONOLIS_COM*     com,
  int          n,
  int              n_dof,
  double _Complex* x);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
void monolis_mpi_update_C_c_main(
  int              n,
  int              n_dof,
  double _Complex* x,
  int              comm,
  int              recv_n_neib,
  int              recv_nitem,
  int*             recv_neib_pe,
  int*             recv_index,
  int*             recv_item,
  int              send_n_neib,
  int              send_nitem,
  int*             send_neib_pe,
  int*             send_index,
  int*             send_item);

#ifdef __cplusplus
}
#endif

#endif
