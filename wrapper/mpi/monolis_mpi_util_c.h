/* monolis_mpi_util_c.h */
#ifndef MONOLIS_MPI_UTIL_C_H
#define MONOLIS_MPI_UTIL_C_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
void monolis_mpi_initialize();

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
void monolis_mpi_finalize();

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
int monolis_mpi_get_global_comm();

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
int monolis_mpi_get_self_comm();

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
int monolis_mpi_get_global_comm_size();

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
int monolis_mpi_get_global_my_rank();

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
int monolis_mpi_get_local_comm_size(
  int comm);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
int monolis_mpi_get_local_my_rank(
  int comm);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
double monolis_get_time();

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
double monolis_get_time_global_sync();

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
double monolis_get_time_local_sync(
  int comm);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
void monolis_mpi_global_barrier();

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
void monolis_mpi_local_barrier(
  int comm);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup mpi
 */
void monolis_mpi_split_comm(
  int  comm,
  int  group_id,
  int* comm_split);

#ifdef __cplusplus
}
#endif

#endif
