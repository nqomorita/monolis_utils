/* monolis_mpi_util_c.h */
#ifndef MONOLIS_MPI_UTIL_C_H
#define MONOLIS_MPI_UTIL_C_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief monolis ライブラリで利用する MPI の初期化関数
 * @ingroup mpi
 */
void monolis_mpi_initialize();

/**
 * @brief monolis ライブラリを利用する MPI の終了処理関数
 * @ingroup mpi
 */
void monolis_mpi_finalize();

/**
 * @brief MPI のグローバルコミュニケータを取得する関数
 * @ingroup mpi
 */
int monolis_mpi_get_global_comm();

/**
 * @brief MPI のセルフコミュニケータを取得する関数
 * @ingroup mpi
 */
int monolis_mpi_get_self_comm();

/**
 * @brief MPI のグローバルランクサイズを取得する関数
 * @ingroup mpi
 */
int monolis_mpi_get_global_comm_size();

/**
 * @brief MPI のグローバルランクを取得する関数
 * @ingroup mpi
 */
int monolis_mpi_get_global_my_rank();

/**
 * @brief MPI のローカルコミュニケータのランクサイズを取得する関数
 * @param[in] comm MPI コミュニケータ
 * @ingroup mpi
 */
int monolis_mpi_get_local_comm_size(
  int comm);

/**
 * @brief MPI のローカルコミュニケータのランクを取得する関数
 * @param[in] comm MPI コミュニケータ
 * @ingroup mpi
 */
int monolis_mpi_get_local_my_rank(
  int comm);

/**
 * @brief MPI 時間計測関数
 * @ingroup mpi
 */
double monolis_get_time();

/**
 * @brief MPI 時間計測関数（グローバルコミュニケータでの同期）
 * @ingroup mpi
 */
double monolis_get_time_global_sync();

/**
 * @brief MPI 時間計測関数（ローカルコミュニケータ）
 * @param[in] comm MPI コミュニケータ
 * @ingroup mpi
 */
double monolis_get_time_local_sync(
  int comm);

/**
 * @brief MPI バリア関数（グローバルコミュニケータ）
 * @ingroup mpi
 */
void monolis_mpi_global_barrier();

/**
 * @brief MPI バリア関数（ローカルコミュニケータ）
 * @param[in] comm MPI コミュニケータ
 * @ingroup mpi
 */
void monolis_mpi_local_barrier(
  int comm);

/**
 * @brief MPI コミュニケータの分割
 * @param[in] comm 分割前の MPI コミュニケータ
 * @param[in] group_id コミュニケータのグループ id
 * @param[out] comm_split 分割後の MPI コミュニケータ
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
