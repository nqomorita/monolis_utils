/* monolis_mpi_util_c.h */
#ifndef MONOLIS_MPI_UTIL_C_H
#define MONOLIS_MPI_UTIL_C_H

#ifdef __cplusplus
extern "C" {
#endif

/** monolis ライブラリで利用する MPI の初期化関数 */
void monolis_mpi_initialize();

/** monolis ライブラリで利用する MPI の終了処理関数 */
void monolis_mpi_finalize();

/** MPI のグローバルコミュニケータを取得する関数 */
int monolis_mpi_global_comm();

/** MPI のグローバルランクサイズを取得する関数 */
int monolis_mpi_global_comm_size();

/** MPI のグローバルランクを取得する関数 */
int monolis_mpi_global_my_rank();

/** MPI のローカルコミュニケータのランクサイズを取得する関数 */
int monolis_mpi_local_comm_size(
  int comm);

/** MPI のローカルコミュニケータのランクサイズを取得する関数 */
int monolis_mpi_local_my_rank(
  int comm);

/** MPI 時間計測関数 */
double monolis_get_time();

/** MPI 時間計測関数（グローバルコミュニケータでの同期） */
double monolis_get_time_global_sync();

/** MPI 時間計測関数（ローカルコミュニケータでの同期） */
double monolis_get_time_local_sync(
  int comm);

/** MPI バリア関数（グローバルコミュニケータ） */
void monolis_mpi_global_barrier();

/** MPI バリア関数（ローカルコミュニケータ） */
void monolis_mpi_local_barrier(
  int comm);

/** MPI コミュニケータの分割 */
void monolis_mpi_split_comm(
  int comm,
  int group_id,
  int comm_split);

#ifdef __cplusplus
}
#endif

#endif
