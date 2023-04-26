/* monolis_def_com_init_c.h */
#ifndef MONOLIS_DEF_COM_INIT_C_H
#define MONOLIS_DEF_COM_INIT_C_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_initialize_by_parted_files(
  MONOLIS_COM* com,
  int          comm,
  const char   top_dir_name,
  const char   part_dir_name,
  const char   file_name);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_initialize_by_global_id(
  MONOLIS_COM* com,
  int          comm,
  int          n_internal_vertex,
  int          n_vertex,
  int*         global_id);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_initialize_by_self(
  MONOLIS_COM* com);

#ifdef __cplusplus
}
#endif

#endif
