/* monolis_io_file_name.h */
#ifndef MONOLIS_IO_FILE_NAME_H
#define MONOLIS_IO_FILE_NAME_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup io
 */
const char* monolis_get_global_input_file_name(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup io
 */
const char* monolis_get_local_input_file_name(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name,
  int         comm);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup io
 */
const char* monolis_get_global_output_file_name(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup io
 */
const char* monolis_get_local_output_file_name(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name,
  int         comm);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup io
 */
const char* monolis_get_output_file_name_by_domain_id(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name,
  int         domain_id);

#ifdef __cplusplus
}
#endif

#endif
