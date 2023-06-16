/* monolis_io_file_name.h */
#ifndef MONOLIS_IO_FILE_NAME_H
#define MONOLIS_IO_FILE_NAME_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief 並列計算用読み込みファイル名の取得（グローバルコミュニケータから指定）
 * @param[in] top_dir_name トップディレクトリパス
 * @param[in] part_dir_name 入力ディレクトリ名
 * @param[in] file_name 入力ファイル名
 * @ingroup io
 */
const char* monolis_get_global_input_file_name(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name);

/**
 * @brief 並列計算用読み込みファイル名の取得（ローカルコミュニケータから指定）
 * @param[in] top_dir_name トップディレクトリパス
 * @param[in] part_dir_name 入力ディレクトリ名
 * @param[in] file_name 入力ファイル名
 * @param[in] comm MPI コミュニケータ
 * @ingroup io
 */
const char* monolis_get_local_input_file_name(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name,
  int         comm);

/**
 * @brief 並列計算用書き出しファイル名の取得（グローバルコミュニケータから指定）
 * @param[in] top_dir_name トップディレクトリパス
 * @param[in] part_dir_name 入力ディレクトリ名
 * @param[in] file_name 入力ファイル名
 * @ingroup io
 */
const char* monolis_get_global_output_file_name(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name);

/**
 * @brief 並列計算用書き出しファイル名の取得（ローカルコミュニケータから指定）
 * @param[in] top_dir_name トップディレクトリパス
 * @param[in] part_dir_name 入力ディレクトリ名
 * @param[in] file_name 入力ファイル名
 * @param[in] comm MPI コミュニケータ
 * @ingroup io
 */
const char* monolis_get_local_output_file_name(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name,
  int         comm);

/**
 * @brief 並列計算用書き出しファイル名の取得（領域番号から指定）
 * @param[in] top_dir_name トップディレクトリパス
 * @param[in] part_dir_name 入力ディレクトリ名
 * @param[in] file_name 入力ファイル名
 * @param[in] domain_id 領域番号
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
