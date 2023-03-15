/* monolis_struct.h */
#ifndef MONOLIS_IO_FILE_NAME_H
#define MONOLIS_IO_FILE_NAME_H

#ifdef __cplusplus
extern "C" {
#endif

/** 並列計算用読み込みファイル名の取得（グローバルコミュニケータから指定） */
const char* monolis_get_global_input_file_name(
  /**  [in] 入力ディレクトリ名 */
  const char* dir_name,
  /**  [in] 入力ファイル名 */
  const char* file_name);

/** 並列計算用読み込みファイル名の取得（ローカルコミュニケータから指定） */
const char* monolis_get_local_input_file_name(
  /**  [in] 入力ディレクトリ名 */
  const char* dir_name,
  /**  [in] 入力ファイル名 */
  const char* file_name,
  /**  [in] MPI コミュニケータ */
  int         comm);

/** 並列計算用書き出しファイル名の取得（グローバルコミュニケータから指定） */
const char* monolis_get_global_output_file_name(
  /**  [in] 出力ディレクトリ名 */
  const char* dir_name,
  /**  [in] 出力ディレクトリ名 */
  const char* file_name);

/** 並列計算用書き出しファイル名の取得（ローカルコミュニケータから指定） */
const char* monolis_get_local_output_file_name(
  /**  [in] 出力ディレクトリ名 */
  const char* dir_name,
  /**  [in] 出力ディレクトリ名 */
  const char* file_name,
  /**  [in] MPI コミュニケータ */
  int         comm);

#ifdef __cplusplus
}
#endif

#endif
