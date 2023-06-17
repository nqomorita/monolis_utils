/* monolis_def_com_init_c.h */
#ifndef MONOLIS_DEF_COM_INIT_C_H
#define MONOLIS_DEF_COM_INIT_C_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief COM 構造体の初期化関数（事前分割したデータファイルを読込）
 * @param[in,out] com COM 構造体
 * @param[in] comm MPI コミュニケータ
 * @param[in] op_dir_name 通信テーブルデータ読込のトップディレクトリ名
 * @param[in] part_dir_name 通信テーブルデータ読込の分割ファイルが格納されるディレクトリ名
 * @param[in] file_name 通信テーブルデータが記載されたファイル名
 * @ingroup com
 */
void monolis_com_initialize_by_parted_files(
  MONOLIS_COM* com,
  int          comm,
  const char*  top_dir_name,
  const char*  part_dir_name,
  const char*  file_name);

/**
 * @brief COM 構造体の初期化関数（グローバル計算点番号から初期化）
 * @param[in,out] com COM 構造体
 * @param[in] comm MPI コミュニケータ
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] global_id グローバル計算点番号
 * @ingroup com
 */
void monolis_com_initialize_by_global_id(
  MONOLIS_COM* com,
  int          comm,
  int          n_internal_vertex,
  int          n_vertex,
  int*         global_id);

/**
 * @brief COM 構造体の初期化関数（MPI SELF コミュニケータで初期化）
 * @param[in,out] com COM 構造体
 * @ingroup com
 */
void monolis_com_initialize_by_self(
  MONOLIS_COM* com);

#ifdef __cplusplus
}
#endif

#endif
