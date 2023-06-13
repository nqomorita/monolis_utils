/* monolis_io.h */
#ifndef MONOLIS_IO_H
#define MONOLIS_IO_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] fname 出力ファイル名
 * @param[out] n_internal 内部自由度の数
 * @ingroup io
 */
void monolis_input_internal_vertex_number(
  const char* fname,
  int*        n_internal);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] fname 出力ファイル名
 * @param[in] n_vertex 分割領域における全計算点数
 * @param[out] vertex_id 計算点 id
 * @ingroup io
 */
void monolis_input_global_id(
  const char* fname,
  int*        n_vertex,
  int**       vertex_id);

#ifdef __cplusplus
}
#endif

#endif
