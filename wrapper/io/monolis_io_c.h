/* monolis_io.h */
#ifndef MONOLIS_IO_H
#define MONOLIS_IO_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup io
 */
void monolis_input_internal_vertex_number(
  const char* fname,
  int*        n_internal);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
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
