/* monolis_std_sort_I.h */
#ifndef MONOLIS_STD_SORT_I_H
#define MONOLIS_STD_SORT_I_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup std_algebra
 */
void monolis_qsort_I_1d(
  int* array,
  int  iS,
  int  iE);

#ifdef __cplusplus
}
#endif

#endif
