/* monolis_std_sort_I.h */
#ifndef MONOLIS_STD_SORT_I_H
#define MONOLIS_STD_SORT_I_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief クイックソート（1次元整数配列）
 * @param[in,out] array 整数配列
 * @param[in] iS ソートする開始位置
 * @param[in] iE ソートする終了位置
 * @ingroup std
 */
void monolis_qsort_I_1d(
  int* array,
  int  iS,
  int  iE);

#ifdef __cplusplus
}
#endif

#endif
