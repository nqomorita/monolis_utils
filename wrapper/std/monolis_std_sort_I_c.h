/* monolis_std_sort_I.h */
#ifndef MONOLIS_STD_SORT_I_H
#define MONOLIS_STD_SORT_I_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief クイックソート（1次元整数配列）
 * @param[in,out] array 整数配列
 * @param[in] n 配列サイズ
 * @param[in] iS ソートする開始位置
 * @param[in] iE ソートする終了位置
 * @ingroup std
 */
void monolis_qsort_I_1d(
  int* array,
  int  n,
  int  iS,
  int  iE);

/**
 * @brief クイックソート（2次元整数配列）
 * @param[in,out] array1 ソートされる整数配列
 * @param[in,out] array2 ソートに従属する整数配列
 * @param[in] n 配列サイズ
 * @param[in] iS ソートする開始位置
 * @param[in] iE ソートする終了位置
 * @ingroup std
 */
void monolis_qsort_I_2d(
  int* array1,
  int* array2,
  int  n,
  int  iS,
  int  iE);

/**
 * @brief 整数配列の二分探索
 * @param[in,out] array 整数配列
 * @param[in] n 配列サイズ
 * @param[in] iS ソートする開始位置
 * @param[in] iE ソートする終了位置
 * @param[in] val 検索する値
 * @param[out] idx 検索した結果の位置（検索結果がない場合 -1 を返す）
 * @ingroup std
 */
void monolis_bsearch_I(
  int* array,
  int  n,
  int  iS,
  int  iE,
  int  val,
  int* idx);

/**
 * @brief 整数の等差数列を生成
 * @param[out] array 整数配列
 * @param[in] n 整数配列のサイズ
 * @param[in] origin 初項
 * @param[in] diff 交差
 * @ingroup std
 */
void monolis_get_sequence_array_I(
  int* array,
  int  n,
  int  origin,
  int  diff);

/**
 * @brief 整数配列の重複要素の削除
 * @param[in,out] array 整数配列
 * @param[in] len 整数配列のサイズ
 * @param[out] newlen 重複を省いた配列サイズ
 * @ingroup std
 */
void monolis_get_uniq_array_I(
  int* array,
  int  len,
  int* newlen);

#ifdef __cplusplus
}
#endif

#endif
