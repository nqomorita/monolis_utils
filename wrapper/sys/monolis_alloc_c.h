/* alloc_c.h */
#ifndef MONOLIS_ALLOC_C_H
#define MONOLIS_ALLOC_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

/**
 * @brief 1 次元整数配列のメモリ確保
 * @details 初期値 0 でメモリ確保がなされる。
 * @param[inout] var メモリ確保する配列
 * @param[in] size 配列サイズ
 * @ingroup alloc
 */
int* monolis_alloc_I_1d(
  int*      var,
  const int size);

/**
 * @brief 1 次元整数配列のメモリ開放
 * @param[inout] var メモリ開放する配列
 * @ingroup alloc
 */
void monolis_dealloc_I_1d(
  int**     var);

/**
 * @brief 1 次元整数配列のメモリ再確保
 * @details 再確保で増えた配列部分は初期値 0 でメモリ確保がなされる。
 * @param[inout] var メモリ確保する配列
 * @param[in] size_old 元の配列サイズ
 * @param[in] size_new 再確保する配列サイズ
 * @ingroup alloc
 */
int* monolis_realloc_I_1d(
  int*      var,
  const int size_old,
  const int size_new);

/**
 * @brief 1 次元整数配列の末尾にデータ配列を追加
 * @param[inout] var データ追加する配列
 * @param[in] size_old 元の配列サイズ
 * @param[in] size_add 追加する配列サイズ
 * @param[in] var_add 追加する配列
 * @ingroup alloc
 */
int* monolis_append_I_1d(
  int* var,
  int  size_old,
  int  size_add,
  int* var_add);

/**
 * @brief 2 次元整数配列のメモリ確保
 * @details 配列サイズは var(size1, size2) として確保される。
 * @param[inout] var メモリ確保する配列
 * @param[in] size1 配列サイズ
 * @param[in] size2 配列サイズ
 * @ingroup alloc
 */
int** monolis_alloc_I_2d(
  int**     var,
  const int size1,
  const int size2);

/**
 * @brief 2 次元整数配列のメモリ開放
 * @param[inout] var メモリ開放する配列
 * @param[in] size1 配列サイズ
 * @param[in] size2 配列サイズ
 * @ingroup alloc
 */
void monolis_dealloc_I_2d(
  int***    var,
  const int size1,
  const int size2);

/**
 * @brief 1 次元浮動小数点配列のメモリ確保
 * @param[inout] var メモリ確保する配列
 * @param[in] size 配列サイズ
 * @ingroup alloc
 */
double* monolis_alloc_R_1d(
  double*   var,
  const int size);

/**
 * @brief 1 次元浮動小数点配列のメモリ開放
 * @param[inout] var メモリ開放する配列
 * @ingroup alloc
 */
void monolis_dealloc_R_1d(
  double**  var);

/**
 * @brief 2 次元浮動小数点配列のメモリ確保
 * @details 配列サイズは var(size1, size2) として確保される。
 * @param[inout] var メモリ確保する配列
 * @param[in] size1 配列サイズ
 * @param[in] size2 配列サイズ
 * @ingroup alloc
 */
double** monolis_alloc_R_2d(
  double**  var,
  const int size1,
  const int size2);

/**
 * @brief 2 次元浮動小数点配列のメモリ開放
 * @param[inout] var メモリ開放する配列
 * @param[in] size1 配列サイズ
 * @param[in] size2 配列サイズ
 * @ingroup alloc
 */
void monolis_dealloc_R_2d(
  double*** var,
  const int size1,
  const int size2);

/**
 * @brief 1 次元複素数型配列のメモリ確保
 * @param[inout] var メモリ確保する配列
 * @param[in] size 配列サイズ
 * @ingroup alloc
 */
double _Complex* monolis_alloc_C_1d(
  double _Complex* var,
  const int        size);

/**
 * @brief 1 次元浮動小数点配列のメモリ開放
 * @param[inout] var メモリ開放する配列
 * @ingroup alloc
 */
void monolis_dealloc_C_1d(
  double _Complex** var);

/**
 * @brief 2 次元浮動小数点配列のメモリ確保
 * @details 配列サイズは var(size1, size2) として確保される。
 * @param[inout] var メモリ確保する配列
 * @param[in] size1 配列サイズ
 * @param[in] size2 配列サイズ
 * @ingroup alloc
 */
double _Complex** monolis_alloc_C_2d(
  double _Complex** var,
  const int         size1,
  const int         size2);

/**
 * @brief 2 次元浮動小数点配列のメモリ開放
 * @param[inout] var メモリ開放する配列
 * @param[in] size1 配列サイズ
 * @param[in] size2 配列サイズ
 * @ingroup alloc
 */
void monolis_dealloc_C_2d(
  double _Complex*** var,
  const int          size1,
  const int          size2);

/**
 * @brief 1 次元論理型配列のメモリ確保
 * @details 初期値 0 でメモリ確保がなされる。
 * @param[inout] var メモリ確保する配列
 * @param[in] size 配列サイズ
 * @ingroup alloc
 */
bool* monolis_alloc_L_1d(
  bool*     var,
  const int size);

/**
 * @brief 1 次元論理型配列のメモリ開放
 * @param[inout] var メモリ開放する配列
 * @ingroup alloc
 */
void monolis_dealloc_L_1d(
  bool**    var);

#ifdef __cplusplus
}
#endif

#endif
