/* alloc_c.h */
#ifndef MONOLIS_ALLOC_C_H
#define MONOLIS_ALLOC_C_H

#ifdef __cplusplus
extern "C" {
#endif

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

#ifdef __cplusplus
}
#endif

#endif
