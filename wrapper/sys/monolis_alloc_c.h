/* alloc_c.h */
#ifndef MONOLIS_ALLOC_C_H
#define MONOLIS_ALLOC_C_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup alloc
 */
int* monolis_alloc_I_1d(
  int*      var,
  const int size);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup alloc
 */
void monolis_dealloc_I_1d(
  int**     var);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup alloc
 */
int** monolis_alloc_I_2d(
  int**     var,
  const int size1,
  const int size2);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup alloc
 */
void monolis_dealloc_I_2d(
  int***    var,
  const int size1,
  const int size2);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup alloc
 */
double* monolis_alloc_R_1d(
  double*   var,
  const int size);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup alloc
 */
void monolis_dealloc_R_1d(
  double**  var);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup alloc
 */
double** monolis_alloc_R_2d(
  double**  var,
  const int size1,
  const int size2);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup alloc
 */
void monolis_dealloc_R_2d(
  double*** var,
  const int size1,
  const int size2);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup alloc
 */
double _Complex* monolis_alloc_C_1d(
  double _Complex* var,
  const int        size);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup alloc
 */
void monolis_dealloc_C_1d(
  double _Complex** var);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup alloc
 */
double _Complex** monolis_alloc_C_2d(
  double _Complex** var,
  const int         size1,
  const int         size2);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
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
