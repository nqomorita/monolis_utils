/* monolis_std_test.h */
#ifndef MONOLIS_STD_TEST_H
#define MONOLIS_STD_TEST_H

#ifdef __cplusplus
extern "C" {
#endif

#include <complex.h>

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_std_log_string(
  const char* array);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_std_global_log_string(
  const char* array);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_test_assert_pass(
  const char* array);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_test_assert_fail(
  const char* array1,
  const char* array2);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_test_check_eq_I1(
  const char* array,
  int         a,
  int         b);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_test_check_eq_R1(
  const char* array,
  double      a,
  double      b);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_test_check_eq_C1(
  const char*     array,
  double _Complex a,
  double _Complex b);

#ifdef __cplusplus
}
#endif

#endif
