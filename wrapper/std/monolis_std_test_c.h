/* monolis_std_test.h */
#ifndef MONOLIS_STD_TEST_H
#define MONOLIS_STD_TEST_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <math.h>
#include "monolis_def_prm_c.h"

/**
 * @brief 通常ログ出力関数
 * @param[in] array 出力ログ
 * @ingroup dev_test
 */
void monolis_std_log_string(
  const char* array);

/**
 * @brief 通常ログ出力関数（MPI ランク 0 のみ出力）
 * @param[in] array 出力ログ
 * @ingroup dev_test
 */
void monolis_std_global_log_string(
  const char* array);

/**
 * @brief テストパス時の標準出力
 * @param[in] array テスト内容を示す文字列
 * @ingroup dev_test
 */
void monolis_test_assert_pass(
  const char* array);

/**
 * @brief テストエラー時の標準出力・エラーストップ
 * @param[in] array1 テスト内容を示す文字列
 * @param[in] array2 エラー内容を示す文字列
 * @ingroup dev_test
 */
void monolis_test_assert_fail(
  const char* array1,
  const char* array2);

/**
 * @brief 整数値（スカラ）の比較
 * @param[in] array テスト内容を示す文字列
 * @param[in] a 入力配列 a
 * @param[in] b 入力配列 b
 * @ingroup dev_test
 */
void monolis_test_check_eq_I1(
  const char* array,
  int         a,
  int         b);

/**
 * @brief 実数値（スカラ）の比較
 * @param[in] array テスト内容を示す文字列
 * @param[in] a 入力配列 a
 * @param[in] b 入力配列 b
 * @ingroup dev_test
 */
void monolis_test_check_eq_R1(
  const char* array,
  double      a,
  double      b);

/**
 * @brief 複素数値（スカラ）の比較
 * @param[in] array テスト内容を示す文字列
 * @param[in] a 入力配列 a
 * @param[in] b 入力配列 b
 * @ingroup dev_test
 */
void monolis_test_check_eq_C1(
  const char*     array,
  double _Complex a,
  double _Complex b);

/**
 * @brief 実数配列の比較
 * @param[in] array テスト内容を示す文字列
 * @param[in] size_a 入力配列 a のサイズ
 * @param[in] a 入力配列 a
 * @param[in] size_a 入力配列 b のサイズ
 * @param[in] b 入力配列 b
 * @ingroup dev_test
 */
void monolis_test_check_eq_R(
  const char* header,
  const int size_a,
  const double* a,
  const int size_b,
  const double* b);

/**
 * @brief 整数配列の比較
 * @param[in] array テスト内容を示す文字列
 * @param[in] size_a 入力配列 a のサイズ
 * @param[in] a 入力配列 a
 * @param[in] size_a 入力配列 b のサイズ
 * @param[in] b 入力配列 b
 * @ingroup dev_test
 */
void monolis_test_check_eq_I(
  const char* header,
  const int size_a,
  const int* a,
  const int size_b,
  const int* b);

/**
 * @brief 複素数配列の比較
 * @param[in] array テスト内容を示す文字列
 * @param[in] size_a 入力配列 a のサイズ
 * @param[in] a 入力配列 a
 * @param[in] size_a 入力配列 b のサイズ
 * @param[in] b 入力配列 b
 * @ingroup dev_test
 */
void monolis_test_check_eq_C(
  const char* header,
  const int size_a,
  const double _Complex* a,
  const int size_b,
  const double _Complex* b);

#ifdef __cplusplus
}
#endif

#endif
