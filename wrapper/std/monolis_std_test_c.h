/* monolis_std_test.h */
#ifndef MONOLIS_STD_TEST_H
#define MONOLIS_STD_TEST_H

#ifdef __cplusplus
extern "C" {
#endif

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

#ifdef __cplusplus
}
#endif

#endif
