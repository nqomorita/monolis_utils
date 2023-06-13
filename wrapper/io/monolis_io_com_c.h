/* monolis_io_com.h */
#ifndef MONOLIS_IO_COM_H
#define MONOLIS_IO_COM_H

#ifdef __cplusplus
extern "C" {
#endif

#include "monolis_def_com_c.h"

/**
 * @brief monolis 通信テーブルの入力（汎用関数）
 * @param[in] fname 入力ファイル名
 * @param[out] n_neib 隣接領域数
 * @param[out] neib_pe 隣接領域 id
 * @param[out] index 通信テーブルの index 配列
 * @param[out] item 通信テーブルの item 配列
 * @ingroup dev_io
 */
void monolis_input_com_table_main(
  const char* fname,
  int*        n_neib,
  int*        neib_pe,
  int*        index,
  int*        item);

/**
 * @brief monolis 通信テーブル send の入力
 * @param[in] fname 入力ファイル名
 * @param[in,out] com 分割領域に対応する COM 構造体
 * @ingroup io
 */
void monolis_input_send_com_table(
  const char*  fname,
  MONOLIS_COM* com);

/**
 * @brief monolis 通信テーブル recv の入力
 * @param[in] fname 入力ファイル名
 * @param[in,out] com 分割領域に対応する COM 構造体
 * @ingroup io
 */
void monolis_input_recv_com_table(
  const char*  fname,
  MONOLIS_COM* com);

#ifdef __cplusplus
}
#endif

#endif
