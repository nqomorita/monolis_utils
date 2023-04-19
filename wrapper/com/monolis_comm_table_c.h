/* monolis_comm_table_c.h */
#ifndef MONOLIS_COMM_TABLE_C_H
#define MONOLIS_COMM_TABLE_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include "monolis_def_com_c.h"

/**
 * @brief 通信テーブルを作成（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in,out] com 分割領域に対応する com 構造体
 * @ingroup com
 */
void monolis_com_get_comm_table_parallel(
  int          n_internal_vertex,
  int          n_vertex,
  int*         vertex_id,
  MONOLIS_COM* com);

#ifdef __cplusplus
}
#endif

#endif
