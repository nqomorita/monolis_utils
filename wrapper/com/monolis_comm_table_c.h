/* monolis_comm_table_c.h */
#ifndef MONOLIS_COMM_TABLE_C_H
#define MONOLIS_COMM_TABLE_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
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

void monolis_get_bool_list_of_internal_simple_mesh(
  MONOLIS_COM* com,
  int          n_node,
  int          n_elem,
  int          n_base,
  int**        elem,
  bool*        list);

void monolis_get_bool_list_of_internal_connetivity(
  MONOLIS_COM* com,
  int          n_node,
  int          n_elem,
  int*         index,
  int*         item,
  bool*        list);

#ifdef __cplusplus
}
#endif

#endif
