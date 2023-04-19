/* monolis_comm_par_util_c.h */
#ifndef MONOLIS_COMM_PAR_UTIL_C_H
#define MONOLIS_COMM_PAR_UTIL_C_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] com 分割領域に対応する com 構造体
 * @ingroup com
 */
void  monolis_comm_get_recv_parallel_n_neib(
  int  comm,
  int  n_vertex,
  int* outer_domain_id_all,
  int  comm_size,
  int* displs,
  int* recv_n_neib,
  int* is_neib);

/**
 * @brief 通信テーブルを作成（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] com 分割領域に対応する com 構造体
 * @ingroup com
 */
void monolis_comm_get_recv_parallel_neib_id(
  int  comm,
  int  comm_size,
  int* is_neib,
  int* recv_neib_pe);

/**
 * @brief 通信テーブルを作成（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] com 分割領域に対応する com 構造体
 * @ingroup com
 */
void  monolis_comm_get_recv_parallel_index(
  int  comm,
  int  comm_size,
  int* displs,
  int  n_outer_node,
  int* outer_domain_id_all,
  int  recv_n_neib,
  int* recv_neib_pe,
  int* recv_index);

/**
 * @brief 通信テーブルを作成（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] com 分割領域に対応する com 構造体
 * @ingroup com
 */
void monolis_comm_get_recv_parallel_item(
  int  n_vertex,
  int* vertex_id,
  int  comm,
  int  comm_size,
  int  n_outer_node,
  int* outer_node_id_all,
  int* outer_domain_id_all,
  int* displs,
  int  recv_n_neib,
  int* recv_neib_pe,
  int* recv_index,
  int  nz,
  int* recv_item);

/**
 * @brief 通信テーブルを作成（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] com 分割領域に対応する com 構造体
 * @ingroup com
 */
void monolis_comm_get_send_parallel_n_list(
  int  comm,
  int  comm_size,
  int  recv_n_neib,
  int* recv_neib_pe,
  int* recv_index,
  int* send_n_list);

/**
 * @brief 通信テーブルを作成（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] com 分割領域に対応する com 構造体
 * @ingroup com
 */
void monolis_comm_get_send_parallel_n_neib(
  int  comm,
  int  comm_size,
  int* send_n_list,
  int* send_n_neib);

/**
 * @brief 通信テーブルを作成（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] com 分割領域に対応する com 構造体
 * @ingroup com
 */
void monolis_comm_get_send_parallel_neib_id(
  int  comm,
  int  comm_size,
  int  n_neib_send,
  int* send_n_list,
  int* send_neib_pe);

/**
 * @brief 通信テーブルを作成（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] com 分割領域に対応する com 構造体
 * @ingroup com
 */
void monolis_comm_get_send_parallel_index(
  int  comm,
  int  comm_size,
  int* send_n_list,
  int  send_n_neib,
  int* send_index);

/**
 * @brief 通信テーブルを作成（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] com 分割領域に対応する com 構造体
 * @ingroup com
 */
void monolis_comm_get_send_parallel_item(
  int  comm,
  int  n_vertex,
  int* vertex_id,
  int  recv_n_neib,
  int* recv_neib_pe,
  int* recv_index,
  int  recv_nz,
  int* recv_item,
  int  send_n_neib,
  int* send_neib_pe,
  int* send_index,
  int  send_nz,
  int* send_item);

/**
 * @brief 通信テーブルを作成（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] com 分割領域に対応する com 構造体
 * @ingroup com
 */
void monolis_comm_get_all_external_n_node_parallel(
  int  n_internal_vertex,
  int  n_vertex,
  int  comm,
  int* n_outer_node);

/**
 * @brief 通信テーブルを作成（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] com 分割領域に対応する com 構造体
 * @ingroup com
 */
void monolis_comm_get_all_external_node_parallel(
  int  n_internal_vertex,
  int  n_vertex,
  int* vertex_id,
  int  comm,
  int  n_outer_node,
  int* outer_node_id_all_global,
  int  comm_size,
  int* displs);

/**
 * @brief 通信テーブルを作成（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] com 分割領域に対応する com 構造体
 * @ingroup com
 */
void monolis_comm_get_all_external_node_domain_id_parallel(
  int  n_internal_vertex,
  int  n_vertex,
  int* vertex_id,
  int  comm,
  int  n_outer_node,
  int* outer_node_id_all_global,
  int* outer_domain_id_all,
  int  comm_size,
  int* displs);

#ifdef __cplusplus
}
#endif

#endif
