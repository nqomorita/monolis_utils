/* monolis_comm_par_util_c.h */
#ifndef MONOLIS_COMM_PAR_UTIL_C_H
#define MONOLIS_COMM_PAR_UTIL_C_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] comm MPI コミュニケータ
 * @param[in] n_vertex 全計算点数
 * @param[in] outer_domain_id_all 全ての外部計算点が属する領域番号
 * @param[in] comm_size コミュニケータサイズ
 * @param[in] displs 全ての外部計算点配列の各領域に属する計算点数
 * @param[out] recv_n_neib 隣接する領域数
 * @param[out] is_neib 隣接する領域フラグ（サイズ：[comm_size]）
 * @ingroup dev_com
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
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] comm MPI コミュニケータ
 * @param[in] comm_size コミュニケータサイズ
 * @param[in] is_neib 隣接する領域フラグ（サイズ：[comm_size]）
 * @param[out] recv_neib_pe !
 * @ingroup dev_com
 */
void monolis_comm_get_recv_parallel_neib_id(
  int  comm,
  int  comm_size,
  int* is_neib,
  int* recv_neib_pe);

/**
 * @brief データ通信する recv 隣接領域の index 配列取得（並列実行版）
 * @param[in] comm MPI コミュニケータ
 * @param[in] comm_size MPI コミュニケータに属する領域数
 * @param[in] displs 全ての外部計算点配列の各領域に属する計算点数
 * @param[in] n_outer_node !
 * @param[in] outer_domain_id_all 全ての外部計算点が属する領域番号
 * @param[in] recv_n_neib 隣接する領域数
 * @param[in] recv_neib_pe 隣接する領域番号
 * @param[out] recv_index recv 隣接領域の index 配列
 * @ingroup dev_com
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
 * @brief データ通信する recv 隣接領域の item 配列取得（並列実行版）
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] comm MPI コミュニケータ
 * @param[in] comm_size MPI コミュニケータに属する領域数
 * @param[in] n_outer_node !
 * @param[in] outer_node_id_all 全ての外部計算点番号
 * @param[in] outer_domain_id_all 全ての外部計算点が属する領域番号
 * @param[in] displs 全ての外部計算点配列の各領域に属する計算点数
 * @param[in] recv_n_neib 隣接する領域数
 * @param[in] recv_neib_pe 隣接する領域番号
 * @param[in] recv_index recv 隣接領域の index 配列
 * @param[in] nz !
 * @param[out] recv_item recv 隣接領域の item 配列
 * @ingroup dev_com
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
 * @brief データ通信する send 隣接領域の取得（並列実行版）
 * @param[in] comm MPI コミュニケータ
 * @param[in] comm_size MPI コミュニケータに属する領域数
 * @param[in] recv_n_neib 隣接する領域数
 * @param[in] recv_neib_pe 隣接する領域番号
 * @param[in] recv_index recv 隣接領域の index 配列
 * @param[out] send_n_list send 計算点の個数リスト
 * @ingroup dev_com
 */
void monolis_comm_get_send_parallel_n_list(
  int  comm,
  int  comm_size,
  int  recv_n_neib,
  int* recv_neib_pe,
  int* recv_index,
  int* send_n_list);

/**
 * @brief データ通信する send 隣接領域の取得（並列実行版）
 * @param[in] comm MPI コミュニケータ
 * @param[in] comm_size MPI コミュニケータに属する領域数
 * @param[in] send_n_list send 計算点の個数リスト
 * @param[out] send_n_neib 隣接する領域番号
 * @ingroup dev_com
 */
void monolis_comm_get_send_parallel_n_neib(
  int  comm,
  int  comm_size,
  int* send_n_list,
  int* send_n_neib);

/**
 * @brief データ通信する send 隣接領域の取得（並列実行版）
 * @param[in] comm MPI コミュニケータ
 * @param[in] comm_size MPI コミュニケータに属する領域数
 * @param[in] n_neib_send 隣接する領域数
 * @param[in] send_n_list send 計算点の個数リスト
 * @param[out] send_neib_pe 隣接する領域番号
 * @ingroup dev_com
 */
void monolis_comm_get_send_parallel_neib_id(
  int  comm,
  int  comm_size,
  int  n_neib_send,
  int* send_n_list,
  int* send_neib_pe);

/**
 * @brief データ通信する send 隣接領域の取得（並列実行版）
 * @param[in] comm MPI コミュニケータ
 * @param[in] comm_size MPI コミュニケータに属する領域数
 * @param[in] send_n_list send 計算点の個数リスト
 * @param[in] send_n_neib 隣接する領域番号
 * @param[out] send_index 隣接領域の index 配列（送信）
 * @ingroup dev_com
 */
void monolis_comm_get_send_parallel_index(
  int  comm,
  int  comm_size,
  int* send_n_list,
  int  send_n_neib,
  int* send_index);

/**
 * @brief データ通信する send の item 配列の取得（並列実行版）
 * @param[in] comm MPI コミュニケータ
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] recv_n_neib 隣接する領域数（受信）
 * @param[in] recv_neib_pe 隣接する領域番号（受信）
 * @param[in] recv_index recv 隣接領域の index 配列（受信）
 * @param[in] recv_nz recv 隣接領域の index 配列の要素数（受信）
 * @param[in] recv_item recv 隣接領域の item 配列（受信）
 * @param[in] send_n_neib 隣接する領域数（送信）
 * @param[in] send_neib_pe 隣接する領域番号（送信）
 * @param[in] send_index 隣接領域の index 配列（送信）
 * @param[in] send_nz 隣接領域の index 配列の要素数（送信）
 * @param[out] send_item 隣接領域の item 配列（送信）
 * @ingroup dev_com
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
 * @brief 全ての外部計算点を取得
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] comm MPI コミュニケータ
 * @param[out] n_outer_node !
 * @ingroup dev_com
 */
void monolis_comm_get_all_external_n_node_parallel(
  int  n_internal_vertex,
  int  n_vertex,
  int  comm,
  int* n_outer_node);

/**
 * @brief 全ての外部計算点を取得
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] comm MPI コミュニケータ
 * @param[in] n_outer_node 全ての外部計算点数
 * @param[out] outer_node_id_all_global 全ての外部計算点のグローバル計算点番号
 * @param[in] comm_size MPI コミュニケータに属する領域数
 * @param[out] displs 全ての外部計算点配列の各領域に属する計算点数
 * @ingroup dev_com
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
 * @brief 全ての外部計算点が所属する領域番号を取得
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @param[in] n_vertex 全計算点数
 * @param[in] vertex_id 計算点 id
 * @param[in] comm MPI コミュニケータ
 * @param[in] n_outer_node 全ての外部計算点数
 * @param[in] outer_node_id_all_global 全ての外部計算点のグローバル計算点番号
 * @param[out] outer_domain_id_all 全ての外部計算点が属する領域番号
 * @param[in] comm_size MPI コミュニケータに属する領域数
 * @param[in] displs 全ての外部計算点配列の各領域に属する計算点数
 * @ingroup dev_com
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
