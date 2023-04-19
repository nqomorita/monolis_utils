/* monolis_def_com_c.h */
#ifndef MONOLIS_DEF_COM_C_H
#define MONOLIS_DEF_COM_C_H

#ifdef __cplusplus
extern "C" {
#endif

/** COM 構造体 */
typedef struct {
  /** MPI コミュニケータ */
  int comm;
  /** MPI ランク番号 */
  int my_rank;
  /** MPI コミュニケータサイズ */
  int comm_size;
  /** 内部領域に属する自由度数 */
  int n_internal_vertex;
  /** 受信する領域数 */
  int recv_n_neib;
  /** 受信する領域番号リスト */
  int* recv_neib_pe;
  /** 受信するノード番号の index 配列 */
  int* recv_index;
  /** 受信するノード番号の item 配列 */
  int* recv_item;
  /** 送信する領域数 */
  int send_n_neib;
  /** 送信する領域番号リスト */
  int* send_neib_pe;
  /** 送信するノード番号の index 配列 */
  int* send_index;
  /** 送信するノード番号の item 配列 */
  int* send_item;
  /** 通信テーブルデータ読込のトップディレクトリ名 */
  char top_dir_name[1024];
  /** 通信テーブルデータ読込の分割ファイルが格納されるディレクトリ名 */
  char part_dir_name[1024];
  /** 通信テーブルデータが記載されたファイル名 */
  char file_name[1024];
} MONOLIS_COM;

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_finalize(
  MONOLIS_COM* com);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_copy(
  MONOLIS_COM* in,
  MONOLIS_COM* out);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_set_communicator(
  MONOLIS_COM* com,
  int          comm);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_get_communicator(
  MONOLIS_COM* com,
  int*         comm);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_set_my_rank(
  MONOLIS_COM* com,
  int          my_rank);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_get_my_rank(
  MONOLIS_COM* com,
  int*         my_rank);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_set_comm_size(
  MONOLIS_COM* com,
  int          comm_size);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_get_comm_size(
  MONOLIS_COM* com,
  int*         comm_size);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_set_n_internal_vertex(
  MONOLIS_COM* com,
  int          n_internal_vertex);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_get_n_internal_vertex(
  MONOLIS_COM* com,
  int*         n_internal_vertex);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_set_input_top_directory_name(
  MONOLIS_COM* com,
  const char*  param);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_set_input_part_directory_name(
  MONOLIS_COM* com,
  const char*  param);

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_set_input_file_name(
  MONOLIS_COM* com,
  const char*  param);

#ifdef __cplusplus
}
#endif

#endif
