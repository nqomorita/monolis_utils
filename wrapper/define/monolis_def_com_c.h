/* monolis_def_com_c.h */
#ifndef MONOLIS_DEF_COM_C_H
#define MONOLIS_DEF_COM_C_H

#ifdef __cplusplus
extern "C" {
#endif

  /** 通信テーブルデータ読込のトップディレクトリ名 */
  static const char* MONOLIS_DEFAULT_TOP_DIR = "./";

  /** 通信テーブルデータ読込の分割ファイルが格納されるディレクトリ名 */
  static const char* MONOLIS_DEFAULT_PART_DIR = "parted.0";

  /** 通信テーブルデータが記載されたファイル名 */
  static const char* MONOLIS_DEFAULT_FILE_NAME = "graph.dat";

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
} MONOLIS_COM;

/**
 * @brief COM 構造体の終了処理関数
 * @param[in,out] com COM 構造体
 * @ingroup com
 */
void monolis_com_finalize(
  MONOLIS_COM* com);

/**
 * @brief COM 構造体のコピー関数
 * @param[in] in COM 構造体（コピー元）
 * @param[out] out COM 構造体（コピー先）
 * @ingroup com
 */
void monolis_com_copy(
  MONOLIS_COM* in,
  MONOLIS_COM* out);

/**
 * @brief COM 構造体に MPI コミュニケータを設定
 * @param[out] com COM 構造体
 * @param[in] comm MPI コミュニケータ
 * @ingroup com
 */
void monolis_com_set_communicator(
  MONOLIS_COM* com,
  int          comm);

/**
 * @brief COM 構造体から MPI コミュニケータを取得
 * @param[in] com COM 構造体
 * @param[out] comm MPI コミュニケータ
 * @ingroup com
 */
void monolis_com_get_communicator(
  MONOLIS_COM* com,
  int*         comm);

/**
 * @brief COM 構造体に MPI ランク番号を設定
 * @param[out] com COM 構造体
 * @param[in] my_rank MPI ランク番号
 * @ingroup com
 */
void monolis_com_set_my_rank(
  MONOLIS_COM* com,
  int          my_rank);

/**
 * @brief COM 構造体から MPI ランク番号を取得
 * @param[in] com COM 構造体
 * @param[out] my_rank MPI ランク番号
 * @ingroup com
 */
void monolis_com_get_my_rank(
  MONOLIS_COM* com,
  int*         my_rank);

/**
 * @brief COM 構造体に MPI コミュニケータサイズを設定
 * @param[out] com COM 構造体
 * @param[in] comm_size MPI コミュニケータサイズ
 * @ingroup com
 */
void monolis_com_set_comm_size(
  MONOLIS_COM* com,
  int          comm_size);

/**
 * @brief COM 構造体から MPI コミュニケータサイズを取得
 * @param[in] com COM 構造体
 * @param[out] comm_size MPI コミュニケータサイズ
 * @ingroup com
 */
void monolis_com_get_comm_size(
  MONOLIS_COM* com,
  int*         comm_size);

/**
 * @brief COM 構造体に分割領域における内部計算点数を設定
 * @param[out] com COM 構造体
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_set_n_internal_vertex(
  MONOLIS_COM* com,
  int          n_internal_vertex);

/**
 * @brief COM 構造体から分割領域における内部計算点数を取得
 * @param[in] com COM 構造体
 * @param[out] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
void monolis_com_get_n_internal_vertex(
  MONOLIS_COM* com,
  int*         n_internal_vertex);

#ifdef __cplusplus
}
#endif

#endif
