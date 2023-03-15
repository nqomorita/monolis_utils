/* monolis_struct.h */
#ifndef MONOLIS_STRUCT_H
#define MONOLIS_STRUCT_H

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
} MONOLIS_COM;

/** COM 構造体の初期化関数 */
void monolis_com_initialize(
  MONOLIS_COM* com);

/** COM 構造体の終了処理関数 */
void monolis_com_finalize(
  MONOLIS_COM* com);

/** COM 構造体に MPI コミュニケータを設定 */
void monolis_com_set_communicator(
  MONOLIS_COM* com,
  int          comm);

/** COM 構造体から MPI コミュニケータを取得 */
void monolis_com_get_communicator(
  MONOLIS_COM* com,
  int*         comm);

/** COM 構造体に MPI ランク番号を設定 */
void monolis_com_set_my_rank(
  MONOLIS_COM* com,
  int          my_rank);

/** COM 構造体から MPI ランク番号を取得 */
void monolis_com_get_my_rank(
  MONOLIS_COM* com,
  int*         my_rank);

/** COM 構造体に MPI コミュニケータサイズを設定 */
void monolis_com_set_comm_size(
  MONOLIS_COM* com,
  int          comm_size);

/** COM 構造体から MPI コミュニケータサイズを取得 */
void monolis_com_get_comm_size(
  MONOLIS_COM* com,
  int*         comm_size);

/** COM 構造体に内部領域に属する自由度数を設定 */
void monolis_com_set_n_internal_vertex(
  MONOLIS_COM* com,
  int          n_internal_vertex);

/** COM 構造体から内部領域に属する自由度数を取得 */
void monolis_com_get_n_internal_vertex(
  MONOLIS_COM* com,
  int*         n_internal_vertex);

#ifdef __cplusplus
}
#endif

#endif
