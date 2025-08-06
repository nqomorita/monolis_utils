/* monolis_mpi_c.h */
#ifndef MONOLIS_MPI_C_H
#define MONOLIS_MPI_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include "monolis_def_prm_c.h"
#include "monolis_def_com_c.h"

static const int MONOLIS_MPI_SUM = 1;

static const int MONOLIS_MPI_MAX = 2;

static const int MONOLIS_MPI_MIN = 3;

/**
 * @brief allreduce 関数（整数配列型）
 * @param[in] n 配列サイズ
 * @param[inout] val 入出力値（整数型）
 * @param[in] tag MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
 * @param[in] comm MPI コミュニケータ
 * @ingroup mpi
 */
void monolis_allreduce_I(
  int  n,
  int* val,
  int  tag,
  int  comm);

/**
 * @brief allreduce 関数（浮動小数点配列型）
 * @param[in] n 配列サイズ
 * @param[inout] val 入出力値（浮動小数点配列型）
 * @param[in] tag MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
 * @param[in] comm MPI コミュニケータ
 * @ingroup mpi
 */
void monolis_allreduce_R(
  int     n,
  double* val,
  int     tag,
  int     comm);

/**
 * @brief allreduce 関数（複素数型）
 * @param[in] n 配列サイズ
 * @param[inout] val 入出力値（浮動小数点配列型）
 * @param[in] tag MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
 * @param[in] comm MPI コミュニケータ
 * @ingroup mpi
 */
void monolis_allreduce_C(
  int              n,
  double _Complex* val,
  int              tag,
  int              comm);

/**
 * @brief ベクトルのアップデート関数（実数型）
 * @param[in] com COM 構造体
 * @param[in] n 配列サイズ（オーバーラップ領域を含む）
 * @param[in] n_dof 計算点が持つ自由度
 * @param[inout] x 入出力ベクトル
 * @ingroup mpi
 */
void monolis_mpi_update_R(
  MONOLIS_COM* com,
  int          n,
  int          n_dof,
  double*      x);

/**
 * @brief ベクトルのアップデート関数（実数型）
 * @param[in] n 配列サイズ（オーバーラップ領域を含む）
 * @param[in] n_dof 計算点が持つ自由度
 * @param[inout] x 入出力ベクトル
 * @param[in] comm MPI コミュニケータ
 * @param[in] recv_n_neib recv する隣接領域数
 * @param[in] recv_nitem recv の item 数
 * @param[in] recv_neib_pe recv する隣接領域 id
 * @param[in] recv_index recv の index 配列
 * @param[in] recv_item recv の item 配列（受信する節点番号データ）
 * @param[in] send_n_neib send する隣接領域数
 * @param[in] send_nitem send の item 数
 * @param[in] send_neib_pe send する隣接領域 id
 * @param[in] send_index send の index 配列
 * @param[in] send_item send の item 配列（送信する節点番号データ）
 * @ingroup wrap_mpi
 */
void monolis_mpi_update_R_c_main(
  int     n,
  int     n_dof,
  double* x,
  int     comm,
  int     recv_n_neib,
  int     recv_nitem,
  int*    recv_neib_pe,
  int*    recv_index,
  int*    recv_item,
  int     send_n_neib,
  int     send_nitem,
  int*    send_neib_pe,
  int*    send_index,
  int*    send_item);

/**
 * @brief ベクトルのアップデート関数（整数型）
 * @param[in] com COM 構造体
 * @param[in] n 配列サイズ（オーバーラップ領域を含む）
 * @param[in] n_dof 計算点が持つ自由度
 * @param[inout] x 入出力ベクトル
 * @ingroup mpi
 */
void monolis_mpi_update_I(
  MONOLIS_COM* com,
  int          n,
  int          n_dof,
  int*         x);

/**
 * @brief ベクトルのアップデート関数（整数型）
 * @param[in] n 配列サイズ（オーバーラップ領域を含む）
 * @param[in] n_dof 計算点が持つ自由度
 * @param[inout] x 入出力ベクトル
 * @param[in] comm MPI コミュニケータ
 * @param[in] recv_n_neib recv する隣接領域数
 * @param[in] recv_nitem recv の item 数
 * @param[in] recv_neib_pe recv する隣接領域 id
 * @param[in] recv_index recv の index 配列
 * @param[in] recv_item recv の item 配列（受信する節点番号データ）
 * @param[in] send_n_neib send する隣接領域数
 * @param[in] send_nitem send の item 数
 * @param[in] send_neib_pe send する隣接領域 id
 * @param[in] send_index send の index 配列
 * @param[in] send_item send の item 配列（送信する節点番号データ）
 * @ingroup wrap_mpi
 */
void monolis_mpi_update_I_c_main(
  int  n,
  int  n_dof,
  int* x,
  int  comm,
  int  recv_n_neib,
  int  recv_nitem,
  int* recv_neib_pe,
  int* recv_index,
  int* recv_item,
  int  send_n_neib,
  int  send_nitem,
  int* send_neib_pe,
  int* send_index,
  int* send_item);

/**
 * @brief ベクトルのアップデート関数（複素数型）
 * @param[in] com COM 構造体
 * @param[in] n 配列サイズ（オーバーラップ領域を含む）
 * @param[in] n_dof 計算点が持つ自由度
 * @param[inout] x 入出力ベクトル
 * @ingroup mpi
 */
void monolis_mpi_update_C(
  MONOLIS_COM*     com,
  int              n,
  int              n_dof,
  double _Complex* x);

/**
 * @brief ベクトルのアップデート関数（複素数型、ラッパー関数）
 * @param[in] n 配列サイズ（オーバーラップ領域を含む）
 * @param[in] n_dof 計算点が持つ自由度
 * @param[inout] x 入出力ベクトル
 * @param[in] comm MPI コミュニケータ
 * @param[in] recv_n_neib recv する隣接領域数
 * @param[in] recv_nitem recv の item 数
 * @param[in] recv_neib_pe recv する隣接領域 id
 * @param[in] recv_index recv の index 配列
 * @param[in] recv_item recv の item 配列（受信する節点番号データ）
 * @param[in] send_n_neib send する隣接領域数
 * @param[in] send_nitem send の item 数
 * @param[in] send_neib_pe send する隣接領域 id
 * @param[in] send_index send の index 配列
 * @param[in] send_item send の item 配列（送信する節点番号データ）
 * @ingroup wrap_mpi
 */
void monolis_mpi_update_C_c_main(
  int              n,
  int              n_dof,
  double _Complex* x,
  int              comm,
  int              recv_n_neib,
  int              recv_nitem,
  int*             recv_neib_pe,
  int*             recv_index,
  int*             recv_item,
  int              send_n_neib,
  int              send_nitem,
  int*             send_neib_pe,
  int*             send_index,
  int*             send_item);

/**
 * @brief 隣接領域の任意本数ベクトルのベクトル数取得関数
 * @param[in] com COM 構造体
 * @param[in] n_vec 自領域のベクトル数
 * @param[out] n_neib_vec 自領域と隣接領域の合計ベクトル数
 * @ingroup mpi
 */
void monolis_mpi_get_n_neib_vector(
  MONOLIS_COM* com,
  int          n_vec,
  int*         n_neib_vec);

/**
 * @brief 隣接領域の任意本数ベクトルのベクトル数取得関数（ラッパー関数）
 * @ingroup wrap_mpi
 */
void monolis_mpi_get_n_neib_vector_c_main(
  int  n_vec,
  int* n_neib_vec,
  int  comm,
  int  recv_n_neib,
  int* recv_neib_pe);

/**
 * @brief 隣接領域の任意本数ベクトルの取得関数（実数型）
 * @param[in] com COM 構造体
 * @param[in] n 配列サイズ（オーバーラップ領域を含む）
 * @param[in] n_dof 計算点が持つ自由度
 * @param[in] n_vec 自領域のベクトル数
 * @param[in] n_neib_vec 自領域と隣接領域の合計ベクトル数
 * @param[in] my_vec 自領域のベクトル
 * @param[out] neib_vec 自領域と隣接領域が並んだベクトル（自領域、隣接領域の順で並ぶ）
 * @ingroup mpi
 */
void monolis_mpi_get_neib_vector_R(
  MONOLIS_COM* com,
  int          np,
  int          n_dof,
  int          n_vec,
  int          n_neib_vec,
  double**     my_vec,
  double**     neib_vec);

/**
 * @brief 隣接領域の任意本数ベクトルの取得関数（実数型、ラッパー関数）
 * @ingroup wrap_mpi
 */
void monolis_mpi_get_neib_vector_R_c_main(
  int          n_internal_vertex,
  int          np,
  int          n_dof,
  int          n_vec,
  int          n_neib_vec,
  double*      my_vec_t,
  double*      neib_vec_t,
  int          comm,
  int          recv_n_neib,
  int          recv_nitem,
  int*         recv_neib_pe,
  int*         recv_index,
  int*         recv_item,
  int          send_n_neib,
  int          send_nitem,
  int*         send_neib_pe,
  int*         send_index,
  int*         send_item);

#ifdef __cplusplus
}
#endif

#endif
