/* monolis_struct.h */
#ifndef MONOLIS_STRUCT_H
#define MONOLIS_STRUCT_H

#ifdef __cplusplus
extern "C" {
#endif

/** 構造体の説明 */
typedef struct {
  /** 構造体の説明 */
  int comm;
  /** 構造体の説明 */
  int my_rank;
  /** 構造体の説明 */
  int comm_size;
  /** 構造体の説明 */
  int n_internal_vertex;
  /** 構造体の説明 */
  int recv_n_neib;
  /** 構造体の説明 */
  int* recv_neib_pe;
  /** 構造体の説明 */
  int* recv_index;
  /** 構造体の説明 */
  int* recv_item;
  /** 構造体の説明 */
  int send_n_neib;
  /** 構造体の説明 */
  int* send_neib_pe;
  /** 構造体の説明 */
  int* send_index;
  /** 構造体の説明 */
  int* send_item;
} MONOLIS_COM;

/** 構造体の説明 */
void monolis_com_initialize(
  MONOLIS_COM* com);

/** 構造体の説明 */
void monolis_com_finalize(
  MONOLIS_COM* com);

/** 構造体の説明 */
void monolis_com_set_communicator(
  MONOLIS_COM* com,
  int*         comm);

/** 構造体の説明 */
void monolis_com_get_communicator(
  MONOLIS_COM* com,
  int*         comm);

/** 構造体の説明 */
void monolis_com_set_my_rank(
  MONOLIS_COM* com,
  int*         my_rank);

/** 構造体の説明 */
void monolis_com_get_my_rank(
  MONOLIS_COM* com,
  int*         my_rank);

/** 構造体の説明 */
void monolis_com_set_comm_size(
  MONOLIS_COM* com,
  int*         comm_size);

/** 構造体の説明 */
void monolis_com_get_comm_size(
  MONOLIS_COM* com,
  int*         comm_size);

/** 構造体の説明 */
void monolis_com_set_n_internal_vertex(
  MONOLIS_COM* com,
  int*         n_internal_vertex);

/** 構造体の説明 */
void monolis_com_get_n_internal_vertex(
  MONOLIS_COM* com,
  int*         n_internal_vertex);

#ifdef __cplusplus
}
#endif

#endif
