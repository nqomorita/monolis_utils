#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "def_com_c.h"

/** 構造体の説明 */
void monolis_com_initialize(
  MONOLIS_COM* com)
{
    com->recv_n_neib = 0;
    com->send_n_neib = 0;
    com->n_internal_vertex = 0;
    com->my_rank = 0;
    com->comm = 0;
    com->comm_size = 0;

    //call monolis_dealloc_I_1d(COM%recv_neib_pe)
    //call monolis_dealloc_I_1d(COM%recv_index)
    //call monolis_dealloc_I_1d(COM%recv_item)
    //call monolis_dealloc_I_1d(COM%send_neib_pe)
    //call monolis_dealloc_I_1d(COM%send_index)
    //call monolis_dealloc_I_1d(COM%send_item)
}

/** 構造体の説明 */
void monolis_com_finalize(
  MONOLIS_COM* com)
{
    com->recv_n_neib = 0;
    com->send_n_neib = 0;
    com->n_internal_vertex = 0;
    com->my_rank = 0;
    com->comm = 0;
    com->comm_size = 0;

    //call monolis_dealloc_I_1d(COM%recv_neib_pe)
    //call monolis_dealloc_I_1d(COM%recv_index)
    //call monolis_dealloc_I_1d(COM%recv_item)
    //call monolis_dealloc_I_1d(COM%send_neib_pe)
    //call monolis_dealloc_I_1d(COM%send_index)
    //call monolis_dealloc_I_1d(COM%send_item)
}

/** 構造体の説明 */
void monolis_com_set_communicator(
  MONOLIS_COM* com,
  int*         comm)
{

}

/** 構造体の説明 */
void monolis_com_get_communicator(
  MONOLIS_COM* com,
  int*         comm)
{

}

/** 構造体の説明 */
void monolis_com_set_my_rank(
  MONOLIS_COM* com,
  int*         my_rank)
{

}

/** 構造体の説明 */
void monolis_com_get_my_rank(
  MONOLIS_COM* com,
  int*         my_rank)
{

}

/** 構造体の説明 */
void monolis_com_set_comm_size(
  MONOLIS_COM* com,
  int*         comm_size)
{

}

/** 構造体の説明 */
void monolis_com_get_comm_size(
  MONOLIS_COM* com,
  int*         comm_size)
{

}

/** 構造体の説明 */
void monolis_com_set_n_internal_vertex(
  MONOLIS_COM* com,
  int*         n_internal_vertex)
{

}

/** 構造体の説明 */
void monolis_com_get_n_internal_vertex(
  MONOLIS_COM* com,
  int*         n_internal_vertex)
{

}
