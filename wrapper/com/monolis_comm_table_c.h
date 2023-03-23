/* monolis_comm_table_c.h */
#ifndef MONOLIS_COMM_TABLE_C_H
#define MONOLIS_COMM_TABLE_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include "monolis_def_com_c.h"

void monolis_com_get_comm_table_parallel(
  int          n_internal_vertex,
  int          n_vertex,
  int*         vertex_id,
  MONOLIS_COM* com);

#ifdef __cplusplus
}
#endif

#endif
