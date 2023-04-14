/* monolis_io_com.h */
#ifndef MONOLIS_IO_COM_H
#define MONOLIS_IO_COM_H

#ifdef __cplusplus
extern "C" {
#endif

#include "monolis_def_com_c.h"

void monolis_input_com_table_main(
  const char* fname,
  int*        n_neib,
  int*        neib_pe,
  int*        index,
  int*        item);

void monolis_input_send_com_table(
  const char*  fname,
  MONOLIS_COM* com);

void monolis_input_recv_com_table(
  const char*  fname,
  MONOLIS_COM* com);

#ifdef __cplusplus
}
#endif

#endif
