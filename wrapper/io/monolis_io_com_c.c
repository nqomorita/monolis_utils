#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_alloc_c.h"
#include "monolis_def_com_c.h"
#include "monolis_def_prm_c.h"
#include "monolis_io_util_c.h"

void monolis_input_com_table_main(
  const char* fname,
  int*        n_neib,
  int**       neib_pe,
  int**       index,
  int**       item)
{
  int i;
  int nitem;
  int ierr;
  FILE* fp;

  fp = monolis_open_file(fp, fname);
  ierr = fscanf(fp, "%d %d", n_neib, &nitem);

  if(n_neib == 0){
    *neib_pe = monolis_alloc_I_1d(*neib_pe, 1);
    *index = monolis_alloc_I_1d(*index, 1);
    *item = monolis_alloc_I_1d(*item, 1);
    fclose(fp);
    return;
  } else {
    *neib_pe = monolis_alloc_I_1d(*neib_pe, *n_neib);
    *index = monolis_alloc_I_1d(*index, *n_neib + 1);
    *item = monolis_alloc_I_1d(*item, nitem);
  }

  for(i = 0; i < *n_neib; i++){
    ierr = fscanf(fp, "%d", &(*neib_pe)[i]);
  }

  for(i = 0; i < *n_neib + 1; i++){
    ierr = fscanf(fp, "%d", &(*index)[i]);
  }

  for(i = 0; i < nitem; i++){
    ierr = fscanf(fp, "%d", &(*item)[i]);
  }

  fclose(fp);
}

void monolis_input_send_com_table(
  const char*  fname,
  MONOLIS_COM* com)
{
  monolis_input_com_table_main(fname, &com->send_n_neib, &com->send_neib_pe, &com->send_index, &com->send_item);
}

void monolis_input_recv_com_table(
  const char*  fname,
  MONOLIS_COM* com)
{
  monolis_input_com_table_main(fname, &com->recv_n_neib, &com->recv_neib_pe, &com->recv_index, &com->recv_item);
}
