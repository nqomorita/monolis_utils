#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_alloc_c.h"
#include "monolis_def_com_c.h"
#include "monolis_def_prm_c.h"
#include "monolis_io_util_c.h"

void monolis_input_internal_vertex_number(
  const char* fname,
  int*        n_internal)
{
  FILE* fp;
  char* ctmp;

  fp = monolis_open_file(fp, fname);
  fscanf(fp, "%s", ctmp);
  fscanf(fp, "%d", n_internal);

  fclose(fp);
}

void monolis_input_global_id(
  const char* fname,
  int*        n_vertex,
  int*        vertex_id)
{
  FILE* fp;
  int i;
  int* itmp;
  char* ctmp;

  fp = monolis_open_file(fp, fname);
  fscanf(fp, "%s", ctmp);
  fscanf(fp, "%d %d", n_vertex, itmp);

  vertex_id = monolis_alloc_I_1d(vertex_id, *n_vertex);

  for(i = 0; i < *n_vertex; i++){
    fscanf(fp, "%d", &vertex_id[i]);
  }

  fclose(fp);
}
