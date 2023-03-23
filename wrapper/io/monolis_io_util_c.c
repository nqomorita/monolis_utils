#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_def_prm_c.h"

FILE* monolis_open_file(
  FILE*       fp,
  const char* fname)
{
  fp = fopen(fname, "r");
  if( fp == NULL ){
    printf("** error: file name %s\n", fname);
    printf("** error: monolis_open_file\n");
    exit(MONOLIS_FAIL);
  }
  return fp;
}
