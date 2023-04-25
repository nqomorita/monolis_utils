#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_alloc_c.h"

/** 1 次元整数配列のメモリ確保 */
int* monolis_alloc_I_1d(
  int*      var,
  const int size)
{
  var = (int*)calloc(size, sizeof(int));
  return var;
}

/** 1 次元実数配列のメモリ解放 */
void monolis_dealloc_I_1d(
  int** var)
{
  free(*var);
  *var = NULL;
}

/** 2 次元整数配列のメモリ確保 */
int** monolis_alloc_I_2d(
  int**     var,
  const int size1,
  const int size2)
{
  int i;
  var = (int**)calloc(size1, sizeof(int*));
  for(i = 0; i < size1; i++) {
    var[i] = (int*)calloc(size2, sizeof(int));
  }
  return var;
}

/** 2 次元実数配列のメモリ解放 */
void monolis_dealloc_I_2d(
  int***    var,
  const int size1,
  const int size2)
{
  int i;
  for(i = 0; i < size1; i++) {
    free((*var)[i]);
  }
  free(*var);
  *var = NULL;
}

/** 1 次元実数配列のメモリ確保 */
double* monolis_alloc_R_1d(
  double*   var,
  const int size)
{
  var = (double*)calloc(size, sizeof(double));
  return var;
}

/** 1 次元実数配列のメモリ解放 */
void monolis_dealloc_R_1d(
  double** var)
{
  free(*var);
  *var = NULL;
}

/** 2 次元実数配列のメモリ確保 */
double** monolis_alloc_R_2d(
  double**  var,
  const int size1,
  const int size2)
{
  int i;
  var = (double**)calloc(size1, sizeof(double*));
  for(i = 0; i < size1; i++) {
    var[i] = (double*)calloc(size2, sizeof(double));
  }
  return var;
}

/** 2 次元実数配列のメモリ解放 */
void monolis_dealloc_R_2d(
  double*** var,
  const int size1,
  const int size2)
{
  int i;
  for(i = 0; i < size1; i++) {
    free((*var)[i]);
  }
  free(*var);
  *var = NULL;
}

/** 1 次元複素数配列のメモリ確保 */
double _Complex* monolis_alloc_C_1d(
  double _Complex* var,
  const int        size)
{
  var = (_Complex double*)calloc(size, sizeof(_Complex double));
  return var;
}

/** 1 次元複素数配列のメモリ解放 */
void monolis_dealloc_C_1d(
  double _Complex** var)
{
  free(*var);
  *var = NULL;
}

/** 2 次元複素数配列のメモリ確保 */
double _Complex** monolis_alloc_C_2d(
  double _Complex** var,
  const int        size1,
  const int        size2)
{
  int i;
  var = (double _Complex**)calloc(size1, sizeof(double _Complex*));
  for(i = 0; i < size1; i++) {
    var[i] = (double _Complex*)calloc(size2, sizeof(double _Complex));
  }
  return var;
}

/** 2 次元複素数配列のメモリ解放 */
void monolis_dealloc_C_2d(
  double _Complex*** var,
  const int         size1,
  const int         size2)
{
  int i;
  for(i = 0; i < size1; i++) {
    free((*var)[i]);
  }
  free(*var);
  *var = NULL;
}
