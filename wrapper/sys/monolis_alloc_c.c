#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
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

/** 1 次元整数配列のメモリ再確保 */
int* monolis_realloc_I_1d(
  int*      var,
  const int size_old,
  const int size_new)
{
  if (var == NULL) {
    var = monolis_alloc_I_1d(var, size_new);
    return var;
  }

  int* temp;

  temp = monolis_alloc_I_1d(temp, size_old);

  for (int i = 0; i < size_old; ++i) {
    temp[i] = var[i];
  }

  monolis_dealloc_I_1d(&var);
  var = monolis_alloc_I_1d(var, size_new);

  for (int i = 0; i < size_old; ++i) {
    var[i] = temp[i];
  }

  return var;
}

/** 1 次元整数配列の末尾にデータ配列を追加 */
int* monolis_append_I_1d(
  int* var,
  int  size_old,
  int  size_add,
  int* var_add)
{
  if (var == NULL) {
    size_old = 0;
  }

  int n_all = size_old + size_add;

  var = monolis_realloc_I_1d(var, size_old, n_all);

  for (int i = size_old; i < n_all; ++i) {
    var[i] = var_add[i - size_old];
  }

  return var;
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

/** 1 次元論理型配列のメモリ確保 */
bool* monolis_alloc_L_1d(
  bool*     var,
  const int size)
{
  var = (bool*)calloc(size, sizeof(bool));
  return var;
}

/** 1 次元論理型配列のメモリ開放 */
void monolis_dealloc_L_1d(
  bool**    var)
{
  free(*var);
  *var = NULL;
}
