/* alloc_c.h */
#ifndef MONOLIS_ALLOC_C_H
#define MONOLIS_ALLOC_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include <complex.h>

/** 1 次元整数配列のメモリ確保 */
int* monolis_alloc_I_1d(
  int*      var,
  const int size);

/** 1 次元実数配列のメモリ解放 */
void monolis_dealloc_I_1d(
  int**     var);

/** 2 次元整数配列のメモリ確保 */
int** monolis_alloc_I_2d(
  int**     var,
  const int size1,
  const int size2);

/** 2 次元実数配列のメモリ解放 */
void monolis_dealloc_I_2d(
  int***    var,
  const int size1,
  const int size2);

/** 1 次元実数配列のメモリ確保 */
double* monolis_alloc_R_1d(
  double*   var,
  const int size);

/** 1 次元実数配列のメモリ解放 */
void monolis_dealloc_R_1d(
  double**  var);

/** 2 次元実数配列のメモリ確保 */
double** monolis_alloc_R_2d(
  double**  var,
  const int size1,
  const int size2);

/** 2 次元実数配列のメモリ解放 */
void monolis_dealloc_R_2d(
  double*** var,
  const int size1,
  const int size2);

/** 1 次元複素数配列のメモリ確保 */
complex double* monolis_alloc_C_1d(
  complex double* var,
  const int       size);

/** 1 次元複素数配列のメモリ解放 */
void monolis_dealloc_C_1d(
  complex double** var);

/** 2 次元複素数配列のメモリ確保 */
complex double** monolis_alloc_C_2d(
  complex double** var,
  const int        size1,
  const int        size2);

/** 2 次元複素数配列のメモリ解放 */
void monolis_dealloc_C_2d(
  complex double*** var,
  const int         size1,
  const int         size2);

#ifdef __cplusplus
}
#endif

#endif
