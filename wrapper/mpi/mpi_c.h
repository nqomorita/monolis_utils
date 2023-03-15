/* alloc_c.h */
#ifndef MONOLIS_ALLOC_C_H
#define MONOLIS_ALLOC_C_H

#ifdef __cplusplus
extern "C" {
#endif

/** 1 次元整数配列のメモリ確保 */
int* monolis_alloc_I_1d(
  int*      var,
  const int size);

#ifdef __cplusplus
}
#endif

#endif
