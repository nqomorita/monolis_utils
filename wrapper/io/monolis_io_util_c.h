/* monolis_io_util.h */
#ifndef MONOLIS_IO_UTIL_H
#define MONOLIS_IO_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

/**
 * @brief データ通信する recv 隣接領域の取得（並列実行版）
 * @param[in] n_internal_vertex 分割領域における内部計算点数
 * @ingroup com
 */
FILE* monolis_open_file(
  FILE*       fp,
  const char* fname);

#ifdef __cplusplus
}
#endif

#endif
