/* monolis_io_util.h */
#ifndef MONOLIS_IO_UTIL_H
#define MONOLIS_IO_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

/**
 * @brief IO のためのファイルポインタの取得
 * @param[out] fp ファイルポインタ
 * @param[in] fname アクセスするファイル名
 * @ingroup io
 */
FILE* monolis_open_file(
  FILE*       fp,
  const char* fname);

#ifdef __cplusplus
}
#endif

#endif
