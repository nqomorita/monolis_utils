/* monolis_io.h */
#ifndef MONOLIS_IO_H
#define MONOLIS_IO_H

#ifdef __cplusplus
extern "C" {
#endif

void monolis_input_internal_vertex_number(
  const char* fname,
  int*        n_internal);

void monolis_input_global_id(
  const char* fname,
  int*        n_vertex,
  int**       vertex_id);

#ifdef __cplusplus
}
#endif

#endif
