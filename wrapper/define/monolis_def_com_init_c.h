/* monolis_def_com_init_c.h */
#ifndef MONOLIS_DEF_COM_INIT_C_H
#define MONOLIS_DEF_COM_INIT_C_H

#ifdef __cplusplus
extern "C" {
#endif

/** COM 構造体の初期化関数 */
void monolis_com_initialize_by_parted_files(
  MONOLIS_COM* com,
  int          comm);

/** COM 構造体の初期化関数 */
void monolis_com_initialize_by_global_id(
  MONOLIS_COM* com,
  int          comm,
  int          n_internal_vertex,
  int          n_vertex,
  int*         global_id);

/** COM 構造体の初期化関数 */
void monolis_com_initialize_by_self(
  MONOLIS_COM* com);

#ifdef __cplusplus
}
#endif

#endif
