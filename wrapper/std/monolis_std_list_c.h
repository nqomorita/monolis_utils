/* monolis_std_list.h */
#ifndef MONOLIS_STD_LIST_H
#define MONOLIS_STD_LIST_H

#ifdef __cplusplus
extern "C" {
#endif

#include "monolis_def_prm_c.h"

typedef struct{
  int n;
  double* array;
} MONOLIS_LIST_R;

typedef struct{
  int n;
  int* array;
} MONOLIS_LIST_I;

typedef struct{
  int n;
  double _Complex* array;
} MONOLIS_LIST_C;

void monolis_list_initialize_R(
  MONOLIS_LIST_R* list_struct_R,
  const int n);

void monolis_list_initialize_I(
  MONOLIS_LIST_I* list_struct_I,
  const int n);

void monolis_list_initialize_C(
  MONOLIS_LIST_C* list_struct_C,
  const int n);

void monolis_list_finalize_R(
  MONOLIS_LIST_R* list_struct_R,
  const int n);

void monolis_list_finalize_I(
  MONOLIS_LIST_I* list_struct_I,
  const int n);

void monolis_list_finalize_C(
  MONOLIS_LIST_C* list_struct_C,
  const int n);

void monolis_list_set_R(
  MONOLIS_LIST_R* list_struct_R,
  const int id,
  const int n,
  const double* array);

void monolis_list_set_I(
  MONOLIS_LIST_I* list_struct_I,
  const int id,
  const int n,
  const int* array);

void monolis_list_set_C(
  MONOLIS_LIST_C* list_struct_C,
  const int id,
  const int n,
  const double _Complex* array);

void monolis_list_get_R(
  const MONOLIS_LIST_R* list_struct_R,
  const int id,
  double** array);

void monolis_list_get_I(
  const MONOLIS_LIST_I* list_struct_I,
  const int id,
  int** array);

void monolis_list_get_C(
  const MONOLIS_LIST_C* list_struct_C,
  const int id,
  double _Complex** array);

#ifdef __cplusplus
}
#endif

#endif
