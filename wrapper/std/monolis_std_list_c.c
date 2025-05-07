#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_std_list_c.h"
#include "monolis_alloc_c.h"

void monolis_list_initialize_R(
  MONOLIS_LIST_R* list_struct_R,
  const int n)
{
  for (int i = 0; i < n; i++) {
    list_struct_R[i].n = 0;
    list_struct_R[i].array = NULL;
  }
}

void monolis_list_initialize_I(
  MONOLIS_LIST_I* list_struct_I,
  const int n)
{
  for (int i = 0; i < n; i++) {
    list_struct_I[i].n = 0;
    list_struct_I[i].array = NULL;
  }
}

void monolis_list_initialize_C(
  MONOLIS_LIST_C* list_struct_C,
  const int n)
{
  for (int i = 0; i < n; i++) {
    list_struct_C[i].n = 0;
    list_struct_C[i].array = NULL;
  }
}

void monolis_list_finalize_R(
  MONOLIS_LIST_R* list_struct_R,
  const int n)
{
  for (int i = 0; i < n; i++) {
    list_struct_R[i].n = 0;
    monolis_dealloc_R_1d(&list_struct_R[i].array);
  }
}

void monolis_list_finalize_I(
  MONOLIS_LIST_I* list_struct_I,
  const int n)
{
  for (int i = 0; i < n; i++) {
    list_struct_I[i].n = 0;
    monolis_dealloc_I_1d(&list_struct_I[i].array);
  }
}

void monolis_list_finalize_C(
  MONOLIS_LIST_C* list_struct_C,
  const int n)
{
  for (int i = 0; i < n; i++) {
    list_struct_C[i].n = 0;
    monolis_dealloc_C_1d(&list_struct_C[i].array);
  }
}

void monolis_list_set_R(
  MONOLIS_LIST_R* list_struct_R,
  const int id,
  const int n,
  const double* array)
{
  list_struct_R[id].n = n;
  monolis_dealloc_R_1d(&list_struct_R[id].array);
  list_struct_R[id].array = monolis_alloc_R_1d(list_struct_R[id].array, n);
  for (int i = 0; i < n; i++) {
    list_struct_R[id].array[i] = array[i];
  }
}

void monolis_list_set_I(
  MONOLIS_LIST_I* list_struct_I,
  const int id,
  const int n,
  const int* array)
{
  list_struct_I[id].n = n;
  monolis_dealloc_I_1d(&list_struct_I[id].array);
  list_struct_I[id].array = monolis_alloc_I_1d(list_struct_I[id].array, n);
  for (int i = 0; i < n; i++) {
    list_struct_I[id].array[i] = array[i];
  }
}

void monolis_list_set_C(
  MONOLIS_LIST_C* list_struct_C,
  const int id,
  const int n,
  const double _Complex* array)
{
  list_struct_C[id].n = n;
  monolis_dealloc_C_1d(&list_struct_C[id].array);
  list_struct_C[id].array = monolis_alloc_C_1d(list_struct_C[id].array, n);
  for (int i = 0; i < n; i++) {
    list_struct_C[id].array[i] = array[i];
  }
}

void monolis_list_get_R(
  const MONOLIS_LIST_R* list_struct_R,
  const int id,
  double** array)
{
  monolis_dealloc_R_1d(array);
  *array = monolis_alloc_R_1d(*array, list_struct_R[id].n);
  for (int i = 0; i < list_struct_R[id].n; i++) {
    *array[i] = list_struct_R[id].array[i];
  }
}

void monolis_list_get_I(
  const MONOLIS_LIST_I* list_struct_I,
  const int id,
  int** array)
{
  monolis_dealloc_I_1d(array);
  *array = monolis_alloc_I_1d(*array, list_struct_I[id].n);
  for (int i = 0; i < list_struct_I[id].n; i++) {
    *array[i] = list_struct_I[id].array[i];
  }
}

void monolis_list_get_C(
  const MONOLIS_LIST_C* list_struct_C,
  const int id,
  double _Complex** array)
{
  monolis_dealloc_C_1d(array);
  *array = monolis_alloc_C_1d(*array, list_struct_C[id].n);
  for (int i = 0; i < list_struct_C[id].n; i++) {
    *array[i] = list_struct_C[id].array[i];
  }
}
