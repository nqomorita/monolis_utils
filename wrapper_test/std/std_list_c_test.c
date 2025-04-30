#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>
#include "monolis_utils.h"

void monolis_list_initialize_R_c_test()
{
  MONOLIS_LIST_R list_struct_R[1];

  monolis_std_log_string("monolis_list_initialize_R");

  list_struct_R[0].array = NULL;
  monolis_list_initialize_R(list_struct_R, 1);

  monolis_test_check_eq_I1("monolis_list_initialize_R", list_struct_R[0].n, 0);
}

void monolis_list_initialize_I_c_test()
{
  MONOLIS_LIST_I list_struct_I[1];

  monolis_std_log_string("monolis_list_initialize_I");

  list_struct_I[0].array = NULL;
  monolis_list_initialize_I(list_struct_I, 1);

  monolis_test_check_eq_I1("monolis_list_initialize_I", list_struct_I[0].n, 0);
}

void monolis_list_initialize_C_c_test()
{
  MONOLIS_LIST_C list_struct_C[1];

  monolis_std_log_string("monolis_list_initialize_C");

  list_struct_C[0].array = NULL;
  monolis_list_initialize_C(list_struct_C, 1);

  monolis_test_check_eq_I1("monolis_list_initialize_C", list_struct_C[0].n, 0);
}

void monolis_list_finalize_R_c_test()
{
  MONOLIS_LIST_R list_struct_R[1];

  monolis_std_log_string("monolis_list_finalize_R");

  list_struct_R[0].array = NULL;
  list_struct_R[0].n = 1;
  monolis_list_finalize_R(list_struct_R, 1);

  monolis_test_check_eq_I1("monolis_list_finalize_R", list_struct_R[0].n, 0);
}

void monolis_list_finalize_I_c_test()
{
  MONOLIS_LIST_I list_struct_I[1];

  monolis_std_log_string("monolis_list_finalize_I");

  list_struct_I[0].array = NULL;
  list_struct_I[0].n = 1;
  monolis_list_finalize_I(list_struct_I, 1);

  monolis_test_check_eq_I1("monolis_list_finalize_I", list_struct_I[0].n, 0);
}

void monolis_list_finalize_C_c_test()
{
  MONOLIS_LIST_C list_struct_C[1];

  monolis_std_log_string("monolis_list_finalize_C");

  list_struct_C[0].array = NULL;
  list_struct_C[0].n = 1;
  monolis_list_finalize_C(list_struct_C, 1);

  monolis_test_check_eq_I1("monolis_list_finalize_C", list_struct_C[0].n, 0);
}

void monolis_list_set_R_c_test()
{
  MONOLIS_LIST_R list_struct_R[1];
  double array[1];

  monolis_std_log_string("monolis_list_set_R");

  list_struct_R[0].array = NULL;
  array[0] = 1.0;
  monolis_list_initialize_R(list_struct_R, 1);
  monolis_list_set_R(list_struct_R, 0, 1, array);

  monolis_test_check_eq_I1("monolis_list_set_R n", list_struct_R[0].n, 1);
  monolis_test_check_eq_R1("monolis_list_set_R array", list_struct_R[0].array[0], 1.0);
}

void monolis_list_set_I_c_test()
{
  MONOLIS_LIST_I list_struct_I[1];
  int array[1];

  monolis_std_log_string("monolis_list_set_I");

  list_struct_I[0].array = NULL;
  array[0] = 1;
  monolis_list_initialize_I(list_struct_I, 1);
  monolis_list_set_I(list_struct_I, 0, 1, array);

  monolis_test_check_eq_I1("monolis_list_set_I n", list_struct_I[0].n, 1);
  monolis_test_check_eq_I1("monolis_list_set_I array", list_struct_I[0].array[0], 1);
}

void monolis_list_set_C_c_test()
{
  MONOLIS_LIST_C list_struct_C[1];
  double _Complex array[1];

  monolis_std_log_string("monolis_list_set_C");

  list_struct_C[0].array = NULL;
  array[0] = 1.0 + 1.0*I;
  monolis_list_initialize_C(list_struct_C, 1);
  monolis_list_set_C(list_struct_C, 0, 1, array);

  monolis_test_check_eq_I1("monolis_list_set_C n", list_struct_C[0].n, 1);
  monolis_test_check_eq_C1("monolis_list_set_C array", list_struct_C[0].array[0], 1.0 + 1.0*I);
}

void monolis_list_get_R_c_test()
{
  MONOLIS_LIST_R list_struct_R[1];
  double array1[1];
  double *array2 = NULL;

  monolis_std_log_string("monolis_list_get_R");

  list_struct_R[0].array = NULL;
  array1[0] = 1.0;
  monolis_list_initialize_R(list_struct_R, 1);
  monolis_list_set_R(list_struct_R, 0, 1, array1);
  monolis_list_get_R(list_struct_R, 0, &array2);

  monolis_test_check_eq_R1("monolis_list_get_R", list_struct_R[0].array[0], array2[0]);
}

void monolis_list_get_I_c_test()
{
  MONOLIS_LIST_I list_struct_I[1];
  int array1[1];
  int *array2 = NULL;

  monolis_std_log_string("monolis_list_get_I");

  list_struct_I[0].array = NULL;
  array1[0] = 1.0;
  monolis_list_initialize_I(list_struct_I, 1);
  monolis_list_set_I(list_struct_I, 0, 1, array1);
  monolis_list_get_I(list_struct_I, 0, &array2);

  monolis_test_check_eq_I1("monolis_list_get_I", list_struct_I[0].array[0], array2[0]);
}

void monolis_list_get_C_c_test()
{
  MONOLIS_LIST_C list_struct_C[1];
  double _Complex array1[1];
  double _Complex *array2 = NULL;

  monolis_std_log_string("monolis_list_get_C");

  list_struct_C[0].array = NULL;
  array1[0] = 1.0 + 1.0*I;
  monolis_list_initialize_C(list_struct_C, 1);
  monolis_list_set_C(list_struct_C, 0, 1, array1);
  monolis_list_get_C(list_struct_C, 0, &array2);

  monolis_test_check_eq_C1("monolis_list_get_C", list_struct_C[0].array[0], array2[0]);
}

void monolis_list_test()
{
  monolis_list_initialize_R_c_test();
  monolis_list_initialize_I_c_test();
  monolis_list_initialize_C_c_test();
  monolis_list_finalize_R_c_test();
  monolis_list_finalize_I_c_test();
  monolis_list_finalize_C_c_test();
  monolis_list_set_R_c_test();
  monolis_list_set_I_c_test();
  monolis_list_set_C_c_test();
  monolis_list_get_R_c_test();
  monolis_list_get_I_c_test();
  monolis_list_get_C_c_test();
}
