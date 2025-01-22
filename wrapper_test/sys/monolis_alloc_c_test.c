#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <complex.h>
#include "monolis_utils.h"
#include "monolis_alloc_c_test.h"

void monolis_alloc_I_1d_test()
{
  int* var;
  int  size;

  monolis_std_global_log_string("monolis_alloc_I_1d");

  size = 5;

  var = monolis_alloc_I_1d(var, size);

  monolis_test_check_eq_I1("monolis_alloc_I_1d_test", var[0], 0);
  monolis_test_check_eq_I1("monolis_alloc_I_1d_test", var[1], 0);
  monolis_test_check_eq_I1("monolis_alloc_I_1d_test", var[2], 0);
  monolis_test_check_eq_I1("monolis_alloc_I_1d_test", var[3], 0);
  monolis_test_check_eq_I1("monolis_alloc_I_1d_test", var[4], 0);
}

void monolis_dealloc_I_1d_test()
{
  int* var;
  int  size;

  monolis_std_global_log_string("monolis_dealloc_I_1d");

  size = 5;

  var = monolis_alloc_I_1d(var, size);

  monolis_dealloc_I_1d(&var);

  if(var == NULL){
    monolis_test_assert_pass("monolis_dealloc_I_1d_test");
  } else {
    monolis_test_assert_fail("monolis_dealloc_I_1d_test", "not NULL");
  }
}

void monolis_realloc_I_1d_test()
{
  int* var;
  int  size;
  int  size_new;

  monolis_std_global_log_string("monolis_realloc_I_1d");

  size = 3;

  var = monolis_alloc_I_1d(var, size);

  monolis_test_check_eq_I1("monolis_realloc_I_1d_test", var[0], 0);
  monolis_test_check_eq_I1("monolis_realloc_I_1d_test", var[1], 0);
  monolis_test_check_eq_I1("monolis_realloc_I_1d_test", var[2], 0);

  var[0] = 1;
  var[1] = 2;
  var[2] = 3;

  size_new = 5;

  var = monolis_realloc_I_1d(var, size, size_new);

  monolis_test_check_eq_I1("monolis_realloc_I_1d_test", var[0], 1);
  monolis_test_check_eq_I1("monolis_realloc_I_1d_test", var[1], 2);
  monolis_test_check_eq_I1("monolis_realloc_I_1d_test", var[2], 3);
  monolis_test_check_eq_I1("monolis_realloc_I_1d_test", var[3], 0);
  monolis_test_check_eq_I1("monolis_realloc_I_1d_test", var[4], 0);
}

void monolis_append_I_1d_test()
{
  int* var;
  int  size;

  monolis_std_global_log_string("monolis_append_I_1d");

  size = 3;

  var = monolis_alloc_I_1d(var, size);

  monolis_test_check_eq_I1("monolis_append_I_1d_test", var[0], 0);
  monolis_test_check_eq_I1("monolis_append_I_1d_test", var[1], 0);
  monolis_test_check_eq_I1("monolis_append_I_1d_test", var[2], 0);

  var[0] = 1;
  var[1] = 2;
  var[2] = 3;

  int n_add = 2;
  int var_add[2];

  var_add[0] = 4;
  var_add[1] = 5;

  var = monolis_append_I_1d(var, size, n_add, var_add);

  monolis_test_check_eq_I1("monolis_append_I_1d_test", var[0], 1);
  monolis_test_check_eq_I1("monolis_append_I_1d_test", var[1], 2);
  monolis_test_check_eq_I1("monolis_append_I_1d_test", var[2], 3);
  monolis_test_check_eq_I1("monolis_append_I_1d_test", var[3], 4);
  monolis_test_check_eq_I1("monolis_append_I_1d_test", var[4], 5);
}

void monolis_alloc_I_2d_test()
{
  int** var;
  int   size1;
  int   size2;

  monolis_std_global_log_string("monolis_alloc_I_2d");

  size1 = 5;
  size2 = 2;

  var = monolis_alloc_I_2d(var, size1, size2);

  monolis_test_check_eq_I1("monolis_alloc_I_2d_test", var[0][0], 0);
  monolis_test_check_eq_I1("monolis_alloc_I_2d_test", var[1][0], 0);
  monolis_test_check_eq_I1("monolis_alloc_I_2d_test", var[2][0], 0);
  monolis_test_check_eq_I1("monolis_alloc_I_2d_test", var[3][0], 0);
  monolis_test_check_eq_I1("monolis_alloc_I_2d_test", var[4][0], 0);
  monolis_test_check_eq_I1("monolis_alloc_I_2d_test", var[0][1], 0);
  monolis_test_check_eq_I1("monolis_alloc_I_2d_test", var[1][1], 0);
  monolis_test_check_eq_I1("monolis_alloc_I_2d_test", var[2][1], 0);
  monolis_test_check_eq_I1("monolis_alloc_I_2d_test", var[3][1], 0);
  monolis_test_check_eq_I1("monolis_alloc_I_2d_test", var[4][1], 0);
}

void monolis_dealloc_I_2d_test()
{
  int** var;
  int   size1;
  int   size2;

  monolis_std_global_log_string("monolis_dealloc_I_2d");

  size1 = 5;
  size2 = 2;

  var = monolis_alloc_I_2d(var, size1, size2);

  monolis_dealloc_I_2d(&var, size1, size2);

  if(var == NULL){
    monolis_test_assert_pass("monolis_dealloc_I_2d_test");
  } else {
    monolis_test_assert_fail("monolis_dealloc_I_2d_test", "not NULL");
  }
}

void monolis_alloc_R_1d_test()
{
  double* var;
  double ans;
  int  size;

  monolis_std_global_log_string("monolis_alloc_R_1d");

  size = 5;

  var = monolis_alloc_R_1d(var, size);

  ans = 0;

  monolis_test_check_eq_R1("monolis_alloc_R_1d_test", var[0], ans);
  monolis_test_check_eq_R1("monolis_alloc_R_1d_test", var[1], ans);
  monolis_test_check_eq_R1("monolis_alloc_R_1d_test", var[2], ans);
  monolis_test_check_eq_R1("monolis_alloc_R_1d_test", var[3], ans);
  monolis_test_check_eq_R1("monolis_alloc_R_1d_test", var[4], ans);
}

void monolis_dealloc_R_1d_test()
{
  double* var;
  int  size;

  monolis_std_global_log_string("monolis_dealloc_R_1d");

  size = 5;

  var = monolis_alloc_R_1d(var, size);

  monolis_dealloc_R_1d(&var);

  if(var == NULL){
    monolis_test_assert_pass("monolis_dealloc_R_1d_test");
  } else {
    monolis_test_assert_fail("monolis_dealloc_R_1d_test", "not NULL");
  }
}

void monolis_alloc_R_2d_test()
{
  double** var;
  int      size1;
  int      size2;

  monolis_std_global_log_string("monolis_alloc_R_2d");

  size1 = 5;
  size2 = 2;

  var = monolis_alloc_R_2d(var, size1, size2);

  monolis_test_check_eq_R1("monolis_alloc_R_2d_test", var[0][0], 0);
  monolis_test_check_eq_R1("monolis_alloc_R_2d_test", var[1][0], 0);
  monolis_test_check_eq_R1("monolis_alloc_R_2d_test", var[2][0], 0);
  monolis_test_check_eq_R1("monolis_alloc_R_2d_test", var[3][0], 0);
  monolis_test_check_eq_R1("monolis_alloc_R_2d_test", var[4][0], 0);
  monolis_test_check_eq_R1("monolis_alloc_R_2d_test", var[0][1], 0);
  monolis_test_check_eq_R1("monolis_alloc_R_2d_test", var[1][1], 0);
  monolis_test_check_eq_R1("monolis_alloc_R_2d_test", var[2][1], 0);
  monolis_test_check_eq_R1("monolis_alloc_R_2d_test", var[3][1], 0);
  monolis_test_check_eq_R1("monolis_alloc_R_2d_test", var[4][1], 0);
}

void monolis_dealloc_R_2d_test()
{
  double** var;
  int      size1;
  int      size2;

  monolis_std_global_log_string("monolis_dealloc_R_2d");

  size1 = 5;
  size2 = 2;

  var = monolis_alloc_R_2d(var, size1, size2);

  monolis_dealloc_R_2d(&var, size1, size2);

  if(var == NULL){
    monolis_test_assert_pass("monolis_dealloc_R_2d_test");
  } else {
    monolis_test_assert_fail("monolis_dealloc_R_2d_test", "not NULL");
  }
}

void monolis_alloc_C_1d_test()
{
  double _Complex* var;
  double _Complex  ans;
  int  size;

  monolis_std_global_log_string("monolis_alloc_C_1d");

  size = 5;

  var = monolis_alloc_C_1d(var, size);

  ans = 0.0 + 0.0*I;

  monolis_test_check_eq_C1("monolis_alloc_C_1d_test", var[0], ans);
  monolis_test_check_eq_C1("monolis_alloc_C_1d_test", var[1], ans);
  monolis_test_check_eq_C1("monolis_alloc_C_1d_test", var[2], ans);
  monolis_test_check_eq_C1("monolis_alloc_C_1d_test", var[3], ans);
  monolis_test_check_eq_C1("monolis_alloc_C_1d_test", var[4], ans);
}

void monolis_dealloc_C_1d_test()
{
  double _Complex* var;
  int size;

  monolis_std_global_log_string("monolis_dealloc_C_1d");

  size = 5;

  var = monolis_alloc_C_1d(var, size);

  monolis_dealloc_C_1d(&var);

  if(var == NULL){
    monolis_test_assert_pass("monolis_dealloc_C_1d_test");
  } else {
    monolis_test_assert_fail("monolis_dealloc_C_1d_test", "not NULL");
  }
}

void monolis_alloc_C_2d_test()
{
  double _Complex** var;
  int               size1;
  int               size2;

  monolis_std_global_log_string("monolis_alloc_C_2d");

  size1 = 5;
  size2 = 2;

  var = monolis_alloc_C_2d(var, size1, size2);

  monolis_test_check_eq_C1("monolis_alloc_C_2d_test", var[0][0], 0);
  monolis_test_check_eq_C1("monolis_alloc_C_2d_test", var[1][0], 0);
  monolis_test_check_eq_C1("monolis_alloc_C_2d_test", var[2][0], 0);
  monolis_test_check_eq_C1("monolis_alloc_C_2d_test", var[3][0], 0);
  monolis_test_check_eq_C1("monolis_alloc_C_2d_test", var[4][0], 0);
  monolis_test_check_eq_C1("monolis_alloc_C_2d_test", var[0][1], 0);
  monolis_test_check_eq_C1("monolis_alloc_C_2d_test", var[1][1], 0);
  monolis_test_check_eq_C1("monolis_alloc_C_2d_test", var[2][1], 0);
  monolis_test_check_eq_C1("monolis_alloc_C_2d_test", var[3][1], 0);
  monolis_test_check_eq_C1("monolis_alloc_C_2d_test", var[4][1], 0);
}

void monolis_dealloc_C_2d_test()
{
  double _Complex** var;
  int              size1;
  int              size2;

  monolis_std_global_log_string("monolis_dealloc_C_2d");

  size1 = 5;
  size2 = 2;

  var = monolis_alloc_C_2d(var, size1, size2);

  monolis_dealloc_C_2d(&var, size1, size2);

  if(var == NULL){
    monolis_test_assert_pass("monolis_dealloc_C_2d_test");
  } else {
    monolis_test_assert_fail("monolis_dealloc_C_2d_test", "not NULL");
  }
}

void monolis_alloc_L_1d_test()
{
  bool* var;
  int  size;

  monolis_std_global_log_string("monolis_alloc_L_1d");

  size = 5;

  var = monolis_alloc_L_1d(var, size);

  if(var[0]){monolis_test_assert_fail("monolis_alloc_L_1d", "not .FALSE.");}
  if(var[1]){monolis_test_assert_fail("monolis_alloc_L_1d", "not .FALSE.");}
  if(var[2]){monolis_test_assert_fail("monolis_alloc_L_1d", "not .FALSE.");}
  if(var[3]){monolis_test_assert_fail("monolis_alloc_L_1d", "not .FALSE.");}
  if(var[4]){monolis_test_assert_fail("monolis_alloc_L_1d", "not .FALSE.");}
}

void monolis_dealloc_L_1d_test()
{
  bool* var;
  int  size;

  monolis_std_global_log_string("monolis_dealloc_L_1d");

  size = 5;

  var = monolis_alloc_L_1d(var, size);

  monolis_dealloc_L_1d(&var);

  if(var == NULL){
    monolis_test_assert_pass("monolis_dealloc_L_1d_test");
  } else {
    monolis_test_assert_fail("monolis_dealloc_L_1d_test", "not NULL");
  }
}

void monolis_alloc_test()
{
   monolis_alloc_I_1d_test();
   monolis_dealloc_I_1d_test();
   monolis_realloc_I_1d_test();
   monolis_append_I_1d_test();

   monolis_alloc_I_2d_test();
   monolis_dealloc_I_2d_test();

   monolis_alloc_R_1d_test();
   monolis_dealloc_R_1d_test();

   monolis_alloc_R_2d_test();
   monolis_dealloc_R_2d_test();

   monolis_alloc_C_1d_test();
   monolis_dealloc_C_1d_test();

   monolis_alloc_C_2d_test();
   monolis_dealloc_C_2d_test();

   monolis_alloc_L_1d_test();
   monolis_dealloc_L_1d_test();
}
