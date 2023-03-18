#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>
#include "monolis_utils.h"
#include "monolis_alloc_c_test.h"

void monolis_alloc_I_1d_test()
{
  int* var;
  int  size;

  monolis_std_log_string("monolis_alloc_I_1d_test");

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

  monolis_std_log_string("monolis_dealloc_I_1d_test");

  size = 5;

  var = monolis_alloc_I_1d(var, size);

  monolis_dealloc_I_1d(&var);

  if(var == NULL){
    monolis_test_assert_pass("monolis_dealloc_I_1d_test");
  } else {
    monolis_test_assert_fail("monolis_dealloc_I_1d_test", "not NULL");
  }
}

void monolis_alloc_R_1d_test()
{
  double* var;
  double ans;
  int  size;

  monolis_std_log_string("monolis_alloc_R_1d_test");

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

  monolis_std_log_string("monolis_dealloc_R_1d_test");

  size = 5;

  var = monolis_alloc_R_1d(var, size);

  monolis_dealloc_R_1d(&var);

  if(var == NULL){
    monolis_test_assert_pass("monolis_dealloc_R_1d_test");
  } else {
    monolis_test_assert_fail("monolis_dealloc_R_1d_test", "not NULL");
  }
}

void monolis_alloc_C_1d_test()
{
  double complex* var;
  double complex ans;
  int  size;

  monolis_std_log_string("monolis_alloc_C_1d_test");

  size = 5;

  var = monolis_alloc_C_1d(var, size);

  monolis_test_check_eq_C1("monolis_alloc_C_1d_test", var[0], ans);
  monolis_test_check_eq_C1("monolis_alloc_C_1d_test", var[1], ans);
  monolis_test_check_eq_C1("monolis_alloc_C_1d_test", var[2], ans);
  monolis_test_check_eq_C1("monolis_alloc_C_1d_test", var[3], ans);
  monolis_test_check_eq_C1("monolis_alloc_C_1d_test", var[4], ans);
}

void monolis_dealloc_C_1d_test()
{
  double complex* var;
  int  size;

  monolis_std_log_string("monolis_dealloc_C_1d_test");

  size = 5;

  var = monolis_alloc_C_1d(var, size);

  monolis_dealloc_C_1d(&var);

  if(var == NULL){
    monolis_test_assert_pass("monolis_dealloc_C_1d_test");
  } else {
    monolis_test_assert_fail("monolis_dealloc_C_1d_test", "not NULL");
  }
}

void monolis_alloc_test()
{
   monolis_alloc_I_1d_test();
   monolis_dealloc_I_1d_test();

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
}
