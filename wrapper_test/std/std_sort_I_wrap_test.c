#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_utils.h"

void monolis_sort_I_warp_test()
{
  int a[5];

  monolis_std_global_log_string("monolis_qsort_I_1d");

  a[0] = 5;
  a[1] = 1;
  a[2] = 4;
  a[3] = 3;
  a[4] = 2;

  monolis_qsort_I_1d(a, 1, 5);

  monolis_test_check_eq_I1("monolis_sort_I_warp_test 1_c", a[0], 1);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 1_c", a[1], 2);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 1_c", a[2], 3);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 1_c", a[3], 4);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 1_c", a[4], 5);

  a[0] = 5;
  a[1] = 1;
  a[2] = 4;
  a[3] = 3;
  a[4] = 2;

  monolis_qsort_I_1d(a, 1, 3);

  monolis_test_check_eq_I1("monolis_sort_I_warp_test 2_c", a[0], 1);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 2_c", a[1], 4);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 2_c", a[2], 5);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 2_c", a[3], 3);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 2_c", a[4], 2);

  a[0] = 5;
  a[1] = 5;
  a[2] = 4;
  a[3] = 4;
  a[4] = 2;

  monolis_qsort_I_1d(a, 1, 5);

  monolis_test_check_eq_I1("monolis_sort_I_warp_test 3_c", a[0], 2);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 3_c", a[1], 4);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 3_c", a[2], 4);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 3_c", a[3], 5);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 3_c", a[4], 5);
}
