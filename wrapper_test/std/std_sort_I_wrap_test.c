#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_utils.h"

void monolis_sort_I_warp_test()
{
  int a[5];
  int b[5];
  int idx;

  /* monolis_qsort_I_1d */
  monolis_std_global_log_string("monolis_qsort_I_1d");

  a[0] = 5;
  a[1] = 1;
  a[2] = 4;
  a[3] = 3;
  a[4] = 2;

  monolis_qsort_I_1d(a, 0, 4);

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

  monolis_qsort_I_1d(a, 0, 2);

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

  monolis_qsort_I_1d(a, 0, 4);

  monolis_test_check_eq_I1("monolis_sort_I_warp_test 3_c", a[0], 2);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 3_c", a[1], 4);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 3_c", a[2], 4);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 3_c", a[3], 5);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 3_c", a[4], 5);

  /* monolis_qsort_I_2d */
  monolis_std_global_log_string("monolis_qsort_I_2d");

  a[0] = 5; b[0] = 50;
  a[1] = 1; b[1] = 10;
  a[2] = 4; b[2] = 40;
  a[3] = 3; b[3] = 30;
  a[4] = 2; b[4] = 20;

  monolis_qsort_I_2d(a, b, 0, 4);

  monolis_test_check_eq_I1("monolis_sort_I_warp_test 4_c", a[0], 1);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 4_c", a[1], 2);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 4_c", a[2], 3);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 4_c", a[3], 4);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 4_c", a[4], 5);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 4_c", b[0], 10);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 4_c", b[1], 20);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 4_c", b[2], 30);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 4_c", b[3], 40);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 4_c", b[4], 50);

  a[0] = 5; b[0] = 50;
  a[1] = 1; b[1] = 10;
  a[2] = 4; b[2] = 40;
  a[3] = 3; b[3] = 30;
  a[4] = 2; b[4] = 20;

  monolis_qsort_I_2d(a, b, 0, 2);

  monolis_test_check_eq_I1("monolis_sort_I_warp_test 5_c", a[0], 1);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 5_c", a[1], 4);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 5_c", a[2], 5);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 5_c", a[3], 3);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 5_c", a[4], 2);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 5_c", b[0], 10);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 5_c", b[1], 40);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 5_c", b[2], 50);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 5_c", b[3], 30);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 5_c", b[4], 20);

  a[0] = 5; b[0] = 50;
  a[1] = 5; b[1] = 50;
  a[2] = 4; b[2] = 40;
  a[3] = 4; b[3] = 40;
  a[4] = 2; b[4] = 20;

  monolis_qsort_I_2d(a, b, 0, 4);

  monolis_test_check_eq_I1("monolis_sort_I_warp_test 6_c", a[0], 2);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 6_c", a[1], 4);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 6_c", a[2], 4);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 6_c", a[3], 5);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 6_c", a[4], 5);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 6_c", b[0], 20);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 6_c", b[1], 40);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 6_c", b[2], 40);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 6_c", b[3], 50);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 6_c", b[4], 50);

  /* monolis_bsearch_I */
  monolis_std_global_log_string("monolis_bsearch_I");

  a[0] = 10;
  a[1] = 20;
  a[2] = 30;
  a[3] = 40;
  a[4] = 50;

  monolis_bsearch_I(a, 0, 4, 20, &idx);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 7_c", idx, 1);

  monolis_bsearch_I(a, 0, 4, 10, &idx);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 7_c", idx, 0);

  monolis_bsearch_I(a, 0, 2, 40, &idx);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 7_c", idx, -1);

  /* monolis_get_sequence_array_I */
  monolis_std_global_log_string("monolis_get_sequence_array_I");

  monolis_get_sequence_array_I(a, 5, 1, 1);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 8_c", a[0], 1);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 8_c", a[1], 2);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 8_c", a[2], 3);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 8_c", a[3], 4);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 8_c", a[4], 5);

  monolis_get_sequence_array_I(a, 5, 2, 2);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 8_c", a[0], 2);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 8_c", a[1], 4);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 8_c", a[2], 6);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 8_c", a[3], 8);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 8_c", a[4], 10);

  /* monolis_get_uniq_array_I */
  monolis_std_global_log_string("monolis_get_uniq_array_I");

  a[0] = 1;
  a[1] = 2;
  a[2] = 2;
  a[3] = 4;
  a[4] = 4;

  monolis_get_uniq_array_I(a, 5, &idx);

  monolis_test_check_eq_I1("monolis_sort_I_warp_test 9_c", idx, 3);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 9_c", a[0], 1);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 9_c", a[1], 2);
  monolis_test_check_eq_I1("monolis_sort_I_warp_test 9_c", a[2], 4);
}
