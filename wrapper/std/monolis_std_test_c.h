/* monolis_std_test.h */
#ifndef MONOLIS_STD_TEST_H
#define MONOLIS_STD_TEST_H

#ifdef __cplusplus
extern "C" {
#endif

#include <complex.h>

void monolis_std_log_string(
  const char* array);

void monolis_test_check_eq_I1(
  const char* array,
  int         a,
  int         b);

void monolis_test_check_eq_R1(
  const char* array,
  double      a,
  double      b);

void monolis_test_check_eq_C1(
  const char*    array,
  double complex a,
  double complex b);

#ifdef __cplusplus
}
#endif

#endif
