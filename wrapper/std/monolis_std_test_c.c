#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>
#include "monolis_def_prm_c.h"
#include "monolis_std_test_c.h"
#include "monolis_alloc_c.h"

void monolis_test_check_eq_R(
  const char* header,
  const int size_a,
  const double* a,
  const int size_b,
  const double* b)
{
  bool is_eq;

  if (size_a != size_b) {
    monolis_test_assert_fail(header, "size mismatch");
  }

  for (int i = 0; i < size_a; i++) {
    if (fabs(a[i]) < 1.0e-20) {
      if (fabs(a[i] - b[i]) > 1.0e-8) {
        is_eq = false;
      } else {
        is_eq = true;
      }
    } else if (fabs(a[i] - b[i])/fabs(a[i]) > 1.0e-8) {
      is_eq = false;
    } else {
        is_eq = true;
    }
    if (!is_eq) {
      monolis_test_assert_fail(header, "value mismatch");
    }
  }

  monolis_test_assert_pass(header);
}

void monolis_test_check_eq_I(
  const char* header,
  const int size_a,
  const int* a,
  const int size_b,
  const int* b)
{
  bool is_eq;

  if (size_a != size_b) {
    monolis_test_assert_fail(header, "size mismatch");
  }

  for (int i = 0; i < size_a; i++) {
    if (a[i] != b[i]) {
      is_eq = false;
    } else {
      is_eq = true;
    }
    if (!is_eq) {
      monolis_test_assert_fail(header, "value mismatch");
    }
  }

  monolis_test_assert_pass(header);
}

void monolis_test_check_eq_C(
  const char* header,
  const int size_a,
  const double _Complex* a,
  const int size_b,
  const double _Complex* b)
{
  bool is_eq;

  if (size_a != size_b) {
    monolis_test_assert_fail(header, "size mismatch");
  }

  for (int i = 0; i < size_a; i++) {
    if (fabs(creal(a[i])) < 1.0e-20 || fabs(cimag(a[i])) < 1.0e-20) {
      if (fabs(creal(a[i]) - creal(b[i])) + fabs(cimag(a[i]) - cimag(b[i])) > 1.0e-8) {
        is_eq = false;
      } else {
        is_eq = true;
      }
    } else if (fabs(creal(a[i]) - creal(b[i]))/fabs(creal(a[i])) + fabs(cimag(a[i]) - cimag(b[i]))/fabs(cimag(a[i])) > 1.0e-8) {
      is_eq = false;
    } else {
      is_eq = true;
    }
    if (!is_eq) {
      monolis_test_assert_fail(header, "value mismatch");
    }
  }

  monolis_test_assert_pass(header);
}