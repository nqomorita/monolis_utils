#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_utils.h"

void monolis_io_c_test()
{
  int n_internal_vertex;
  int n_vertex;
  int* vertex_id;

  monolis_std_global_log_string("monolis_input_internal_vertex_number");

  monolis_input_internal_vertex_number("io/input/n_internal.txt", &n_internal_vertex);

  monolis_test_check_eq_I1("monolis_input_internal_vertex_number 1", n_internal_vertex, 5);

  monolis_std_global_log_string("monolis_input_global_id");

  monolis_input_global_id("io/input/id.txt", &n_vertex, &vertex_id);

  monolis_test_check_eq_I1("monolis_input_global_id_test 1", n_vertex, 5);
  monolis_test_check_eq_I1("monolis_input_global_id_test 2", vertex_id[0], 10);
  monolis_test_check_eq_I1("monolis_input_global_id_test 3", vertex_id[1], 20);
  monolis_test_check_eq_I1("monolis_input_global_id_test 4", vertex_id[2], 30);
  monolis_test_check_eq_I1("monolis_input_global_id_test 5", vertex_id[3], 40);
  monolis_test_check_eq_I1("monolis_input_global_id_test 6", vertex_id[4], 50);
}
