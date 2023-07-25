#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_utils.h"

void monolis_io_file_name_c_test()
{
  const char* top_name = ".";
  const char* dir_name = "parted";
  const char* file_name = "input.text";
  const char* out1;
  const char* out2;
  const char* out3;
  const char* out4;
  const char* out5;
  int comm;
  int domain_id;
  int comm_size;
  int my_rank;

  monolis_std_global_log_string("monolis_get_global_input_file_name");

  comm = monolis_mpi_get_global_comm();

  comm_size = monolis_mpi_get_global_comm_size();

  my_rank = monolis_mpi_get_global_my_rank();

  out1 = monolis_get_global_input_file_name(top_name, dir_name, file_name);

  if(comm_size == 1){
    if(strcmp(out1, "./input.text") == 0){
      monolis_test_assert_pass("monolis_io_file_name_c_test 1_c");
    } else {
      monolis_test_assert_fail("monolis_io_file_name_c_test 1_c", "");
    }
  } else {
    if(my_rank == 0){
      if(strcmp(out1, "./parted/input.text.0") == 0){
        monolis_test_assert_pass("monolis_io_file_name_c_test 1_c");
      } else {
        monolis_test_assert_fail("monolis_io_file_name_c_test 1_c", "");
      }
    } else {
      if(strcmp(out1, "./parted/input.text.1") == 0){
        monolis_test_assert_pass("monolis_io_file_name_c_test 1_c");
      } else {
        monolis_test_assert_fail("monolis_io_file_name_c_test 1_c", "");
      }
    }
  }

  monolis_std_global_log_string("monolis_get_local_input_file_name");

  out2 = monolis_get_local_input_file_name(top_name, dir_name, file_name, comm);

  if(comm_size == 1){
    if(strcmp(out2, "./input.text") == 0){
      monolis_test_assert_pass("monolis_io_file_name_c_test 2_c");
    } else {
      monolis_test_assert_fail("monolis_io_file_name_c_test 2_c", "");
    }
  } else {
    if(my_rank == 0){
      if(strcmp(out2, "./parted/input.text.0") == 0){
        monolis_test_assert_pass("monolis_io_file_name_c_test 2_c");
      } else {
        monolis_test_assert_fail("monolis_io_file_name_c_test 2_c", "");
      }
    } else {
      if(strcmp(out2, "./parted/input.text.1") == 0){
        monolis_test_assert_pass("monolis_io_file_name_c_test 2_c");
      } else {
        monolis_test_assert_fail("monolis_io_file_name_c_test 2_c", "");
      }
    }
  }

  monolis_std_global_log_string("monolis_get_global_output_file_name");

  out3 = monolis_get_global_output_file_name(top_name, dir_name, file_name);

  if(comm_size == 1){
    if(strcmp(out3, "./input.text") == 0){
      monolis_test_assert_pass("monolis_io_file_name_c_test 3");
    } else {
      monolis_test_assert_fail("monolis_io_file_name_c_test 3", "");
    }
  } else {
    if(my_rank == 0){
      if(strcmp(out3, "./parted/input.text.0") == 0){
        monolis_test_assert_pass("monolis_io_file_name_c_test 3");
      } else {
        monolis_test_assert_fail("monolis_io_file_name_c_test 3", "");
      }
    } else {
      if(strcmp(out3, "./parted/input.text.1") == 0){
        monolis_test_assert_pass("monolis_io_file_name_c_test 3");
      } else {
        monolis_test_assert_fail("monolis_io_file_name_c_test 3", "");
      }
    }
  }

  monolis_std_global_log_string("monolis_get_local_output_file_name");

  out4 = monolis_get_local_output_file_name(top_name, dir_name, file_name, comm);

  if(comm_size == 1){
    if(strcmp(out4, "./input.text") == 0){
      monolis_test_assert_pass("monolis_io_file_name_c_test 4");
    } else {
      monolis_test_assert_fail("monolis_io_file_name_c_test 4", "");
    }
  } else {
    if(my_rank == 0){
      if(strcmp(out4, "./parted/input.text.0") == 0){
        monolis_test_assert_pass("monolis_io_file_name_c_test 4");
      } else {
        monolis_test_assert_fail("monolis_io_file_name_c_test 4", "");
      }
    } else {
      if(strcmp(out4, "./parted/input.text.1") == 0){
        monolis_test_assert_pass("monolis_io_file_name_c_test 4");
      } else {
        monolis_test_assert_fail("monolis_io_file_name_c_test 4", "");
      }
    }
  }

  domain_id = 2;

  monolis_std_global_log_string("monolis_get_output_file_name_by_domain_id");

  out5 = monolis_get_output_file_name_by_domain_id(top_name, dir_name, file_name, domain_id);

  if(strcmp(out5, "./parted/input.text.2") == 0){
    monolis_test_assert_pass("monolis_io_file_name_c_test 5");
  } else {
    monolis_test_assert_fail("monolis_io_file_name_c_test 5", "");
  }
}
