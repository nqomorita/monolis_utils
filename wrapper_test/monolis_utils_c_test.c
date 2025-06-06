#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "monolis_utils.h"
#include "./define/monolis_def_com_c_test.h"
#include "./std/std_sort_I_wrap_test.h"
#include "./std/std_list_c_test.h"
#include "./sys/monolis_alloc_c_test.h"
#include "./mpi/monolis_mpi_c_test.h"
#include "./mpi/monolis_mpi_util_c_test.h"
#include "./io/monolis_io_file_name_c_test.h"
#include "./io/monolis_io_com_c_test.h"
#include "./io/monolis_io_c_test.h"
#include "./shape/monolis_shape_2d_quad_1st_c_test.h"
#include "./shape/monolis_shape_2d_tri_1st_c_test.h"
#include "./shape/monolis_shape_2d_tri_2nd_c_test.h"
#include "./shape/monolis_shape_3d_hex_1st_c_test.h"
#include "./shape/monolis_shape_3d_tet_1st_c_test.h"
#include "./shape/monolis_shape_3d_tet_2nd_c_test.h"
#include "./com/monolis_comm_table_c_test.h"

int main()
{
  monolis_mpi_initialize();

  monolis_def_com_test();

  monolis_sort_I_warp_test();

  monolis_list_test();

  monolis_alloc_test();

  monolis_io_file_name_c_test();

  monolis_io_com_c_test();

  monolis_io_c_test();

  monolis_mpi_test();

  monolis_mpi_util_test();

  monolis_comm_table_c_test();

  monolis_shape_2d_quad_1st_test();

  monolis_shape_2d_tri_1st_test();

  monolis_shape_2d_tri_2nd_test();

  monolis_shape_3d_hex_1st_test();

  monolis_shape_3d_tet_1st_test();

  monolis_shape_3d_tet_2nd_test();

  monolis_mpi_finalize();
}
