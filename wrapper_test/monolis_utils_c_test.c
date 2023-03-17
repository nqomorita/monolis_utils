#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "monolis_utils.h"
#include "./define/monolis_def_com_c_test.h"

int main()
{
  monolis_mpi_initialize();

  monolis_def_com_test();

  monolis_mpi_finalize();
}
