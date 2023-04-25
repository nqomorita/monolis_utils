#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_utils.h"

void monolis_io_com_c_test()
{
  MONOLIS_COM com1;
  MONOLIS_COM com2;

  monolis_std_global_log_string("monolis_input_send_com_table");

  monolis_input_send_com_table("io/input/com.txt", &com1);

  monolis_test_check_eq_I1("monolis_input_send_com_table 1", com1.send_n_neib, 2);
  monolis_test_check_eq_I1("monolis_input_send_com_table 2", com1.send_neib_pe[0], 0);
  monolis_test_check_eq_I1("monolis_input_send_com_table 3", com1.send_neib_pe[1], 1);
  monolis_test_check_eq_I1("monolis_input_send_com_table 4", com1.send_index[0], 0);
  monolis_test_check_eq_I1("monolis_input_send_com_table 5", com1.send_index[1], 2);
  monolis_test_check_eq_I1("monolis_input_send_com_table 6", com1.send_index[2], 4);
  monolis_test_check_eq_I1("monolis_input_send_com_table 7", com1.send_item[0], 10);
  monolis_test_check_eq_I1("monolis_input_send_com_table 8", com1.send_item[1], 20);
  monolis_test_check_eq_I1("monolis_input_send_com_table 9", com1.send_item[2], 30);
  monolis_test_check_eq_I1("monolis_input_send_com_table 10", com1.send_item[3], 40);

  monolis_std_global_log_string("monolis_input_recv_com_table");

  monolis_input_recv_com_table("io/input/com.txt", &com2);

  monolis_test_check_eq_I1("monolis_input_recv_com_table 1", com2.recv_n_neib, 2);
  monolis_test_check_eq_I1("monolis_input_recv_com_table 2", com2.recv_neib_pe[0], 0);
  monolis_test_check_eq_I1("monolis_input_recv_com_table 3", com2.recv_neib_pe[1], 1);
  monolis_test_check_eq_I1("monolis_input_recv_com_table 4", com2.recv_index[0], 0);
  monolis_test_check_eq_I1("monolis_input_recv_com_table 5", com2.recv_index[1], 2);
  monolis_test_check_eq_I1("monolis_input_recv_com_table 6", com2.recv_index[2], 4);
  monolis_test_check_eq_I1("monolis_input_recv_com_table 7", com2.recv_item[0], 10);
  monolis_test_check_eq_I1("monolis_input_recv_com_table 8", com2.recv_item[1], 20);
  monolis_test_check_eq_I1("monolis_input_recv_com_table 9", com2.recv_item[2], 30);
  monolis_test_check_eq_I1("monolis_input_recv_com_table 10", com2.recv_item[3], 40);
}
