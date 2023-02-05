!> IO 引数モジュール
module mod_monolis_io_com_test
  use mod_monolis_utils_error
  use mod_monolis_utils_std_test
  use mod_monolis_io_com
  implicit none

contains

  subroutine monolis_io_com_test()
    implicit none

    call monolis_input_send_com_table_test()
    call monolis_input_recv_com_table_test()
    call monolis_output_send_com_table_test()
    call monolis_output_recv_com_table_test()
  end subroutine monolis_io_com_test

  subroutine monolis_input_send_com_table_test()
    implicit none
    type(monolis_COM) :: COM
    integer(kint) :: i_ans(4)

    call monolis_std_log_string("monolis_input_send_com_table_test")

    call monolis_input_send_com_table("io/input/com.txt", COM)

    call monolis_test_check_eq_I1("monolis_input_send_com_table_test 1", COM%send_n_neib, 2)

    i_ans(1) = 0
    i_ans(2) = 1
    call monolis_test_check_eq_I ("monolis_input_send_com_table_test 2", COM%send_neib_pe, i_ans(1:2))

    i_ans(1) = 0
    i_ans(2) = 2
    i_ans(3) = 4
    call monolis_test_check_eq_I ("monolis_input_send_com_table_test 3", COM%send_index, i_ans(1:3))

    i_ans(1) = 10
    i_ans(2) = 20
    i_ans(3) = 30
    i_ans(4) = 40
    call monolis_test_check_eq_I ("monolis_input_send_com_table_test 4", COM%send_item, i_ans)
  end subroutine monolis_input_send_com_table_test

  subroutine monolis_input_recv_com_table_test()
    implicit none
    type(monolis_COM) :: COM
    integer(kint) :: i_ans(4)

    call monolis_std_log_string("monolis_input_recv_com_table_test")

    call monolis_input_recv_com_table("io/input/com.txt", COM)

    call monolis_test_check_eq_I1("monolis_input_recv_com_table_test 1", COM%recv_n_neib, 2)

    i_ans(1) = 0
    i_ans(2) = 1
    call monolis_test_check_eq_I ("monolis_input_recv_com_table_test 2", COM%recv_neib_pe, i_ans(1:2))

    i_ans(1) = 0
    i_ans(2) = 2
    i_ans(3) = 4
    call monolis_test_check_eq_I ("monolis_input_recv_com_table_test 3", COM%recv_index, i_ans(1:3))

    i_ans(1) = 10
    i_ans(2) = 20
    i_ans(3) = 30
    i_ans(4) = 40
    call monolis_test_check_eq_I ("monolis_input_recv_com_table_test 4", COM%recv_item, i_ans)
  end subroutine monolis_input_recv_com_table_test

  subroutine monolis_output_send_com_table_test()
    implicit none
    type(monolis_COM) :: COM

    call monolis_std_log_string("monolis_output_send_com_table_test")

    COM%send_n_neib = 2

    call monolis_alloc_I_1d(COM%send_neib_pe, 2)
    COM%send_neib_pe(1) = 0
    COM%send_neib_pe(2) = 1

    call monolis_alloc_I_1d(COM%send_index, 3)
    COM%send_index(1) = 0
    COM%send_index(2) = 2
    COM%send_index(3) = 4

    call monolis_alloc_I_1d(COM%send_item, 4)
    COM%send_item(1) = 10
    COM%send_item(2) = 20
    COM%send_item(3) = 30
    COM%send_item(4) = 40

    call monolis_output_send_com_table("io/input/com.txt.1.out", COM)
  end subroutine monolis_output_send_com_table_test

  subroutine monolis_output_recv_com_table_test()
    implicit none
    type(monolis_COM) :: COM

    call monolis_std_log_string("monolis_output_recv_com_table_test")

    COM%recv_n_neib = 2

    call monolis_alloc_I_1d(COM%recv_neib_pe, 2)
    COM%recv_neib_pe(1) = 0
    COM%recv_neib_pe(2) = 1

    call monolis_alloc_I_1d(COM%recv_index, 3)
    COM%recv_index(1) = 0
    COM%recv_index(2) = 2
    COM%recv_index(3) = 4

    call monolis_alloc_I_1d(COM%recv_item, 4)
    COM%recv_item(1) = 10
    COM%recv_item(2) = 20
    COM%recv_item(3) = 30
    COM%recv_item(4) = 40

    call monolis_output_recv_com_table("io/input/com.txt.2.out", COM)
  end subroutine monolis_output_recv_com_table_test
end module mod_monolis_io_com_test
