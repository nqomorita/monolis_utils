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

  subroutine monolis_output_send_com_table_test()
    implicit none
    type(monolis_COM) :: COM

    call monolis_std_log_string("monolis_output_send_com_table_test")

    COM%send_n_neib = 0
    call monolis_output_send_com_table("io/input/com.output.1.txt", COM)
  end subroutine monolis_output_send_com_table_test

  subroutine monolis_output_recv_com_table_test()
    implicit none
    type(monolis_COM) :: COM

    call monolis_std_log_string("monolis_output_recv_com_table_test")

    COM%recv_n_neib = 0
    call monolis_output_recv_com_table("io/input/com.output.2.txt", COM)
  end subroutine monolis_output_recv_com_table_test

  subroutine monolis_input_send_com_table_test()
    implicit none
    type(monolis_COM) :: COM

    call monolis_std_log_string("monolis_input_send_com_table_test")

    call monolis_input_send_com_table("io/input/com.txt", COM)
  end subroutine monolis_input_send_com_table_test

  subroutine monolis_input_recv_com_table_test()
    implicit none
    type(monolis_COM) :: COM

    call monolis_std_log_string("monolis_input_recv_com_table_test")

    call monolis_input_recv_com_table("io/input/com.txt", COM)
  end subroutine monolis_input_recv_com_table_test
end module mod_monolis_io_com_test
