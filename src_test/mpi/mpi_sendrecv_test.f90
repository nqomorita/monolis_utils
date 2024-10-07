!> MPI テストモジュール
module mod_monolis_mpi_sendrecv_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_mpi_sendrecv_test()
    implicit none

    call monolis_send_recv_test()
    call monolis_send_recv_V_test()
    call monolis_update_test()
    call monolis_updateV_test()

    call monolis_std_global_log_string("monolis_mpi_initialize")
    call monolis_std_global_log_string("monolis_mpi_finalize")

    call monolis_std_global_log_string("monolis_mpi_get_global_my_rank")
    call monolis_std_global_log_string("monolis_mpi_get_local_my_rank")

    call monolis_std_global_log_string("monolis_mpi_global_barrier")
    call monolis_std_global_log_string("monolis_mpi_local_barrier")

    call monolis_std_global_log_string("monolis_get_time")
    call monolis_std_global_log_string("monolis_get_time_global_sync")
    call monolis_std_global_log_string("monolis_get_time_local_sync")

    call monolis_std_global_log_string("monolis_Irecv_C")
    call monolis_std_global_log_string("monolis_Irecv_I")
    call monolis_std_global_log_string("monolis_Irecv_R")
    call monolis_std_global_log_string("monolis_Isend_C")
    call monolis_std_global_log_string("monolis_Isend_I")
    call monolis_std_global_log_string("monolis_Isend_R")
  end subroutine monolis_mpi_sendrecv_test

  subroutine monolis_send_recv_test()
    implicit none
    integer(kint) :: comm, send_n_neib, send_neib_pe(1), recv_n_neib, recv_neib_pe(1)
    integer(kint) :: send_index(0:1), send_item(2), recv_index(0:1), recv_item(2), ndof
    integer(kint) :: i(8), i_ans(8)
    real(kdouble) :: r(8), r_ans(8)
    complex(kdouble) :: c(8), c_ans(8)

    call monolis_std_global_log_string("monolis_SendRecv_I")
    call monolis_std_global_log_string("monolis_SendRecv_R")
    call monolis_std_global_log_string("monolis_SendRecv_C")

    if(monolis_mpi_get_global_comm_size() == 1) return

    send_n_neib = 1
    recv_n_neib = 1
    send_index(0) = 0
    send_index(1) = 2
    recv_index(0) = 0
    recv_index(1) = 2
    send_item(1) = 1
    send_item(2) = 2
    recv_item(1) = 3
    recv_item(2) = 4
    ndof = 2
    comm = monolis_mpi_get_global_comm()

    if(monolis_mpi_get_global_my_rank() == 0)then
      send_neib_pe(1) = 1
      recv_neib_pe(1) = 1
    else
      send_neib_pe(1) = 0
      recv_neib_pe(1) = 0
    endif

    !> case 1
    if(monolis_mpi_get_global_my_rank() == 0)then
      i(1) = 1
      i(2) = 2
      i(3) = 3
      i(4) = 4
    else
      i(1) = 5
      i(2) = 6
      i(3) = 7
      i(4) = 8
    endif

    call monolis_SendRecv_I(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
        & send_index, send_item, recv_index, recv_item, &
        & i, i, ndof, comm)

    if(monolis_mpi_get_global_my_rank() == 0)then
      i_ans(1) = 1
      i_ans(2) = 2
      i_ans(3) = 3
      i_ans(4) = 4
      i_ans(5) = 5
      i_ans(6) = 6
      i_ans(7) = 7
      i_ans(8) = 8
    else
      i_ans(1) = 5
      i_ans(2) = 6
      i_ans(3) = 7
      i_ans(4) = 8
      i_ans(5) = 1
      i_ans(6) = 2
      i_ans(7) = 3
      i_ans(8) = 4
    endif

    call monolis_test_check_eq_I("monolis_SendRecv_I  1", i, i_ans)

    !> case 2
    if(monolis_mpi_get_global_my_rank() == 0)then
      r(1) = 1.0d0
      r(2) = 2.0d0
      r(3) = 3.0d0
      r(4) = 4.0d0
    else
      r(1) = 5.0d0
      r(2) = 6.0d0
      r(3) = 7.0d0
      r(4) = 8.0d0
    endif

    call monolis_SendRecv_R(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
        & send_index, send_item, recv_index, recv_item, &
        & r, r, ndof, comm)

    if(monolis_mpi_get_global_my_rank() == 0)then
      r_ans(1) = 1.0d0
      r_ans(2) = 2.0d0
      r_ans(3) = 3.0d0
      r_ans(4) = 4.0d0
      r_ans(5) = 5.0d0
      r_ans(6) = 6.0d0
      r_ans(7) = 7.0d0
      r_ans(8) = 8.0d0
    else
      r_ans(1) = 5.0d0
      r_ans(2) = 6.0d0
      r_ans(3) = 7.0d0
      r_ans(4) = 8.0d0
      r_ans(5) = 1.0d0
      r_ans(6) = 2.0d0
      r_ans(7) = 3.0d0
      r_ans(8) = 4.0d0
    endif

    call monolis_test_check_eq_R("monolis_SendRecv_R  1", r, r_ans)

    !> case 3
    if(monolis_mpi_get_global_my_rank() == 0)then
      c(1) = (1.0d0, 1.0d0)
      c(2) = (2.0d0, 2.0d0)
      c(3) = (3.0d0, 3.0d0)
      c(4) = (4.0d0, 4.0d0)
    else
      c(1) = (5.0d0, 5.0d0)
      c(2) = (6.0d0, 6.0d0)
      c(3) = (7.0d0, 7.0d0)
      c(4) = (8.0d0, 8.0d0)
    endif

    call monolis_SendRecv_C(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
        & send_index, send_item, recv_index, recv_item, &
        & c, c, ndof, comm)

    if(monolis_mpi_get_global_my_rank() == 0)then
      c_ans(1) = (1.0d0, 1.0d0)
      c_ans(2) = (2.0d0, 2.0d0)
      c_ans(3) = (3.0d0, 3.0d0)
      c_ans(4) = (4.0d0, 4.0d0)
      c_ans(5) = (5.0d0, 5.0d0)
      c_ans(6) = (6.0d0, 6.0d0)
      c_ans(7) = (7.0d0, 7.0d0)
      c_ans(8) = (8.0d0, 8.0d0)
    else
      c_ans(1) = (5.0d0, 5.0d0)
      c_ans(2) = (6.0d0, 6.0d0)
      c_ans(3) = (7.0d0, 7.0d0)
      c_ans(4) = (8.0d0, 8.0d0)
      c_ans(5) = (1.0d0, 1.0d0)
      c_ans(6) = (2.0d0, 2.0d0)
      c_ans(7) = (3.0d0, 3.0d0)
      c_ans(8) = (4.0d0, 4.0d0)
    endif

    call monolis_test_check_eq_C("monolis_SendRecv_C  1", c, c_ans)
  end subroutine monolis_send_recv_test

  subroutine monolis_send_recv_V_test()
    implicit none
    integer(kint) :: comm, send_n_neib, send_neib_pe(1), recv_n_neib, recv_neib_pe(1)
    integer(kint) :: send_index(0:1), send_item(2), recv_index(0:1), recv_item(2)
    integer(kint) :: ndof_index(5)
    integer(kint) :: i(10), i_ans(10)
    real(kdouble) :: r(10), r_ans(10)
    complex(kdouble) :: c(10), c_ans(10)

    call monolis_std_global_log_string("monolis_SendRecvV_I")
    call monolis_std_global_log_string("monolis_SendRecvV_R")
    call monolis_std_global_log_string("monolis_SendRecvV_C")

    if(monolis_mpi_get_global_comm_size() == 1) return

    send_n_neib = 1
    recv_n_neib = 1
    send_index(0) = 0
    send_index(1) = 2
    recv_index(0) = 0
    recv_index(1) = 2
    send_item(1) = 1
    send_item(2) = 2
    recv_item(1) = 3
    recv_item(2) = 4
    comm = monolis_mpi_get_global_comm()

    if(monolis_mpi_get_global_my_rank() == 0)then
      send_neib_pe(1) = 1
      recv_neib_pe(1) = 1
      ndof_index(1) = 0
      ndof_index(2) = 2
      ndof_index(3) = 4
      ndof_index(4) = 7
      ndof_index(5) = 10
    else
      send_neib_pe(1) = 0
      recv_neib_pe(1) = 0
      ndof_index(1) = 0
      ndof_index(2) = 3
      ndof_index(3) = 6
      ndof_index(4) = 8
      ndof_index(5) = 10
    endif

    !> case 1
    if(monolis_mpi_get_global_my_rank() == 0)then
      i(1) = 1
      i(2) = 2
      i(3) = 3
      i(4) = 4
    else
      i(1) = 5
      i(2) = 6
      i(3) = 7
      i(4) = 8
      i(5) = 9
      i(6) =10
    endif

    call monolis_SendRecv_V_I(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
        & send_index, send_item, recv_index, recv_item, &
        & i, i, ndof_index, comm)

    if(monolis_mpi_get_global_my_rank() == 0)then
      i_ans(1) = 1
      i_ans(2) = 2
      i_ans(3) = 3
      i_ans(4) = 4
      i_ans(5) = 5
      i_ans(6) = 6
      i_ans(7) = 7
      i_ans(8) = 8
      i_ans(9) = 9
      i_ans(10)=10
    else
      i_ans(1) = 5
      i_ans(2) = 6
      i_ans(3) = 7
      i_ans(4) = 8
      i_ans(5) = 9
      i_ans(6) =10
      i_ans(7) = 1
      i_ans(8) = 2
      i_ans(9) = 3
      i_ans(10)= 4
    endif

    call monolis_test_check_eq_I("monolis_SendRecvV_I  1", i, i_ans)

    !> case 2
    if(monolis_mpi_get_global_my_rank() == 0)then
      r(1) = 1.0d0
      r(2) = 2.0d0
      r(3) = 3.0d0
      r(4) = 4.0d0
    else
      r(1) = 5.0d0
      r(2) = 6.0d0
      r(3) = 7.0d0
      r(4) = 8.0d0
      r(5) = 9.0d0
      r(6) =10.0d0
    endif

    call monolis_SendRecv_V_R(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
        & send_index, send_item, recv_index, recv_item, &
        & r, r, ndof_index, comm)

    if(monolis_mpi_get_global_my_rank() == 0)then
      r_ans(1) = 1.0d0
      r_ans(2) = 2.0d0
      r_ans(3) = 3.0d0
      r_ans(4) = 4.0d0
      r_ans(5) = 5.0d0
      r_ans(6) = 6.0d0
      r_ans(7) = 7.0d0
      r_ans(8) = 8.0d0
      r_ans(9) = 9.0d0
      r_ans(10)=10.0d0
    else
      r_ans(1) = 5.0d0
      r_ans(2) = 6.0d0
      r_ans(3) = 7.0d0
      r_ans(4) = 8.0d0
      r_ans(5) = 9.0d0
      r_ans(6) =10.0d0
      r_ans(7) = 1.0d0
      r_ans(8) = 2.0d0
      r_ans(9) = 3.0d0
      r_ans(10)= 4.0d0
    endif

    call monolis_test_check_eq_R("monolis_SendRecvV_R  1", r, r_ans)

    !> case 3
    if(monolis_mpi_get_global_my_rank() == 0)then
      c(1) = (1.0d0, 1.0d0)
      c(2) = (2.0d0, 2.0d0)
      c(3) = (3.0d0, 3.0d0)
      c(4) = (4.0d0, 4.0d0)
    else
      c(1) = (5.0d0, 5.0d0)
      c(2) = (6.0d0, 6.0d0)
      c(3) = (7.0d0, 7.0d0)
      c(4) = (8.0d0, 8.0d0)
      c(5) = (9.0d0, 9.0d0)
      c(6) = (10.0d0, 10.0d0)
    endif

    call monolis_SendRecv_V_C(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
        & send_index, send_item, recv_index, recv_item, &
        & c, c, ndof_index, comm)

    if(monolis_mpi_get_global_my_rank() == 0)then
      c_ans(1) = (1.0d0, 1.0d0)
      c_ans(2) = (2.0d0, 2.0d0)
      c_ans(3) = (3.0d0, 3.0d0)
      c_ans(4) = (4.0d0, 4.0d0)
      c_ans(5) = (5.0d0, 5.0d0)
      c_ans(6) = (6.0d0, 6.0d0)
      c_ans(7) = (7.0d0, 7.0d0)
      c_ans(8) = (8.0d0, 8.0d0)
      c_ans(9) = (9.0d0, 9.0d0)
      c_ans(10)= (10.0d0, 10.0d0)
    else
      c_ans(1) = (5.0d0, 5.0d0)
      c_ans(2) = (6.0d0, 6.0d0)
      c_ans(3) = (7.0d0, 7.0d0)
      c_ans(4) = (8.0d0, 8.0d0)
      c_ans(5) = (9.0d0, 9.0d0)
      c_ans(6) = (10.0d0, 10.0d0)
      c_ans(7) = (1.0d0, 1.0d0)
      c_ans(8) = (2.0d0, 2.0d0)
      c_ans(9) = (3.0d0, 3.0d0)
      c_ans(10)= (4.0d0, 4.0d0)
    endif

    call monolis_test_check_eq_C("monolis_SendRecvV_C  1", c, c_ans)
  end subroutine monolis_send_recv_V_test

  subroutine monolis_update_test()
    implicit none
    type(monolis_com) :: monoCOM
    integer(kint) :: ndof
    integer(kint) :: i(8), i_ans(8)
    real(kdouble) :: r(8), r_ans(8)
    complex(kdouble) :: c(8), c_ans(8)

    call monolis_std_global_log_string("monolis_mpi_update_I")
    call monolis_std_global_log_string("monolis_mpi_update_R")
    call monolis_std_global_log_string("monolis_mpi_update_C")
    call monolis_std_global_log_string("monolis_mpi_update_reverse_R")
    call monolis_std_global_log_string("monolis_SendRecv_reverse_R")

    if(monolis_mpi_get_global_comm_size() == 1) return

    ndof = 2
    monoCOM%comm = monolis_mpi_get_global_comm()
    monoCOM%comm_size = monolis_mpi_get_global_comm_size()
    monoCOM%send_n_neib = 1
    monoCOM%recv_n_neib = 1

    call monolis_palloc_I_1d(monoCOM%send_index, 2)
    monoCOM%send_index(1) = 0
    monoCOM%send_index(2) = 2

    call monolis_palloc_I_1d(monoCOM%recv_index, 2)
    monoCOM%recv_index(1) = 0
    monoCOM%recv_index(2) = 2

    call monolis_palloc_I_1d(monoCOM%send_item, 2)
    monoCOM%send_item(1) = 1
    monoCOM%send_item(2) = 2

    call monolis_palloc_I_1d(monoCOM%recv_item, 2)
    monoCOM%recv_item(1) = 3
    monoCOM%recv_item(2) = 4

    call monolis_palloc_I_1d(monoCOM%send_neib_pe, 1)
    call monolis_palloc_I_1d(monoCOM%recv_neib_pe, 1)
    if(monolis_mpi_get_global_my_rank() == 0)then
      monoCOM%send_neib_pe(1) = 1
      monoCOM%recv_neib_pe(1) = 1
    else
      monoCOM%send_neib_pe(1) = 0
      monoCOM%recv_neib_pe(1) = 0
    endif

    !> case 1
    i = 0
    if(monolis_mpi_get_global_my_rank() == 0)then
      i(1) = 1
      i(2) = 2
      i(3) = 3
      i(4) = 4
    else
      i(1) = 5
      i(2) = 6
      i(3) = 7
      i(4) = 8
    endif

    call monolis_mpi_update_I(monoCOM, ndof, i)

    if(monolis_mpi_get_global_my_rank() == 0)then
      i_ans(1) = 1
      i_ans(2) = 2
      i_ans(3) = 3
      i_ans(4) = 4
      i_ans(5) = 5
      i_ans(6) = 6
      i_ans(7) = 7
      i_ans(8) = 8
    else
      i_ans(1) = 5
      i_ans(2) = 6
      i_ans(3) = 7
      i_ans(4) = 8
      i_ans(5) = 1
      i_ans(6) = 2
      i_ans(7) = 3
      i_ans(8) = 4
    endif

    call monolis_test_check_eq_I("monolis_mpi_update_I  1", i, i_ans)

    !> case 2
    r = 0.0d0
    if(monolis_mpi_get_global_my_rank() == 0)then
      r(1) = 1.0d0
      r(2) = 2.0d0
      r(3) = 3.0d0
      r(4) = 4.0d0
    else
      r(1) = 5.0d0
      r(2) = 6.0d0
      r(3) = 7.0d0
      r(4) = 8.0d0
    endif

    call monolis_mpi_update_R(monoCOM, ndof, r)

    if(monolis_mpi_get_global_my_rank() == 0)then
      r_ans(1) = 1.0d0
      r_ans(2) = 2.0d0
      r_ans(3) = 3.0d0
      r_ans(4) = 4.0d0
      r_ans(5) = 5.0d0
      r_ans(6) = 6.0d0
      r_ans(7) = 7.0d0
      r_ans(8) = 8.0d0
    else
      r_ans(1) = 5.0d0
      r_ans(2) = 6.0d0
      r_ans(3) = 7.0d0
      r_ans(4) = 8.0d0
      r_ans(5) = 1.0d0
      r_ans(6) = 2.0d0
      r_ans(7) = 3.0d0
      r_ans(8) = 4.0d0
    endif

    call monolis_test_check_eq_R("monolis_mpi_update_R  1", r, r_ans)

    !> case 3
    c = 0.0d0
    if(monolis_mpi_get_global_my_rank() == 0)then
      c(1) = (1.0d0, 1.0d0)
      c(2) = (2.0d0, 2.0d0)
      c(3) = (3.0d0, 3.0d0)
      c(4) = (4.0d0, 4.0d0)
    else
      c(1) = (5.0d0, 5.0d0)
      c(2) = (6.0d0, 6.0d0)
      c(3) = (7.0d0, 7.0d0)
      c(4) = (8.0d0, 8.0d0)
    endif

    call monolis_mpi_update_C(monoCOM, ndof, c)

    if(monolis_mpi_get_global_my_rank() == 0)then
      c_ans(1) = (1.0d0, 1.0d0)
      c_ans(2) = (2.0d0, 2.0d0)
      c_ans(3) = (3.0d0, 3.0d0)
      c_ans(4) = (4.0d0, 4.0d0)
      c_ans(5) = (5.0d0, 5.0d0)
      c_ans(6) = (6.0d0, 6.0d0)
      c_ans(7) = (7.0d0, 7.0d0)
      c_ans(8) = (8.0d0, 8.0d0)
    else
      c_ans(1) = (5.0d0, 5.0d0)
      c_ans(2) = (6.0d0, 6.0d0)
      c_ans(3) = (7.0d0, 7.0d0)
      c_ans(4) = (8.0d0, 8.0d0)
      c_ans(5) = (1.0d0, 1.0d0)
      c_ans(6) = (2.0d0, 2.0d0)
      c_ans(7) = (3.0d0, 3.0d0)
      c_ans(8) = (4.0d0, 4.0d0)
    endif

    call monolis_test_check_eq_C("monolis_mpi_update_C  1", c, c_ans)

    !> case 4
    r = 0.0d0
    if(monolis_mpi_get_global_my_rank() == 0)then
      r(5) = 5.0d0
      r(6) = 6.0d0
      r(7) = 7.0d0
      r(8) = 8.0d0
    else
      r(5) = 1.0d0
      r(6) = 2.0d0
      r(7) = 3.0d0
      r(8) = 4.0d0
    endif

    call monolis_mpi_update_reverse_R(monoCOM, ndof, r)

    if(monolis_mpi_get_global_my_rank() == 0)then
      r_ans(1) = 1.0d0
      r_ans(2) = 2.0d0
      r_ans(3) = 3.0d0
      r_ans(4) = 4.0d0
      r_ans(5) = 0.0d0
      r_ans(6) = 0.0d0
      r_ans(7) = 0.0d0
      r_ans(8) = 0.0d0
    else
      r_ans(1) = 5.0d0
      r_ans(2) = 6.0d0
      r_ans(3) = 7.0d0
      r_ans(4) = 8.0d0
      r_ans(5) = 0.0d0
      r_ans(6) = 0.0d0
      r_ans(7) = 0.0d0
      r_ans(8) = 0.0d0
    endif

    call monolis_test_check_eq_R("monolis_mpi_update_reverse_R  1", r, r_ans)
  end subroutine monolis_update_test

  subroutine monolis_updateV_test()
    implicit none
    type(monolis_com) :: monoCOM
    integer(kint) :: ndof
    integer(kint) :: i(8), i_ans(8)
    real(kdouble) :: r(8), r_ans(8)
    complex(kdouble) :: c(8), c_ans(8)

    call monolis_std_global_log_string("monolis_mpi_updateV_I")
    call monolis_std_global_log_string("monolis_mpi_updateV_R")
    call monolis_std_global_log_string("monolis_mpi_updateV_C")

    if(monolis_mpi_get_global_comm_size() == 1) return

    ndof = 2
    monoCOM%comm = monolis_mpi_get_global_comm()
    monoCOM%comm_size = monolis_mpi_get_global_comm_size()
    monoCOM%send_n_neib = 1
    monoCOM%recv_n_neib = 1

    call monolis_palloc_I_1d(monoCOM%send_index, 2)
    monoCOM%send_index(1) = 0
    monoCOM%send_index(2) = 2

    call monolis_palloc_I_1d(monoCOM%recv_index, 2)
    monoCOM%recv_index(1) = 0
    monoCOM%recv_index(2) = 2

    call monolis_palloc_I_1d(monoCOM%send_item, 2)
    monoCOM%send_item(1) = 1
    monoCOM%send_item(2) = 2

    call monolis_palloc_I_1d(monoCOM%recv_item, 2)
    monoCOM%recv_item(1) = 3
    monoCOM%recv_item(2) = 4

    call monolis_palloc_I_1d(monoCOM%send_neib_pe, 1)
    call monolis_palloc_I_1d(monoCOM%recv_neib_pe, 1)
    if(monolis_mpi_get_global_my_rank() == 0)then
      monoCOM%send_neib_pe(1) = 1
      monoCOM%recv_neib_pe(1) = 1
    else
      monoCOM%send_neib_pe(1) = 0
      monoCOM%recv_neib_pe(1) = 0
    endif

    !> case 1
    i = 0
    if(monolis_mpi_get_global_my_rank() == 0)then
      i(1) = 1
      i(2) = 2
      i(3) = 3
      i(4) = 4
    else
      i(1) = 5
      i(2) = 6
      i(3) = 7
      i(4) = 8
    endif

    !call monolis_mpi_update_V_I(monoCOM, ndof, i)

    if(monolis_mpi_get_global_my_rank() == 0)then
      i_ans(1) = 1
      i_ans(2) = 2
      i_ans(3) = 3
      i_ans(4) = 4
      i_ans(5) = 5
      i_ans(6) = 6
      i_ans(7) = 7
      i_ans(8) = 8
    else
      i_ans(1) = 5
      i_ans(2) = 6
      i_ans(3) = 7
      i_ans(4) = 8
      i_ans(5) = 1
      i_ans(6) = 2
      i_ans(7) = 3
      i_ans(8) = 4
    endif

    call monolis_test_check_eq_I("monolis_mpi_updateV_I  1", i, i_ans)

    !> case 2
    r = 0.0d0
    if(monolis_mpi_get_global_my_rank() == 0)then
      r(1) = 1.0d0
      r(2) = 2.0d0
      r(3) = 3.0d0
      r(4) = 4.0d0
    else
      r(1) = 5.0d0
      r(2) = 6.0d0
      r(3) = 7.0d0
      r(4) = 8.0d0
    endif

    !call monolis_mpi_update_V_R(monoCOM, ndof, r)

    if(monolis_mpi_get_global_my_rank() == 0)then
      r_ans(1) = 1.0d0
      r_ans(2) = 2.0d0
      r_ans(3) = 3.0d0
      r_ans(4) = 4.0d0
      r_ans(5) = 5.0d0
      r_ans(6) = 6.0d0
      r_ans(7) = 7.0d0
      r_ans(8) = 8.0d0
    else
      r_ans(1) = 5.0d0
      r_ans(2) = 6.0d0
      r_ans(3) = 7.0d0
      r_ans(4) = 8.0d0
      r_ans(5) = 1.0d0
      r_ans(6) = 2.0d0
      r_ans(7) = 3.0d0
      r_ans(8) = 4.0d0
    endif

    call monolis_test_check_eq_R("monolis_mpi_updateV_R  1", r, r_ans)

    !> case 3
    c = 0.0d0
    if(monolis_mpi_get_global_my_rank() == 0)then
      c(1) = (1.0d0, 1.0d0)
      c(2) = (2.0d0, 2.0d0)
      c(3) = (3.0d0, 3.0d0)
      c(4) = (4.0d0, 4.0d0)
    else
      c(1) = (5.0d0, 5.0d0)
      c(2) = (6.0d0, 6.0d0)
      c(3) = (7.0d0, 7.0d0)
      c(4) = (8.0d0, 8.0d0)
    endif

    !call monolis_mpi_update_V_C(monoCOM, ndof, c)

    if(monolis_mpi_get_global_my_rank() == 0)then
      c_ans(1) = (1.0d0, 1.0d0)
      c_ans(2) = (2.0d0, 2.0d0)
      c_ans(3) = (3.0d0, 3.0d0)
      c_ans(4) = (4.0d0, 4.0d0)
      c_ans(5) = (5.0d0, 5.0d0)
      c_ans(6) = (6.0d0, 6.0d0)
      c_ans(7) = (7.0d0, 7.0d0)
      c_ans(8) = (8.0d0, 8.0d0)
    else
      c_ans(1) = (5.0d0, 5.0d0)
      c_ans(2) = (6.0d0, 6.0d0)
      c_ans(3) = (7.0d0, 7.0d0)
      c_ans(4) = (8.0d0, 8.0d0)
      c_ans(5) = (1.0d0, 1.0d0)
      c_ans(6) = (2.0d0, 2.0d0)
      c_ans(7) = (3.0d0, 3.0d0)
      c_ans(8) = (4.0d0, 4.0d0)
    endif

    call monolis_test_check_eq_C("monolis_mpi_updateV_C  1", c, c_ans)
  end subroutine monolis_updateV_test

end module mod_monolis_mpi_sendrecv_test
