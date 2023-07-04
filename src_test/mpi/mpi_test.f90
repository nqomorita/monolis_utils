!> MPI テストモジュール
module mod_monolis_mpi_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_mpi_test()
    implicit none

    call monolis_allreduce_x1_test()
    call monolis_allreduce_x_test()
    call monolis_gather_test()
    call monolis_scatterv_test()
    call monolis_allgather_test()
    call monolis_allgather_1_test()
    call monolis_alltoall_1_test()
    call monolis_send_recv_test()
    call monolis_update_test()

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
  end subroutine monolis_mpi_test

  !> unit test
  subroutine monolis_allreduce_x1_test()
    implicit none
    integer(kint) :: i, comm
    real(kdouble) :: r
    complex(kdouble) :: c

    call monolis_std_global_log_string("monolis_allreduce_I1")
    call monolis_std_global_log_string("monolis_allreduce_R1")
    call monolis_std_global_log_string("monolis_allreduce_C1")

    comm = monolis_mpi_get_global_comm()

    !> case 1
    i = monolis_mpi_get_global_my_rank() + 1
    call monolis_allreduce_I1(i, monolis_mpi_sum, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      call monolis_test_check_eq_I1("monolis_allreduce_I1  1", i, 1)
    else
      call monolis_test_check_eq_I1("monolis_allreduce_I1  1", i, 3)
    endif

    i = monolis_mpi_get_global_my_rank() + 1
    call monolis_allreduce_I1(i, monolis_mpi_max, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      call monolis_test_check_eq_I1("monolis_allreduce_I1  1", i, 1)
    else
      call monolis_test_check_eq_I1("monolis_allreduce_I1  2", i, 2)
    endif

    i = monolis_mpi_get_global_my_rank() + 1
    call monolis_allreduce_I1(i, monolis_mpi_min, comm)
    call monolis_test_check_eq_I1("monolis_allreduce_I1  3", i, 1)

    !> case 2
    r = dble(monolis_mpi_get_global_my_rank() + 1.0d0)
    call monolis_allreduce_R1(r, monolis_mpi_sum, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      call monolis_test_check_eq_R1("monolis_allreduce_R1  1", r, 1.0d0)
    else
      call monolis_test_check_eq_R1("monolis_allreduce_R1  1", r, 3.0d0)
    endif

    r = dble(monolis_mpi_get_global_my_rank() + 1.0d0)
    call monolis_allreduce_R1(r, monolis_mpi_max, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      call monolis_test_check_eq_R1("monolis_allreduce_R1  1", r, 1.0d0)
    else
      call monolis_test_check_eq_R1("monolis_allreduce_R1  2", r, 2.0d0)
    endif

    r = dble(monolis_mpi_get_global_my_rank() + 1.0d0)
    call monolis_allreduce_R1(r, monolis_mpi_min, comm)
    call monolis_test_check_eq_R1("monolis_allreduce_R1  3", r, 1.0d0)

    !> case 3
    r = dble(monolis_mpi_get_global_my_rank() + 1.0d0)
    c = cmplx(r, r)
    call monolis_allreduce_C1(c, monolis_mpi_sum, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      call monolis_test_check_eq_C1("monolis_allreduce_C1  1", c, (1.0d0, 1.0d0))
    else
      call monolis_test_check_eq_C1("monolis_allreduce_C1  1", c, (3.0d0, 3.0d0))
    endif
  end subroutine monolis_allreduce_x1_test

  subroutine monolis_allreduce_x_test()
    implicit none
    integer(kint) :: i(2), i_ans(2), comm
    real(kdouble) :: r(2), r_ans(2)
    complex(kdouble) :: c(2), c_ans(2)

    call monolis_std_global_log_string("monolis_allreduce_I")
    call monolis_std_global_log_string("monolis_allreduce_R")
    call monolis_std_global_log_string("monolis_allreduce_C")

    comm = monolis_mpi_get_global_comm()

    !> case 1
    i(1) = 2*monolis_mpi_get_global_my_rank() + 1
    i(2) = 2*monolis_mpi_get_global_my_rank() + 2
    call monolis_allreduce_I(2, i, monolis_mpi_sum, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      i_ans(1) = 1
      i_ans(2) = 2
    else
      i_ans(1) = 4
      i_ans(2) = 6
    endif

    call monolis_test_check_eq_I("monolis_allreduce_I  1", i, i_ans)

    i(1) = 2*monolis_mpi_get_global_my_rank() + 1
    i(2) = 2*monolis_mpi_get_global_my_rank() + 2
    call monolis_allreduce_I(2, i, monolis_mpi_max, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      i_ans(1) = 1
      i_ans(2) = 2
    else
      i_ans(1) = 3
      i_ans(2) = 4
    endif

    call monolis_test_check_eq_I("monolis_allreduce_I  2", i, i_ans)

    i(1) = 2*monolis_mpi_get_global_my_rank() + 1
    i(2) = 2*monolis_mpi_get_global_my_rank() + 2
    call monolis_allreduce_I(2, i, monolis_mpi_min, comm)

    i_ans(1) = 1
    i_ans(2) = 2
    call monolis_test_check_eq_I("monolis_allreduce_I  3", i, i_ans)

    !> case 2
    r(1) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 1.0d0
    r(2) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 2.0d0
    call monolis_allreduce_R(2, r, monolis_mpi_sum, comm)


    if(monolis_mpi_get_global_comm_size() == 1)then
      r_ans(1) = 1.0d0
      r_ans(2) = 2.0d0
    else
      r_ans(1) = 4.0d0
      r_ans(2) = 6.0d0
    endif

    call monolis_test_check_eq_R("monolis_allreduce_R  1", r, r_ans)

    r(1) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 1.0d0
    r(2) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 2.0d0
    call monolis_allreduce_R(2, r, monolis_mpi_max, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      r_ans(1) = 1.0d0
      r_ans(2) = 2.0d0
    else
      r_ans(1) = 3.0d0
      r_ans(2) = 4.0d0
    endif

    call monolis_test_check_eq_R("monolis_allreduce_R  2", r, r_ans)

    r(1) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 1.0d0
    r(2) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 2.0d0
    call monolis_allreduce_R(2, r, monolis_mpi_min, comm)

    r_ans(1) = 1.0d0
    r_ans(2) = 2.0d0
    call monolis_test_check_eq_R("monolis_allreduce_R  3", r, r_ans)

    !> case 3
    r(1) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 1.0d0
    r(2) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 2.0d0
    c(1) = cmplx(r(1), r(1))
    c(2) = cmplx(r(2), r(2))
    call monolis_allreduce_C(2, c, monolis_mpi_sum, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      c_ans(1) = (1.0d0, 1.0d0)
      c_ans(2) = (2.0d0, 2.0d0)
    else
      c_ans(1) = (4.0d0, 4.0d0)
      c_ans(2) = (6.0d0, 6.0d0)
    endif

    call monolis_test_check_eq_C("monolis_allreduce_C  1", c, c_ans)
  end subroutine monolis_allreduce_x_test

  subroutine monolis_gather_test()
    implicit none
    integer(kint) :: sc, rc(2), disp(2), root, comm
    integer(kint) :: i_sbuf(2), i_rbuf(4), i_ans(4)
    real(kdouble) :: r_sbuf(2), r_rbuf(4), r_ans(4)
    complex(kdouble) :: c_sbuf(2), c_rbuf(4), c_ans(4)

    call monolis_std_global_log_string("monolis_gatherv_I")
    call monolis_std_global_log_string("monolis_gatherv_R")
    call monolis_std_global_log_string("monolis_gatherv_C")

    comm = monolis_mpi_get_global_comm()

    !> case 1
    root = 0
    sc = 2
    rc(1) = 2
    rc(2) = 2
    i_sbuf(1) = 2*monolis_mpi_get_global_my_rank() + 1
    i_sbuf(2) = 2*monolis_mpi_get_global_my_rank() + 2
    disp(1) = 0
    disp(2) = 2
    i_rbuf = 0

    call monolis_gatherv_I(i_sbuf, sc, i_rbuf, rc, disp, root, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      i_ans(1) = 1
      i_ans(2) = 2
      i_ans(3) = 0
      i_ans(4) = 0
    else
      i_ans(1) = 1
      i_ans(2) = 2
      i_ans(3) = 3
      i_ans(4) = 4
    endif

    if(monolis_mpi_get_global_my_rank() == 0)then
      call monolis_test_check_eq_I("monolis_gatherv_I  1", i_rbuf, i_ans)
    endif

    if(monolis_mpi_get_global_comm_size() == 2)then
      root = 1
      sc = 2
      rc(1) = 2
      rc(2) = 2
      i_sbuf(1) = 2*monolis_mpi_get_global_my_rank() + 1
      i_sbuf(2) = 2*monolis_mpi_get_global_my_rank() + 2
      disp(1) = 0
      disp(2) = 2
      i_rbuf = 0

      call monolis_gatherv_I(i_sbuf, sc, i_rbuf, rc, disp, root, comm)

      i_ans(1) = 1
      i_ans(2) = 2
      i_ans(3) = 3
      i_ans(4) = 4

      if(monolis_mpi_get_global_my_rank() == 1)then
        call monolis_test_check_eq_I("monolis_gatherv_I  2", i_rbuf, i_ans)
      endif
    endif

    !> case 2
    root = 0
    sc = 2
    rc(1) = 2
    rc(2) = 2
    r_sbuf(1) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 1.0d0
    r_sbuf(2) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 2.0d0
    disp(1) = 0
    disp(2) = 2
    r_rbuf = 0.0d0

    call monolis_gatherv_R(r_sbuf, sc, r_rbuf, rc, disp, root, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      r_ans(1) = 1.0d0
      r_ans(2) = 2.0d0
      r_ans(3) = 0.0d0
      r_ans(4) = 0.0d0
    else
      r_ans(1) = 1.0d0
      r_ans(2) = 2.0d0
      r_ans(3) = 3.0d0
      r_ans(4) = 4.0d0
    endif

    if(monolis_mpi_get_global_my_rank() == 0)then
      call monolis_test_check_eq_R("monolis_gatherv_R  1", r_rbuf, r_ans)
    endif

    if(monolis_mpi_get_global_comm_size() == 2)then
      root = 1
      sc = 2
      rc(1) = 2
      rc(2) = 2
      r_sbuf(1) = 2*dble(monolis_mpi_get_global_my_rank()) + 1.0d0
      r_sbuf(2) = 2*dble(monolis_mpi_get_global_my_rank()) + 2.0d0
      disp(1) = 0
      disp(2) = 2
      r_rbuf = 0.0d0

      call monolis_gatherv_R(r_sbuf, sc, r_rbuf, rc, disp, root, comm)

      r_ans(1) = 1.0d0
      r_ans(2) = 2.0d0
      r_ans(3) = 3.0d0
      r_ans(4) = 4.0d0

      if(monolis_mpi_get_global_my_rank() == 1)then
        call monolis_test_check_eq_R("monolis_gatherv_R  2", r_rbuf, r_ans)
      endif
    endif

    !> case 3
    root = 0
    sc = 2
    rc(1) = 2
    rc(2) = 2
    r_sbuf(1) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 1.0d0
    r_sbuf(2) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 2.0d0
    c_sbuf(1) = cmplx(r_sbuf(1), r_sbuf(1))
    c_sbuf(2) = cmplx(r_sbuf(2), r_sbuf(2))
    disp(1) = 0
    disp(2) = 2
    c_rbuf = (0.0d0, 0.0d0)

    call monolis_gatherv_C(c_sbuf, sc, c_rbuf, rc, disp, root, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      c_ans(1) = (1.0d0, 1.0d0)
      c_ans(2) = (2.0d0, 2.0d0)
      c_ans(3) = (0.0d0, 0.0d0)
      c_ans(4) = (0.0d0, 0.0d0)
    else
      c_ans(1) = (1.0d0, 1.0d0)
      c_ans(2) = (2.0d0, 2.0d0)
      c_ans(3) = (3.0d0, 3.0d0)
      c_ans(4) = (4.0d0, 4.0d0)
    endif

    if(monolis_mpi_get_global_my_rank() == 0)then
      call monolis_test_check_eq_C("monolis_gatherv_C  1", c_rbuf, c_ans)
    endif

    if(monolis_mpi_get_global_comm_size() == 2)then
      root = 1
      sc = 2
      rc(1) = 2
      rc(2) = 2
      r_sbuf(1) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 1.0d0
      r_sbuf(2) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 2.0d0
      c_sbuf(1) = cmplx(r_sbuf(1), r_sbuf(1))
      c_sbuf(2) = cmplx(r_sbuf(2), r_sbuf(2))
      disp(1) = 0
      disp(2) = 2
      c_ans = (0.0d0, 0.0d0)

      call monolis_gatherv_C(c_sbuf, sc, c_rbuf, rc, disp, root, comm)

      c_ans(1) = (1.0d0, 1.0d0)
      c_ans(2) = (2.0d0, 2.0d0)
      c_ans(3) = (3.0d0, 3.0d0)
      c_ans(4) = (4.0d0, 4.0d0)

      if(monolis_mpi_get_global_my_rank() == 1)then
        call monolis_test_check_eq_C("monolis_gatherv_C  2", c_rbuf, c_ans)
      endif
    endif
  end subroutine monolis_gather_test

  subroutine monolis_scatterv_test()
    implicit none
    integer(kint) :: sc(2), rc, disp(2), root, comm
    integer(kint) :: i_sbuf(4), i_rbuf(2), i_ans(2)
    real(kdouble) :: r_sbuf(4), r_rbuf(2), r_ans(2)
    complex(kdouble) :: c_sbuf(4), c_rbuf(2), c_ans(2)

    call monolis_std_global_log_string("monolis_scatterv_I")
    call monolis_std_global_log_string("monolis_scatterv_R")
    call monolis_std_global_log_string("monolis_scatterv_C")

    comm = monolis_mpi_get_global_comm()

    !> case 1
    root = 0
    sc(1) = 2
    sc(2) = 2
    rc = 2
    i_sbuf(1) = 1
    i_sbuf(2) = 2
    i_sbuf(3) = 3
    i_sbuf(4) = 4
    disp(1) = 0
    disp(2) = 2

    call monolis_scatterv_I(i_sbuf, sc, disp, i_rbuf, rc, root, comm)

    i_ans(1) = 2*monolis_mpi_get_global_my_rank() + 1
    i_ans(2) = 2*monolis_mpi_get_global_my_rank() + 2

    call monolis_test_check_eq_I("monolis_scatterv_I  1", i_rbuf, i_ans)

    !> case 2
    root = 0
    sc(1) = 2
    sc(2) = 2
    rc = 2
    r_sbuf(1) = 1
    r_sbuf(2) = 2
    r_sbuf(3) = 3
    r_sbuf(4) = 4
    disp(1) = 0
    disp(2) = 2

    call monolis_scatterv_R(r_sbuf, sc, disp, r_rbuf, rc, root, comm)

    r_ans(1) = 2.0d0*monolis_mpi_get_global_my_rank() + 1.0d0
    r_ans(2) = 2.0d0*monolis_mpi_get_global_my_rank() + 2.0d0

    call monolis_test_check_eq_R("monolis_scatterv_R  1", r_rbuf, r_ans)

    !> case 3
    root = 0
    sc(1) = 2
    sc(2) = 2
    rc = 2
    c_sbuf(1) = (1.0d0, 1.0d0)
    c_sbuf(2) = (2.0d0, 2.0d0)
    c_sbuf(3) = (3.0d0, 3.0d0)
    c_sbuf(4) = (4.0d0, 4.0d0)
    disp(1) = 0
    disp(2) = 2

    call monolis_scatterv_C(c_sbuf, sc, disp, c_rbuf, rc, root, comm)

    r_ans(1) = 2.0d0*monolis_mpi_get_global_my_rank() + 1.0d0
    r_ans(2) = 2.0d0*monolis_mpi_get_global_my_rank() + 2.0d0
    c_ans(1) = cmplx(r_ans(1), r_ans(1))
    c_ans(2) = cmplx(r_ans(2), r_ans(2))

    call monolis_test_check_eq_C("monolis_scatterv_C  1", c_rbuf, c_ans)
  end subroutine monolis_scatterv_test

  subroutine monolis_allgather_1_test()
    implicit none
    integer(kint) :: comm
    integer(kint) :: i_sbuf, i_rbuf(2), i_ans(2)
    real(kdouble) :: r_sbuf, r_rbuf(2), r_ans(2)
    complex(kdouble) :: c_sbuf, c_rbuf(2), c_ans(2)

    call monolis_std_global_log_string("monolis_allgather_I1")
    call monolis_std_global_log_string("monolis_allgather_R1")
    call monolis_std_global_log_string("monolis_allgather_C1")

    comm = monolis_mpi_get_global_comm()

    !> case 1
    i_sbuf = monolis_mpi_get_global_my_rank() + 1
    i_rbuf = 0

    call monolis_allgather_I1(i_sbuf, i_rbuf, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      i_ans(1) = 1
      i_ans(2) = 0
    else
      i_ans(1) = 1
      i_ans(2) = 2
    endif

    call monolis_test_check_eq_I("monolis_allgather_I1  1", i_rbuf, i_ans)

    !> case 2
    r_sbuf = monolis_mpi_get_global_my_rank() + 1
    r_rbuf = 0.0d0

    call monolis_allgather_R1(r_sbuf, r_rbuf, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      r_ans(1) = 1.0d0
      r_ans(2) = 0.0d0
    else
      r_ans(1) = 1.0d0
      r_ans(2) = 2.0d0
    endif

    call monolis_test_check_eq_R("monolis_allgather_R1  1", r_rbuf, r_ans)

    !> case 3
    r_sbuf = monolis_mpi_get_global_my_rank() + 1
    c_sbuf = cmplx(r_sbuf, r_sbuf)
    c_rbuf = (0.0d0, 0.0d0)

    call monolis_allgather_C1(c_sbuf, c_rbuf, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      c_ans(1) = (1.0d0, 1.0d0)
      c_ans(2) = (0.0d0, 0.0d0)
    else
      c_ans(1) = (1.0d0, 1.0d0)
      c_ans(2) = (2.0d0, 2.0d0)
    endif

    call monolis_test_check_eq_C("monolis_allgather_C1  1", c_rbuf, c_ans)
  end subroutine monolis_allgather_1_test

  subroutine monolis_allgather_test()
    implicit none
    integer(kint) :: sc, rc(2), disp(2), comm
    integer(kint) :: i_sbuf(2), i_rbuf(4), i_ans(4)
    real(kdouble) :: r_sbuf(2), r_rbuf(4), r_ans(4)
    complex(kdouble) :: c_sbuf(2), c_rbuf(4), c_ans(4)

    call monolis_std_global_log_string("monolis_allgatherv_I")
    call monolis_std_global_log_string("monolis_allgatherv_R")
    call monolis_std_global_log_string("monolis_allgatherv_C")

    comm = monolis_mpi_get_global_comm()

    !> case 1
    sc = 2
    rc(1) = 2
    rc(2) = 2
    i_sbuf(1) = 2*monolis_mpi_get_global_my_rank() + 1
    i_sbuf(2) = 2*monolis_mpi_get_global_my_rank() + 2
    disp(1) = 0
    disp(2) = 2
    i_rbuf = 0

    call monolis_allgatherv_I(sc, i_sbuf, i_rbuf, rc, disp, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      i_ans(1) = 1
      i_ans(2) = 2
      i_ans(3) = 0
      i_ans(4) = 0
    else
      i_ans(1) = 1
      i_ans(2) = 2
      i_ans(3) = 3
      i_ans(4) = 4
    endif

    call monolis_test_check_eq_I("monolis_allgatherv_I  1", i_rbuf, i_ans)

    !> case 2
    sc = 2
    rc(1) = 2
    rc(2) = 2
    r_sbuf(1) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 1.0d0
    r_sbuf(2) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 2.0d0
    disp(1) = 0
    disp(2) = 2
    r_rbuf = 0.0d0

    call monolis_allgatherv_R(sc, r_sbuf, r_rbuf, rc, disp, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      r_ans(1) = 1.0d0
      r_ans(2) = 2.0d0
      r_ans(3) = 0.0d0
      r_ans(4) = 0.0d0
    else
      r_ans(1) = 1.0d0
      r_ans(2) = 2.0d0
      r_ans(3) = 3.0d0
      r_ans(4) = 4.0d0
    endif

    call monolis_test_check_eq_R("monolis_allgatherv_R  1", r_rbuf, r_ans)

    !>  3
    sc = 2
    rc(1) = 2
    rc(2) = 2
    r_sbuf(1) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 1.0d0
    r_sbuf(2) = 2.0d0*dble(monolis_mpi_get_global_my_rank()) + 2.0d0
    c_sbuf(1) = cmplx(r_sbuf(1), r_sbuf(1))
    c_sbuf(2) = cmplx(r_sbuf(2), r_sbuf(2))
    disp(1) = 0
    disp(2) = 2
    c_rbuf = (0.0d0, 0.0d0)

    call monolis_allgatherv_C(sc, c_sbuf, c_rbuf, rc, disp, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      c_ans(1) = (1.0d0, 1.0d0)
      c_ans(2) = (2.0d0, 2.0d0)
      c_ans(3) = (0.0d0, 0.0d0)
      c_ans(4) = (0.0d0, 0.0d0)
    else
      c_ans(1) = (1.0d0, 1.0d0)
      c_ans(2) = (2.0d0, 2.0d0)
      c_ans(3) = (3.0d0, 3.0d0)
      c_ans(4) = (4.0d0, 4.0d0)
    endif

    call monolis_test_check_eq_C("monolis_allgatherv_C  1", c_rbuf, c_ans)
  end subroutine monolis_allgather_test

  subroutine monolis_alltoall_1_test()
    implicit none
    integer(kint) :: comm, comm_size, sbuf(2), i_ans(2)

    call monolis_std_global_log_string("monolis_alltoall_I1")

    if(monolis_mpi_get_global_comm_size() == 1) return

    comm = monolis_mpi_get_global_comm()
    comm_size = monolis_mpi_get_global_comm_size()

    sbuf = monolis_mpi_get_global_my_rank() + 1

    call monolis_alltoall_I1(comm_size, sbuf, comm)

    i_ans(1) = 1
    i_ans(2) = 2

    call monolis_test_check_eq_I("monolis_alltoall_I1  1", sbuf, i_ans)
  end subroutine monolis_alltoall_1_test

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

    if(monolis_mpi_get_global_comm_size() == 1) return

    ndof = 2
    monoCOM%comm = monolis_mpi_get_global_comm()
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
  end subroutine monolis_update_test
end module mod_monolis_mpi_test
