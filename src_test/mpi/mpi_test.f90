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
    call monolis_alltoallv_test()
    call monolis_mpi_get_neib_vector_test()

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
    type(monolis_R_N128) :: d

    call monolis_std_global_log_string("monolis_allreduce_I1")
    call monolis_std_global_log_string("monolis_allreduce_R1")
    call monolis_std_global_log_string("monolis_allreduce_C1")
    call monolis_std_global_log_string("monolis_allreduce_R1_N128")

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

    !> case 4
    r = dble(monolis_mpi_get_global_my_rank() + 1.0d0)
    if(monolis_mpi_get_global_my_rank() == 1) r = 1.0d-20*r
    d = monolis_conv_R_to_R_N128(r)
    call monolis_allreduce_R1_N128(d, monolis_mpi_sum, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      call monolis_test_check_eq_R1("monolis_allreduce_R1_N128 1a", d%hi, 1.0d0)
      call monolis_test_check_eq_R1("monolis_allreduce_R1_N128 1b", d%lo, 0.0d0)
    else
      call monolis_test_check_eq_R1("monolis_allreduce_R1_N128 1pa", d%hi, 1.0d0)
      call monolis_test_check_eq_R1("monolis_allreduce_R1_N128 1pb", d%lo, 2.0d-20)
    endif

  end subroutine monolis_allreduce_x1_test

  subroutine monolis_allreduce_x_test()
    implicit none
    integer(kint) :: i(2), i_ans(2), comm
    real(kdouble) :: r(2), r_ans(2)
    complex(kdouble) :: c(2), c_ans(2)
    type(monolis_R_N128) :: d(2)

    call monolis_std_global_log_string("monolis_allreduce_I")
    call monolis_std_global_log_string("monolis_allreduce_R")
    call monolis_std_global_log_string("monolis_allreduce_C")
    call monolis_std_global_log_string("monolis_allreduce_R_N128")

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

    !> case 4
    r(1) = dble(monolis_mpi_get_global_my_rank() + 1.0d0)
    r(2) = dble(monolis_mpi_get_global_my_rank() + 2.0d0)
    if(monolis_mpi_get_global_my_rank() == 1) r(1) = 1.0d-20*r(1)
    if(monolis_mpi_get_global_my_rank() == 1) r(2) = 1.0d-20*r(2)
    d(1) = monolis_conv_R_to_R_N128(r(1))
    d(2) = monolis_conv_R_to_R_N128(r(2))
    call monolis_allreduce_R_N128(2, d, monolis_mpi_sum, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      call monolis_test_check_eq_R1("monolis_allreduce_R_N128 1a", d(1)%hi, 1.0d0)
      call monolis_test_check_eq_R1("monolis_allreduce_R_N128 1b", d(1)%lo, 0.0d0)
      call monolis_test_check_eq_R1("monolis_allreduce_R_N128 1c", d(2)%hi, 2.0d0)
      call monolis_test_check_eq_R1("monolis_allreduce_R_N128 1d", d(2)%lo, 0.0d0)
    else
      call monolis_test_check_eq_R1("monolis_allreduce_R_N128 1pa", d(1)%hi, 1.0d0)
      call monolis_test_check_eq_R1("monolis_allreduce_R_N128 1pb", d(1)%lo, 2.0d-20)
      call monolis_test_check_eq_R1("monolis_allreduce_R_N128 1pc", d(2)%hi, 2.0d0)
      call monolis_test_check_eq_R1("monolis_allreduce_R_N128 1pd", d(2)%lo, 3.0d-20)
    endif
  end subroutine monolis_allreduce_x_test

  subroutine monolis_gather_test()
    implicit none
    integer(kint) :: sc, rc(2), disp(2), root, comm
    integer(kint) :: i_sbuf(2), i_rbuf(4), i_ans(4)
    real(kdouble) :: r_sbuf(2), r_rbuf(4), r_ans(4)
    complex(kdouble) :: c_sbuf(2), c_rbuf(4), c_ans(4)

    call monolis_std_global_log_string("monolis_gather_V_I")
    call monolis_std_global_log_string("monolis_gather_V_R")
    call monolis_std_global_log_string("monolis_gather_V_C")

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

    call monolis_gather_V_I(i_sbuf, sc, i_rbuf, rc, disp, root, comm)

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

      call monolis_gather_V_I(i_sbuf, sc, i_rbuf, rc, disp, root, comm)

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

    call monolis_gather_V_R(r_sbuf, sc, r_rbuf, rc, disp, root, comm)

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

      call monolis_gather_V_R(r_sbuf, sc, r_rbuf, rc, disp, root, comm)

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

    call monolis_gather_V_C(c_sbuf, sc, c_rbuf, rc, disp, root, comm)

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

      call monolis_gather_V_C(c_sbuf, sc, c_rbuf, rc, disp, root, comm)

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

    call monolis_std_global_log_string("monolis_scatter_V_I")
    call monolis_std_global_log_string("monolis_scatter_V_R")
    call monolis_std_global_log_string("monolis_scatter_V_C")

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

    call monolis_scatter_V_I(i_sbuf, sc, disp, i_rbuf, rc, root, comm)

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

    call monolis_scatter_V_R(r_sbuf, sc, disp, r_rbuf, rc, root, comm)

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

    call monolis_scatter_V_C(c_sbuf, sc, disp, c_rbuf, rc, root, comm)

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

    call monolis_std_global_log_string("monolis_allgather_V_I")
    call monolis_std_global_log_string("monolis_allgather_V_R")
    call monolis_std_global_log_string("monolis_allgather_V_C")

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

    call monolis_allgather_V_I(sc, i_sbuf, i_rbuf, rc, disp, comm)

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

    call monolis_allgather_V_R(sc, r_sbuf, r_rbuf, rc, disp, comm)

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

    call monolis_allgather_V_C(sc, c_sbuf, c_rbuf, rc, disp, comm)

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

  subroutine monolis_alltoallv_test()
    implicit none
    integer(kint) :: comm, comm_size
    integer(kint) :: scounts(2), sdispls(2), rcounts(2), rdispls(2)
    integer(kint) :: sbuf_i(3), rbuf_i(4)
    real(kdouble) :: sbuf_r(3), rbuf_r(4)

    call monolis_std_global_log_string("monolis_alltoallv_I")
    call monolis_std_global_log_string("monolis_alltoallv_R")

    if(monolis_mpi_get_global_comm_size() == 1) return

    comm = monolis_mpi_get_global_comm()
    comm_size = monolis_mpi_get_global_comm_size()

    scounts(1) = 1
    scounts(2) = 2

    sdispls(1) = 0
    sdispls(2) = 1

    if(monolis_mpi_get_global_my_rank() == 0)then
      rcounts(1) = 1
      rcounts(2) = 1

      rdispls(1) = 0
      rdispls(2) = 1
    else
      rcounts(1) = 2
      rcounts(2) = 2

      rdispls(1) = 0
      rdispls(2) = 2
    endif

    sbuf_i(1) = 10*monolis_mpi_get_global_my_rank() + 1
    sbuf_i(2) = 10*monolis_mpi_get_global_my_rank() + 2
    sbuf_i(3) = 10*monolis_mpi_get_global_my_rank() + 3

     call monolis_alltoall_V_I(sbuf_i, scounts, sdispls, rbuf_i, rcounts, rdispls, comm)

    if(monolis_mpi_get_global_my_rank() == 0)then
      call monolis_test_check_eq_I1("monolis_alltoallv_I 1a", rbuf_i(1), 1)
      call monolis_test_check_eq_I1("monolis_alltoallv_I 2a", rbuf_i(2), 11)
    else
      call monolis_test_check_eq_I1("monolis_alltoallv_I 1b", rbuf_i(1), 2)
      call monolis_test_check_eq_I1("monolis_alltoallv_I 2b", rbuf_i(2), 3)
      call monolis_test_check_eq_I1("monolis_alltoallv_I 3b", rbuf_i(3), 12)
      call monolis_test_check_eq_I1("monolis_alltoallv_I 4b", rbuf_i(4), 13)
    endif

    sbuf_r(1) = 10.0d0*monolis_mpi_get_global_my_rank() + 1.0d0
    sbuf_r(2) = 10.0d0*monolis_mpi_get_global_my_rank() + 2.0d0
    sbuf_r(3) = 10.0d0*monolis_mpi_get_global_my_rank() + 3.0d0

    call monolis_alltoall_V_R(sbuf_r, scounts, sdispls, rbuf_r, rcounts, rdispls, comm)

    if(monolis_mpi_get_global_my_rank() == 0)then
      call monolis_test_check_eq_R1("monolis_alltoallv_R 1a", rbuf_r(1), 1.0d0)
      call monolis_test_check_eq_R1("monolis_alltoallv_R 2a", rbuf_r(2), 11.0d0)
    else
      call monolis_test_check_eq_R1("monolis_alltoallv_R 1b", rbuf_r(1), 2.0d0)
      call monolis_test_check_eq_R1("monolis_alltoallv_R 2b", rbuf_r(2), 3.0d0)
      call monolis_test_check_eq_R1("monolis_alltoallv_R 3b", rbuf_r(3), 12.0d0)
      call monolis_test_check_eq_R1("monolis_alltoallv_R 4b", rbuf_r(4), 13.0d0)
    endif
  end subroutine monolis_alltoallv_test

  subroutine monolis_mpi_get_neib_vector_test()
    implicit none
    type(monolis_com) :: monoCOM
    integer(kint) :: ndof, n_vec, n_neib_vec
    real(kdouble) :: my_vec1(8,1), my_vec2(8,2), neib_vec(8,3), r_ans(8,3)

    call monolis_std_global_log_string("monolis_mpi_get_n_neib_vector")
    call monolis_std_global_log_string("monolis_mpi_get_neib_vector_R")

    if(monolis_mpi_get_global_comm_size() == 1) return

    ndof = 2
    monoCOM%comm = monolis_mpi_get_global_comm()
    monoCOM%comm_size = monolis_mpi_get_global_comm_size()
    monoCOM%n_internal_vertex = 2
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

    my_vec1 = 0.0d0
    my_vec2 = 0.0d0
    if(monolis_mpi_get_global_my_rank() == 0)then
      n_vec = 1
      my_vec1(1,1) = 1.0d0
      my_vec1(2,1) = 2.0d0
      my_vec1(3,1) = 3.0d0
      my_vec1(4,1) = 4.0d0
    else
      n_vec = 2
      my_vec2(1,1) = 5.0d0
      my_vec2(2,1) = 6.0d0
      my_vec2(3,1) = 7.0d0
      my_vec2(4,1) = 8.0d0
      my_vec2(1,2) =15.0d0
      my_vec2(2,2) =16.0d0
      my_vec2(3,2) =17.0d0
      my_vec2(4,2) =18.0d0
    endif

    if(monolis_mpi_get_global_my_rank() == 0)then
      call monolis_mpi_get_n_neib_vector(monoCOM, n_vec, n_neib_vec)
      call monolis_test_check_eq_I1("monolis_mpi_get_n_neib_vector 1", n_neib_vec, 3)

      call monolis_mpi_get_neib_vector_R(monoCOM, n_vec, ndof, my_vec1, neib_vec)

      r_ans(1,1) = 1.0d0; r_ans(1,2) = 0.0d0; r_ans(1,3) = 0.0d0
      r_ans(2,1) = 2.0d0; r_ans(2,2) = 0.0d0; r_ans(2,3) = 0.0d0
      r_ans(3,1) = 3.0d0; r_ans(3,2) = 0.0d0; r_ans(3,3) = 0.0d0
      r_ans(4,1) = 4.0d0; r_ans(4,2) = 0.0d0; r_ans(4,3) = 0.0d0
      r_ans(5,1) = 0.0d0; r_ans(5,2) = 5.0d0; r_ans(5,3) =15.0d0
      r_ans(6,1) = 0.0d0; r_ans(6,2) = 6.0d0; r_ans(6,3) =16.0d0
      r_ans(7,1) = 0.0d0; r_ans(7,2) = 7.0d0; r_ans(7,3) =17.0d0
      r_ans(8,1) = 0.0d0; r_ans(8,2) = 8.0d0; r_ans(8,3) =18.0d0

      call monolis_test_check_eq_R("monolis_mpi_update_R 1", neib_vec(:,1), r_ans(:,1))
      call monolis_test_check_eq_R("monolis_mpi_update_R 2", neib_vec(:,2), r_ans(:,2))
      call monolis_test_check_eq_R("monolis_mpi_update_R 3", neib_vec(:,3), r_ans(:,3))
    else
      call monolis_mpi_get_n_neib_vector(monoCOM, n_vec, n_neib_vec)
      call monolis_test_check_eq_I1("monolis_mpi_get_n_neib_vector 1", n_neib_vec, 3)

      call monolis_mpi_get_neib_vector_R(monoCOM, n_vec, ndof, my_vec2, neib_vec)

      r_ans(1,1) = 5.0d0; r_ans(1,2) =15.0d0; r_ans(1,3) = 0.0d0
      r_ans(2,1) = 6.0d0; r_ans(2,2) =16.0d0; r_ans(2,3) = 0.0d0
      r_ans(3,1) = 7.0d0; r_ans(3,2) =17.0d0; r_ans(3,3) = 0.0d0
      r_ans(4,1) = 8.0d0; r_ans(4,2) =18.0d0; r_ans(4,3) = 0.0d0
      r_ans(5,1) = 0.0d0; r_ans(5,2) = 0.0d0; r_ans(5,3) = 1.0d0
      r_ans(6,1) = 0.0d0; r_ans(6,2) = 0.0d0; r_ans(6,3) = 2.0d0
      r_ans(7,1) = 0.0d0; r_ans(7,2) = 0.0d0; r_ans(7,3) = 3.0d0
      r_ans(8,1) = 0.0d0; r_ans(8,2) = 0.0d0; r_ans(8,3) = 4.0d0

      call monolis_test_check_eq_R("monolis_mpi_update_R 1", neib_vec(:,1), r_ans(:,1))
      call monolis_test_check_eq_R("monolis_mpi_update_R 2", neib_vec(:,2), r_ans(:,2))
      call monolis_test_check_eq_R("monolis_mpi_update_R 3", neib_vec(:,3), r_ans(:,3))
    endif
  end subroutine monolis_mpi_get_neib_vector_test

end module mod_monolis_mpi_test
