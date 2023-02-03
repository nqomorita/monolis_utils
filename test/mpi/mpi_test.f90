!> MPI テストモジュール
module mod_monolis_mpi_test
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_test
  use mod_monolis_mpi
  use mod_monolis_mpi_util
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
    call monolis_send_recv_test()
  end subroutine monolis_mpi_test

  !> unit test
  subroutine monolis_allreduce_x1_test()
    implicit none
    integer(kint) :: i, comm
    real(kdouble) :: r
    complex(kdouble) :: c

    comm = monolis_mpi_global_comm()

    !> case 1
    i = monolis_mpi_global_my_rank() + 1
    call monolis_allreduce_I1(i, monolis_mpi_sum, comm)
    call monolis_test_check_eq_I1("monolis_allreduce_I1 case 1", i, 3)

    i = monolis_mpi_global_my_rank() + 1
    call monolis_allreduce_I1(i, monolis_mpi_max, comm)
    call monolis_test_check_eq_I1("monolis_allreduce_I1 case 2", i, 2)

    i = monolis_mpi_global_my_rank() + 1
    call monolis_allreduce_I1(i, monolis_mpi_min, comm)
    call monolis_test_check_eq_I1("monolis_allreduce_I1 case 3", i, 1)

    !> case 2
    r = dble(monolis_mpi_global_my_rank() + 1.0d0)
    call monolis_allreduce_R1(r, monolis_mpi_sum, comm)
    call monolis_test_check_eq_R1("monolis_allreduce_R1 case 1", r, 3.0d0)

    r = dble(monolis_mpi_global_my_rank() + 1.0d0)
    call monolis_allreduce_R1(r, monolis_mpi_max, comm)
    call monolis_test_check_eq_R1("monolis_allreduce_R1 case 2", r, 2.0d0)

    r = dble(monolis_mpi_global_my_rank() + 1.0d0)
    call monolis_allreduce_R1(r, monolis_mpi_min, comm)
    call monolis_test_check_eq_R1("monolis_allreduce_R1 case 3", r, 1.0d0)

    !> case 3
    r = dble(monolis_mpi_global_my_rank() + 1.0d0)
    c = complex(r, r)
    call monolis_allreduce_C1(c, monolis_mpi_sum, comm)
    call monolis_test_check_eq_C1("monolis_allreduce_C1 case 1", c, (3.0d0, 3.0d0))
  end subroutine monolis_allreduce_x1_test

  subroutine monolis_allreduce_x_test()
    implicit none
    integer(kint) :: i(2), i_ans(2), comm
    real(kdouble) :: r(2), r_ans(2)
    complex(kdouble) :: c(2), c_ans(2)

    comm = monolis_mpi_global_comm()

    !> case 1
    i(1) = 2*monolis_mpi_global_my_rank() + 1
    i(2) = 2*monolis_mpi_global_my_rank() + 2
    call monolis_allreduce_I(2, i, monolis_mpi_sum, comm)

    i_ans(1) = 4
    i_ans(2) = 6
    call monolis_test_check_eq_I("monolis_allreduce_I case 1", i, i_ans)

    i(1) = 2*monolis_mpi_global_my_rank() + 1
    i(2) = 2*monolis_mpi_global_my_rank() + 2
    call monolis_allreduce_I(2, i, monolis_mpi_max, comm)

    i_ans(1) = 3
    i_ans(2) = 4
    call monolis_test_check_eq_I("monolis_allreduce_I case 2", i, i_ans)

    i(1) = 2*monolis_mpi_global_my_rank() + 1
    i(2) = 2*monolis_mpi_global_my_rank() + 2
    call monolis_allreduce_I(2, i, monolis_mpi_min, comm)

    i_ans(1) = 1
    i_ans(2) = 2
    call monolis_test_check_eq_I("monolis_allreduce_I case 3", i, i_ans)

    !> case 2
    r(1) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 1.0d0
    r(2) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 2.0d0
    call monolis_allreduce_R(2, r, monolis_mpi_sum, comm)

    r_ans(1) = 4.0d0
    r_ans(2) = 6.0d0
    call monolis_test_check_eq_R("monolis_allreduce_R case 1", r, r_ans)

    r(1) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 1.0d0
    r(2) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 2.0d0
    call monolis_allreduce_R(2, r, monolis_mpi_max, comm)

    r_ans(1) = 3.0d0
    r_ans(2) = 4.0d0
    call monolis_test_check_eq_R("monolis_allreduce_R case 2", r, r_ans)

    r(1) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 1.0d0
    r(2) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 2.0d0
    call monolis_allreduce_R(2, r, monolis_mpi_min, comm)

    r_ans(1) = 1.0d0
    r_ans(2) = 2.0d0
    call monolis_test_check_eq_R("monolis_allreduce_R case 3", r, r_ans)

    !> case 3
    r(1) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 1.0d0
    r(2) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 2.0d0
    c(1) = complex(r(1), r(1))
    c(2) = complex(r(2), r(2))
    call monolis_allreduce_C(2, c, monolis_mpi_sum, comm)

    c_ans(1) = (4.0d0, 4.0d0)
    c_ans(2) = (6.0d0, 6.0d0)
    call monolis_test_check_eq_C("monolis_allreduce_C case 1", c, c_ans)
  end subroutine monolis_allreduce_x_test

  subroutine monolis_gather_test()
    implicit none
    integer(kint) :: sc, rc(2), disp(2), root, comm
    integer(kint) :: i_sbuf(2), i_rbuf(4), i_ans(4)
    real(kdouble) :: r_sbuf(2), r_rbuf(4), r_ans(4)
    complex(kdouble) :: c_sbuf(2), c_rbuf(4), c_ans(4)

    comm = monolis_mpi_global_comm()

    !> case 1
    root = 0
    sc = 2
    rc(1) = 2
    rc(2) = 2
    i_sbuf(1) = 2*monolis_mpi_global_my_rank() + 1
    i_sbuf(2) = 2*monolis_mpi_global_my_rank() + 2
    disp(1) = 0
    disp(2) = 2

    call monolis_gatherv_I(i_sbuf, sc, i_rbuf, rc, disp, root, comm)

    i_ans(1) = 1
    i_ans(2) = 2
    i_ans(3) = 3
    i_ans(4) = 4

    if(monolis_mpi_global_my_rank() == 0)then
      call monolis_test_check_eq_I("monolis_gatherv_I case 1", i_rbuf, i_ans)
    endif

    root = 1
    sc = 2
    rc(1) = 2
    rc(2) = 2
    i_sbuf(1) = 2*monolis_mpi_global_my_rank() + 1
    i_sbuf(2) = 2*monolis_mpi_global_my_rank() + 2
    disp(1) = 0
    disp(2) = 2

    call monolis_gatherv_I(i_sbuf, sc, i_rbuf, rc, disp, root, comm)

    i_ans(1) = 1
    i_ans(2) = 2
    i_ans(3) = 3
    i_ans(4) = 4

    if(monolis_mpi_global_my_rank() == 1)then
      call monolis_test_check_eq_I("monolis_gatherv_I case 2", i_rbuf, i_ans)
    endif

    !> case 2
    root = 0
    sc = 2
    rc(1) = 2
    rc(2) = 2
    r_sbuf(1) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 1.0d0
    r_sbuf(2) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 2.0d0
    disp(1) = 0
    disp(2) = 2

    call monolis_gatherv_R(r_sbuf, sc, r_rbuf, rc, disp, root, comm)

    r_ans(1) = 1.0d0
    r_ans(2) = 2.0d0
    r_ans(3) = 3.0d0
    r_ans(4) = 4.0d0

    if(monolis_mpi_global_my_rank() == 0)then
      call monolis_test_check_eq_R("monolis_gatherv_R case 1", r_rbuf, r_ans)
    endif

    root = 1
    sc = 2
    rc(1) = 2
    rc(2) = 2
    r_sbuf(1) = 2*dble(monolis_mpi_global_my_rank()) + 1.0d0
    r_sbuf(2) = 2*dble(monolis_mpi_global_my_rank()) + 2.0d0
    disp(1) = 0
    disp(2) = 2

    call monolis_gatherv_R(r_sbuf, sc, r_rbuf, rc, disp, root, comm)

    r_ans(1) = 1.0d0
    r_ans(2) = 2.0d0
    r_ans(3) = 3.0d0
    r_ans(4) = 4.0d0

    if(monolis_mpi_global_my_rank() == 1)then
      call monolis_test_check_eq_R("monolis_gatherv_R case 2", r_rbuf, r_ans)
    endif

    !> case 3
    root = 0
    sc = 2
    rc(1) = 2
    rc(2) = 2
    r_sbuf(1) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 1.0d0
    r_sbuf(2) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 2.0d0
    c_sbuf(1) = complex(r_sbuf(1), r_sbuf(1))
    c_sbuf(2) = complex(r_sbuf(2), r_sbuf(2))
    disp(1) = 0
    disp(2) = 2

    call monolis_gatherv_C(c_sbuf, sc, c_rbuf, rc, disp, root, comm)

    c_ans(1) = (1.0d0, 1.0d0)
    c_ans(2) = (2.0d0, 2.0d0)
    c_ans(3) = (3.0d0, 3.0d0)
    c_ans(4) = (4.0d0, 4.0d0)

    if(monolis_mpi_global_my_rank() == 0)then
      call monolis_test_check_eq_C("monolis_gatherv_C case 1", c_rbuf, c_ans)
    endif

    root = 1
    sc = 2
    rc(1) = 2
    rc(2) = 2
    r_sbuf(1) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 1.0d0
    r_sbuf(2) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 2.0d0
    c_sbuf(1) = complex(r_sbuf(1), r_sbuf(1))
    c_sbuf(2) = complex(r_sbuf(2), r_sbuf(2))
    disp(1) = 0
    disp(2) = 2

    call monolis_gatherv_C(c_sbuf, sc, c_rbuf, rc, disp, root, comm)

    c_ans(1) = (1.0d0, 1.0d0)
    c_ans(2) = (2.0d0, 2.0d0)
    c_ans(3) = (3.0d0, 3.0d0)
    c_ans(4) = (4.0d0, 4.0d0)

    if(monolis_mpi_global_my_rank() == 1)then
      call monolis_test_check_eq_C("monolis_gatherv_C case 2", c_rbuf, c_ans)
    endif
  end subroutine monolis_gather_test

  subroutine monolis_scatterv_test()
    implicit none
    integer(kint) :: sc(2), rc, disp(2), root, comm
    integer(kint) :: i_sbuf(4), i_rbuf(2), i_ans(2)
    real(kdouble) :: r_sbuf(4), r_rbuf(2), r_ans(2)
    complex(kdouble) :: c_sbuf(4), c_rbuf(2), c_ans(2)

    comm = monolis_mpi_global_comm()

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

    i_ans(1) = 2*monolis_mpi_global_my_rank() + 1
    i_ans(2) = 2*monolis_mpi_global_my_rank() + 2

    call monolis_test_check_eq_I("monolis_scatterv_I case 1", i_rbuf, i_ans)

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

    r_ans(1) = 2.0d0*monolis_mpi_global_my_rank() + 1.0d0
    r_ans(2) = 2.0d0*monolis_mpi_global_my_rank() + 2.0d0

    call monolis_test_check_eq_R("monolis_scatterv_R case 1", r_rbuf, r_ans)

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

    r_ans(1) = 2.0d0*monolis_mpi_global_my_rank() + 1.0d0
    r_ans(2) = 2.0d0*monolis_mpi_global_my_rank() + 2.0d0
    c_ans(1) = complex(r_ans(1), r_ans(1))
    c_ans(2) = complex(r_ans(2), r_ans(2))

    call monolis_test_check_eq_C("monolis_scatterv_C case 1", c_rbuf, c_ans)
  end subroutine monolis_scatterv_test

  subroutine monolis_allgather_1_test()
    implicit none
    integer(kint) :: comm
    integer(kint) :: i_sbuf, i_rbuf(2), i_ans(2)
    real(kdouble) :: r_sbuf, r_rbuf(2), r_ans(2)
    complex(kdouble) :: c_sbuf, c_rbuf(2), c_ans(2)

    comm = monolis_mpi_global_comm()

    !> case 1
    i_sbuf = monolis_mpi_global_my_rank() + 1

    call monolis_allgather_I1(i_sbuf, i_rbuf, comm)

    i_ans(1) = 1
    i_ans(2) = 2

    call monolis_test_check_eq_I("monolis_allgather_I1 case 1", i_rbuf, i_ans)

    !> case 2
    r_sbuf = monolis_mpi_global_my_rank() + 1

    call monolis_allgather_R1(r_sbuf, r_rbuf, comm)

    r_ans(1) = 1.0d0
    r_ans(2) = 2.0d0

    call monolis_test_check_eq_R("monolis_allgather_R1 case 1", r_rbuf, r_ans)

    !> case 3
    r_sbuf = monolis_mpi_global_my_rank() + 1
    c_sbuf = complex(r_sbuf, r_sbuf)

    call monolis_allgather_C1(c_sbuf, c_rbuf, comm)

    c_ans(1) = (1.0d0, 1.0d0)
    c_ans(2) = (2.0d0, 2.0d0)

    call monolis_test_check_eq_C("monolis_allgather_C1 case 1", c_rbuf, c_ans)
  end subroutine monolis_allgather_1_test

  subroutine monolis_allgather_test()
    implicit none
    integer(kint) :: sc, rc(2), disp(2), comm
    integer(kint) :: i_sbuf(2), i_rbuf(4), i_ans(4)
    real(kdouble) :: r_sbuf(2), r_rbuf(4), r_ans(4)
    complex(kdouble) :: c_sbuf(2), c_rbuf(4), c_ans(4)

    comm = monolis_mpi_global_comm()

    !> case 1
    sc = 2
    rc(1) = 2
    rc(2) = 2
    i_sbuf(1) = 2*monolis_mpi_global_my_rank() + 1
    i_sbuf(2) = 2*monolis_mpi_global_my_rank() + 2
    disp(1) = 0
    disp(2) = 2

    call monolis_allgatherv_I(sc, i_sbuf, i_rbuf, rc, disp, comm)

    i_ans(1) = 1
    i_ans(2) = 2
    i_ans(3) = 3
    i_ans(4) = 4

    call monolis_test_check_eq_I("monolis_allgatherv_I case 1", i_rbuf, i_ans)

    !> case 2
    sc = 2
    rc(1) = 2
    rc(2) = 2
    r_sbuf(1) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 1.0d0
    r_sbuf(2) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 2.0d0
    disp(1) = 0
    disp(2) = 2

    call monolis_allgatherv_R(sc, r_sbuf, r_rbuf, rc, disp, comm)

    r_ans(1) = 1.0d0
    r_ans(2) = 2.0d0
    r_ans(3) = 3.0d0
    r_ans(4) = 4.0d0

    call monolis_test_check_eq_R("monolis_allgatherv_R case 1", r_rbuf, r_ans)

    !> case 3
    sc = 2
    rc(1) = 2
    rc(2) = 2
    r_sbuf(1) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 1.0d0
    r_sbuf(2) = 2.0d0*dble(monolis_mpi_global_my_rank()) + 2.0d0
    c_sbuf(1) = complex(r_sbuf(1), r_sbuf(1))
    c_sbuf(2) = complex(r_sbuf(2), r_sbuf(2))
    disp(1) = 0
    disp(2) = 2

    call monolis_allgatherv_C(sc, c_sbuf, c_rbuf, rc, disp, comm)

    c_ans(1) = (1.0d0, 1.0d0)
    c_ans(2) = (2.0d0, 2.0d0)
    c_ans(3) = (3.0d0, 3.0d0)
    c_ans(4) = (4.0d0, 4.0d0)

    call monolis_test_check_eq_C("monolis_allgatherv_C case 1", c_rbuf, c_ans)
  end subroutine monolis_allgather_test

  subroutine monolis_send_recv_test()
    implicit none
    integer(kint) :: i(2), i_ans(2), comm
    real(kdouble) :: r(2), r_ans(2)
    complex(kdouble) :: c(2), c_ans(2)

    !> case 1
    !call monolis_SendRecv_R(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    !    & send_index, send_item, recv_index, recv_item, &
    !    & val, ndof, comm)

    !> case 2
    !call monolis_SendRecv_I(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    !    & send_index, send_item, recv_index, recv_item, &
    !    & val, ndof, comm)

    !> case 3
    !call monolis_SendRecv_C(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    !    & send_index, send_item, recv_index, recv_item, &
    !    & val, ndof, comm)
  end subroutine monolis_send_recv_test
end module mod_monolis_mpi_test
