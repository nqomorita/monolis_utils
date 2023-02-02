!> MPI ¥Æ¥¹¥È¥â¥¸¥å©`¥ë
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
end module mod_monolis_mpi_test
