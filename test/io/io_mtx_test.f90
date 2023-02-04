!> IO 行列モジュール
module mod_monolis_io_mtx_test
  use mod_monolis_utils_error
  use mod_monolis_utils_std_test
  use mod_monolis_io_mtx
  implicit none

contains

  subroutine monolis_io_mtx_test()
    implicit none

    call monolis_input_mtx_R_test()
    call monolis_input_mtx_C_test()
  end subroutine monolis_io_mtx_test

  subroutine monolis_input_mtx_R_test()
    implicit none

    !call monolis_input_mtx_R(fname, N, NZ, elem, coef)
  end subroutine monolis_input_mtx_R_test

  subroutine monolis_input_mtx_C_test()
    implicit none

    !call monolis_input_mtx_C(fname, N, NZ, elem, coef)
  end subroutine monolis_input_mtx_C_test
end module mod_monolis_io_mtx_test
