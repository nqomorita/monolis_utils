!> MPI wrap モジュール
module mod_monolis_mpi_wrap
  use mod_monolis_mpi
  implicit none

contains

  !> @ingroup mpi
  !> allreduce 関数（整数配列型）
  subroutine monolis_allreduce_I_c(n, val, tag, comm)
    implicit none
    !> [in] 配列サイズ
    integer(kint) :: n
    !> [in,out] 入出力値（整数型）
    integer(kint) :: val(n)
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(kint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm

  end subroutine monolis_allreduce_I_c

  !> @ingroup mpi
  !> allreduce 関数（浮動小数点配列型）
  subroutine monolis_allreduce_R_c(n, val, tag, comm)
    implicit none
    !> [in] 配列サイズ
    integer(kint) :: n
    !> [in,out] 入出力値（浮動小数点配列型）
    real(kdouble) :: val(n)
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(kint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm

  end subroutine monolis_allreduce_R_c

  !> @ingroup mpi
  !> allreduce 関数（複素数型）
  subroutine monolis_allreduce_C_c(n, val, tag, comm)
    implicit none
    !> [in] 配列サイズ
    integer(kint) :: n
    !> [in,out] 入出力値（浮動小数点配列型）
    complex(kdouble) :: val(n)
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(kint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm

  end subroutine monolis_allreduce_C_c

  !> @ingroup mpi
  !> ベクトルのアップデート関数（実数型）
  subroutine monolis_mpi_update_R_c(monoCOM, ndof, X)
    implicit none
    !> [in] COM 構造体
    type(monolis_com) :: monoCOM
    !> [in] 節点あたりの自由度
    integer(kint) :: ndof
    !> [in,out] 入出力ベクトル
    real(kdouble) :: X(:)
    real(kdouble) :: tcomm

  end subroutine monolis_mpi_update_R_c

  !> @ingroup mpi
  !> ベクトルのアップデート関数（整数型）
  subroutine monolis_mpi_update_I_c(monoCOM, ndof, X)
    implicit none
    !> [in] COM 構造体
    type(monolis_com) :: monoCOM
    !> [in] 節点あたりの自由度
    integer(kint) :: ndof
    !> [in,out] 入出力ベクトル
    integer(kint) :: X(:)
    !> [in,out] 入出力ベクトル
    real(kdouble) :: tcomm

  end subroutine monolis_mpi_update_I_c

  !> @ingroup mpi
  !> ベクトルのアップデート関数（複素数型）
  subroutine monolis_mpi_update_C_c(monoCOM, ndof, X)
    implicit none
    !> [in] COM 構造体
    type(monolis_com) :: monoCOM
    !> [in] 節点あたりの自由度
    integer(kint) :: ndof
    !> [in,out] 入出力ベクトル
    complex(kdouble) :: X(:)
    !> [in,out] 入出力ベクトル
    real(kdouble) :: tcomm

  end subroutine monolis_mpi_update_C_c
end module mod_monolis_mpi_wrap
