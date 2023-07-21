!> MPI wrap モジュール
module mod_monolis_mpi_wrap
  use mod_monolis_mpi
  use iso_c_binding
  implicit none

contains

  !> @ingroup wrap_mpi
  !> allreduce 関数（整数配列型）
  subroutine monolis_allreduce_I_c(n, val, tag, comm)&
    & bind(c, name = "monolis_allreduce_I")
    implicit none
    !> [in] 配列サイズ
    integer(c_int), intent(in), value :: n
    !> [in,out] 入出力値（整数型）
    integer(c_int) :: val(n)
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(c_int), intent(in), value :: tag
    !> [in] MPI コミュニケータ
    integer(c_int), intent(in), value :: comm
    call monolis_allreduce_I(n, val, tag, comm)
  end subroutine monolis_allreduce_I_c

  !> @ingroup wrap_mpi
  !> allreduce 関数（浮動小数点配列型）
  subroutine monolis_allreduce_R_c(n, val, tag, comm)&
    & bind(c, name = "monolis_allreduce_R")
    implicit none
    !> [in] 配列サイズ
    integer(c_int), intent(in), value :: n
    !> [in,out] 入出力値（浮動小数点配列型）
    real(c_double) :: val(n)
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(c_int), intent(in), value :: tag
    !> [in] MPI コミュニケータ
    integer(c_int), intent(in), value :: comm
    call monolis_allreduce_R(n, val, tag, comm)
  end subroutine monolis_allreduce_R_c

  !> @ingroup wrap_mpi
  !> allreduce 関数（複素数型）
  subroutine monolis_allreduce_C_c(n, val, tag, comm)&
    & bind(c, name = "monolis_allreduce_C")
    implicit none
    !> [in] 配列サイズ
    integer(c_int), intent(in), value :: n
    !> [in,out] 入出力値（浮動小数点配列型）
    complex(c_double) :: val(n)
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(c_int), intent(in), value :: tag
    !> [in] MPI コミュニケータ
    integer(c_int), intent(in), value :: comm
    call monolis_allreduce_C(n, val, tag, comm)
  end subroutine monolis_allreduce_C_c

  !> @ingroup wrap_mpi
  !> ベクトルのアップデート関数（実数型）
  subroutine monolis_mpi_update_R_c(NP, n_dof, X, comm, &
    recv_n_neib, recv_nitem, recv_neib_pe, recv_index, recv_item, &
    send_n_neib, send_nitem, send_neib_pe, send_index, send_item) &
    & bind(c, name = "monolis_mpi_update_R_c_main")
    implicit none
    !> [in] 配列サイズ
    integer(c_int), intent(in), value :: NP
    !> [in] 計算点が持つ自由度
    integer(c_int), intent(in), value :: n_dof
    !> [in] MPI コミュニケータ
    integer(c_int), intent(in), value :: comm
    !> [in] recv する隣接領域数
    integer(c_int), intent(in), value :: recv_n_neib
    !> [in] recv の item 数
    integer(c_int), intent(in), value :: recv_nitem
    !> [in] recv する隣接領域 id
    integer(c_int), intent(in), target :: recv_neib_pe(recv_n_neib)
    !> [in] recv の index 配列
    integer(c_int), intent(in), target :: recv_index(recv_n_neib + 1)
    !> [in,out] recv の item 配列（受信する節点番号データ）
    integer(c_int), intent(inout), target :: recv_item(recv_nitem)
    !> [in] send する隣接領域数
    integer(c_int), intent(in), value :: send_n_neib
    !> [in] send の item 数
    integer(c_int), intent(in), value :: send_nitem
    !> [in,] send する隣接領域 id
    integer(c_int), intent(in), target :: send_neib_pe(send_n_neib)
    !> [in] send の index 配列
    integer(c_int), intent(in), target :: send_index(send_n_neib + 1)
    !> [in,out] send の item 配列（送信する節点番号データ）
    integer(c_int), intent(inout), target :: send_item(send_nitem)
    !> [in] 配列
    real(c_double), target :: X(n_dof*NP)

    send_item = send_item + 1
    recv_item = recv_item + 1

    call monolis_SendRecv_R(send_n_neib, send_neib_pe, &
       & recv_n_neib, recv_neib_pe, &
       & send_index, send_item, recv_index, recv_item, &
       & X, X, n_dof, comm)

    send_item = send_item - 1
    recv_item = recv_item - 1
  end subroutine monolis_mpi_update_R_c

  !> @ingroup wrap_mpi
  !> ベクトルのアップデート関数（整数型）
  subroutine monolis_mpi_update_I_c(NP, n_dof, X, comm, &
    recv_n_neib, recv_nitem, recv_neib_pe, recv_index, recv_item, &
    send_n_neib, send_nitem, send_neib_pe, send_index, send_item) &
    & bind(c, name = "monolis_mpi_update_I_c_main")
    implicit none
    !> [in] 配列サイズ
    integer(c_int), intent(in), value :: NP
    !> [in] 計算点が持つ自由度
    integer(c_int), intent(in), value :: n_dof
    !> [in] MPI コミュニケータ
    integer(c_int), intent(in), value :: comm
    !> [in] recv する隣接領域数
    integer(c_int), intent(in), value :: recv_n_neib
    !> [in] recv の item 数
    integer(c_int), intent(in), value :: recv_nitem
    !> [in] recv する隣接領域 id
    integer(c_int), intent(in), target :: recv_neib_pe(recv_n_neib)
    !> [in] recv の index 配列
    integer(c_int), intent(in), target :: recv_index(recv_n_neib + 1)
    !> [in,out] recv の item 配列（受信する節点番号データ）
    integer(c_int), intent(inout), target :: recv_item(recv_nitem)
    !> [in] send する隣接領域数
    integer(c_int), intent(in), value :: send_n_neib
    !> [in] send の item 数
    integer(c_int), intent(in), value :: send_nitem
    !> [in] send する隣接領域 id
    integer(c_int), intent(in), target :: send_neib_pe(send_n_neib)
    !> [in] send の index 配列
    integer(c_int), intent(in), target :: send_index(send_n_neib + 1)
    !> [in,out] send の item 配列（送信する節点番号データ）
    integer(c_int), intent(inout), target :: send_item(send_nitem)
    !> [in] 配列
    integer(c_int), target :: X(n_dof*NP)

    send_item = send_item + 1
    recv_item = recv_item + 1

    call monolis_SendRecv_I(send_n_neib, send_neib_pe, &
       & recv_n_neib, recv_neib_pe, &
       & send_index, send_item, recv_index, recv_item, &
       & X, X, n_dof, comm)

    send_item = send_item - 1
    recv_item = recv_item - 1
  end subroutine monolis_mpi_update_I_c

  !> @ingroup wrap_mpi
  !> ベクトルのアップデート関数（複素数型）
  subroutine monolis_mpi_update_C_c(NP, n_dof, X, comm, &
    recv_n_neib, recv_nitem, recv_neib_pe, recv_index, recv_item, &
    send_n_neib, send_nitem, send_neib_pe, send_index, send_item) &
    & bind(c, name = "monolis_mpi_update_C_c_main")
    implicit none
    !> [in] 配列サイズ
    integer(c_int), intent(in), value :: NP
    !> [in] 計算点が持つ自由度
    integer(c_int), intent(in), value :: n_dof
    !> [in] MPI コミュニケータ
    integer(c_int), intent(in), value :: comm
    !> [in] recv する隣接領域数
    integer(c_int), intent(in), value :: recv_n_neib
    !> [in] recv の item 数
    integer(c_int), intent(in), value :: recv_nitem
    !> [in] recv する隣接領域 id
    integer(c_int), intent(in), target :: recv_neib_pe(recv_n_neib)
    !> [in] recv の index 配列
    integer(c_int), intent(in), target :: recv_index(recv_n_neib + 1)
    !> [in,out] recv の item 配列（受信する節点番号データ）
    integer(c_int), intent(inout), target :: recv_item(recv_nitem)
    !> [in] send する隣接領域数
    integer(c_int), intent(in), value :: send_n_neib
    !> [in] send の item 数
    integer(c_int), intent(in), value :: send_nitem
    !> [in] send する隣接領域 id
    integer(c_int), intent(in), target :: send_neib_pe(send_n_neib)
    !> [in] send の index 配列
    integer(c_int), intent(in), target :: send_index(send_n_neib + 1)
    !> [in,out] send の item 配列（送信する節点番号データ）
    integer(c_int), intent(inout), target :: send_item(send_nitem)
    !> [in] 配列
    complex(c_double), target :: X(n_dof*NP)

    send_item = send_item + 1
    recv_item = recv_item + 1

    call monolis_SendRecv_C(send_n_neib, send_neib_pe, &
       & recv_n_neib, recv_neib_pe, &
       & send_index, send_item, recv_index, recv_item, &
       & X, X, n_dof, comm)

    send_item = send_item - 1
    recv_item = recv_item - 1
  end subroutine monolis_mpi_update_C_c
end module mod_monolis_mpi_wrap
