!> グラフ分割モジュール（通信テーブル作成）
module mod_monolis_comm_table
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  use mod_monolis_utils_define_com
  use mod_monolis_mpi_util
  implicit none

contains

  !> @ingroup com
  !> 各領域の内部節点数リスト vtxdist を作成
  subroutine monolis_com_n_vertex_list(n_internal_vertex, comm, vtxdist)
    implicit none
    !> [in] 分割領域の内部節点数
    integer(kint) :: n_internal_vertex
    !> [in] MPI コミュニケータ
    integer(kint) :: comm
    !> [out] 各領域の内部節点数リスト
    integer(kint), allocatable :: vtxdist(:)
    integer(kint) :: n_size, i

    n_size = monolis_mpi_local_comm_size(comm)

    call monolis_alloc_int_1d(vtxdist, n_size + 1)

    call monolis_allgather_I1(n_internal_vertex, vtxdist(2:n_size + 1), comm)

    do i = 1, n_size
      vtxdist(i + 1) = vtxdist(i + 1) + vtxdist(i)
    enddo
  end subroutine monolis_com_n_vertex_list

  !> @ingroup com
  !> 通信テーブルを作成（並列実行版）
  subroutine monolis_com_get_comm_table_parallel(n_internal_vertex, n_vertex, vertex_id, com)
    implicit none
    !> [in] 内部節点数
    integer(kint) :: n_internal_vertex
    !> [in] 全節点数
    integer(kint) :: n_vertex
    !> [in] グローバル節点番号
    integer(kint) :: vertex_id(:)
    !> [out] 分割領域に対応する com 構造体
    type(monolis_COM) :: com
    !> 全ての外部節点番号
    integer(kint), allocatable :: outer_node_id_all(:)
    !> 全ての外部節点が属する領域番号
    integer(kint), allocatable :: outer_domain_id_all(:)
    !> 全ての外部節点配列の各領域に属する節点数
    integer(kint), allocatable :: displs(:)
    type(monolis_comm_node_list), allocatable :: recv_list(:)

    call monolis_comm_get_all_external_node(n_internal_vertex, n_vertex, vertex_id, com, &
      & outer_node_id_all, displs)

    call monolis_comm_get_all_external_node_domain_id(n_internal_vertex, vertex_id, com, &
      & outer_node_id_all, outer_domain_id_all, displs)

    call monolis_comm_get_recv_parallel(n_vertex, vertex_id, com, &
      & outer_node_id_all, outer_domain_id_all, displs, recv_list)

    call monolis_comm_get_send_parallel(n_vertex, vertex_id, com, recv_list)
  end subroutine monolis_com_get_comm_table_parallel
end module mod_monolis_comm_table
