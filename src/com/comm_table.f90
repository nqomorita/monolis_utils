!> グラフ分割モジュール（通信テーブル作成）
module mod_monolis_comm_table
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  use mod_monolis_utils_define_com
  use mod_monolis_mpi
  use mod_monolis_mpi_util
  use mod_monolis_comm_par_util
  implicit none

contains

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
    !> 全ての外部節点番号（グローバル番号）
    integer(kint), allocatable :: outer_node_id_all_global(:)
    !> 全ての外部節点が属する領域番号
    integer(kint), allocatable :: outer_domain_id_all(:)
    !> 全ての外部節点配列の各領域に属する節点数
    integer(kint), allocatable :: displs(:)
    type(monolis_comm_node_list), allocatable :: recv_list(:)

    com%my_rank = monolis_mpi_get_local_my_rank(com%comm)
    com%comm_size = monolis_mpi_get_local_comm_size(com%comm)
    com%n_internal_vertex = n_internal_vertex

    call monolis_comm_get_all_external_node_parallel(n_internal_vertex, n_vertex, vertex_id, com, &
      & outer_node_id_all_global, displs)

    call monolis_comm_get_all_external_node_domain_id_parallel(n_internal_vertex, vertex_id, com, &
      & outer_node_id_all_global, outer_domain_id_all, displs)

    call monolis_comm_get_recv_parallel(n_vertex, vertex_id, com, &
      & outer_node_id_all_global, outer_domain_id_all, displs, recv_list)

    call monolis_comm_get_send_parallel(n_vertex, vertex_id, com, recv_list)
  end subroutine monolis_com_get_comm_table_parallel
end module mod_monolis_comm_table
