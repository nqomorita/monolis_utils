!> 通信テーブル作成モジュール（並列実行版）
module mod_monolis_comm_par_util_wrap
  use mod_monolis_utils_define_prm
  use mod_monolis_comm_par_util
  use iso_c_binding
  implicit none

contains

  !> @ingroup dev_com
  !> 全ての外部節点を取得
  subroutine monolis_comm_get_all_external_n_node_parallel_c(n_internal_vertex, n_vertex, comm, n_outer)&
    & bind(c, name = "monolis_comm_get_all_external_n_node_parallel")
    implicit none
    !> [in] 内部節点数
    integer(c_int), value :: n_internal_vertex
    !> [in] 全節点数
    integer(c_int) :: n_vertex
    !> [in] MPI コミュニケータ
    integer(c_int), value :: comm
    !> [out] 全ての外部節点配列に属する節点数
    integer(c_int) :: n_outer

    call monolis_comm_get_all_external_n_node_parallel(n_internal_vertex, n_vertex, comm, n_outer)
  end subroutine monolis_comm_get_all_external_n_node_parallel_c

  !> @ingroup dev_com
  !> 全ての外部節点を取得
  subroutine monolis_comm_get_all_external_node_parallel_c(n_internal_vertex, n_vertex, vertex_id, &
    & comm, outer_node_id_all, displs)&
    & bind(c, name = "monolis_comm_get_all_external_node_parallel")
    implicit none
    !> [in] 内部節点数
    integer(c_int), value :: n_internal_vertex
    !> [in] 全節点数
    integer(c_int), value :: n_vertex
    !> [in] グローバル節点番号
    integer(c_int) :: vertex_id(:)
    !> [in] MPI コミュニケータ
    integer(c_int), value :: comm
    !> [out] 全ての外部節点番号
    integer(c_int) :: outer_node_id_all(:)
    !> 全ての外部節点配列の各領域に属する節点数
    integer(c_int) :: displs(:)

    call monolis_comm_get_all_external_node_parallel(n_internal_vertex, n_vertex, vertex_id, &
      & comm, outer_node_id_all, displs)
  end subroutine monolis_comm_get_all_external_node_parallel_c

  !> @ingroup dev_com
  !> 全ての外部節点が所属する領域番号を取得
  subroutine monolis_comm_get_all_external_node_domain_id_parallel_c(n_internal_vertex, vertex_id, comm, &
    & outer_node_id_all, outer_domain_id_all, displs)&
    & bind(c, name = "monolis_comm_get_all_external_node_domain_id_parallel")
    implicit none
    !> [in] 内部節点数
    integer(c_int), value :: n_internal_vertex
    !> [in] グローバル節点番号
    integer(c_int) :: vertex_id(:)
    !> [in] MPI コミュニケータ
    integer(c_int), value :: comm
    !> [in] 全ての外部節点番号
    integer(c_int) :: outer_node_id_all(:)
    !> [out] 全ての外部節点が属する領域番号
    integer(c_int) :: outer_domain_id_all(:)
    !> 全ての外部節点配列の各領域に属する節点数
    integer(c_int) :: displs(:)

    call monolis_comm_get_all_external_node_domain_id_parallel(n_internal_vertex, vertex_id, comm, &
      & outer_node_id_all, outer_domain_id_all, displs)
  end subroutine monolis_comm_get_all_external_node_domain_id_parallel_c

  !> @ingroup dev_com
  !> データ通信する recv 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_recv_parallel_n_neib_c(comm, outer_domain_id_all, displs, n_neib_recv, is_neib)&
    & bind(c, name = "monolis_comm_get_recv_parallel_n_neib")
    implicit none
    !> [in] MPI コミュニケータ
    integer(c_int), value :: comm
    !> [in] 全ての外部節点が属する領域番号
    integer(c_int) :: outer_domain_id_all(:)
    !> [in] 全ての外部節点配列の各領域に属する節点数
    integer(c_int) :: displs(:)
    !> [out] 隣接する領域数
    integer(c_int) :: n_neib_recv
    !> [out] 隣接する領域フラグ（サイズ：[comm_size]）
    integer(c_int) :: is_neib(:)

    call monolis_comm_get_recv_parallel_n_neib(comm, outer_domain_id_all, displs, n_neib_recv, is_neib)
  end subroutine monolis_comm_get_recv_parallel_n_neib_c

  !> @ingroup dev_com
  !> データ通信する recv 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_recv_parallel_neib_id_c(comm, is_neib, neib_id)&
    & bind(c, name = "monolis_comm_get_recv_parallel_neib_id")
    implicit none
    !> [in] MPI コミュニケータ
    integer(c_int), value :: comm
    !> [in] 隣接する領域フラグ（サイズ：[comm_size]）
    integer(c_int) :: is_neib(:)
    !> [out] 隣接領域番号
    integer(c_int) :: neib_id(:)

    call monolis_comm_get_recv_parallel_neib_id(comm, is_neib, neib_id)
  end subroutine monolis_comm_get_recv_parallel_neib_id_c

  !> @ingroup dev_com
  !> データ通信する recv 隣接領域の index 配列取得（並列実行版）
  subroutine monolis_comm_get_recv_parallel_index_c(comm, displs, outer_domain_id_all, n_neib_recv, neib_id, index)&
    & bind(c, name = "monolis_comm_get_recv_parallel_index_c")
    implicit none
    !> [in] MPI コミュニケータ
    integer(c_int), value :: comm
    !> [in] 全ての外部節点配列の各領域に属する節点数
    integer(c_int) :: displs(:)
    !> [in] 全ての外部節点が属する領域番号
    integer(c_int) :: outer_domain_id_all(:)
    !> [in] 隣接する領域数
    integer(c_int), value :: n_neib_recv
    !> [in] 隣接領域番号
    integer(c_int) :: neib_id(:)
    !> [out] recv 隣接領域の index 配列
    integer(c_int) :: index(:)

    call monolis_comm_get_recv_parallel_index(comm, displs, outer_domain_id_all, n_neib_recv, neib_id, index)
  end subroutine monolis_comm_get_recv_parallel_index_c

  !> @ingroup dev_com
  !> データ通信する recv 隣接領域の item 配列取得（並列実行版）
  subroutine monolis_comm_get_recv_parallel_item_c(n_vertex, vertex_id, comm, &
    & outer_node_id_all, outer_domain_id_all, displs, recv_n_neib, neib_id, index, item)&
    & bind(c, name = "monolis_comm_get_recv_parallel_item")
    implicit none
    !> [in] 全節点数
    integer(c_int), value :: n_vertex
    !> [in] グローバル節点番号
    integer(c_int) :: vertex_id(:)
    !> [in] MPI コミュニケータ
    integer(c_int), value :: comm
    !> [in] 全ての外部節点番号
    integer(c_int) :: outer_node_id_all(:)
    !> [in] 全ての外部節点が属する領域番号
    integer(c_int) :: outer_domain_id_all(:)
    !> [in] 全ての外部節点配列の各領域に属する節点数
    integer(c_int) :: displs(:)
    !> [in] 隣接する領域数
    integer(c_int), value :: recv_n_neib
    !> [in] 隣接領域番号
    integer(c_int) :: neib_id(:)
    !> [in] recv 隣接領域の index 配列
    integer(c_int) :: index(:)
    !> [in] recv 隣接領域の item 配列
    integer(c_int) :: item(:)

    call monolis_comm_get_recv_parallel_item(n_vertex, vertex_id, comm, &
      & outer_node_id_all, outer_domain_id_all, displs, recv_n_neib, neib_id, index, item)
  end subroutine monolis_comm_get_recv_parallel_item_c

  !> @ingroup dev_com
  !> データ通信する send 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_send_parallel_n_list_c(comm, recv_n_neib, recv_neib_pe, recv_index, send_n_list)&
    & bind(c, name = "monolis_comm_get_send_parallel_n_list")
    implicit none
    !> [in] MPI コミュニケータ
    integer(c_int), value :: comm
    !> [in] 隣接する領域数
    integer(c_int), value :: recv_n_neib
    !> [in] 隣接領域番号
    integer(c_int) :: recv_neib_pe(:)
    !> [in] recv 隣接領域の index 配列
    integer(c_int) :: recv_index(:)
    !> [out] send 節点の個数リスト
    integer(c_int) :: send_n_list(:)

    call monolis_comm_get_send_parallel_n_list(comm, recv_n_neib, recv_neib_pe, recv_index, send_n_list)
  end subroutine monolis_comm_get_send_parallel_n_list_c

  !> @ingroup dev_com
  !> データ通信する send 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_send_parallel_n_neib_c(comm, send_n_list, n_neib_send)&
    & bind(c, name = "monolis_comm_get_send_parallel_n_neib")
    implicit none
    !> [in] MPI コミュニケータ
    integer(c_int), value :: comm
    !> [out] send 節点の個数リスト
    integer(c_int) :: send_n_list(:)
    !> [in] 隣接する領域数
    integer(c_int) :: n_neib_send

    call monolis_comm_get_send_parallel_n_neib(comm, send_n_list, n_neib_send)
  end subroutine monolis_comm_get_send_parallel_n_neib_c

  !> @ingroup dev_com
  !> データ通信する send 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_send_parallel_neib_id_c(comm, send_n_list, send_neib_pe)&
    & bind(c, name = "monolis_comm_get_send_parallel_neib_id")
    implicit none
    !> [in] MPI コミュニケータ
    integer(c_int), value :: comm
    !> [out] send 節点の個数リスト
    integer(c_int) :: send_n_list(:)
    !> [in] 隣接する領域番号
    integer(c_int) :: send_neib_pe(:)

    call monolis_comm_get_send_parallel_neib_id(comm, send_n_list, send_neib_pe)
  end subroutine monolis_comm_get_send_parallel_neib_id_c

  !> @ingroup dev_com
  !> データ通信する send 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_send_parallel_index_c(comm, send_n_list, send_n_neib, send_index)&
    & bind(c, name = "monolis_comm_get_send_parallel_index")
    implicit none
    !> [in] MPI コミュニケータ
    integer(c_int), value :: comm
    !> [out] send 節点の個数リスト
    integer(c_int) :: send_n_list(:)
    !> [in] 隣接する領域数
    integer(c_int), value :: send_n_neib
    !> [in] send 節点の index 配列
    integer(c_int) :: send_index(:)

    call monolis_comm_get_send_parallel_index(comm, send_n_list, send_n_neib, send_index)
  end subroutine monolis_comm_get_send_parallel_index_c

  !> @ingroup dev_com
  !> データ通信する send の item 配列の取得（並列実行版）
  subroutine monolis_comm_get_send_parallel_item_c(comm, n_vertex, vertex_id, &
    & recv_n_neib, recv_neib_pe, recv_index, recv_item, &
    & send_n_neib, send_neib_pe, send_index, send_item)&
    & bind(c, name = "monolis_comm_get_send_parallel_item")
    implicit none
    !> [in] MPI コミュニケータ
    integer(c_int), value :: comm
    !> [in] ノード数
    integer(c_int), value :: n_vertex
    !> [in] 隣接する領域数
    integer(c_int) :: vertex_id(:)
    !> [in] 隣接する領域数
    integer(c_int), value :: recv_n_neib
    !> [in] 隣接する領域番号
    integer(c_int) :: recv_neib_pe(:)
    !> [in] 隣接する領域数
    integer(c_int) :: recv_index(:)
    !> [in] 隣接する領域数
    integer(c_int) :: recv_item(:)
    !> [in] 隣接する領域数
    integer(c_int), value :: send_n_neib
    !> [in] 隣接する領域番号
    integer(c_int) :: send_neib_pe(:)
    !> [in] 隣接する領域数
    integer(c_int) :: send_index(:)
    !> [in] 隣接する領域数
    integer(c_int) :: send_item(:)

    call monolis_comm_get_send_parallel_item(comm, n_vertex, vertex_id, &
      & recv_n_neib, recv_neib_pe, recv_index, recv_item, &
      & send_n_neib, send_neib_pe, send_index, send_item)
  end subroutine monolis_comm_get_send_parallel_item_c

end module mod_monolis_comm_par_util_wrap
