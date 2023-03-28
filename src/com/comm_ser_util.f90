!> 通信テーブル作成モジュール（逐次実行版）
module mod_monolis_comm_ser_util
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  use mod_monolis_utils_palloc
  use mod_monolis_utils_define_com
  use mod_monolis_utils_std_sort_I
  implicit none

contains

  !> @ingroup dev_com
  !> 全ての外部節点が所属する領域番号を取得（逐次実行版）
  subroutine monolis_comm_get_all_external_node_domain_id_serial(vertex_domain_id, n_domain, &
    & outer_node_id_all_global, outer_domain_id_all, displs)
    implicit none
    !> [in] 節点の所属する領域番号（全節点数）
    integer(kint) :: vertex_domain_id(:)
    !> [in] 領域分割数
    integer(kint) :: n_domain
    !> [in] 全ての外部節点番号
    integer(kint) :: outer_node_id_all_global(:)
    !> 全ての外部節点が属する領域番号
    integer(kint), allocatable :: outer_domain_id_all(:)
    !> [in] 全ての外部節点配列の各領域に属する節点数
    integer(kint) :: displs(:)
    integer(kint) :: i, in, id

    call monolis_alloc_I_1d(outer_domain_id_all, displs(n_domain + 1))

    do i = 1, displs(n_domain + 1)
      in = outer_node_id_all_global(i)
      id = vertex_domain_id(in)
      outer_domain_id_all(i) = id
    enddo
  end subroutine monolis_comm_get_all_external_node_domain_id_serial

  !> @ingroup dev_com
  !> データ通信する recv 隣接領域の取得（逐次実行版）
  subroutine monolis_comm_get_recv_serial(n_domain, domain_id, n_internal_vertex, &
    & outer_node_id_all_global, outer_domain_id_all, displs, com, recv_list)
    implicit none
    !> [in] 分割領域数
    integer(kint) :: n_domain
    !> [in] 領域番号
    integer(kint) :: domain_id
    !> [in] 領域の内部節点数
    integer(kint) :: n_internal_vertex
    !> 全ての外部節点番号（グローバル番号）
    integer(kint) :: outer_node_id_all_global(:)
    !> [in] 節点が属する領域番号（全節点）
    integer(kint) :: outer_domain_id_all(:)
    !> 全ての外部節点配列の各領域に属する節点数
    integer(kint) :: displs(:)
    !> [in] 分割領域に対応する com 構造体
    type(monolis_COM) :: com
    !> [in] recv 節点の情報
    type(monolis_comm_node_list) :: recv_list(:)
    integer(kint) :: i, in, j, jn, jS, jE, n_neib, n_data
    integer(kint) :: recv_rank
    integer(kint), allocatable :: domain(:), master_vertex_local_id(:)
    integer(kint), allocatable :: iadd(:)

    !> recv_n_neib
    domain_id = domain_id + 1

    call monolis_alloc_I_1d(domain, n_domain)

    do j = displs(domain_id) + 1, displs(domain_id + 1)
      in = outer_domain_id_all(j) + 1
      domain(in) = domain(in) + 1
    enddo

    n_neib = 0
    do j = 1, n_domain
      if(domain(j) /= 0) n_neib = n_neib + 1
    enddo

    com%recv_n_neib = n_neib

    if(n_neib == 0)then
      call monolis_palloc_I_1d(com%recv_neib_pe, 1)
      call monolis_palloc_I_1d(com%recv_index, 2)
    else
      call monolis_palloc_I_1d(com%recv_neib_pe, n_neib)
      call monolis_palloc_I_1d(com%recv_index, n_neib + 1)
    endif

    !> recv_neib_pe
    in = 0
    do j = 1, n_domain
      if(domain(j) /= 0)then
        in = in + 1
        com%recv_neib_pe(in) = j - 1 !> conver to 0 origin
        com%recv_index(in + 1) = com%recv_index(in) + domain(j)
      endif
    enddo

    !> recv_list の構築
    do i = 1, n_neib
      recv_rank = com%recv_neib_pe(i)
      jS = displs(domain_id) + 1
      jE = displs(domain_id + 1)

      n_data = 0
      do j = jS, jE
        in = outer_domain_id_all(j)
        if(recv_rank == in)then
          n_data = n_data + 1
        endif
      enddo

      recv_list(recv_rank + 1)%n_node = recv_list(recv_rank + 1)%n_node + n_data

      !> add domain id
      call monolis_alloc_I_1d(iadd, n_data)
      iadd = domain_id - 1
      call monolis_append_I_1d(recv_list(recv_rank + 1)%domid, n_data, iadd)
      call monolis_dealloc_I_1d(iadd)

      !> add global id
      call monolis_alloc_I_1d(iadd, n_data)
      in = 1
      do j = displs(domain_id) + 1, displs(domain_id + 1)
        iadd(in) = outer_node_id_all_global(j)
        in = in + 1
      enddo
      call monolis_append_I_1d(recv_list(recv_rank + 1)%global_id, n_data, iadd)
      call monolis_dealloc_I_1d(iadd)
    enddo

    !> com recv の構築
    in = com%recv_index(n_neib + 1)

    call monolis_palloc_I_1d(com%recv_item, in)

    in = 0
    do i = 1, n_neib
      do j = displs(domain_id) + 1, displs(domain_id + 1)
        if(com%recv_neib_pe(i) == outer_domain_id_all(j))then
          in = in + 1
          com%recv_item(in) = n_internal_vertex + j - displs(domain_id)
        endif
      enddo
    enddo

    domain_id = domain_id - 1
  end subroutine monolis_comm_get_recv_serial

  !> @ingroup dev_com
  !> データ通信する send 隣接領域の取得（逐次実行版）
  subroutine monolis_comm_get_send_serial(n_domain, n_vertex, vertex_id, com, recv_list)
    implicit none
    !> [in] 分割領域数
    integer(kint) :: n_domain
    !> [in] 節点数
    integer(kint) :: n_vertex
    !> [in] 節点 id
    integer(kint) :: vertex_id(:)
    !> [in] 分割領域に対応する com 構造体
    type(monolis_COM) :: com
    !> [in] recv 節点の情報
    type(monolis_comm_node_list) :: recv_list
    integer(kint) :: i, in, n_neib, id
    integer(kint), allocatable :: temp(:), local_nid(:), domain(:)

    call monolis_alloc_I_1d(domain, n_domain)

    do i = 1, recv_list%n_node
      in = recv_list%domid(i) + 1
      domain(in) = domain(in) + 1
    enddo

    n_neib = 0
    do i = 1, n_domain
      if(domain(i) /= 0) n_neib = n_neib + 1
    enddo

    com%send_n_neib = n_neib

    if(n_neib == 0)then
      call monolis_palloc_I_1d(com%send_neib_pe, 1)
      call monolis_palloc_I_1d(com%send_index, 2)
      call monolis_palloc_I_1d(com%send_item, 1)
    else
      call monolis_palloc_I_1d(com%send_neib_pe, n_neib)
      call monolis_palloc_I_1d(com%send_index, n_neib + 1)
      call monolis_palloc_I_1d(com%send_item, recv_list%n_node)
    endif

    in = 0
    do i = 1, n_domain
      if(domain(i) /= 0)then
        in = in + 1
        com%send_neib_pe(in) = i - 1 !> conver to 0 origin
        com%send_index(in + 1) = com%recv_index(in) + domain(i)
      endif
    enddo

    !> local_nid に変換
    call monolis_dealloc_I_1d(temp)
    call monolis_alloc_I_1d(local_nid, n_vertex)
    call monolis_alloc_I_1d(temp, n_vertex)

    do i = 1, n_vertex
      temp(i) = vertex_id(i)
      local_nid(i) = i
    enddo

    call monolis_qsort_I_2d(temp, local_nid, 1, n_vertex)

    do i = 1, recv_list%n_node
      call monolis_bsearch_I(temp, 1, n_vertex, recv_list%global_id(i), id)
      if(id == -1) cycle
      com%send_item(i) = local_nid(id)
    enddo
  end subroutine monolis_comm_get_send_serial
end module mod_monolis_comm_ser_util
