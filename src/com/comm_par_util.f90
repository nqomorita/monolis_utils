!> 通信テーブル作成モジュール（並列実行版）
module mod_monolis_comm_par_util
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  use mod_monolis_utils_palloc
  use mod_monolis_utils_define_com
  use mod_monolis_mpi
  use mod_monolis_mpi_util
  use mod_monolis_utils_std_sort_I
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

    n_size = monolis_mpi_get_local_comm_size(comm)

    call monolis_alloc_I_1d(vtxdist, n_size + 1)

    call monolis_allgather_I1(n_internal_vertex, vtxdist(2:n_size + 1), comm)

    do i = 1, n_size
      vtxdist(i + 1) = vtxdist(i + 1) + vtxdist(i)
    enddo
  end subroutine monolis_com_n_vertex_list

  !> @ingroup com
  !> グローバル節点番号を新たに生成
  subroutine monolis_generate_global_vertex_id(n_internal_vertex, n_vertex, vertex_id, com)
    implicit none
    !> [in] 内部ノード数
    integer(kint), intent(in) :: n_internal_vertex
    !> [in] ノード数
    integer(kint), intent(in) :: n_vertex
    !> [in] 領域ごとのノード数を示す配列
    integer(kint), allocatable :: vtxdist(:)
    !> [out] グローバルノード番号配列
    integer(kint) :: vertex_id(:)
    !> [in] com 構造体
    type(monolis_COM) :: com
    integer(kint) :: my_rank, i, origin

    my_rank = monolis_mpi_get_local_my_rank(com%comm)

    call monolis_com_n_vertex_list(n_internal_vertex, com%comm, vtxdist)

    origin = vtxdist(my_rank + 1)

    do i = 1, n_vertex
      vertex_id(i) = origin + i
    enddo

    call monolis_SendRecv_I(com%send_n_neib, com%send_neib_pe, com%recv_n_neib, com%recv_neib_pe, &
    & com%send_index, com%send_item, com%recv_index, com%recv_item, &
    & vertex_id, 1, com%comm)
  end subroutine monolis_generate_global_vertex_id

  !> @ingroup com
  !> 外部節点のグローバル節点番号を取得
  subroutine monolis_update_vertex_domain_id(vertex_domain_id, com)
    implicit none
    !> [out] グローバルノード番号配列
    integer(kint) :: vertex_domain_id(:)
    !> [in] com 構造体
    type(monolis_COM) :: com

    call monolis_SendRecv_I(com%send_n_neib, com%send_neib_pe, com%recv_n_neib, com%recv_neib_pe, &
    & com%send_index, com%send_item, com%recv_index, com%recv_item, &
    & vertex_domain_id, 1, com%comm)
  end subroutine monolis_update_vertex_domain_id

  !> @ingroup dev_com
  !> 全ての外部節点を取得
  subroutine monolis_comm_get_all_external_n_node_parallel(n_internal_vertex, n_vertex, com, n_outer)
    implicit none
    !> [in] 内部節点数
    integer(kint), intent(in) :: n_internal_vertex
    !> [in] 全節点数
    integer(kint), intent(in) :: n_vertex
    !> [in] 分割領域に対応する com 構造体
    type(monolis_COM) :: com
    !> [out] 全ての外部節点配列に属する節点数
    integer(kint) :: n_outer
    integer(kint) :: M, comm_size, i
    integer(kint), allocatable :: counts(:)

    M = n_vertex - n_internal_vertex
    comm_size = monolis_mpi_get_local_comm_size(com%comm)

    !> 個数の共有
    call monolis_alloc_I_1d(counts, comm_size)
    call monolis_allgather_I1(M, counts, com%comm)

    n_outer = 0
    do i = 1, comm_size
      n_outer = n_outer + counts(i)
    enddo
  end subroutine monolis_comm_get_all_external_n_node_parallel

  !> @ingroup dev_com
  !> 全ての外部節点を取得
  subroutine monolis_comm_get_all_external_node_parallel(n_internal_vertex, n_vertex, vertex_id, &
    & com, outer_node_id_all, displs)
    implicit none
    !> [in] 内部節点数
    integer(kint), intent(in) :: n_internal_vertex
    !> [in] 全節点数
    integer(kint), intent(in) :: n_vertex
    !> [in] グローバル節点番号
    integer(kint) :: vertex_id(:)
    !> [in] 分割領域に対応する com 構造体
    type(monolis_COM) :: com
    !> [out] 全ての外部節点番号
    integer(kint) :: outer_node_id_all(:)
    !> 全ての外部節点配列の各領域に属する節点数
    integer(kint) :: displs(:)
    integer(kint) :: M, comm_size, i
    integer(kint), allocatable :: counts(:)
    integer(kint), allocatable :: outer_node_id_local(:)

    M = n_vertex - n_internal_vertex
    comm_size = monolis_mpi_get_local_comm_size(com%comm)

    !> 個数の共有
    call monolis_alloc_I_1d(counts, comm_size)
    call monolis_allgather_I1(M, counts, com%comm)

    !> MPI 通信用 displs 配列の作成
    call monolis_alloc_I_1d(outer_node_id_local, M)

    do i = n_internal_vertex + 1, n_vertex
      outer_node_id_local(i - n_internal_vertex) = vertex_id(i)
    enddo

    do i = 1, comm_size
      displs(i + 1) = displs(i) + counts(i)
    enddo

    !> 全ての外部節点を取得
    call monolis_allgatherv_I(M, outer_node_id_local, outer_node_id_all, counts, displs, com%comm)
  end subroutine monolis_comm_get_all_external_node_parallel

  !> @ingroup dev_com
  !> 全ての外部節点が所属する領域番号を取得
  subroutine monolis_comm_get_all_external_node_domain_id_parallel(n_internal_vertex, vertex_id, com, &
    & outer_node_id_all, outer_domain_id_all, displs)
    implicit none
    !> [in] 内部節点数
    integer(kint), intent(in) :: n_internal_vertex
    !> [in] グローバル節点番号
    integer(kint), intent(in) :: vertex_id(:)
    !> [in] 分割領域に対応する com 構造体
    type(monolis_COM) :: com
    !> [in] 全ての外部節点番号
    integer(kint) :: outer_node_id_all(:)
    !> [out] 全ての外部節点が属する領域番号
    integer(kint) :: outer_domain_id_all(:)
    !> 全ての外部節点配列の各領域に属する節点数
    integer(kint) :: displs(:)
    integer(kint) :: comm_size, n_outer, my_rank
    integer(kint) :: i, j, jS, jE, id, idx
    integer(kint), allocatable :: internal_node_id(:)

    my_rank = monolis_mpi_get_local_my_rank(com%comm)
    comm_size = monolis_mpi_get_local_comm_size(com%comm)
    n_outer = displs(comm_size + 1)

    !> 外点が属する領域番号を取得
    call monolis_alloc_I_1d(internal_node_id, n_internal_vertex)

    do i = 1, n_internal_vertex
      internal_node_id(i) = vertex_id(i)
    enddo

    call monolis_qsort_I_1d(internal_node_id, 1, n_internal_vertex)

    outer_domain_id_all(:) = comm_size + 1

    aa:do i = 1, comm_size
      !> 自領域であればスキップ
      if(i == my_rank + 1) cycle
      !> 他領域の外点と自領域の内点が重複するか判定
      jS = displs(i) + 1
      jE = displs(i + 1)
      do j = jS, jE
        id = outer_node_id_all(j)
        call monolis_bsearch_I(internal_node_id, 1, n_internal_vertex, id, idx)
        if(idx /= -1)then
          outer_domain_id_all(j) = my_rank
        endif
      enddo
    enddo aa

    call monolis_allreduce_I(n_outer, outer_domain_id_all, monolis_mpi_min, com%comm)
  end subroutine monolis_comm_get_all_external_node_domain_id_parallel

  !> @ingroup dev_com
  !> データ通信する recv 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_recv_parallel(n_vertex, vertex_id, com, &
    & outer_node_id_all, outer_domain_id_all, displs, recv_list)
    implicit none
    !> [in] 全節点数
    integer(kint), intent(in) :: n_vertex
    !> [in] グローバル節点番号
    integer(kint), intent(in) :: vertex_id(:)
    !> [in,out] 分割領域に対応する comm 構造体
    type(monolis_COM) :: com
    !> [in] 全ての外部節点番号
    integer(kint) :: outer_node_id_all(:)
    !> [in] 全ての外部節点が属する領域番号
    integer(kint) :: outer_domain_id_all(:)
    !> [in] 全ての外部節点配列の各領域に属する節点数
    integer(kint) :: displs(:)
    !> [out] 分割領域に対応する recv list 構造体
    type(monolis_comm_node_list), allocatable :: recv_list(:)
    integer(kint) :: i, in, j, jS, jE, id, comm_size, my_rank
    integer(kint) :: n_neib_recv, recv_rank, n_data, global_id, idx
    integer(kint), allocatable :: is_neib(:)
    integer(kint), allocatable :: local_nid(:)
    integer(kint), allocatable :: neib_id(:)
    integer(kint), allocatable :: temp(:)

    !> 隣接領域の取得
    my_rank = monolis_mpi_get_local_my_rank(com%comm)
    comm_size = monolis_mpi_get_local_comm_size(com%comm)

    call monolis_alloc_I_1d(is_neib, comm_size)

    in = my_rank + 1
    jS = displs(in) + 1
    jE = displs(in + 1)
    do j = jS, jE
      id = outer_domain_id_all(j)
      is_neib(id + 1) = 1
    enddo
    is_neib(my_rank + 1) = 0

    n_neib_recv = 0
    do i = 1, comm_size
      if(is_neib(i) == 1) n_neib_recv = n_neib_recv + 1
    enddo

    call monolis_alloc_I_1d(neib_id, n_neib_recv)

    j = 0
    do i = 1, comm_size
      if(is_neib(i) == 1)then
        j = j + 1
        neib_id(j) = i - 1
      endif
    enddo

    !> recv の作成
    allocate(recv_list(n_neib_recv))
    call monolis_alloc_I_1d(local_nid, n_vertex)
    call monolis_alloc_I_1d(temp, n_vertex)

    temp(:) = vertex_id(:)
    do i = 1, n_vertex
      local_nid(i) = i
    enddo

    call monolis_qsort_I_2d(temp, local_nid, 1, n_vertex)

    do i = 1, n_neib_recv
      recv_rank = neib_id(i)
      in = my_rank + 1
      jS = displs(in) + 1
      jE = displs(in + 1)

      n_data = 0
      do j = jS, jE
        id = outer_domain_id_all(j)
        if(recv_rank == id)then
          n_data = n_data + 1
        endif
      enddo

      recv_list(i)%n_node = n_data
      call monolis_alloc_I_1d(recv_list(i)%domid, 1)
      call monolis_alloc_I_1d(recv_list(i)%global_id, n_data)
      recv_list(i)%domid = recv_rank

      n_data = 0
      do j = jS, jE
        id = outer_domain_id_all(j)
        if(recv_rank == id)then
          n_data = n_data + 1
          global_id = outer_node_id_all(j)
          call monolis_bsearch_I(temp, 1, n_vertex, global_id, idx)
          recv_list(i)%global_id(n_data) = local_nid(idx)
        endif
      enddo
    enddo

    !> monolis com の構築
    !> recv
    com%recv_n_neib = n_neib_recv
    call monolis_palloc_I_1d(com%recv_neib_pe, n_neib_recv)

    do i = 1, n_neib_recv
      com%recv_neib_pe(i) = recv_list(i)%domid(1)
    enddo

    call monolis_palloc_I_1d(com%recv_index, n_neib_recv + 1)

    do i = 1, n_neib_recv
      com%recv_index(i + 1) = com%recv_index(i) + recv_list(i)%n_node
    enddo

    in = com%recv_index(n_neib_recv + 1)

    call monolis_palloc_I_1d(com%recv_item, in)

    in = 0
    do i = 1, n_neib_recv
      jE = recv_list(i)%n_node
      do j = 1, jE
        in = in + 1
        idx = recv_list(i)%global_id(j)
        com%recv_item(in) = idx
      enddo
    enddo
  end subroutine monolis_comm_get_recv_parallel

  !> @ingroup dev_com
  !> データ通信する send 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_send_parallel(n_vertex, vertex_id, com, recv_list)
    implicit none
    !> [in] 全節点数
    integer(kint), intent(in) :: n_vertex
    !> [in] グローバル節点番号
    integer(kint), intent(in) :: vertex_id(:)
    !> [in,out] 分割領域に対応する com 構造体
    type(monolis_COM) :: com
    !> [out] 分割領域に対応する send list 構造体
    type(monolis_comm_node_list) :: recv_list(:)
    !> 分割領域に対応する send list 構造体
    type(monolis_comm_node_list), allocatable :: send_list(:)

    integer(kint) :: i, in, j, jS, jE, id, comm_size
    integer(kint) :: n_neib_recv, n_data, idx, ierr
    integer(kint) :: n_neib_send
    integer(kint), allocatable :: send_n_list(:)
    integer(kint), allocatable :: local_nid(:)
    integer(kint), allocatable :: temp(:)
    integer(kint), allocatable :: sta1(:,:)
    integer(kint), allocatable :: sta2(:,:)
    integer(kint), allocatable :: req1(:)
    integer(kint), allocatable :: req2(:)
    integer(kint), allocatable :: wr(:)
    integer(kint), allocatable :: ws(:)

    !> send の作成
    !> slave から master に個数を送信
    comm_size = monolis_mpi_get_local_comm_size(com%comm)
    call monolis_alloc_I_1d(send_n_list, comm_size)

    n_neib_recv = com%recv_n_neib

    do i = 1, n_neib_recv
      id = recv_list(i)%domid(1)
      in = recv_list(i)%n_node
      send_n_list(id + 1) = in
    enddo

    call monolis_alltoall_I1(comm_size, send_n_list, com%comm)

    !> send 個数の確保
    n_neib_send = 0
    do i = 1, comm_size
      if(send_n_list(i) > 0) n_neib_send = n_neib_send + 1
    enddo

    allocate(send_list(n_neib_send))

    n_neib_send = 0
    do i = 1, comm_size
      if(send_n_list(i) > 0)then
        n_neib_send = n_neib_send + 1
        call monolis_alloc_I_1d(send_list(n_neib_send)%domid, 1)
        send_list(n_neib_send)%domid(1) = i - 1

        n_data = send_n_list(i)
        call monolis_alloc_I_1d(send_list(n_neib_send)%global_id, n_data)
        send_list(n_neib_send)%n_node = n_data
      endif
    enddo

    !> send の構築
    com%send_n_neib = n_neib_send
    call monolis_palloc_I_1d(com%send_neib_pe, n_neib_send)
    do i = 1, n_neib_send
      com%send_neib_pe(i) = send_list(i)%domid(1)
    enddo
    call monolis_palloc_I_1d(com%send_index, n_neib_send + 1)
    do i = 1, n_neib_send
      com%send_index(i + 1) = com%send_index(i) + send_list(i)%n_node
    enddo
    in = com%send_index(n_neib_send + 1)
    call monolis_palloc_I_1d(com%send_item, in)

    !> slave から master に global_nid を送信
    call monolis_alloc_I_2d(sta1, monolis_mpi_status_size, com%recv_n_neib)
    call monolis_alloc_I_2d(sta2, monolis_mpi_status_size, com%send_n_neib)
    call monolis_alloc_I_1d(req1, com%recv_n_neib)
    call monolis_alloc_I_1d(req2, com%send_n_neib)

    in = com%recv_index(n_neib_recv + 1)
    allocate(ws(in), source = 0)

    do i = 1, com%recv_n_neib
      id = recv_list(i)%domid(1)
      in = recv_list(i)%n_node
      jS = com%recv_index(i) + 1
      jE = com%recv_index(i + 1)
      do j = jS, jE
        idx = com%recv_item(j)
        ws(j) = vertex_id(idx)
      enddo
      call MPI_Isend(ws(jS:jE), in, MPI_INTEGER, id, 0, com%comm, req1(i), ierr)
    enddo

    in = com%send_index(n_neib_send + 1)
    allocate(wr(in), source = 0)
    do i = 1, com%send_n_neib
      id = send_list(i)%domid(1)
      in = send_list(i)%n_node
      jS = com%send_index(i) + 1
      jE = com%send_index(i + 1)
      call MPI_Irecv(wr(jS:jE), in, MPI_INTEGER, id, 0, com%comm, req2(i), ierr)
    enddo

    call MPI_waitall(com%recv_n_neib, req2, sta2, ierr)
    call MPI_waitall(com%send_n_neib, req1, sta1, ierr)

    !> local_nid に変換
    call monolis_alloc_I_1d(local_nid, n_vertex)
    call monolis_alloc_I_1d(temp, n_vertex)

    do i = 1, n_vertex
      temp(i) = vertex_id(i)
      local_nid(i) = i
    enddo

    call monolis_qsort_I_2d(temp, local_nid, 1, n_vertex)

    in = com%send_index(n_neib_send + 1)
    do i = 1, in
      call monolis_bsearch_I(temp, 1, n_vertex, wr(i), id)
      com%send_item(i) = local_nid(id)
    enddo
  end subroutine monolis_comm_get_send_parallel
end module mod_monolis_comm_par_util
