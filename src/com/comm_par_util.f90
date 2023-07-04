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
  !> @brief 分割領域の内部計算点数リスト vtxdist を作成
  subroutine monolis_com_n_vertex_list(n_internal_vertex, comm, vtxdist)
    implicit none
    !> [in] 分割領域における内部計算点数
    integer(kint), intent(in) :: n_internal_vertex
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [out] 分割領域の内部計算点数リスト
    integer(kint), allocatable, intent(out) :: vtxdist(:)
    integer(kint) :: n_size, i

    n_size = monolis_mpi_get_local_comm_size(comm)

    call monolis_alloc_I_1d(vtxdist, n_size + 1)

    call monolis_allgather_I1(n_internal_vertex, vtxdist(2:n_size + 1), comm)

    do i = 1, n_size
      vtxdist(i + 1) = vtxdist(i + 1) + vtxdist(i)
    enddo
  end subroutine monolis_com_n_vertex_list

  !> @ingroup com
  !> @brief グローバル計算点番号を新たに生成
  subroutine monolis_generate_global_vertex_id(n_internal_vertex, n_vertex, vertex_id, com)
    implicit none
    !> [in] 分割領域における内部計算点数
    integer(kint), intent(in) :: n_internal_vertex
    !> [in] ノード数
    integer(kint), intent(in) :: n_vertex
    !> [out] 計算点 id
    integer(kint), intent(out) :: vertex_id(:)
    !> [in] com 構造体
    type(monolis_COM), intent(in) :: com
    integer(kint) :: my_rank, i, origin
    real(kdouble) :: tcomm
    integer(kint), allocatable :: vtxdist(:)

    my_rank = monolis_mpi_get_local_my_rank(com%comm)

    call monolis_com_n_vertex_list(n_internal_vertex, com%comm, vtxdist)

    origin = vtxdist(my_rank + 1)

    do i = 1, n_vertex
      vertex_id(i) = origin + i
    enddo

    call monolis_mpi_update_I(com, 1, vertex_id, tcomm)
  end subroutine monolis_generate_global_vertex_id

  !> @ingroup com
  !> @brief 外部計算点のグローバル計算点番号を取得
  subroutine monolis_update_vertex_domain_id(vertex_domain_id, com)
    implicit none
    !> [in,out] グローバルノード番号配列
    integer(kint), intent(inout) :: vertex_domain_id(:)
    !> [in] com 構造体
    type(monolis_COM), intent(in) :: com
    real(kdouble) :: tcomm

    call monolis_mpi_update_I(com, 1, vertex_domain_id, tcomm)
  end subroutine monolis_update_vertex_domain_id

  !> @ingroup dev_com
  !> @brief 全ての外部計算点を取得
  subroutine monolis_comm_get_all_external_n_node_parallel(n_internal_vertex, n_vertex, comm, n_outer)
    implicit none
    !> [in] 分割領域における内部計算点数
    integer(kint), intent(in) :: n_internal_vertex
    !> [in] 全計算点数
    integer(kint), intent(in) :: n_vertex
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [out] 全ての外部計算点配列に属する計算点数
    integer(kint), intent(out) :: n_outer
    integer(kint) :: M, comm_size, i
    integer(kint), allocatable :: counts(:)

    M = n_vertex - n_internal_vertex
    comm_size = monolis_mpi_get_local_comm_size(comm)

    !> 個数の共有
    call monolis_alloc_I_1d(counts, comm_size)
    call monolis_allgather_I1(M, counts, comm)

    n_outer = 0
    do i = 1, comm_size
      n_outer = n_outer + counts(i)
    enddo
  end subroutine monolis_comm_get_all_external_n_node_parallel

  !> @ingroup dev_com
  !> @brief 全ての外部計算点を取得
  subroutine monolis_comm_get_all_external_node_parallel(n_internal_vertex, n_vertex, vertex_id, &
    & comm, outer_node_id_all, displs)
    implicit none
    !> [in] 分割領域における内部計算点数
    integer(kint), intent(in) :: n_internal_vertex
    !> [in] 全計算点数
    integer(kint), intent(in) :: n_vertex
    !> [in] 計算点 id
    integer(kint), intent(in) :: vertex_id(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [out] 全ての外部計算点番号
    integer(kint), intent(out) :: outer_node_id_all(:)
    !> [in,out] 全ての外部計算点配列の各領域に属する計算点数
    integer(kint), intent(inout) :: displs(:)
    integer(kint) :: M, comm_size, i
    integer(kint), allocatable :: counts(:)
    integer(kint), allocatable :: outer_node_id_local(:)

    M = n_vertex - n_internal_vertex
    comm_size = monolis_mpi_get_local_comm_size(comm)

    !> 個数の共有
    call monolis_alloc_I_1d(counts, comm_size)
    call monolis_allgather_I1(M, counts, comm)

    !> MPI 通信用 displs 配列の作成
    call monolis_alloc_I_1d(outer_node_id_local, M)

    do i = n_internal_vertex + 1, n_vertex
      outer_node_id_local(i - n_internal_vertex) = vertex_id(i)
    enddo

    displs = 0

    do i = 1, comm_size
      displs(i + 1) = displs(i) + counts(i)
    enddo

    !> 全ての外部計算点を取得
    call monolis_allgatherv_I(M, outer_node_id_local, outer_node_id_all, counts, displs, comm)
  end subroutine monolis_comm_get_all_external_node_parallel

  !> @ingroup dev_com
  !> @brief 全ての外部計算点が所属する領域番号を取得
  subroutine monolis_comm_get_all_external_node_domain_id_parallel(n_internal_vertex, vertex_id, comm, &
    & outer_node_id_all, outer_domain_id_all, displs)
    implicit none
    !> [in] 分割領域における内部計算点数
    integer(kint), intent(in) :: n_internal_vertex
    !> [in] 計算点 id
    integer(kint), intent(in) :: vertex_id(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] 全ての外部計算点番号
    integer(kint), intent(in) :: outer_node_id_all(:)
    !> [out] 全ての外部計算点が属する領域番号
    integer(kint), intent(out) :: outer_domain_id_all(:)
    !> [in] 全ての外部計算点配列の各領域に属する計算点数
    integer(kint), intent(in) :: displs(:)
    integer(kint) :: comm_size, n_outer, my_rank
    integer(kint) :: i, j, jS, jE, id, idx
    integer(kint), allocatable :: internal_node_id(:)

    my_rank = monolis_mpi_get_local_my_rank(comm)
    comm_size = monolis_mpi_get_local_comm_size(comm)
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

    call monolis_allreduce_I(n_outer, outer_domain_id_all, monolis_mpi_min, comm)
  end subroutine monolis_comm_get_all_external_node_domain_id_parallel

  !> @ingroup dev_com
  !> @brief データ通信する recv 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_recv_parallel_n_neib(comm, outer_domain_id_all, displs, n_neib_recv, is_neib)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] 全ての外部計算点が属する領域番号
    integer(kint), intent(in) :: outer_domain_id_all(:)
    !> [in] 全ての外部計算点配列の各領域に属する計算点数
    integer(kint), intent(in) :: displs(:)
    !> [out] 隣接する領域数
    integer(kint), intent(out) :: n_neib_recv
    !> [out] 隣接する領域フラグ（サイズ：[comm_size]）
    integer(kint), intent(out) :: is_neib(:)
    integer(kint) :: i, in, j, jS, jE, id, comm_size, my_rank

    !> 隣接領域の取得
    my_rank = monolis_mpi_get_local_my_rank(comm)
    comm_size = monolis_mpi_get_local_comm_size(comm)

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
  end subroutine monolis_comm_get_recv_parallel_n_neib

  !> @ingroup dev_com
  !> @brief データ通信する recv 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_recv_parallel_neib_id(comm, is_neib, neib_id)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] 隣接する領域フラグ（サイズ：[comm_size]）
    integer(kint), intent(in) :: is_neib(:)
    !> [out] 隣接領域番号
    integer(kint), intent(out) :: neib_id(:)
    integer(kint) :: i, j, comm_size

    comm_size = monolis_mpi_get_local_comm_size(comm)

    j = 0
    do i = 1, comm_size
      if(is_neib(i) == 1)then
        j = j + 1
        neib_id(j) = i - 1
      endif
    enddo
  end subroutine monolis_comm_get_recv_parallel_neib_id

  !> @ingroup dev_com
  !> @brief データ通信する recv 隣接領域の index 配列取得（並列実行版）
  subroutine monolis_comm_get_recv_parallel_index(comm, displs, outer_domain_id_all, n_neib_recv, neib_id, index)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] 全ての外部計算点配列の各領域に属する計算点数
    integer(kint), intent(in) :: displs(:)
    !> [in] 全ての外部計算点が属する領域番号
    integer(kint), intent(in) :: outer_domain_id_all(:)
    !> [in] 隣接する領域数
    integer(kint), intent(in) :: n_neib_recv
    !> [in] 隣接領域番号
    integer(kint), intent(in) :: neib_id(:)
    !> [out] recv 隣接領域の index 配列
    integer(kint), intent(out) :: index(:)
    integer(kint) :: i, in, j, jS, jE, n_data, id, my_rank, recv_rank

    index = 0
    my_rank = monolis_mpi_get_local_my_rank(comm)

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

      index(i + 1) = n_data
    enddo

    do i = 1, n_neib_recv
      index(i + 1) = index(i + 1) + index(i)
    enddo
  end subroutine monolis_comm_get_recv_parallel_index

  !> @ingroup dev_com
  !> @brief データ通信する recv 隣接領域の item 配列取得（並列実行版）
  subroutine monolis_comm_get_recv_parallel_item(n_vertex, vertex_id, comm, &
    & outer_node_id_all, outer_domain_id_all, displs, recv_n_neib, neib_id, index, item)
    implicit none
    !> [in] 全計算点数
    integer(kint), intent(in) :: n_vertex
    !> [in] 計算点 id
    integer(kint), intent(in) :: vertex_id(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] 全ての外部計算点番号
    integer(kint), intent(in) :: outer_node_id_all(:)
    !> [in] 全ての外部計算点が属する領域番号
    integer(kint), intent(in) :: outer_domain_id_all(:)
    !> [in] 全ての外部計算点配列の各領域に属する計算点数
    integer(kint), intent(in) :: displs(:)
    !> [in] 隣接する領域数
    integer(kint), intent(in) :: recv_n_neib
    !> [in] 隣接領域番号
    integer(kint), intent(in) :: neib_id(:)
    !> [in] recv 隣接領域の index 配列
    integer(kint), intent(in) :: index(:)
    !> [out] recv 隣接領域の item 配列
    integer(kint), intent(out) :: item(:)
    integer(kint) :: i, in, j, jn, jS, jE, n_data, id, my_rank
    integer(kint) :: recv_rank, global_id, idx
    integer(kint), allocatable :: local_nid(:)
    integer(kint), allocatable :: temp(:)
    integer(kint), allocatable :: count(:)

    my_rank = monolis_mpi_get_local_my_rank(comm)

    call monolis_alloc_I_1d(local_nid, n_vertex)
    call monolis_get_sequence_array_I(local_nid, n_vertex, 1, 1)

    call monolis_alloc_I_1d(temp, n_vertex)
    temp(:) = vertex_id(:)

    call monolis_qsort_I_2d(temp, local_nid, 1, n_vertex)

    call monolis_alloc_I_1d(count, recv_n_neib)

    item = 0

    count = 0
    do i = 1, recv_n_neib
      n_data = 0
      recv_rank = neib_id(i)
      in = my_rank + 1
      jS = displs(in) + 1
      jE = displs(in + 1)
      do j = jS, jE
        id = outer_domain_id_all(j)
        if(recv_rank == id)then
          n_data = n_data + 1
          global_id = outer_node_id_all(j)
          call monolis_bsearch_I(temp, 1, n_vertex, global_id, idx)
          jn = index(i) + 1 + count(i)
          item(jn) = local_nid(idx)
          count(i) = count(i) + 1
        endif
      enddo
    enddo
  end subroutine monolis_comm_get_recv_parallel_item

  !> @ingroup dev_com
  !> @brief データ通信する recv 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_recv_parallel(n_vertex, vertex_id, com, &
    & outer_node_id_all, outer_domain_id_all, displs)
    implicit none
    !> [in] 全計算点数
    integer(kint), intent(in) :: n_vertex
    !> [in] 計算点 id
    integer(kint), intent(in) :: vertex_id(:)
    !> [in,out] 分割領域に対応する comm 構造体
    type(monolis_COM), intent(inout) :: com
    !> [in] 全ての外部計算点番号
    integer(kint), intent(in) :: outer_node_id_all(:)
    !> [in] 全ての外部計算点が属する領域番号
    integer(kint), intent(in) :: outer_domain_id_all(:)
    !> [in] 全ての外部計算点配列の各領域に属する計算点数
    integer(kint), intent(in) :: displs(:)
    integer(kint) :: comm_size
    integer(kint), allocatable :: is_neib(:)

    !> 隣接領域の取得
    comm_size = monolis_mpi_get_local_comm_size(com%comm)

    call monolis_alloc_I_1d(is_neib, comm_size)

    call monolis_comm_get_recv_parallel_n_neib(com%comm, outer_domain_id_all, displs, com%recv_n_neib, is_neib)

    call monolis_palloc_I_1d(com%recv_neib_pe, com%recv_n_neib)

    call monolis_comm_get_recv_parallel_neib_id(com%comm, is_neib, com%recv_neib_pe)

    call monolis_palloc_I_1d(com%recv_index, com%recv_n_neib + 1)

    call monolis_comm_get_recv_parallel_index(com%comm, displs, outer_domain_id_all, &
      & com%recv_n_neib, com%recv_neib_pe, com%recv_index)

    call monolis_palloc_I_1d(com%recv_item, com%recv_index(com%recv_n_neib + 1))

    call monolis_comm_get_recv_parallel_item(n_vertex, vertex_id, com%comm, &
      & outer_node_id_all, outer_domain_id_all, displs, com%recv_n_neib, com%recv_neib_pe, com%recv_index, com%recv_item)
  end subroutine monolis_comm_get_recv_parallel

  !> @ingroup dev_com
  !> @brief データ通信する send 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_send_parallel_n_list(comm, recv_n_neib, recv_neib_pe, recv_index, send_n_list)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] 隣接する領域数
    integer(kint), intent(in) :: recv_n_neib
    !> [in] 隣接領域番号
    integer(kint), intent(in) :: recv_neib_pe(:)
    !> [in] recv 隣接領域の index 配列
    integer(kint), intent(in) :: recv_index(:)
    !> [out] send 計算点の個数リスト
    integer(kint), intent(out) :: send_n_list(:)
    integer(kint) :: i, jS, jE, id, comm_size

    comm_size = monolis_mpi_get_local_comm_size(comm)

    send_n_list = 0

    do i = 1, recv_n_neib
      id = recv_neib_pe(i)
      jS = recv_index(i)
      jE = recv_index(i + 1)
      send_n_list(id + 1) = jE - jS
    enddo

    call monolis_alltoall_I1(comm_size, send_n_list, comm)
  end subroutine monolis_comm_get_send_parallel_n_list

  !> @ingroup dev_com
  !> @brief データ通信する send 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_send_parallel_n_neib(comm, send_n_list, n_neib_send)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] send 計算点の個数リスト
    integer(kint), intent(in) :: send_n_list(:)
    !> [out] 隣接する領域数
    integer(kint), intent(out) :: n_neib_send
    integer(kint) :: i, comm_size

    comm_size = monolis_mpi_get_local_comm_size(comm)

    !> send 個数の確保
    n_neib_send = 0
    do i = 1, comm_size
      if(send_n_list(i) > 0) n_neib_send = n_neib_send + 1
    enddo
  end subroutine monolis_comm_get_send_parallel_n_neib

  !> @ingroup dev_com
  !> @brief データ通信する send 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_send_parallel_neib_id(comm, send_n_list, send_neib_pe)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] send 計算点の個数リスト
    integer(kint), intent(in):: send_n_list(:)
    !> [out] 隣接する領域番号
    integer(kint), intent(out) :: send_neib_pe(:)
    integer(kint) :: i, in, comm_size

    comm_size = monolis_mpi_get_local_comm_size(comm)

    send_neib_pe = 0

    in = 0
    do i = 1, comm_size
      if(send_n_list(i) > 0)then
        in = in + 1
        send_neib_pe(in) = i - 1
      endif
    enddo
  end subroutine monolis_comm_get_send_parallel_neib_id

  !> @ingroup dev_com
  !> @brief データ通信する send 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_send_parallel_index(comm, send_n_list, send_n_neib, send_index)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] send 計算点の個数リスト
    integer(kint), intent(in) :: send_n_list(:)
    !> [in] 隣接する領域数
    integer(kint), intent(in) :: send_n_neib
    !> [out] send 計算点の index 配列
    integer(kint), intent(out) :: send_index(:)
    integer(kint) :: i, in, comm_size

    comm_size = monolis_mpi_get_local_comm_size(comm)

    in = 1
    send_index = 0
    do i = 1, comm_size
      if(send_n_list(i) > 0)then
        in = in + 1
        send_index(in) = send_n_list(i)
      endif
    enddo

    do i = 1, send_n_neib
      send_index(i + 1) = send_index(i + 1) + send_index(i)
    enddo
  end subroutine monolis_comm_get_send_parallel_index

  !> @ingroup dev_com
  !> @brief データ通信する send の item 配列の取得（並列実行版）
  subroutine monolis_comm_get_send_parallel_item(comm, n_vertex, vertex_id, &
    & recv_n_neib, recv_neib_pe, recv_index, recv_item, &
    & send_n_neib, send_neib_pe, send_index, send_item)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] ノード数
    integer(kint), intent(in) :: n_vertex
    !> [in] 計算点 id
    integer(kint), intent(in) :: vertex_id(:)
    !> [in] 隣接する領域数
    integer(kint), intent(in) :: recv_n_neib
    !> [in] 隣接する領域番号
    integer(kint), intent(in) :: recv_neib_pe(:)
    !> [in] 隣接する領域数
    integer(kint), intent(in) :: recv_index(:)
    !> [in] 隣接する領域数
    integer(kint), intent(in) :: recv_item(:)
    !> [in] 隣接する領域数
    integer(kint), intent(in) :: send_n_neib
    !> [in] 隣接する領域番号
    integer(kint), intent(in) :: send_neib_pe(:)
    !> [in] 隣接する領域数
    integer(kint), intent(in) :: send_index(:)
    !> [out] 隣接する領域数
    integer(kint), intent(out) :: send_item(:)
    integer(kint) :: i, id, j, jS, jE, idx, ierr
    integer(kint), allocatable :: ws(:)
    integer(kint), allocatable :: wr(:)
    integer(kint), allocatable :: sta1(:,:)
    integer(kint), allocatable :: sta2(:,:)
    integer(kint), allocatable :: req1(:)
    integer(kint), allocatable :: req2(:)
    integer(kint), allocatable :: local_nid(:)
    integer(kint), allocatable :: temp(:)
    integer(kint), allocatable :: count(:)

    !> slave から master に global_nid を送信
    call monolis_alloc_I_2d(sta1, monolis_mpi_status_size, recv_n_neib)
    call monolis_alloc_I_2d(sta2, monolis_mpi_status_size, send_n_neib)
    call monolis_alloc_I_1d(req1, recv_n_neib)
    call monolis_alloc_I_1d(req2, send_n_neib)

    call monolis_alloc_I_1d(ws, recv_index(recv_n_neib + 1))

    do i = 1, recv_n_neib
      id = recv_neib_pe(i)
      jS = recv_index(i) + 1
      jE = recv_index(i + 1)
      do j = jS, jE
        idx = recv_item(j)
        ws(j) = vertex_id(idx)
      enddo
      call MPI_Isend(ws(jS:jE), jE - jS + 1, MPI_INTEGER, id, 0, comm, req1(i), ierr)
    enddo

    call monolis_alloc_I_1d(wr, send_index(send_n_neib + 1))

    do i = 1, send_n_neib
      id = send_neib_pe(i)
      jS = send_index(i) + 1
      jE = send_index(i + 1)
      call MPI_Irecv(wr(jS:jE), jE - jS + 1, MPI_INTEGER, id, 0, comm, req2(i), ierr)
    enddo

    call MPI_waitall(recv_n_neib, req1, sta1, ierr)
    call MPI_waitall(send_n_neib, req2, sta2, ierr)

    !> local_nid に変換
    call monolis_alloc_I_1d(local_nid, n_vertex)
    call monolis_get_sequence_array_I(local_nid, n_vertex, 1, 1)

    call monolis_alloc_I_1d(temp, n_vertex)
    temp = vertex_id

    call monolis_qsort_I_2d(temp, local_nid, 1, n_vertex)

    do i = 1, send_index(send_n_neib + 1)
      call monolis_bsearch_I(temp, 1, n_vertex, wr(i), id)
      send_item(i) = local_nid(id)
    enddo
  end subroutine monolis_comm_get_send_parallel_item

  !> @ingroup dev_com
  !> @brief データ通信する send 隣接領域の取得（並列実行版）
  subroutine monolis_comm_get_send_parallel(n_vertex, vertex_id, com)
    implicit none
    !> [in] 全計算点数
    integer(kint), intent(in) :: n_vertex
    !> [in] 計算点 id
    integer(kint), intent(in) :: vertex_id(:)
    !> [in,out] 分割領域に対応する com 構造体
    type(monolis_COM), intent(inout) :: com
    integer(kint) :: comm_size
    integer(kint), allocatable :: send_n_list(:)

    comm_size = monolis_mpi_get_local_comm_size(com%comm)

    call monolis_alloc_I_1d(send_n_list, comm_size)

    call monolis_comm_get_send_parallel_n_list(com%comm, &
      & com%recv_n_neib, com%recv_neib_pe, com%recv_index, send_n_list)

    call monolis_comm_get_send_parallel_n_neib(com%comm, send_n_list, com%send_n_neib)

    call monolis_palloc_I_1d(com%send_neib_pe, com%send_n_neib)

    call monolis_comm_get_send_parallel_neib_id(com%comm, send_n_list, com%send_neib_pe)

    call monolis_palloc_I_1d(com%send_index, com%send_n_neib + 1)

    call monolis_comm_get_send_parallel_index(com%comm, send_n_list, com%send_n_neib, com%send_index)

    call monolis_palloc_I_1d(com%send_item, com%send_index(com%send_n_neib + 1))

    call monolis_comm_get_send_parallel_item(com%comm, n_vertex, vertex_id, &
      & com%recv_n_neib, com%recv_neib_pe, com%recv_index, com%recv_item, &
      & com%send_n_neib, com%send_neib_pe, com%send_index, com%send_item)
  end subroutine monolis_comm_get_send_parallel
end module mod_monolis_comm_par_util
