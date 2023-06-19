!> グラフ分割モジュール（通信テーブル作成）
module mod_monolis_comm_table
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_define_com
  use mod_monolis_mpi
  use mod_monolis_mpi_util
  use mod_monolis_comm_par_util
  implicit none

contains

  !> @ingroup com
  !> @brief 通信テーブルを作成（並列実行版）
  subroutine monolis_com_get_comm_table_parallel(n_internal_vertex, n_vertex, vertex_id, com)
    implicit none
    !> [in] 分割領域における内部計算点数
    integer(kint), intent(in) :: n_internal_vertex
    !> [in] 全計算点数
    integer(kint), intent(in) :: n_vertex
    !> [in] 計算点 id
    integer(kint), intent(in) :: vertex_id(:)
    !> [in,out] 分割領域に対応する com 構造体
    type(monolis_COM) :: com
    !> 全ての外部計算点番号（グローバル番号）
    integer(kint), allocatable :: outer_node_id_all_global(:)
    !> 全ての外部計算点が属する領域番号
    integer(kint), allocatable :: outer_domain_id_all(:)
    !> 全ての外部計算点配列の各領域に属する計算点数
    integer(kint), allocatable :: displs(:)
    integer(kint) :: n_outer_node

    com%my_rank = monolis_mpi_get_local_my_rank(com%comm)
    com%comm_size = monolis_mpi_get_local_comm_size(com%comm)
    com%n_internal_vertex = n_internal_vertex

    call monolis_comm_get_all_external_n_node_parallel(n_internal_vertex, n_vertex, com%comm, n_outer_node)

    call monolis_alloc_I_1d(outer_node_id_all_global, n_outer_node)
    call monolis_alloc_I_1d(displs, com%comm_size + 1)

    call monolis_comm_get_all_external_node_parallel(n_internal_vertex, n_vertex, vertex_id, com%comm, &
      & outer_node_id_all_global, displs)

    call monolis_alloc_I_1d(outer_domain_id_all, n_outer_node)

    call monolis_comm_get_all_external_node_domain_id_parallel(n_internal_vertex, vertex_id, com%comm, &
      & outer_node_id_all_global, outer_domain_id_all, displs)

    call monolis_comm_get_recv_parallel(n_vertex, vertex_id, com, &
      & outer_node_id_all_global, outer_domain_id_all, displs)

    call monolis_comm_get_send_parallel(n_vertex, vertex_id, com)
  end subroutine monolis_com_get_comm_table_parallel

  !> @ingroup com
  !> @brief monolis 構造体に内部領域に属する単一メッシュのリストを取得
  subroutine monolis_get_bool_list_of_internal_simple_mesh(monoCOM, n_node, n_elem, n_base, elem, list)
    implicit none
    !> [in] 分割領域に対応する com 構造体
    type(monolis_COM), intent(in) :: monoCOM
    !> [in] 節点数
    integer(kint), intent(in) :: n_node
    !> [in] 要素数
    integer(kint), intent(in) :: n_elem
    !> [in] 要素を構成する形状関数の数
    integer(kint), intent(in) :: n_base
    !> [in] 要素コネクティビティ
    integer(kint), intent(in) :: elem(:,:)
    !> [out] 内部領域の所属を表すフラグ配列（false：内部領域に属する、true：内部領域に属さない）
    logical, intent(out) :: list(:)
    integer(kint) :: i, in, j, id(n_base), n_internal_vertex, my_rank
    integer(kint), allocatable :: domain_id(:)

    if(monolis_mpi_get_local_comm_size(monoCOM%comm) == 1)then
      list = .true.
      return
    endif

    call monolis_com_get_n_internal_vertex(monoCOM, n_internal_vertex)

    my_rank = monolis_mpi_get_local_my_rank(monoCOM%comm)

    call monolis_alloc_I_1d(domain_id, n_node)

    do i = 1, n_internal_vertex
      domain_id(i) = my_rank
    enddo

    call monolis_mpi_update_I(monoCOM, 1, domain_id)

    list = .false.
    do i = 1, n_elem
      do j = 1, n_base
        id(j) = domain_id(elem(j,i))
      enddo
      in = minval(id)
      if(in == my_rank) list(i) = .true.
    enddo
  end subroutine monolis_get_bool_list_of_internal_simple_mesh

  !> @ingroup com
  !> @brief monolis 構造体に内部領域に属するコネクティビティのリストを取得
  subroutine monolis_get_bool_list_of_internal_connetivity(monoCOM, n_node, n_elem, index, item, list)
    implicit none
    !> [in] 分割領域に対応する com 構造体
    type(monolis_COM), intent(in) :: monoCOM
    !> [in] 節点数
    integer(kint), intent(in) :: n_node
    !> [in] 要素数
    integer(kint), intent(in) :: n_elem
    !> [in] 要素コネクティビティの index 配列
    integer(kint), intent(in) :: index(:)
    !> [in] 要素コネクティビティの item 配列
    integer(kint), intent(in) :: item(:)
    !> [out] 内部領域の所属を表すフラグ配列（false：内部領域に属する、true：内部領域に属さない）
    logical, intent(out) :: list(:)
    integer(kint) :: i, in, j, jS, jE, n_internal_vertex, my_rank
    integer(kint), allocatable :: id(:)
    integer(kint), allocatable :: domain_id(:)

    if(monolis_mpi_get_local_comm_size(monoCOM%comm) == 1)then
      list = .true.
      return
    endif

    call monolis_com_get_n_internal_vertex(monoCOM, n_internal_vertex)

    my_rank = monolis_mpi_get_local_my_rank(monoCOM%comm)

    call monolis_alloc_I_1d(domain_id, n_node)

    do i = 1, n_internal_vertex
      domain_id(i) = my_rank
    enddo

    call monolis_mpi_update_I(monoCOM, 1, domain_id)

    list = .false.
    do i = 1, n_elem
      jS = index(i) + 1
      jE = index(i + 1)

      call monolis_alloc_I_1d(id, jE - jS + 1)
      do j = jS, jE
        id(j - jS + 1) = domain_id(item(j))
      enddo

      in = minval(id)
      call monolis_dealloc_I_1d(id)

      if(in == my_rank) list(i) = .true.
    enddo
  end subroutine monolis_get_bool_list_of_internal_connetivity

end module mod_monolis_comm_table
