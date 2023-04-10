!> 通信データモジュール
!# subroutine monolis_com_initialize(COM)
!# subroutine monolis_com_finalize(COM)
!# subroutine monolis_com_set_communicator(COM, comm)
!# subroutine monolis_com_get_communicator(COM, comm)
!# subroutine monolis_com_set_my_rank(COM, my_rank)
!# subroutine monolis_com_get_my_rank(COM, my_rank)
!# subroutine monolis_com_set_comm_size(COM, comm_size)
!# subroutine monolis_com_get_comm_size(COM, comm_size)
!# subroutine monolis_com_set_n_internal_vertex(COM, n_internal_vertex)
!# subroutine monolis_com_get_n_internal_vertex(COM, n_internal_vertex)
!# subroutine monolis_com_debug_write(COM)
module mod_monolis_utils_define_com
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  use mod_monolis_utils_palloc
  use mod_monolis_mpi_util
  implicit none

  !> COM 構造体
  type monolis_COM
    !> MPI コミュニケータ
    integer(kint) :: comm
    !> MPI ランク番号
    integer(kint) :: my_rank
    !> MPI コミュニケータサイズ
    integer(kint) :: comm_size
    !> 分割領域における内部計算点数
    integer(kint) :: n_internal_vertex
    !> 受信する領域数
    integer(kint) :: recv_n_neib
    !> 受信する領域番号リスト
    integer(kint), pointer :: recv_neib_pe(:) => null()
    !> 受信するノード番号の index 配列
    integer(kint), pointer :: recv_index(:) => null()
    !> 受信するノード番号の item 配列
    integer(kint), pointer :: recv_item(:) => null()
    !> 送信する領域数
    integer(kint) :: send_n_neib
    !> 送信する領域番号リスト
    integer(kint), pointer :: send_neib_pe(:) => null()
    !> 送信するノード番号の index 配列
    integer(kint), pointer :: send_index(:) => null()
    !> 送信するノード番号の item 配列
    integer(kint), pointer :: send_item(:) => null()
  end type monolis_COM

  !> 通信テーブル作成用ノードリスト構造体
  type monolis_comm_node_list
    integer(kint) :: n_node = 0
    integer(kint), allocatable :: domid(:)
    integer(kint), allocatable :: global_id(:)
  end type monolis_comm_node_list

contains

  !> @ingroup com
  !> COM 構造体の初期化関数
  subroutine monolis_com_initialize(COM)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM  !inout?

    COM%comm = monolis_mpi_get_global_comm()
    COM%my_rank = monolis_mpi_get_global_my_rank()
    COM%comm_size = monolis_mpi_get_global_comm_size()
    COM%n_internal_vertex = 0

    COM%recv_n_neib = 0
    call monolis_pdealloc_I_1d(COM%recv_neib_pe)
    call monolis_pdealloc_I_1d(COM%recv_index)
    call monolis_pdealloc_I_1d(COM%recv_item)

    COM%send_n_neib = 0
    call monolis_pdealloc_I_1d(COM%send_neib_pe)
    call monolis_pdealloc_I_1d(COM%send_index)
    call monolis_pdealloc_I_1d(COM%send_item)
  end subroutine monolis_com_initialize

  !> @ingroup com
  !> COM 構造体の終了処理関数
  subroutine monolis_com_finalize(COM)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM  !inout?

    COM%comm = 0
    COM%my_rank = 0
    COM%comm_size = 1
    COM%n_internal_vertex = 0

    COM%recv_n_neib = 0
    call monolis_pdealloc_I_1d(COM%recv_neib_pe)
    call monolis_pdealloc_I_1d(COM%recv_index)
    call monolis_pdealloc_I_1d(COM%recv_item)

    COM%send_n_neib = 0
    call monolis_pdealloc_I_1d(COM%send_neib_pe)
    call monolis_pdealloc_I_1d(COM%send_index)
    call monolis_pdealloc_I_1d(COM%send_item)
  end subroutine monolis_com_finalize

  !> @ingroup com
  !> COM 構造体に MPI コミュニケータを設定
  subroutine monolis_com_set_communicator(COM, comm)
    implicit none
    !> [out] COM 構造体
    type(monolis_COM), intent(out) :: COM
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    COM%comm = comm
  end subroutine monolis_com_set_communicator

  !> @ingroup com
  !> COM 構造体から MPI コミュニケータを取得
  subroutine monolis_com_get_communicator(COM, comm)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    !> [out] MPI コミュニケータ
    integer(kint), intent(out) :: comm
    comm = COM%comm
  end subroutine monolis_com_get_communicator

  !> @ingroup com
  !> COM 構造体に MPI ランク番号を設定
  subroutine monolis_com_set_my_rank(COM, my_rank)
    implicit none
    !> [out] COM 構造体
    type(monolis_COM), intent(out) :: COM
    !> [in] MPI ランク番号
    integer(kint), intent(in) :: my_rank
    COM%my_rank = my_rank
  end subroutine monolis_com_set_my_rank

  !> @ingroup com
  !> COM 構造体から MPI ランク番号を取得
  subroutine monolis_com_get_my_rank(COM, my_rank)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    !> [out] MPI ランク番号
    integer(kint), intent(out) :: my_rank
    my_rank = COM%my_rank
  end subroutine monolis_com_get_my_rank

  !> @ingroup com
  !> COM 構造体に MPI コミュニケータサイズを設定
  subroutine monolis_com_set_comm_size(COM, comm_size)
    implicit none
    !> [out] COM 構造体
    type(monolis_COM), intent(out) :: COM
    !> [in] MPI コミュニケータサイズ
    integer(kint), intent(in) :: comm_size
    COM%comm_size = comm_size
  end subroutine monolis_com_set_comm_size

  !> @ingroup com
  !> COM 構造体から MPI コミュニケータサイズを取得
  subroutine monolis_com_get_comm_size(COM, comm_size)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    !> [out] MPI コミュニケータサイズ
    integer(kint), intent(out) :: comm_size
    comm_size = COM%comm_size
  end subroutine monolis_com_get_comm_size

  !> @ingroup com
  !> COM 構造体に分割領域における内部計算点数を設定
  subroutine monolis_com_set_n_internal_vertex(COM, n_internal_vertex)
    implicit none
    !> [out] COM 構造体
    type(monolis_COM), intent(out) :: COM
    !> [in] 分割領域における内部計算点数
    integer(kint), intent(in) :: n_internal_vertex
    COM%n_internal_vertex = n_internal_vertex
  end subroutine monolis_com_set_n_internal_vertex

  !> @ingroup com
  !> COM 構造体から分割領域における内部計算点数を取得
  subroutine monolis_com_get_n_internal_vertex(COM, n_internal_vertex)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    !> [out] 分割領域における内部計算点数
    integer(kint), intent(out) :: n_internal_vertex
    n_internal_vertex = COM%n_internal_vertex
  end subroutine monolis_com_get_n_internal_vertex

  !> @ingroup dev_com
  !> COM 構造体のデバッグ用データ書き出し
  subroutine monolis_com_debug_write(COM)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM

    write(*,*)"--- monolis_com_debug_write"
    write(*,*)"COM%comm"
    write(*,*)COM%comm
    write(*,*)"COM%my_rank"
    write(*,*)COM%my_rank
    write(*,*)"COM%comm_size"
    write(*,*)COM%comm_size
    write(*,*)"COM%n_internal_vertex"
    write(*,*)COM%n_internal_vertex
    write(*,*)"COM%recv_n_neib"
    write(*,*)COM%recv_n_neib
    write(*,*)"COM%recv_neib_pe"
    write(*,*)COM%recv_neib_pe
    write(*,*)"COM%recv_index"
    write(*,*)COM%recv_index
    write(*,*)"COM%recv_item"
    write(*,*)COM%recv_item
    write(*,*)"COM%send_n_neib"
    write(*,*)COM%send_n_neib
    write(*,*)"COM%send_neib_pe"
    write(*,*)COM%send_neib_pe
    write(*,*)"COM%send_index"
    write(*,*)COM%send_index
    write(*,*)"COM%send_item"
    write(*,*)COM%send_item
  end subroutine monolis_com_debug_write
end module mod_monolis_utils_define_com