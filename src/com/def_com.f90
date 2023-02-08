!> 通信データモジュール
module mod_monolis_utils_define_com
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  implicit none

  !> COM 構造体
  type monolis_COM
    !> MPI コミュニケータ
    integer(kint) :: comm
    !> MPI ランク番号
    integer(kint) :: my_rank
    !> MPI コミュニケータサイズ
    integer(kint) :: comm_size
    !> 内部領域に属する自由度数
    integer(kint) :: n_internal_vertex
    !> 受信する領域数
    integer(kint) :: recv_n_neib
    !> 受信する領域番号リスト
    integer(kint), allocatable :: recv_neib_pe(:)
    !> 受信するノード番号の index 配列
    integer(kint), allocatable :: recv_index(:)
    !> 受信するノード番号の item 配列
    integer(kint), allocatable :: recv_item(:)
    !> 送信する領域数
    integer(kint) :: send_n_neib
    !> 送信する領域番号リスト
    integer(kint), allocatable :: send_neib_pe(:)
    !> 送信するノード番号の index 配列
    integer(kint), allocatable :: send_index(:)
    !> 送信するノード番号の item 配列
    integer(kint), allocatable :: send_item(:)
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
    type(monolis_COM) :: COM

    COM%comm = 0
    COM%my_rank = 0
    COM%comm_size = 0
    COM%n_internal_vertex = 0

    COM%recv_n_neib = 0
    call monolis_dealloc_I_1d(COM%recv_neib_pe)
    call monolis_dealloc_I_1d(COM%recv_index)
    call monolis_dealloc_I_1d(COM%recv_item)

    COM%send_n_neib = 0
    call monolis_dealloc_I_1d(COM%send_neib_pe)
    call monolis_dealloc_I_1d(COM%send_index)
    call monolis_dealloc_I_1d(COM%send_item)
  end subroutine monolis_com_initialize

  !> @ingroup com
  !> COM 構造体の初期化関数
  subroutine monolis_com_finalize(COM)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM

    COM%comm = 0
    COM%my_rank = 0
    COM%comm_size = 0
    COM%n_internal_vertex = 0

    COM%recv_n_neib = 0
    call monolis_dealloc_I_1d(COM%recv_neib_pe)
    call monolis_dealloc_I_1d(COM%recv_index)
    call monolis_dealloc_I_1d(COM%recv_item)

    COM%send_n_neib = 0
    call monolis_dealloc_I_1d(COM%send_neib_pe)
    call monolis_dealloc_I_1d(COM%send_index)
    call monolis_dealloc_I_1d(COM%send_item)
  end subroutine monolis_com_finalize

  !> @ingroup com
  !> COM 構造体に MPI コミュニケータを設定
  subroutine monolis_com_set_communicator(COM, comm)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM
    !> MPI コミュニケータ
    integer(kint) :: comm
    COM%comm = comm
  end subroutine monolis_com_set_communicator

  !> @ingroup com
  !> COM 構造体から MPI コミュニケータを取得
  subroutine monolis_com_get_communicator(COM, comm)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM
    !> MPI コミュニケータ
    integer(kint) :: comm
    comm = COM%comm
  end subroutine monolis_com_get_communicator

  !> @ingroup com
  !> COM 構造体に MPI ランク番号を設定
  subroutine monolis_com_set_my_rank(COM, my_rank)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM
    !> MPI ランク番号
    integer(kint) :: my_rank
    COM%my_rank = my_rank
  end subroutine monolis_com_set_my_rank

  !> @ingroup com
  !> COM 構造体から MPI ランク番号を取得
  subroutine monolis_com_get_my_rank(COM, my_rank)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM
    !> MPI ランク番号
    integer(kint) :: my_rank
    my_rank = COM%my_rank
  end subroutine monolis_com_get_my_rank

  !> @ingroup com
  !> COM 構造体に MPI コミュニケータサイズを設定
  subroutine monolis_com_set_comm_size(COM, comm_size)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM
    !> MPI コミュニケータサイズ
    integer(kint) :: comm_size
    COM%comm_size = comm_size
  end subroutine monolis_com_set_comm_size

  !> @ingroup com
  !> COM 構造体から MPI コミュニケータサイズを取得
  subroutine monolis_com_get_comm_size(COM, comm_size)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM
    !> MPI コミュニケータサイズ
    integer(kint) :: comm_size
    comm_size = COM%comm_size
  end subroutine monolis_com_get_comm_size

  !> @ingroup com
  !> COM 構造体に内部領域に属する自由度数を設定
  subroutine monolis_com_set_n_internal_vertex(COM, n_internal_vertex)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM
    !> 内部領域に属する自由度数
    integer(kint) :: n_internal_vertex
    COM%n_internal_vertex = n_internal_vertex
  end subroutine monolis_com_set_n_internal_vertex

  !> @ingroup com
  !> COM 構造体から内部領域に属する自由度数を取得
  subroutine monolis_com_get_n_internal_vertex(COM, n_internal_vertex)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM
    !> 内部領域に属する自由度数
    integer(kint) :: n_internal_vertex
    n_internal_vertex = COM%n_internal_vertex
  end subroutine monolis_com_get_n_internal_vertex

  !> @ingroup dev_com
  !> COM 構造体のデバッグ用データ書き出し
  subroutine monolis_com_debug_write(COM)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM

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