!> 通信データモジュール
module mod_monolis_utils_define_com
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  implicit none

  !> @ingroup dev_com
  !> COM 構造体
  type monolis_COM
    !> MPI コミュニケータ
    integer(kint) :: comm
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

contains

  !> @ingroup com
  !> COM 構造体の初期化関数
  subroutine monolis_com_initialize(COM)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM

    COM%comm = 0

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

    COM%recv_n_neib = 0
    call monolis_dealloc_I_1d(COM%recv_neib_pe)
    call monolis_dealloc_I_1d(COM%recv_index)
    call monolis_dealloc_I_1d(COM%recv_item)

    COM%send_n_neib = 0
    call monolis_dealloc_I_1d(COM%send_neib_pe)
    call monolis_dealloc_I_1d(COM%send_index)
    call monolis_dealloc_I_1d(COM%send_item)
  end subroutine monolis_com_finalize

  !> @ingroup dev_com
  !> COM 構造体のデバッグ用データ書き出し
  subroutine monolis_com_debug_write(COM)
    implicit none
    !> [in] COM 構造体
    type(monolis_COM) :: COM

    write(*,*)"--- monolis_com_debug_write"
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