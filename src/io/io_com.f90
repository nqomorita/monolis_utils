!> IO 通信テーブルデータモジュール
!# monolis_output_send_com_table(fname, COM)
!# monolis_output_recv_com_table(fname, COM)
!# monolis_output_com_table_main(fname, n_neib, neib_pe, index, item)
!# monolis_input_send_com_table(fname, COM)
!# monolis_input_recv_com_table(fname, COM)
!# monolis_input_com_table_main(fname, n_neib, neib_pe, index, item)
module mod_monolis_io_com
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_define_com
  use mod_monolis_utils_alloc
  use mod_monolis_utils_palloc
  implicit none

contains

  !> @ingroup io
  !> monolis 通信テーブル send の出力
  subroutine monolis_output_send_com_table(fname, COM)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in] 分割領域に対応する COM 構造体
    type(monolis_COM), intent(in) :: COM

    call monolis_output_com_table_main(fname, &
      & COM%send_n_neib, COM%send_neib_pe, COM%send_index, COM%send_item)
  end subroutine monolis_output_send_com_table

  !> @ingroup io
  !> monolis 通信テーブル recv の出力
  subroutine monolis_output_recv_com_table(fname, COM)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in] 分割領域に対応する COM 構造体
    type(monolis_COM), intent(in) :: COM

    call monolis_output_com_table_main(fname, &
      & COM%recv_n_neib, COM%recv_neib_pe, COM%recv_index, COM%recv_item)
  end subroutine monolis_output_recv_com_table

  !> @ingroup dev_io
  !> monolis 通信テーブルの出力（メイン関数）
  subroutine monolis_output_com_table_main(fname, n_neib, neib_pe, index, item)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in] 隣接領域数
    integer(kint), intent(in) :: n_neib
    !> [in] 隣接領域 id
    integer(kint), intent(in) :: neib_pe(:)
    !> [in] 通信テーブルの index 配列
    integer(kint), intent(in) :: index(:)
    !> [in] 通信テーブルの item 配列
    integer(kint), intent(in) :: item(:)
    integer(kint) :: i

    open(20, file = fname, status = "replace")
      if(n_neib == 0)then
        write(20,"(i0,x,i0)")n_neib, 0
        close(20)
        return
      else
        write(20,"(i0,x,i0)")n_neib, index(n_neib + 1)
      endif

      do i = 1, n_neib
        write(20,"(i0)")neib_pe(i)
      enddo

      do i = 1, n_neib + 1
        write(20,"(i0)")index(i)
      enddo

      do i = 1, index(n_neib + 1)
        write(20,"(i0)")item(i)
      enddo
    close(20)
  end subroutine monolis_output_com_table_main

  !> @ingroup io
  !> monolis 通信テーブル send の入力
  subroutine monolis_input_send_com_table(fname, COM)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in,out] 分割領域に対応する COM 構造体
    type(monolis_COM), intent(inout) :: COM

    call monolis_input_com_table_main(fname, &
      & COM%send_n_neib, COM%send_neib_pe, COM%send_index, COM%send_item)
  end subroutine monolis_input_send_com_table

  !> @ingroup io
  !> monolis 通信テーブル recv の入力
  subroutine monolis_input_recv_com_table(fname, COM)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in,out] 分割領域に対応する COM 構造体
    type(monolis_COM), intent(inout) :: COM

    call monolis_input_com_table_main(fname, &
      & COM%recv_n_neib, COM%recv_neib_pe, COM%recv_index, COM%recv_item)
  end subroutine monolis_input_recv_com_table

  !> @ingroup dev_io
  !> monolis 通信テーブルの入力（汎用関数）
  subroutine monolis_input_com_table_main(fname, n_neib, neib_pe, index, item)
    implicit none
    !> [in] 入力ファイル名
    character(*), intent(in) :: fname
    !> [out] 隣接領域数
    integer(kint), intent(out) :: n_neib
    !> [out] 隣接領域 id
    integer(kint), pointer, intent(out) :: neib_pe(:)
    !> [out] 通信テーブルの index 配列
    integer(kint), pointer, intent(out) :: index(:)
    !> [out] 通信テーブルの item 配列
    integer(kint), pointer, intent(out) :: item(:)
    integer(kint) :: i, nz

    open(20, file = fname, status = "old")
      read(20,*)n_neib, nz

      if(n_neib == 0)then
        nz = 0
        call monolis_palloc_I_1d(neib_pe, 1)
        call monolis_palloc_I_1d(index, 1)
        call monolis_palloc_I_1d(item, 1)
        close(20)
        return
      else
        call monolis_palloc_I_1d(neib_pe, n_neib)
        call monolis_palloc_I_1d(index, n_neib + 1)
        call monolis_palloc_I_1d(item, nz)
      endif

      do i = 1, n_neib
        read(20,*)neib_pe(i)
      enddo

      do i = 1, n_neib + 1
        read(20,*)index(i)
      enddo

      do i = 1, nz
        read(20,*)item(i)
      enddo
    close(20)
  end subroutine monolis_input_com_table_main
end module mod_monolis_io_com
