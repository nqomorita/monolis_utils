!> 通信データモジュール
module mod_monolis_utils_define_com_init
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  use mod_monolis_utils_palloc
  use mod_monolis_mpi_util
  use mod_monolis_utils_define_com
  use mod_monolis_comm_table
  use mod_monolis_io_file_name
  use mod_monolis_io_com
  use mod_monolis_io
  implicit none

contains

  !> @ingroup com
  !> @brief COM 構造体の初期化関数
  subroutine monolis_com_input_comm_table(COM, top_dir_name, part_dir_name, file_name)
    implicit none
    !> [in,out] COM 構造体
    type(monolis_com), intent(inout) :: COM
    !> [in] 通信テーブルデータ読込のトップディレクトリ名
    character(*), intent(in) :: top_dir_name
    !> [in] 通信テーブルデータ読込の分割ファイルが格納されるディレクトリ名
    character(*), intent(in) :: part_dir_name
    !> [in] 通信テーブルデータが記載されたファイル名
    character(*), intent(in) :: file_name
    integer(kint) :: i, j, nitem
    character(monolis_charlen) :: fname

    if(COM%comm_size <= 1)then
      COM%comm_size = 1
      return
    endif

    fname = monolis_get_output_file_name_by_domain_id(trim(top_dir_name), trim(part_dir_name), &
      & trim(file_name)//".recv", COM%my_rank)
    call monolis_input_recv_com_table(fname, COM)

    fname = monolis_get_output_file_name_by_domain_id(trim(top_dir_name), trim(part_dir_name), &
      & trim(file_name)//".send", COM%my_rank)
    call monolis_input_send_com_table(fname, COM)

    fname = monolis_get_output_file_name_by_domain_id(trim(top_dir_name), trim(part_dir_name), &
      & trim(file_name)//".n_internal", COM%my_rank)

    call monolis_input_internal_vertex_number(fname, COM%n_internal_vertex)
  end subroutine monolis_com_input_comm_table

  !> @ingroup com
  !> @brief COM 構造体の初期化関数
  subroutine monolis_com_initialize_by_parted_files(COM, comm, top_dir_name, part_dir_name, file_name)
    implicit none
    !> [in,out] COM 構造体
    type(monolis_COM), intent(inout) :: COM
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] 通信テーブルデータ読込のトップディレクトリ名
    character(*), intent(in) :: top_dir_name
    !> [in] 通信テーブルデータ読込の分割ファイルが格納されるディレクトリ名
    character(*), intent(in) :: part_dir_name
    !> [in] 通信テーブルデータが記載されたファイル名
    character(*), intent(in) :: file_name

    if(monolis_mpi_get_local_comm_size(comm) == 1)then
      call monolis_com_initialize_by_self(COM)
      return
    endif

    COM%comm = comm
    COM%my_rank = monolis_mpi_get_local_my_rank(comm)
    COM%comm_size = monolis_mpi_get_local_comm_size(comm)
    COM%n_internal_vertex = 0

    COM%recv_n_neib = 0
    call monolis_pdealloc_I_1d(COM%recv_neib_pe)
    call monolis_pdealloc_I_1d(COM%recv_index)
    call monolis_pdealloc_I_1d(COM%recv_item)

    COM%send_n_neib = 0
    call monolis_pdealloc_I_1d(COM%send_neib_pe)
    call monolis_pdealloc_I_1d(COM%send_index)
    call monolis_pdealloc_I_1d(COM%send_item)

    call monolis_com_input_comm_table(COM, top_dir_name, part_dir_name, file_name)
  end subroutine monolis_com_initialize_by_parted_files

  !> @ingroup com
  !> @brief COM 構造体の初期化関数
  subroutine monolis_com_initialize_by_global_id(COM, comm, n_internal_vertex, n_vertex, global_id)
    implicit none
    !> [in,out] COM 構造体
    type(monolis_COM), intent(inout) :: COM
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] 分割領域における内部計算点数
    integer(kint), intent(in) :: n_internal_vertex
    !> [in] 全計算点数
    integer(kint), intent(in) :: n_vertex
    !> [in] グローバル計算点番号
    integer(kint), intent(in) :: global_id(:)

    if(monolis_mpi_get_local_comm_size(comm) == 1)then
      call monolis_com_initialize_by_self(COM)
      return
    endif

    COM%comm = comm
    COM%my_rank = monolis_mpi_get_local_my_rank(comm)
    COM%comm_size = monolis_mpi_get_local_comm_size(comm)
    COM%n_internal_vertex = n_internal_vertex

    COM%recv_n_neib = 0
    call monolis_pdealloc_I_1d(COM%recv_neib_pe)
    call monolis_pdealloc_I_1d(COM%recv_index)
    call monolis_pdealloc_I_1d(COM%recv_item)

    COM%send_n_neib = 0
    call monolis_pdealloc_I_1d(COM%send_neib_pe)
    call monolis_pdealloc_I_1d(COM%send_index)
    call monolis_pdealloc_I_1d(COM%send_item)

    call monolis_com_get_comm_table_parallel(n_internal_vertex, n_vertex, global_id, COM)
  end subroutine monolis_com_initialize_by_global_id

  !> @ingroup com
  !> @brief COM 構造体の初期化関数
  subroutine monolis_com_initialize_by_self(COM)
    implicit none
    !> [in,out] COM 構造体
    type(monolis_COM), intent(inout) :: COM

    COM%comm = MPI_COMM_SELF
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
  end subroutine monolis_com_initialize_by_self

end module mod_monolis_utils_define_com_init