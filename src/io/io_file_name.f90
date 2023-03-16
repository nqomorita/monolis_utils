!> IO ファイル名モジュール
module mod_monolis_io_file_name
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  use mod_monolis_mpi_util
  implicit none

contains

  !> @ingroup io
  !> 並列計算用読み込みファイル名の取得（グローバルコミュニケータから指定）
  function monolis_get_global_input_file_name(dirname, fname)
    implicit none
    !> [out] 戻り値
    character(monolis_charlen) :: monolis_get_global_input_file_name
    !> [in] 入力ディレクトリ名
    character(*) :: dirname
    !> [in] 入力ファイル名
    character(*) :: fname
    integer(kint) :: domain_id
    integer(kint) :: comm_size
    character(monolis_charlen) :: cid

    comm_size = monolis_mpi_global_comm_size()
    domain_id = monolis_mpi_global_my_rank()

    if(comm_size > 1)then
      write(cid,"(i0)") domain_id
      monolis_get_global_input_file_name = trim(dirname)//"/"//trim(fname)//"."//trim(cid)
    else
      monolis_get_global_input_file_name = trim(fname)
    endif
  end function monolis_get_global_input_file_name

  !> @ingroup io
  !> 並列計算用読み込みファイル名の取得（ローカルコミュニケータから指定）
  function monolis_get_local_input_file_name(dirname, fname, comm)
    implicit none
    !> [out] 戻り値
    character(monolis_charlen) :: monolis_get_local_input_file_name
    !> [in] 入力ディレクトリ名
    character(*) :: dirname
    !> [in] 入力ファイル名
    character(*) :: fname
    !> [in] MPI コミュニケータ
    integer(kint) :: comm
    integer(kint) :: domain_id
    integer(kint) :: comm_size
    character(monolis_charlen) :: cid

    comm_size = monolis_mpi_local_comm_size(comm)
    domain_id = monolis_mpi_local_my_rank(comm)

    if(comm_size > 1)then
      write(cid,"(i0)") domain_id
      monolis_get_local_input_file_name = trim(dirname)//"/"//trim(fname)//"."//trim(cid)
    else
      monolis_get_local_input_file_name = trim(fname)
    endif
  end function monolis_get_local_input_file_name

  !> @ingroup io
  !> 並列計算用書き出しファイル名の取得（グローバルコミュニケータから指定）
  function monolis_get_global_output_file_name(dirname, fname)
    implicit none
    !> [out] 戻り値
    character(monolis_charlen) :: monolis_get_global_output_file_name
    !> [in] 出力ディレクトリ名
    character(*) :: dirname
    !> [in] 出力ファイル名
    character(*) :: fname
    integer(kint) :: domain_id
    integer(kint) :: comm_size
    character(monolis_charlen) :: cid

    comm_size = monolis_mpi_global_comm_size()
    domain_id = monolis_mpi_global_my_rank()

    if(comm_size > 1)then
      write(cid,"(i0)") domain_id
      monolis_get_global_output_file_name = trim(dirname)//"/"//trim(fname)//"."//trim(cid)
    else
      monolis_get_global_output_file_name = trim(fname)
    endif
  end function monolis_get_global_output_file_name

  !> @ingroup io
  !> 並列計算用書き出しファイル名の取得（ローカルコミュニケータから指定）
  function monolis_get_local_output_file_name(dirname, fname, comm)
    implicit none
    !> [out] 戻り値
    character(monolis_charlen) :: monolis_get_local_output_file_name
    !> [in] 出力ディレクトリ名
    character(*) :: dirname
    !> [in] 出力ファイル名
    character(*) :: fname
    !> [in] MPI コミュニケータ
    integer(kint) :: comm
    integer(kint) :: domain_id
    integer(kint) :: comm_size
    character(monolis_charlen) :: cid

    comm_size = monolis_mpi_local_comm_size(comm)
    domain_id = monolis_mpi_local_my_rank(comm)

    if(comm_size > 1)then
      write(cid,"(i0)") domain_id
      monolis_get_local_output_file_name = trim(dirname)//"/"//trim(fname)//"."//trim(cid)
    else
      monolis_get_local_output_file_name = trim(fname)
    endif
  end function monolis_get_local_output_file_name

  !> @ingroup io
  !> 並列計算用書き出しファイル名の取得（領域番号から指定）
  function monolis_get_output_file_name_by_domain_id(dirname, fname, domain_id)
    implicit none
    !> [out] 戻り値
    character(monolis_charlen) :: monolis_get_output_file_name_by_domain_id
    !> [in] 出力ディレクトリ名
    character(*) :: dirname
    !> [in] 出力ファイル名
    character(*) :: fname
    !> [in] 領域番号
    integer(kint) :: domain_id
    character(monolis_charlen) :: cid

    write(cid,"(i0)") domain_id
    monolis_get_output_file_name_by_domain_id = trim(dirname)//"/"//trim(fname)//"."//trim(cid)
  end function monolis_get_output_file_name_by_domain_id
end module mod_monolis_io_file_name
