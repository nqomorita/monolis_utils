!> MPI util モジュール
module mod_monolis_mpi_util
  use mod_monolis_utils_define_prm
  implicit none
#ifndef NO_MPI
  include 'mpif.h'
#endif

#ifndef NO_MPI
  integer(kint), parameter :: monolis_mpi_status_size = MPI_STATUS_SIZE
#else
  integer(kint), parameter :: monolis_mpi_status_size = 1
#endif

contains

  !> @ingroup mpi
  !> monolis ライブラリで利用する MPI の初期化関数
  subroutine monolis_mpi_initialize()
    implicit none
    integer(kint) :: ierr
#ifndef NO_MPI
    call MPI_init(ierr)
#endif
  end subroutine monolis_mpi_initialize

  !> @ingroup mpi
  !> monolis ライブラリを利用する MPI の終了処理関数
  subroutine monolis_mpi_finalize()
    implicit none
    integer(kint) :: ierr
#ifndef NO_MPI
    call MPI_finalize(ierr)
#endif
  end subroutine monolis_mpi_finalize

  !> @ingroup mpi
  !> MPI のセルフコミュニケータを取得する関数
  function monolis_mpi_get_self_comm()
    implicit none
    integer(kint) :: monolis_mpi_get_self_comm

#ifndef NO_MPI
    monolis_mpi_get_self_comm = MPI_COMM_SELF
#else
    monolis_mpi_get_self_comm = 0
#endif
  end function monolis_mpi_get_self_comm

  !> @ingroup mpi
  !> MPI のグローバルコミュニケータを取得する関数
  function monolis_mpi_get_global_comm()
    implicit none
    integer(kint) :: monolis_mpi_get_global_comm

#ifndef NO_MPI
    monolis_mpi_get_global_comm = MPI_COMM_WORLD
#else
    monolis_mpi_get_global_comm = 0
#endif
  end function monolis_mpi_get_global_comm

  !> @ingroup mpi
  !> MPI のグローバルランクサイズを取得する関数
  function monolis_mpi_get_global_comm_size()
    implicit none
    integer(kint) :: monolis_mpi_get_global_comm_size, ierr

#ifndef NO_MPI
    call MPI_comm_size(MPI_COMM_WORLD, monolis_mpi_get_global_comm_size, ierr)
#else
    monolis_mpi_get_global_comm_size = 1
#endif
  end function monolis_mpi_get_global_comm_size

  !> @ingroup mpi
  !> MPI のグローバルランクを取得する関数
  function monolis_mpi_get_global_my_rank()
    implicit none
    integer(kint) :: monolis_mpi_get_global_my_rank, ierr

#ifndef NO_MPI
    call MPI_comm_rank(MPI_COMM_WORLD, monolis_mpi_get_global_my_rank, ierr)
#else
    monolis_mpi_get_global_my_rank = 0
#endif
  end function monolis_mpi_get_global_my_rank

  !> @ingroup mpi
  !> MPI のローカルコミュニケータのランクサイズを取得する関数
  function monolis_mpi_get_local_comm_size(comm)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [out] コミュニケータサイズ
    integer(kint) :: monolis_mpi_get_local_comm_size
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_comm_size(comm, monolis_mpi_get_local_comm_size, ierr)
#else
    monolis_mpi_get_local_comm_size = 1
#endif
  end function monolis_mpi_get_local_comm_size

  !> @ingroup mpi
  !> MPI のローカルコミュニケータのランクを取得する関数
  function monolis_mpi_get_local_my_rank(comm)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [out] MPI ランク番号
    integer(kint) :: monolis_mpi_get_local_my_rank
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_comm_rank(comm, monolis_mpi_get_local_my_rank, ierr)
#else
    monolis_mpi_get_local_my_rank = 0
#endif
  end function monolis_mpi_get_local_my_rank

  !> @ingroup mpi
  !> MPI バリア関数（グローバルコミュニケータ）
  subroutine monolis_mpi_global_barrier()
    implicit none
    integer(kint) :: comm
    integer(kint) :: ierr
#ifndef NO_MPI
    comm = monolis_mpi_get_global_comm()
    call MPI_barrier(comm, ierr)
#endif
  end subroutine monolis_mpi_global_barrier

  !> @ingroup mpi
  !> MPI バリア関数（ローカルコミュニケータ）
  subroutine monolis_mpi_local_barrier(comm)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr
#ifndef NO_MPI
    call MPI_barrier(comm, ierr)
#endif
  end subroutine monolis_mpi_local_barrier

  !> @ingroup mpi
  !> MPI コミュニケータの分割
  subroutine monolis_mpi_split_comm(comm, group_id, comm_split)
    implicit none
    !> [in] 分割前の MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] コミュニケータのグループ id
    integer(kint), intent(in) :: group_id
    !> [out] 分割後の MPI コミュニケータ
    integer(kint), intent(out) :: comm_split
    integer(kint) :: key
    integer(kint) :: ierr
#ifndef NO_MPI
    key = 0
    call MPI_COMM_SPLIT(comm, group_id, key, comm_split, ierr)
#endif
  end subroutine monolis_mpi_split_comm

  !> @ingroup mpi
  !> MPI 時間計測関数
  function monolis_get_time()
    implicit none
    !> [out] 時刻
    real(kdouble) :: monolis_get_time

#ifndef NO_MPI
    monolis_get_time = MPI_Wtime()
#else
    monolis_get_time = 0.0d0
#endif
  end function monolis_get_time

  !> @ingroup mpi
  !> MPI 時間計測関数（グローバルコミュニケータでの動機）
  function monolis_get_time_global_sync()
    implicit none
    !> [out] 時刻
    real(kdouble) :: monolis_get_time_global_sync

#ifndef NO_MPI
    call monolis_mpi_global_barrier()
    monolis_get_time_global_sync = MPI_Wtime()
#else
    monolis_get_time_global_sync = 0.0d0
#endif
  end function monolis_get_time_global_sync

  !> @ingroup mpi
  !> MPI 時間計測関数（ローカルコミュニケータ）
  function monolis_get_time_local_sync(comm)
    implicit none
    !> [out] 時刻
    real(kdouble) :: monolis_get_time_local_sync
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm

#ifndef NO_MPI
    call monolis_mpi_local_barrier(comm)
    monolis_get_time_local_sync = MPI_Wtime()
#else
    monolis_get_time_local_sync = 0.0d0
#endif
  end function monolis_get_time_local_sync
end module mod_monolis_mpi_util
