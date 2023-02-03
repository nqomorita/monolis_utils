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
  !> MPI のグローバルコミュニケータを取得する関数
  function monolis_mpi_global_comm()
    implicit none
    integer(kint) :: monolis_mpi_global_comm

#ifndef NO_MPI
    monolis_mpi_global_comm = MPI_COMM_WORLD
#else
    monolis_mpi_global_comm = 0
#endif
  end function monolis_mpi_global_comm

  !> @ingroup mpi
  !> MPI のグローバルランクサイズを取得する関数
  function monolis_mpi_global_comm_size()
    implicit none
    integer(kint) :: monolis_mpi_global_comm_size, ierr

#ifndef NO_MPI
    call MPI_comm_size(MPI_COMM_WORLD, monolis_mpi_global_comm_size, ierr)
#else
    monolis_mpi_global_comm_size = 1
#endif
  end function monolis_mpi_global_comm_size

  !> @ingroup mpi
  !> MPI のグローバルランクを取得する関数
  function monolis_mpi_global_my_rank()
    implicit none
    integer(kint) :: monolis_mpi_global_my_rank, ierr

#ifndef NO_MPI
    call MPI_comm_rank(MPI_COMM_WORLD, monolis_mpi_global_my_rank, ierr)
#else
    monolis_mpi_global_my_rank = 0
#endif
  end function monolis_mpi_global_my_rank

  !> @ingroup mpi
  !> MPI のローカルコミュニケータのランクサイズを取得する関数
  function monolis_mpi_local_comm_size(comm)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint) :: comm
    !> [out] コミュニケータサイズ
    integer(kint) :: monolis_mpi_local_comm_size
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_comm_size(comm, monolis_mpi_local_comm_size, ierr)
#else
    monolis_mpi_local_comm_size = 1
#endif
  end function monolis_mpi_local_comm_size

  !> @ingroup mpi
  !> MPI のローカルコミュニケータのランクサイズを取得する関数
  function monolis_mpi_local_my_rank(comm)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint) :: comm
    !> [out] MPI ランク番号
    integer(kint) :: monolis_mpi_local_my_rank
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_comm_rank(comm, monolis_mpi_local_my_rank, ierr)
#else
    monolis_mpi_local_my_rank = 0
#endif
  end function monolis_mpi_local_my_rank

  !> @ingroup mpi
  !> MPI バリア関数（グローバルコミュニケータ）
  subroutine monolis_mpi_global_barrier()
    implicit none
    integer(kint) :: comm
    integer(kint) :: ierr
#ifndef NO_MPI
    comm = monolis_mpi_global_comm()
    call MPI_barrier(comm, ierr)
#endif
  end subroutine monolis_mpi_global_barrier

  !> @ingroup mpi
  !> MPI バリア関数（ローカルコミュニケータ）
  subroutine monolis_mpi_local_barrier(comm)
    implicit none
    !> [in] MPI コミュニケータ
    integer(kint) :: comm
    integer(kint) :: ierr
#ifndef NO_MPI
    call MPI_barrier(comm, ierr)
#endif
  end subroutine monolis_mpi_local_barrier
end module mod_monolis_mpi_util
