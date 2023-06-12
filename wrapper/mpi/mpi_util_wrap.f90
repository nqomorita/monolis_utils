!> MPI util wrap モジュール
module mod_monolis_mpi_util_wrap
  use mod_monolis_mpi_util
  use iso_c_binding
  implicit none

contains

  !> @ingroup wrap_mpi
  !> monolis ライブラリで利用する MPI の初期化関数
  subroutine monolis_mpi_initialize_c()&
    & bind(c, name = "monolis_mpi_initialize")
    implicit none
    call monolis_mpi_initialize()
  end subroutine monolis_mpi_initialize_c

  !> @ingroup wrap_mpi
  !> monolis ライブラリを利用する MPI の終了処理関数
  subroutine monolis_mpi_finalize_c()&
    & bind(c, name = "monolis_mpi_finalize")
    implicit none
    call monolis_mpi_finalize()
  end subroutine monolis_mpi_finalize_c

  !> @ingroup wrap_mpi
  !> MPI のグローバルコミュニケータを取得する関数
  function monolis_mpi_get_global_comm_c()&
    & bind(c, name = "monolis_mpi_get_global_comm")
    implicit none
    integer(c_int) :: monolis_mpi_get_global_comm_c
    monolis_mpi_get_global_comm_c = monolis_mpi_get_global_comm()
  end function monolis_mpi_get_global_comm_c

  !> @ingroup wrap_mpi
  !> MPI のセルフコミュニケータを取得する関数
  function monolis_mpi_get_self_comm_c()&
    & bind(c, name = "monolis_mpi_get_self_comm")
    implicit none
    integer(c_int) :: monolis_mpi_get_self_comm_c
    monolis_mpi_get_self_comm_c = monolis_mpi_get_self_comm()
  end function monolis_mpi_get_self_comm_c

  !> @ingroup wrap_mpi
  !> MPI のグローバルランクサイズを取得する関数
  function monolis_mpi_get_global_comm_size_c()&
    & bind(c, name = "monolis_mpi_get_global_comm_size")
    implicit none
    integer(c_int) :: monolis_mpi_get_global_comm_size_c
    monolis_mpi_get_global_comm_size_c = monolis_mpi_get_global_comm_size()
  end function monolis_mpi_get_global_comm_size_c

  !> @ingroup wrap_mpi
  !> MPI のグローバルランクを取得する関数
  function monolis_mpi_get_global_my_rank_c()&
    & bind(c, name = "monolis_mpi_get_global_my_rank")
    implicit none
    integer(c_int) :: monolis_mpi_get_global_my_rank_c
    monolis_mpi_get_global_my_rank_c = monolis_mpi_get_global_my_rank()
  end function monolis_mpi_get_global_my_rank_c

  !> @ingroup wrap_mpi
  !> MPI のローカルコミュニケータのランクサイズを取得する関数
  function monolis_mpi_get_local_comm_size_c(comm)&
    & bind(c, name = "monolis_mpi_get_local_comm_size")
    implicit none
    integer(c_int), intent(in), value :: comm
    integer(c_int) :: monolis_mpi_get_local_comm_size_c
    monolis_mpi_get_local_comm_size_c = monolis_mpi_get_local_comm_size(comm)
  end function monolis_mpi_get_local_comm_size_c

  !> @ingroup wrap_mpi
  !> MPI のローカルコミュニケータのランクサイズを取得する関数
  function monolis_mpi_get_local_my_rank_c(comm)&
    & bind(c, name = "monolis_mpi_get_local_my_rank")
    implicit none
    integer(c_int), intent(in), value :: comm
    integer(c_int) :: monolis_mpi_get_local_my_rank_c
    monolis_mpi_get_local_my_rank_c = monolis_mpi_get_local_my_rank(comm)
  end function monolis_mpi_get_local_my_rank_c

  !> @ingroup mpiwrap_mpi
  !> MPI バリア関数（グローバルコミュニケータ）
  subroutine monolis_mpi_global_barrier_c()&
    & bind(c, name = "monolis_mpi_global_barrier")
    implicit none
    call monolis_mpi_global_barrier()
  end subroutine monolis_mpi_global_barrier_c

  !> @ingroup wrap_mpi
  !> MPI バリア関数（ローカルコミュニケータ）
  subroutine monolis_mpi_local_barrier_c(comm)&
    & bind(c, name = "monolis_mpi_local_barrier")
    implicit none
    integer(c_int), intent(in), value :: comm
    call monolis_mpi_local_barrier(comm)
  end subroutine monolis_mpi_local_barrier_c

  !> @ingroup wrap_mpi
  !> MPI コミュニケータの分割
  subroutine monolis_mpi_split_comm_c(comm, group_id, comm_split)&
    & bind(c, name = "monolis_mpi_split_comm")
    implicit none
    integer(c_int), intent(in), value :: comm
    integer(c_int), intent(in), value :: group_id
    integer(c_int), intent(inout) :: comm_split
    call monolis_mpi_split_comm(comm, group_id, comm_split)
  end subroutine monolis_mpi_split_comm_c

  !> @ingroup wrap_mpi
  !> MPI 時間計測関数
  function monolis_get_time_c()&
    & bind(c, name = "monolis_get_time")
    implicit none
    real(c_double) :: monolis_get_time_c
    monolis_get_time_c = monolis_get_time()
  end function monolis_get_time_c

  !> @ingroup wrap_mpi
  !> MPI 時間計測関数（グローバルコミュニケータでの動機）
  function monolis_get_time_global_sync_c()&
    & bind(c, name = "monolis_get_time_global_sync")
    implicit none
    real(c_double) :: monolis_get_time_global_sync_c
    monolis_get_time_global_sync_c = monolis_get_time_global_sync()
  end function monolis_get_time_global_sync_c

  !> @ingroup wrap_mpi
  !> MPI 時間計測関数（ローカルコミュニケータ）
  function monolis_get_time_local_sync_c(comm)&
    & bind(c, name = "monolis_get_time_local_sync")
    implicit none
    integer(c_int), intent(in), value :: comm
    real(c_double) :: monolis_get_time_local_sync_c
    monolis_get_time_local_sync_c = monolis_get_time_local_sync(comm)
  end function monolis_get_time_local_sync_c
end module mod_monolis_mpi_util_wrap
