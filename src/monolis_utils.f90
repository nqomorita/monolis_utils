!> monolis utils モジュール
module mod_monolis_utils
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_define_com
  use mod_monolis_utils_define_com_init
  use mod_monolis_utils_sys
  use mod_monolis_utils_alloc
  use mod_monolis_utils_palloc
  use mod_monolis_utils_error
  use mod_monolis_utils_stdlib
  use mod_monolis_utils_hash
  use mod_monolis_mpi
  use mod_monolis_mpi_util
  use mod_monolis_utils_aabb
  use mod_monolis_utils_kdtree
  use mod_monolis_io
  use mod_monolis_io_arg
  use mod_monolis_io_mtx
  use mod_monolis_io_com
  use mod_monolis_io_util
  use mod_monolis_io_file_name
  use mod_monolis_shape_util
  use mod_monolis_shape_c3d8
  use mod_monolis_shape_c3d4
  use mod_monolis_driver_util
  use mod_monolis_extract_util
  use mod_monolis_refiner_util
  use mod_monolis_comm_par_util
  use mod_monolis_comm_ser_util
  use mod_monolis_comm_table
end module mod_monolis_utils

!> @defgroup io ファイル入出力関数群
!> ファイル入出力に関連する関数グループ

!> @defgroup std 標準関数群
!> 標準的なデータ処理に関連する関数グループ

!> @defgroup std_algebra 標準代数計算関数群
!> 標準的な代数計算に関連する関数グループ

!> @defgroup alloc メモリ確保関数群
!> メモリ確保・メモリ開放に関連する関数グループ

!> @defgroup mpi MPI 関数群
!> MPI に関連する関数グループ

!> @defgroup com 通信テーブルデータ関数群
!> 通信テーブルデータに関連する関数グループ

!> @defgroup aabb バウンディングボックス関数群
!> バウンディングボックスに関連する関数グループ

!> @defgroup kdtree k-d ツリー関数群
!> k-d ツリーに関連する関数グループ

!> @defgroup hash ハッシュ関数群
!> ハッシュに関連する関数グループ

!> @defgroup dev_io 開発者用：ファイル入出力関数群
!> ファイル入出力に関連する関数グループ（開発者用）

!> @defgroup dev_error 開発者用：エラー出力関数群
!> エラー出力に関連する関数グループ（開発者用）

!> @defgroup dev_com 開発者用：通信テーブルデータ関数群
!> 通信テーブルデータに関連する関数グループ（開発者用）

!> @defgroup dev_test 開発者用：テスト関数群
!> テストに関連する関数グループ（開発者用）

!> @defgroup dev_kdtree 開発者用：k-d ツリー関数群
!> k-d ツリーに関連する関数グループ（開発者用）

!> @defgroup dev_hash 開発者用：ハッシュ関数群
!> ハッシュに関連する関数グループ（開発者用）
