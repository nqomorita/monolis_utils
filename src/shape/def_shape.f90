module mod_monolis_def_shape
  use mod_monolis_utils_define_prm
  implicit none

  !> @defgroup shape 有限要素形状関数モジュール
  !> @brief 形状関数に関する定義と設計
  !> @details
  !> ## 設計概要
  !> - 形状関数は各要素タイプごとに独立したモジュールとして実装
  !> - 共通インターフェース（monolis_shape_func_if）を通して統一的にアクセス
  !> - 多次元要素（1次元線、2次元三角形/四角形、3次元四面体/六面体）をサポート
  !>
  !> ## 呼び出し方法
  !> 1. 直接呼び出し: `call monolis_shape_2d_tri_1st_shapefunc(local_coord, shape_values)`
  !> 2. インターフェース経由: `call monolis_shape_get_shape_func(elem_type, shape_func)`
  !>
  !> ## 要素ID定義

  ! 次元、形状タイプ、次数の定数定義
  integer(kint), parameter :: DIM_0D = 0
  integer(kint), parameter :: DIM_1D = 1
  integer(kint), parameter :: DIM_2D = 2
  integer(kint), parameter :: DIM_3D = 3

  integer(kint), parameter :: SHAPE_POINT = 0  ! 点
  integer(kint), parameter :: SHAPE_LINE  = 1  ! 線
  integer(kint), parameter :: SHAPE_TRI   = 3  ! 三角形
  integer(kint), parameter :: SHAPE_QUAD  = 4  ! 四角形
  integer(kint), parameter :: SHAPE_TET   = 4  ! 四面体
  integer(kint), parameter :: SHAPE_HEX   = 6  ! 六面体

  integer(kint), parameter :: ORDER_1ST = 1 ! 1次
  integer(kint), parameter :: ORDER_2ND = 2 ! 2次

  ! 点要素
  integer(kint), parameter :: monolis_shape_point = 1  ! 点要素 (0D)

  ! 1次元要素
  integer(kint), parameter :: monolis_shape_1d_line_1st = 111  ! 線要素1次 (1D)
  integer(kint), parameter :: monolis_shape_1d_line_2nd = 112  ! 線要素2次 (1D)

  ! 2次元要素
  integer(kint), parameter :: monolis_shape_2d_tri_1st  = 231  ! 三角形1次要素 (2D)
  integer(kint), parameter :: monolis_shape_2d_tri_2nd  = 232  ! 三角形2次要素 (2D)
  integer(kint), parameter :: monolis_shape_2d_quad_1st = 241  ! 四角形1次要素 (2D)
  integer(kint), parameter :: monolis_shape_2d_quad_2nd = 242  ! 四角形2次要素 (2D)

  ! 3次元要素
  integer(kint), parameter :: monolis_shape_3d_tet_1st  = 341  ! 四面体1次要素 (3D)
  integer(kint), parameter :: monolis_shape_3d_tet_2nd  = 342  ! 四面体2次要素 (3D)
  integer(kint), parameter :: monolis_shape_3d_hex_1st  = 361  ! 六面体1次要素 (3D)
  integer(kint), parameter :: monolis_shape_3d_hex_2nd  = 362  ! 六面体2次要素 (3D)

  !> 形状関数のインターフェース定義
  interface
    subroutine monolis_shape_func(local_coord, N)
      use mod_monolis_utils_define_prm
      implicit none
      real(kdouble), intent(in) :: local_coord(:)
      real(kdouble), intent(out) :: N(:)
    end subroutine monolis_shape_func
  end interface

  !> 定義域関数のインターフェース定義
  interface
    subroutine monolis_domain_func(local_coord, is_inside)
      use mod_monolis_utils_define_prm
      implicit none
      real(kdouble), intent(in) :: local_coord(:)
      logical, intent(out) :: is_inside
    end subroutine monolis_domain_func
  end interface

  !> 局所座標系における節点位置定義関数のインターフェース定義
  interface
    subroutine monolis_local_node_point_func(i, local_coord)
      use mod_monolis_utils_define_prm
      implicit none
      integer(kint), intent(in) :: i
      real(kdouble), intent(out) :: local_coord
    end subroutine monolis_local_node_point_func
  end interface

  !> エッジ・面上の局所座標系から、３次元局所座標系へのマップ関数のインターフェース定義
  interface
    subroutine monolis_shape_map_func(i_sub, local_coord, local_coord_3d)
      use mod_monolis_utils_define_prm
      implicit none
      integer(kint), intent(in) :: i_sub
      real(kdouble), intent(in) :: local_coord(:)
      real(kdouble), intent(out) :: local_coord_3d(:)
    end subroutine monolis_shape_map_func
  end interface

  !> エッジ情報定義関数のインターフェース定義
  interface
    subroutine monolis_edge_data_func(i_edge, n_edge_node, edge_node_ids, &
      edge_shape_func, edge_domain_func, edge_local_np_fucn, edge_shape_map_func)
      use mod_monolis_utils_define_prm
      implicit none
      integer(kint), intent(in) :: i_edge
      integer(kint), intent(out) :: n_edge_node
      integer(kint), intent(out), allocatable :: edge_node_ids(:)
      procedure(monolis_shape_func) :: edge_shape_func
      procedure(monolis_domain_func) :: edge_domain_func
      procedure(monolis_local_node_point_func) :: edge_local_np_fucn
      procedure(monolis_shape_map_func) :: edge_shape_map_func
    end subroutine monolis_edge_data_func
  end interface

  !> 面情報定義関数のインターフェース定義
  interface
    subroutine monolis_surf_data_func(i_face, n_face_node, face_node_ids, &
      face_shape_func, face_domain_func, n_face_edge, edge_data_func, face_shape_map_func)
      use mod_monolis_utils_define_prm
      implicit none
      integer(kint), intent(in) :: i_face
      integer(kint), intent(out) :: n_face_node
      integer(kint), intent(out) :: n_face_edge
      integer(kint), intent(out), allocatable :: face_node_ids(:)
      procedure(monolis_shape_func) :: face_shape_func
      procedure(monolis_domain_func) :: face_domain_func
      procedure(monolis_local_node_point_func) :: edge_local_np_fucn
      procedure(monolis_edge_data_func) :: edge_data_func
      procedure(monolis_shape_map_func) :: face_shape_map_func
    end subroutine monolis_surf_data_func
  end interface
end module mod_monolis_def_shape
