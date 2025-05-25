!> 要素境界に関する情報提供モジュール
module mod_monolis_shape_boundary
  use mod_monolis_utils_define_prm
  use mod_monolis_def_shape
  use mod_monolis_shape_1d_line_1st
  use mod_monolis_shape_2d_tri_1st
  use mod_monolis_shape_2d_quad_1st
  use mod_monolis_shape_3d_tet_1st
  use mod_monolis_shape_3d_hex_1st
  implicit none

  private

  public :: monolis_shape_get_element_info
  public :: monolis_shape_get_dimension
  public :: monolis_shape_get_face_data
  public :: monolis_shape_get_edge_data
  public :: monolis_shape_map_local_coord
  public :: monolis_shape_is_on_boundary
  public :: monolis_shape_get_vertex_local_coord
  public :: monolis_shape_get_num_faces
  public :: monolis_shape_get_num_edges
  public :: monolis_shape_get_num_vertex
  public :: monolis_shape_get_shape_func
  public :: monolis_shape_get_domain_func
  public :: monolis_shape_get_boundary_shape_func
  public :: monolis_shape_get_boundary_domain_func
  public :: monolis_shape_get_boundary_info
  
  ! 形状関数のインターフェース定義
  interface
    subroutine monolis_shape_func(local_coord, N)
      use mod_monolis_utils_define_prm
      implicit none
      real(kdouble), intent(in) :: local_coord(:)
      real(kdouble), intent(out) :: N(:)
    end subroutine monolis_shape_func
  end interface

  ! 定義域関数のインターフェース定義
  interface
    subroutine monolis_domain_func(local_coord,  is_inside)
      use mod_monolis_utils_define_prm
      implicit none
      real(kdouble), intent(in) :: local_coord(:)
      logical, intent(out) :: is_inside
    end subroutine monolis_domain_func
  end interface

contains

  !> 要素の基本情報を取得する関数
  subroutine monolis_shape_get_element_info(elem_type, n_node, dim, n_face, n_edge)
    implicit none
    !> [in] 要素種別ID
    integer(kint), intent(in) :: elem_type
    !> [out] 節点数
    integer(kint), intent(out) :: n_node
    !> [out] 要素次元（0,1,2,3）
    integer(kint), intent(out) :: dim
    !> [out] 面数
    integer(kint), intent(out) :: n_face
    !> [out] エッジ数
    integer(kint), intent(out) :: n_edge
    
    select case(elem_type)
      case(monolis_shape_point) ! 点要素(1)
        n_node = 1; dim = DIM_0D; n_face = 0; n_edge = 0
      case(monolis_shape_1d_line_1st) ! 線要素1次(111)
        n_node = 2; dim = DIM_1D; n_face = 0; n_edge = 0
      case(monolis_shape_1d_line_2nd) ! 線要素2次(112)
        n_node = 3; dim = DIM_1D; n_face = 0; n_edge = 0
      case(monolis_shape_2d_tri_1st) ! 三角形1次要素(231)
        n_node = 3; dim = DIM_2D; n_face = 0; n_edge = 3
      case(monolis_shape_2d_tri_2nd) ! 三角形2次要素(232)
        n_node = 6; dim = DIM_2D; n_face = 0; n_edge = 3
      case(monolis_shape_2d_quad_1st) ! 四角形1次要素(241)
        n_node = 4; dim = DIM_2D; n_face = 0; n_edge = 4
      case(monolis_shape_2d_quad_2nd) ! 四角形2次要素(242)
        n_node = 8; dim = DIM_2D; n_face = 0; n_edge = 4
      case(monolis_shape_3d_tet_1st) ! 四面体1次要素(341)
        n_node = 4; dim = DIM_3D; n_face = 4; n_edge = 6
      case(monolis_shape_3d_tet_2nd) ! 四面体2次要素(342)
        n_node = 10; dim = DIM_3D; n_face = 4; n_edge = 6
      case(monolis_shape_3d_hex_1st) ! 六面体1次要素(361)
        n_node = 8; dim = DIM_3D; n_face = 6; n_edge = 12
      case(monolis_shape_3d_hex_2nd) ! 六面体2次要素(362)
        n_node = 20; dim = DIM_3D; n_face = 6; n_edge = 12
      case default
        n_node = 0; dim = -1; n_face = 0; n_edge = 0
        print *, "Error: Unknown element type:", elem_type
    end select
  end subroutine monolis_shape_get_element_info

  !> 面情報を取得する関数
  subroutine monolis_shape_get_face_data(elem_type, face_id, face_nodes, face_type)
    implicit none
    !> [in] 親要素種別ID
    integer(kint), intent(in) :: elem_type
    !> [in] 面ID (1-based)
    integer(kint), intent(in) :: face_id
    !> [out] 面を構成する節点番号
    integer(kint), allocatable, intent(out) :: face_nodes(:)
    !> [out] 面要素の種別ID
    integer(kint), intent(out) :: face_type
    
    ! 事前にface_nodesが割り当てられていた場合は解放
    if(allocated(face_nodes)) deallocate(face_nodes)
    
    select case(elem_type)
      case(monolis_shape_3d_tet_1st) ! 四面体1次要素(341)
        call monolis_shape_3d_tet_1st_get_face_data(face_id, face_nodes, face_type)
      case(monolis_shape_3d_tet_2nd) ! 四面体2次要素(342)
        ! 2次四面体の場合は専用関数を呼ぶ（実装後に追加）
        ! 現状は未対応として空配列と-1を設定
        face_type = -1
        allocate(face_nodes(0))
      case(monolis_shape_3d_hex_1st) ! 六面体1次要素(361)
        call monolis_shape_3d_hex_1st_get_face_data(face_id, face_nodes, face_type)
      case(monolis_shape_3d_hex_2nd) ! 六面体2次要素(362)
        ! 2次六面体の場合は専用関数を呼ぶ（実装後に追加）
        ! 現状は未対応として空配列と-1を設定
        face_type = -1
        allocate(face_nodes(0))
      case default
        ! サポートされていない要素タイプ
        face_type = -1
        allocate(face_nodes(0))
        print *, "Warning: get_face_data - Unsupported element type:", elem_type
    end select
  end subroutine monolis_shape_get_face_data

  !> エッジ情報を取得する関数
  subroutine monolis_shape_get_edge_data(elem_type, edge_id, edge_nodes, edge_type)
    implicit none
    !> [in] 親要素種別ID
    integer(kint), intent(in) :: elem_type
    !> [in] エッジID (1-based)
    integer(kint), intent(in) :: edge_id
    !> [out] エッジを構成する節点番号
    integer(kint), allocatable, intent(out) :: edge_nodes(:)
    !> [out] エッジ要素の種別ID
    integer(kint), intent(out) :: edge_type

    ! 事前にedge_nodesが割り当てられていた場合は解放
    if(allocated(edge_nodes)) deallocate(edge_nodes)
    
    select case(elem_type)
      case(monolis_shape_3d_tet_1st) ! 四面体1次要素(341)
        call monolis_shape_3d_tet_1st_get_edge_data(edge_id, edge_nodes, edge_type)
      case(monolis_shape_3d_tet_2nd) ! 四面体2次要素(342)
        ! 2次四面体の場合は専用関数を呼ぶ（実装後に追加）
        ! 現状は未対応として空配列と-1を設定
        edge_type = -1
        allocate(edge_nodes(0))
      case(monolis_shape_3d_hex_1st) ! 六面体1次要素(361)
        call monolis_shape_3d_hex_1st_get_edge_data(edge_id, edge_nodes, edge_type)
      case(monolis_shape_3d_hex_2nd) ! 六面体2次要素(362)
        ! 2次六面体の場合は専用関数を呼ぶ（実装後に追加）
        ! 現状は未対応として空配列と-1を設定
        edge_type = -1
        allocate(edge_nodes(0))
      case(monolis_shape_2d_tri_1st) ! 三角形1次要素(231)
        call monolis_shape_2d_tri_1st_get_edge_data(edge_id, edge_nodes, edge_type)
      case(monolis_shape_2d_tri_2nd) ! 三角形2次要素(232)
        ! 2次三角形の場合は専用関数を呼ぶ（実装後に追加）
        ! 現状は未対応として空配列と-1を設定
        edge_type = -1
        allocate(edge_nodes(0))
      case(monolis_shape_2d_quad_1st) ! 四角形1次要素(241)
        call monolis_shape_2d_quad_1st_get_edge_data(edge_id, edge_nodes, edge_type)
      case(monolis_shape_2d_quad_2nd) ! 四角形2次要素(242)
        ! 2次四角形の場合は専用関数を呼ぶ（実装後に追加）
        ! 現状は未対応として空配列と-1を設定
        edge_type = -1
        allocate(edge_nodes(0))
      case(monolis_shape_1d_line_1st, monolis_shape_1d_line_2nd) ! 線要素は自身がエッジなのでエラー
        edge_type = -1
        allocate(edge_nodes(0))
      case default
        ! サポートされていない要素タイプ
        edge_type = -1
        allocate(edge_nodes(0))
        print *, "Warning: get_edge_data - Unsupported element type:", elem_type
    end select
  end subroutine monolis_shape_get_edge_data

  !> 境界要素の局所座標から親要素の局所座標への変換関数
  subroutine monolis_shape_map_local_coord(elem_type, sub_dim, sub_id, sub_coord, parent_coord)
    implicit none
    !> [in] 要素種別ID
    integer(kint), intent(in) :: elem_type
    !> [in] 部分要素次元（0:頂点, 1:エッジ, 2:面）
    integer(kint), intent(in) :: sub_dim
    !> [in] 部分要素ID (1-based)
    integer(kint), intent(in) :: sub_id
    !> [in] 部分要素での局所座標
    real(kdouble), intent(in) :: sub_coord(:)
    !> [out] 親要素での対応する局所座標
    real(kdouble), intent(out) :: parent_coord(3)

    parent_coord = 0.0d0
    
    select case(elem_type)
      case(monolis_shape_1d_line_1st)
        ! 1次元線要素の場合、対応する専用の関数を呼び出し
        call monolis_shape_1d_line_1st_map_local_coord(sub_dim, sub_id, sub_coord, parent_coord)

      case(monolis_shape_3d_tet_1st)
        ! 四面体1次要素の場合、要素固有の関数を呼び出す
        call monolis_shape_3d_tet_1st_map_local_coord(sub_dim, sub_id, sub_coord, parent_coord)
        
      case(monolis_shape_3d_hex_1st)
        ! 六面体1次要素の場合、要素固有の関数を呼び出す
        call monolis_shape_3d_hex_1st_map_local_coord(sub_dim, sub_id, sub_coord, parent_coord)
      
      case(monolis_shape_2d_tri_1st)
        ! 2D三角形要素の場合、要素固有の関数を呼び出す
        if (size(parent_coord) >= 2) then
          call monolis_shape_2d_tri_1st_map_local_coord(sub_dim, sub_id, sub_coord, parent_coord(1:2))
        endif
      
      case(monolis_shape_2d_quad_1st)
        ! 2D四角形要素の場合、要素固有の関数を呼び出す
        if (size(parent_coord) >= 2) then
          call monolis_shape_2d_quad_1st_map_local_coord(sub_dim, sub_id, sub_coord, parent_coord(1:2))
        endif
      
      ! 他の要素タイプも同様に追加
      case default
        ! 未対応の要素タイプの場合
        parent_coord = 0.0d0
    end select
  end subroutine monolis_shape_map_local_coord

  !> 要素の局所座標が境界上にあるか判定する関数
  subroutine monolis_shape_is_on_boundary(elem_type, local_coord, sub_dim, sub_id, is_on_boundary_out, tol)
    implicit none
    !> [in] 要素種別ID
    integer(kint), intent(in) :: elem_type
    !> [in] 要素局所座標
    real(kdouble), intent(in) :: local_coord(:)
    !> [in] 検査したい部分要素次元（0:頂点, 1:エッジ, 2:面）
    integer(kint), intent(in) :: sub_dim
    !> [in] 検査したい部分要素ID
    integer(kint), intent(in) :: sub_id
    !> [out] 境界上かどうかの判定結果
    logical, intent(out) :: is_on_boundary_out
    !> [in] 判定許容誤差
    real(kdouble), optional, intent(in) :: tol

    real(kdouble) :: tolerance
    
    is_on_boundary_out = .false.
    
    ! 許容誤差の設定
    if(present(tol)) then
      tolerance = tol
    else
      tolerance = 1.0d-10
    endif
    
    ! 特定の部分要素を検査
    select case(elem_type)
      case(monolis_shape_2d_tri_1st)
        if(sub_dim == 1) then ! エッジ
          call monolis_shape_2d_tri_1st_is_on_boundary(local_coord, is_on_boundary_out)
        endif
      case(monolis_shape_2d_quad_1st)
        if(sub_dim == 1) then ! エッジ
          call monolis_shape_2d_quad_1st_is_on_boundary(local_coord, is_on_boundary_out)
        endif
      case(monolis_shape_3d_tet_1st)
        if(sub_dim == 2) then ! 面
          call monolis_shape_3d_tet_1st_is_on_boundary(local_coord, is_on_boundary_out)
        elseif(sub_dim == 1) then ! エッジ
          call monolis_shape_3d_tet_1st_is_on_boundary(local_coord, is_on_boundary_out)
        endif
      case(monolis_shape_3d_hex_1st)
        if(sub_dim == 2) then ! 面
          call monolis_shape_3d_hex_1st_is_on_boundary(local_coord, is_on_boundary_out)
        elseif(sub_dim == 1) then ! エッジ
          call monolis_shape_3d_hex_1st_is_on_boundary(local_coord, is_on_boundary_out)
        endif
    end select
  end subroutine monolis_shape_is_on_boundary

  !> 特定の境界が要素の局所座標上にあるかを判定する内部ヘルパー関数
  subroutine check_specific_boundary(elem_type, local_coord, sub_dim, sub_id, is_on_boundary, tolerance)
    implicit none
    !> [in] 要素種別ID
    integer(kint), intent(in) :: elem_type
    !> [in] 要素局所座標
    real(kdouble), intent(in) :: local_coord(:)
    !> [in] 検査する部分要素次元（0:頂点, 1:エッジ, 2:面）
    integer(kint), intent(in) :: sub_dim
    !> [in] 検査する部分要素ID
    integer(kint), intent(in) :: sub_id
    !> [out] 境界上かどうかの判定結果
    logical, intent(out) :: is_on_boundary
    !> [in] 判定許容誤差
    real(kdouble), intent(in) :: tolerance
    
    is_on_boundary = .false.
    
    select case(elem_type)
      case(monolis_shape_2d_tri_1st)
        if(sub_dim == 1) then ! エッジ
          call monolis_shape_2d_tri_1st_is_on_edge(local_coord, sub_id, is_on_boundary, tolerance)
        endif
      case(monolis_shape_2d_quad_1st)
        if(sub_dim == 1) then ! エッジ
          call monolis_shape_2d_quad_1st_is_on_edge(local_coord, sub_id, is_on_boundary, tolerance)
        endif
      case(monolis_shape_3d_tet_1st)
        if(sub_dim == 2) then ! 面
          call monolis_shape_3d_tet_1st_is_on_face(local_coord, sub_id, is_on_boundary, tolerance)
        elseif(sub_dim == 1) then ! エッジ
          call monolis_shape_3d_tet_1st_is_on_edge(local_coord, sub_id, is_on_boundary, tolerance)
        endif
      case(monolis_shape_3d_hex_1st)
        if(sub_dim == 2) then ! 面
          call monolis_shape_3d_hex_1st_is_on_face(local_coord, sub_id, is_on_boundary, tolerance)
        elseif(sub_dim == 1) then ! エッジ
          call monolis_shape_3d_hex_1st_is_on_edge(local_coord, sub_id, is_on_boundary, tolerance)
        endif
    end select
  end subroutine check_specific_boundary
  
  !> 頂点の局所座標を取得する関数
  subroutine monolis_shape_get_vertex_local_coord(elem_type, vertex_id, local_coord)
    implicit none
    integer(kint), intent(in) :: elem_type
    integer(kint), intent(in) :: vertex_id
    real(kdouble), intent(out) :: local_coord(:)
    integer(kint) :: dim
    
    select case(elem_type)
      case(monolis_shape_1d_line_1st)
        call monolis_shape_1d_line_1st_map_local_coord(0, vertex_id, (/0.0d0/), local_coord)
        
      case(monolis_shape_2d_tri_1st)
        call monolis_shape_2d_tri_1st_map_local_coord(0, vertex_id, (/0.0d0, 0.0d0/), local_coord)
        
      case(monolis_shape_2d_quad_1st)
        call monolis_shape_2d_quad_1st_map_local_coord(0, vertex_id, (/0.0d0, 0.0d0/), local_coord)
        
      case(monolis_shape_3d_tet_1st)
        call monolis_shape_3d_tet_1st_map_local_coord(0, vertex_id, (/0.0d0, 0.0d0, 0.0d0/), local_coord)
        
      case(monolis_shape_3d_hex_1st)
        call monolis_shape_3d_hex_1st_map_local_coord(0, vertex_id, (/0.0d0, 0.0d0, 0.0d0/), local_coord)
        
      case default
        local_coord = 0.0d0
    end select
  end subroutine monolis_shape_get_vertex_local_coord
  
  !> 面数を取得する関数
  function monolis_shape_get_num_faces(elem_type) result(n_face)
    implicit none
    integer(kint), intent(in) :: elem_type
    integer(kint) :: n_face
    integer(kint) :: n_node, dim, n_edge
    
    call monolis_shape_get_element_info(elem_type, n_node, dim, n_face, n_edge)
  end function monolis_shape_get_num_faces
  
  !> エッジ数を取得する関数
  function monolis_shape_get_num_edges(elem_type) result(n_edge)
    implicit none
    integer(kint), intent(in) :: elem_type
    integer(kint) :: n_edge
    integer(kint) :: n_node, dim, n_face
    
    call monolis_shape_get_element_info(elem_type, n_node, dim, n_face, n_edge)
  end function monolis_shape_get_num_edges
  
  !> 節点数を取得する関数
  function monolis_shape_get_num_vertex(elem_type) result(n_node)
    implicit none
    integer(kint), intent(in) :: elem_type
    integer(kint) :: n_node
    integer(kint) :: dim, n_face, n_edge
    
    call monolis_shape_get_element_info(elem_type, n_node, dim, n_face, n_edge)
  end function monolis_shape_get_num_vertex
  
  !> 要素の次元数を取得する関数
  function monolis_shape_get_dimension(elem_type) result(dim)
    implicit none
    integer(kint), intent(in) :: elem_type
    integer(kint) :: dim
    integer(kint) :: n_node, n_face, n_edge
    
    call monolis_shape_get_element_info(elem_type, n_node, dim, n_face, n_edge)
  end function monolis_shape_get_dimension

  !> 要素の形状関数プロシージャポインタを取得する関数
  subroutine monolis_shape_get_shape_func(elem_type, shape_func)
    implicit none
    integer(kint), intent(in) :: elem_type
    procedure(monolis_shape_func), pointer, intent(out) :: shape_func
    
    nullify(shape_func)
    
    select case(elem_type)
      case(monolis_shape_1d_line_1st)
        shape_func => monolis_shape_func_1d_line_1st
      case(monolis_shape_2d_tri_1st)
        shape_func => monolis_shape_func_2d_tri_1st
      case(monolis_shape_2d_quad_1st)
        shape_func => monolis_shape_func_2d_quad_1st
      case(monolis_shape_3d_tet_1st)
        shape_func => monolis_shape_func_3d_tet_1st
      case(monolis_shape_3d_hex_1st)
        shape_func => monolis_shape_func_3d_hex_1st
    end select
  end subroutine monolis_shape_get_shape_func
  
  !> 要素の定義域判定関数プロシージャポインタを取得する関数
  subroutine monolis_shape_get_domain_func(elem_type, domain_func)
    implicit none
    integer(kint), intent(in) :: elem_type
    procedure(monolis_domain_func), pointer, intent(out) :: domain_func
    
    nullify(domain_func)
    
    select case(elem_type)
      case(monolis_shape_1d_line_1st)
        domain_func => monolis_domain_func_1d_line
      case(monolis_shape_2d_tri_1st)
        domain_func => monolis_domain_func_2d_tri
      case(monolis_shape_2d_quad_1st)
        domain_func => monolis_domain_func_2d_quad
      case(monolis_shape_3d_tet_1st)
        domain_func => monolis_domain_func_3d_tet
      case(monolis_shape_3d_hex_1st)
        domain_func => monolis_domain_func_3d_hex
    end select
  end subroutine monolis_shape_get_domain_func
  
  !> 境界要素の形状関数プロシージャポインタを取得する関数
  subroutine monolis_shape_get_boundary_shape_func(elem_type, sub_dim, sub_id, boundary_shape_func)
    implicit none
    integer(kint), intent(in) :: elem_type
    integer(kint), intent(in) :: sub_dim
    integer(kint), intent(in) :: sub_id
    procedure(monolis_shape_func), pointer, intent(out) :: boundary_shape_func
    
    integer(kint) :: boundary_type
    integer(kint), allocatable :: boundary_nodes(:)
    
    nullify(boundary_shape_func)
    
    if(sub_dim == 2) then ! 面
      call monolis_shape_get_face_data(elem_type, sub_id, boundary_nodes, boundary_type)
    elseif(sub_dim == 1) then ! エッジ
      call monolis_shape_get_edge_data(elem_type, sub_id, boundary_nodes, boundary_type)
    else
      return
    endif
    
    call monolis_shape_get_shape_func(boundary_type, boundary_shape_func)
    
    if(allocated(boundary_nodes)) deallocate(boundary_nodes)
  end subroutine monolis_shape_get_boundary_shape_func
  
  !> 境界要素の定義域判定関数プロシージャポインタを取得する関数
  subroutine monolis_shape_get_boundary_domain_func(elem_type, sub_dim, sub_id, boundary_domain_func)
    implicit none
    integer(kint), intent(in) :: elem_type
    integer(kint), intent(in) :: sub_dim
    integer(kint), intent(in) :: sub_id
    procedure(monolis_domain_func), pointer, intent(out) :: boundary_domain_func
    
    integer(kint) :: boundary_type
    integer(kint), allocatable :: boundary_nodes(:)
    
    nullify(boundary_domain_func)
    
    if(sub_dim == 2) then ! 面
      call monolis_shape_get_face_data(elem_type, sub_id, boundary_nodes, boundary_type)
    elseif(sub_dim == 1) then ! エッジ
      call monolis_shape_get_edge_data(elem_type, sub_id, boundary_nodes, boundary_type)
    else
      return
    endif
    
    call monolis_shape_get_domain_func(boundary_type, boundary_domain_func)
    
    if(allocated(boundary_nodes)) deallocate(boundary_nodes)
  end subroutine monolis_shape_get_boundary_domain_func
  
  !> 境界情報を取得する関数
  subroutine monolis_shape_get_boundary_info(elem_type, sub_dim, sub_id, boundary_type, boundary_nodes)
    implicit none
    integer(kint), intent(in) :: elem_type
    integer(kint), intent(in) :: sub_dim
    integer(kint), intent(in) :: sub_id
    integer(kint), intent(out) :: boundary_type
    integer(kint), allocatable, intent(out) :: boundary_nodes(:)
    
    if(sub_dim == 2) then ! 面
      call monolis_shape_get_face_data(elem_type, sub_id, boundary_nodes, boundary_type)
    elseif(sub_dim == 1) then ! エッジ
      call monolis_shape_get_edge_data(elem_type, sub_id, boundary_nodes, boundary_type)
    else
      boundary_type = -1
    endif
  end subroutine monolis_shape_get_boundary_info
end module mod_monolis_shape_boundary