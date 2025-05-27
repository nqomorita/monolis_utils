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
  public :: monolis_shape_get_num_faces
  public :: monolis_shape_get_num_edges
  public :: monolis_shape_get_num_vertex
  public :: monolis_shape_get_shape_func
  public :: monolis_shape_get_domain_func

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
      ! 点要素(1)
      case(monolis_shape_0d_point) 
        n_node = 1; dim = 0; n_face = 0; n_edge = 0

      ! 線要素1次(111)
      case(monolis_shape_1d_line_1st)
        n_node = 2; dim = 1; n_face = 0; n_edge = 0

      ! 線要素2次(112)
      case(monolis_shape_1d_line_2nd)
        n_node = 3; dim = 1; n_face = 0; n_edge = 0

      ! 三角形1次要素(231)
      case(monolis_shape_2d_tri_1st)
        n_node = 3; dim = 2; n_face = 0; n_edge = 3

      ! 三角形2次要素(232)
      case(monolis_shape_2d_tri_2nd) 
        n_node = 6; dim = 2; n_face = 0; n_edge = 3

      ! 四角形1次要素(241)
      case(monolis_shape_2d_quad_1st)
        n_node = 4; dim = 2; n_face = 0; n_edge = 4

      ! 四角形2次要素(242)
      case(monolis_shape_2d_quad_2nd)
        n_node = 8; dim = 2; n_face = 0; n_edge = 4

      ! 四面体1次要素(341)
      case(monolis_shape_3d_tet_1st) 
        n_node = 4; dim = 3; n_face = 4; n_edge = 6

      ! 四面体2次要素(342)
      case(monolis_shape_3d_tet_2nd) 
        n_node = 10; dim = 3; n_face = 4; n_edge = 6

      ! 六面体1次要素(361)
      case(monolis_shape_3d_hex_1st) 
        n_node = 8; dim = 3; n_face = 6; n_edge = 12

      ! 六面体2次要素(362)
      case(monolis_shape_3d_hex_2nd) 
        n_node = 20; dim = 3; n_face = 6; n_edge = 12
  
      case default
        n_node = 0; dim = -1; n_face = 0; n_edge = 0
        print *, "Error: Unknown element type:", elem_type
    end select
  end subroutine monolis_shape_get_element_info

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

end module mod_monolis_shape_boundary