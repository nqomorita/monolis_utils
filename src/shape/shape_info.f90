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
  public :: monolis_shape_get_num_integral_point
  public :: monolis_shape_get_integral_weight
  public :: monolis_shape_get_integral_point
  public :: monolis_shape_get_node_point
  public :: monolis_shape_evaluate_all

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

  !> 要素の積分点数を取得する統合関数
  function monolis_shape_get_num_integral_point(elem_type) result(n_int)
    implicit none
    integer(kint), intent(in) :: elem_type
    integer(kint) :: n_int
    
    select case(elem_type)
      case(monolis_shape_1d_line_1st)
        n_int = monolis_shape_1d_line_1st_num_gauss_point()
      case(monolis_shape_2d_tri_1st)
        n_int = monolis_shape_2d_tri_1st_num_gauss_point()
      case(monolis_shape_2d_quad_1st)
        n_int = monolis_shape_2d_quad_1st_num_gauss_point()
      case(monolis_shape_3d_tet_1st)
        n_int = monolis_shape_3d_tet_1st_num_gauss_point()
      case(monolis_shape_3d_hex_1st)
        n_int = monolis_shape_3d_hex_1st_num_gauss_point()
      case default
        n_int = 0
    end select
  end function monolis_shape_get_num_integral_point

  !> 積分点の重みを取得する統合関数
  function monolis_shape_get_integral_weight(elem_type, i_point) result(weight)
    implicit none
    integer(kint), intent(in) :: elem_type
    integer(kint), intent(in) :: i_point
    real(kdouble) :: weight
    
    select case(elem_type)
      case(monolis_shape_1d_line_1st)
        weight = monolis_shape_1d_line_1st_weight(i_point)
      case(monolis_shape_2d_tri_1st)
        weight = monolis_shape_2d_tri_1st_weight(i_point)
      case(monolis_shape_2d_quad_1st)
        weight = monolis_shape_2d_quad_1st_weight(i_point)
      case(monolis_shape_3d_tet_1st)
        weight = monolis_shape_3d_tet_1st_weight(i_point)
      case(monolis_shape_3d_hex_1st)
        weight = monolis_shape_3d_hex_1st_weight(i_point)
      case default
        weight = 0.0d0
    end select
  end function monolis_shape_get_integral_weight

  !> 要素の積分点の局所座標を取得する統合関数
  subroutine monolis_shape_get_integral_point(elem_type, i_point, local_coord)
    implicit none
    integer(kint), intent(in) :: elem_type
    integer(kint), intent(in) :: i_point
    real(kdouble), allocatable, intent(out) :: local_coord(:)
    
    select case(elem_type)
      case(monolis_shape_1d_line_1st)
        allocate(local_coord(1))
        call monolis_shape_1d_line_1st_integral_point(i_point, local_coord)
      case(monolis_shape_2d_tri_1st)
        allocate(local_coord(2))
        call monolis_shape_2d_tri_1st_integral_point(i_point, local_coord)
      case(monolis_shape_2d_quad_1st)
        allocate(local_coord(2))
        call monolis_shape_2d_quad_1st_integral_point(i_point, local_coord)
      case(monolis_shape_3d_tet_1st)
        allocate(local_coord(3))
        call monolis_shape_3d_tet_1st_integral_point(i_point, local_coord)
      case(monolis_shape_3d_hex_1st)
        allocate(local_coord(3))
        call monolis_shape_3d_hex_1st_integral_point(i_point, local_coord)
      case default
        allocate(local_coord(0))
    end select
  end subroutine monolis_shape_get_integral_point

  !> 要素の節点の局所座標を取得する統合関数
  subroutine monolis_shape_get_node_point(elem_type, i_node, local_coord)
    implicit none
    integer(kint), intent(in) :: elem_type
    integer(kint), intent(in) :: i_node
    real(kdouble), allocatable, intent(out) :: local_coord(:)
    
    select case(elem_type)
      case(monolis_shape_1d_line_1st)
        allocate(local_coord(1))
        call monolis_shape_1d_line_1st_node_point(i_node, local_coord)
      case(monolis_shape_2d_tri_1st)
        allocate(local_coord(2))
        call monolis_shape_2d_tri_1st_node_point(i_node, local_coord)
      case(monolis_shape_2d_quad_1st)
        allocate(local_coord(2))
        call monolis_shape_2d_quad_1st_node_point(i_node, local_coord)
      case(monolis_shape_3d_tet_1st)
        allocate(local_coord(3))
        call monolis_shape_3d_tet_1st_node_point(i_node, local_coord)
      case(monolis_shape_3d_hex_1st)
        allocate(local_coord(3))
        call monolis_shape_3d_hex_1st_node_point(i_node, local_coord)
      case default
        allocate(local_coord(0))
    end select
  end subroutine monolis_shape_get_node_point

  !> 形状関数と勾配を同時に計算する統合関数
  subroutine monolis_shape_evaluate_all(elem_type, local_coord, node_coord, N, dNdx, det)
    implicit none
    integer(kint), intent(in) :: elem_type
    real(kdouble), intent(in) :: local_coord(:)
    real(kdouble), intent(in) :: node_coord(:,:)
    real(kdouble), allocatable, intent(out) :: N(:)
    real(kdouble), allocatable, intent(out) :: dNdx(:,:)
    real(kdouble), intent(out) :: det
    
    select case(elem_type)
      case(monolis_shape_1d_line_1st)
        allocate(N(2), dNdx(2,1))
        call monolis_shape_1d_line_1st_shapefunc(local_coord, N)
        call monolis_shape_1d_line_1st_get_global_deriv(node_coord, local_coord, dNdx, det)
      case(monolis_shape_2d_tri_1st)
        allocate(N(3), dNdx(3,2))
        call monolis_shape_2d_tri_1st_shapefunc(local_coord, N)
        call monolis_shape_2d_tri_1st_get_global_deriv(node_coord, local_coord, dNdx, det)
      case(monolis_shape_2d_quad_1st)
        allocate(N(4), dNdx(4,2))
        call monolis_shape_2d_quad_1st_shapefunc(local_coord, N)
        call monolis_shape_2d_quad_1st_get_global_deriv(node_coord, local_coord, dNdx, det)
      case(monolis_shape_3d_tet_1st)
        allocate(N(4), dNdx(4,3))
        call monolis_shape_3d_tet_1st_shapefunc(local_coord, N)
        call monolis_shape_3d_tet_1st_get_global_deriv(node_coord, local_coord, dNdx, det)
      case(monolis_shape_3d_hex_1st)
        allocate(N(8), dNdx(8,3))
        call monolis_shape_3d_hex_1st_shapefunc(local_coord, N)
        call monolis_shape_3d_hex_1st_get_global_deriv(node_coord, local_coord, dNdx, det)
      case default
        allocate(N(0), dNdx(0,0))
        det = 0.0d0
    end select
  end subroutine monolis_shape_evaluate_all
end module mod_monolis_shape_boundary