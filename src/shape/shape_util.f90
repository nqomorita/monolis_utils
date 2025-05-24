module mod_monolis_shape_util
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
  use mod_monolis_def_shape  ! 基本定義モジュールをインポート
  use mod_monolis_shape_3d_hex_1st
  implicit none

  !> 局所勾配計算関数のインターフェース定義
  interface
    subroutine monolis_shape_deriv_func(local_coord, dNdx)
      use mod_monolis_utils_define_prm
      implicit none
      real(kdouble), intent(in) :: local_coord(:)
      real(kdouble), intent(out) :: dNdx(:,:)
    end subroutine monolis_shape_deriv_func
  end interface

  !> 部分要素の局所座標マッピング関数のインターフェース定義
  interface
    subroutine monolis_map_func(sub_dim, sub_id, sub_coord, parent_coord)
      use mod_monolis_utils_define_prm
      implicit none
      integer(kint), intent(in) :: sub_dim
      integer(kint), intent(in) :: sub_id
      real(kdouble), intent(in) :: sub_coord(:)
      real(kdouble), intent(out) :: parent_coord(:)
    end subroutine monolis_map_func
  end interface

contains

  !> 要素の積分点数を取得する統合関数
  function monolis_shape_get_num_integral_point(elem_type) result(n_int)
    use mod_monolis_def_shape
    use mod_monolis_shape_1d_line_1st
    use mod_monolis_shape_2d_tri_1st
    use mod_monolis_shape_2d_quad_1st
    use mod_monolis_shape_3d_tet_1st
    use mod_monolis_shape_3d_hex_1st
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
    use mod_monolis_def_shape
    use mod_monolis_shape_1d_line_1st
    use mod_monolis_shape_2d_tri_1st
    use mod_monolis_shape_2d_quad_1st
    use mod_monolis_shape_3d_tet_1st
    use mod_monolis_shape_3d_hex_1st
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
    use mod_monolis_def_shape
    use mod_monolis_shape_1d_line_1st
    use mod_monolis_shape_2d_tri_1st
    use mod_monolis_shape_2d_quad_1st
    use mod_monolis_shape_3d_tet_1st
    use mod_monolis_shape_3d_hex_1st
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
    use mod_monolis_def_shape
    use mod_monolis_shape_1d_line_1st
    use mod_monolis_shape_2d_tri_1st
    use mod_monolis_shape_2d_quad_1st
    use mod_monolis_shape_3d_tet_1st
    use mod_monolis_shape_3d_hex_1st
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
    use mod_monolis_def_shape
    use mod_monolis_shape_1d_line_1st
    use mod_monolis_shape_2d_tri_1st
    use mod_monolis_shape_2d_quad_1st
    use mod_monolis_shape_3d_tet_1st
    use mod_monolis_shape_3d_hex_1st
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

end module mod_monolis_shape_util
