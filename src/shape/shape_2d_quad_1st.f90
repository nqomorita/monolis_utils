module mod_monolis_shape_2d_quad_1st
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
  use mod_monolis_def_shape
  implicit none

  private

  real(kdouble), parameter :: gsp(2,4) = reshape([ &
    -0.577350269189626d0,-0.577350269189626d0, &
     0.577350269189626d0,-0.577350269189626d0, &
    -0.577350269189626d0, 0.577350269189626d0, &
     0.577350269189626d0, 0.577350269189626d0  &
    ], [2,4])

  real(kdouble), parameter :: np(2,4) = reshape([ &
     -1.0d0, -1.0d0, &
      1.0d0, -1.0d0, &
      1.0d0,  1.0d0, &
     -1.0d0,  1.0d0  &
    ], [2,4])

  integer(kint), parameter :: monolis_shape_2d_quad_1st_edge(2,4) = reshape([ &
     1, 2, &
     2, 3, &
     3, 4, &
     4, 1  ], [2,4])

  !> [r_1, r_2, r_1 and r_2]
  real(kdouble), parameter :: monolis_shape_2d_quad_1st_edge_constraint_value(3,4) = reshape([ &
     0.0d0,-1.0d0, 0.0d0, &
     1.0d0, 0.0d0, 0.0d0, &
     0.0d0, 1.0d0, 0.0d0, &
    -1.0d0, 0.0d0, 0.0d0  ], [3,4])

  !> [r_1, r_2, r_1 and r_2]
  logical, parameter :: monolis_shape_2d_quad_1st_edge_constraint_flag(3,4) = reshape([ &
     .false., .true. , .false., &
     .true. , .false., .false., &
     .false., .true. , .false., &
     .true. , .false., .false.  ], [3,4])

    public :: monolis_shape_2d_quad_1st_num_gauss_point
    public :: monolis_shape_2d_quad_1st_weight
    public :: monolis_shape_2d_quad_1st_integral_point
    public :: monolis_shape_2d_quad_1st_node_point
    public :: monolis_shape_2d_quad_1st_is_inside_domain
    public :: monolis_shape_2d_quad_1st_shapefunc
    public :: monolis_shape_2d_quad_1st_shapefunc_deriv
    public :: monolis_shape_2d_quad_1st_get_global_position
    public :: monolis_shape_2d_quad_1st_get_global_deriv
    public :: monolis_shape_2d_quad_1st_edge
    public :: monolis_shape_2d_quad_1st_edge_constraint_value
    public :: monolis_shape_2d_quad_1st_edge_constraint_flag
    ! 標準インターフェース用の関数を公開
    public :: monolis_shape_func_2d_quad_1st
    public :: monolis_domain_func_2d_quad
    public :: monolis_shape_2d_quad_1st_get_edge_data
    public :: monolis_shape_2d_quad_1st_is_on_boundary
    public :: monolis_shape_2d_quad_1st_map_local_coord

contains

  function monolis_shape_2d_quad_1st_num_gauss_point()
    implicit none
    integer(kint) :: monolis_shape_2d_quad_1st_num_gauss_point
    monolis_shape_2d_quad_1st_num_gauss_point = 4
  end function monolis_shape_2d_quad_1st_num_gauss_point

  function monolis_shape_2d_quad_1st_weight(i)
    implicit none
    integer(kint), optional, intent(in) :: i
    real(kdouble) :: monolis_shape_2d_quad_1st_weight
    monolis_shape_2d_quad_1st_weight = 1.0d0
  end function monolis_shape_2d_quad_1st_weight

  subroutine monolis_shape_2d_quad_1st_integral_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(2)

    r(1) = gsp(1,i)
    r(2) = gsp(2,i)
  end subroutine monolis_shape_2d_quad_1st_integral_point

  subroutine monolis_shape_2d_quad_1st_node_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(2)

    r(1) = np(1,i)
    r(2) = np(2,i)
  end subroutine monolis_shape_2d_quad_1st_node_point

  subroutine monolis_shape_2d_quad_1st_is_inside_domain(local, is_inside)
    implicit none
    real(kdouble), intent(in) :: local(2)
    logical, intent(out) :: is_inside

    is_inside = .false.
    if(0.0d0 <= local(1) .and. local(1) <= 1.0d0 .and. &
       0.0d0 <= local(2) .and. local(2) <= 1.0d0)then 
      is_inside = .true.
    endif
  end subroutine monolis_shape_2d_quad_1st_is_inside_domain

  subroutine monolis_shape_2d_quad_1st_shapefunc(local, func)
    implicit none
    real(kdouble), intent(in) :: local(2)
    real(kdouble), intent(out) :: func(4)

    func(1) = 0.25d0*(1.0d0-local(1))*(1.0d0-local(2))
    func(2) = 0.25d0*(1.0d0+local(1))*(1.0d0-local(2))
    func(3) = 0.25d0*(1.0d0+local(1))*(1.0d0+local(2))
    func(4) = 0.25d0*(1.0d0-local(1))*(1.0d0+local(2))
  end subroutine monolis_shape_2d_quad_1st_shapefunc

  subroutine monolis_shape_2d_quad_1st_shapefunc_deriv(local, func)
    implicit none
    real(kdouble), intent(in) :: local(2)
    real(kdouble), intent(out) :: func(4,2)

    func(1,1) = -0.25d0*(1.0d0-local(2))
    func(2,1) =  0.25d0*(1.0d0-local(2))
    func(3,1) =  0.25d0*(1.0d0+local(2))
    func(4,1) = -0.25d0*(1.0d0+local(2))

    func(1,2) = -0.25d0*(1.0d0-local(1))
    func(2,2) = -0.25d0*(1.0d0+local(1))
    func(3,2) =  0.25d0*(1.0d0+local(1))
    func(4,2) =  0.25d0*(1.0d0-local(1))
  end subroutine monolis_shape_2d_quad_1st_shapefunc_deriv

  subroutine monolis_shape_2d_quad_1st_get_global_position(node, r, pos)
    implicit none
    real(kdouble), intent(in) :: node(2,4), r(2)
    real(kdouble), intent(out) :: pos(2)
    real(kdouble) :: func(4)

    call monolis_shape_2d_quad_1st_shapefunc(r, func)
    pos = matmul(node, func)
  end subroutine monolis_shape_2d_quad_1st_get_global_position

  subroutine monolis_shape_2d_quad_1st_get_global_deriv(node, r, dndx, det)
    implicit none
    real(kdouble), intent(in) :: node(2,4)
    real(kdouble), intent(in) :: r(2)
    real(kdouble), intent(out) :: dndx(4,2)
    real(kdouble), intent(out) :: det
    real(kdouble) :: deriv(4,2), xj(2,2), inv(2,2)

    call monolis_shape_2d_quad_1st_shapefunc_deriv(r, deriv)
    xj = matmul(node, deriv)
    call monolis_get_inverse_matrix_R_2d(xj, inv, det)
    dndx = matmul(deriv, inv)
  end subroutine monolis_shape_2d_quad_1st_get_global_deriv

  ! 標準インターフェースによる形状関数
  subroutine monolis_shape_func_2d_quad_1st(local_coord, N)
    implicit none
    real(kdouble), intent(in) :: local_coord(:)
    real(kdouble), intent(out) :: N(:)
    
    call monolis_shape_2d_quad_1st_shapefunc(local_coord, N)
  end subroutine monolis_shape_func_2d_quad_1st

  ! 標準インターフェースによる定義域判定関数
  subroutine monolis_domain_func_2d_quad(local_coord, dim, is_inside)
    implicit none
    real(kdouble), intent(in) :: local_coord(:)
    integer(kint), intent(in) :: dim
    logical, intent(out) :: is_inside
    
    call monolis_shape_2d_quad_1st_is_inside_domain(local_coord, is_inside)
  end subroutine monolis_domain_func_2d_quad

  !> 2次元四角形1次要素のエッジ情報を取得する関数
  subroutine monolis_shape_2d_quad_1st_get_edge_data(edge_id, edge_nodes, edge_type)
    use mod_monolis_def_shape, only: monolis_shape_1d_line_1st
    implicit none
    integer(kint), intent(in) :: edge_id
    integer(kint), allocatable, intent(out) :: edge_nodes(:)
    integer(kint), intent(out) :: edge_type
    
    if(edge_id < 1 .or. edge_id > 4) then
      edge_type = -1
      return
    endif
    
    edge_type = monolis_shape_1d_line_1st
    allocate(edge_nodes(2))
    edge_nodes(1:2) = monolis_shape_2d_quad_1st_edge(1:2, edge_id)
  end subroutine monolis_shape_2d_quad_1st_get_edge_data

  !> 2D四角形要素の境界上にあるかを判定する関数
  subroutine monolis_shape_2d_quad_1st_is_on_boundary(local_coord, is_on_boundary)
    implicit none
    real(kdouble), intent(in) :: local_coord(:)
    logical, intent(out) :: is_on_boundary
    real(kdouble) :: eps

    eps = 1.0d-10
    is_on_boundary = .false.
    
    ! 辺上にあるかどうかをチェック
    if (abs(local_coord(1) - (-1.0d0)) < eps .or. &
        abs(local_coord(1) - 1.0d0) < eps .or. &
        abs(local_coord(2) - (-1.0d0)) < eps .or. &
        abs(local_coord(2) - 1.0d0) < eps) then
      is_on_boundary = .true.
    endif
  end subroutine monolis_shape_2d_quad_1st_is_on_boundary

  !> 2D四角形要素の部分要素の局所座標を親要素の局所座標にマップする関数
  subroutine monolis_shape_2d_quad_1st_map_local_coord(sub_dim, sub_id, sub_coord, parent_coord)
    implicit none
    integer(kint), intent(in) :: sub_dim !> 部分要素次元（0:頂点, 1:辺）
    integer(kint), intent(in) :: sub_id  !> 部分要素ID (1-based)
    real(kdouble), intent(in) :: sub_coord(:)  !> 部分要素での局所座標
    real(kdouble), intent(out) :: parent_coord(:) !> 親要素での対応する局所座標
    
    real(kdouble) :: u
    
    parent_coord = 0.0d0
    
    if (sub_dim == 1) then ! 辺
      u = sub_coord(1) ! エッジ上の局所座標 [-1, 1]
      select case(sub_id)
        case(1) ! 辺1-2
          parent_coord(1:2) = (/-1.0d0, -1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                             (/ 1.0d0, -1.0d0/) * ((u+1.0d0)/2.0d0)
        case(2) ! 辺2-3
          parent_coord(1:2) = (/ 1.0d0, -1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                             (/ 1.0d0,  1.0d0/) * ((u+1.0d0)/2.0d0)
        case(3) ! 辺3-4
          parent_coord(1:2) = (/ 1.0d0,  1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                             (/-1.0d0,  1.0d0/) * ((u+1.0d0)/2.0d0)
        case(4) ! 辺4-1
          parent_coord(1:2) = (/-1.0d0,  1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                             (/-1.0d0, -1.0d0/) * ((u+1.0d0)/2.0d0)
      end select
    else if (sub_dim == 0) then ! 頂点
      select case(sub_id)
        case(1); parent_coord(1:2) = (/-1.0d0, -1.0d0/)
        case(2); parent_coord(1:2) = (/ 1.0d0, -1.0d0/)
        case(3); parent_coord(1:2) = (/ 1.0d0,  1.0d0/)
        case(4); parent_coord(1:2) = (/-1.0d0,  1.0d0/)
      end select
    endif
  end subroutine monolis_shape_2d_quad_1st_map_local_coord
end module mod_monolis_shape_2d_quad_1st
