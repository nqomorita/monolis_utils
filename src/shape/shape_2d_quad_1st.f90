module mod_monolis_shape_2d_quad_1st
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
  use mod_monolis_def_shape
  use mod_monolis_shape_1d_line_1st
  use mod_monolis_utils_alloc
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
    ! 標準インターフェース用の関数を公開
    public :: monolis_shape_func_2d_quad_1st
    public :: monolis_domain_func_2d_quad
    public :: monolis_edge_data_func_2d_quad_1st
    public :: monolis_edge_map_func_2d_quad_1st

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
  subroutine monolis_domain_func_2d_quad(local_coord, is_inside)
    implicit none
    real(kdouble), intent(in) :: local_coord(:)
    logical, intent(out) :: is_inside
    
    call monolis_shape_2d_quad_1st_is_inside_domain(local_coord, is_inside)
  end subroutine monolis_domain_func_2d_quad

  !> 標準インターフェースによるエッジ情報定義関数
  subroutine monolis_edge_data_func_2d_quad_1st(i_edge, n_edge_node, edge_node_ids, &
    edge_shape_func, edge_domain_func, edge_local_np_func, edge_shape_map_func)
    implicit none
    integer(kint), intent(in) :: i_edge
    integer(kint), intent(out) :: n_edge_node
    integer(kint), intent(out), allocatable :: edge_node_ids(:)
    procedure(monolis_shape_func), pointer, intent(out) :: edge_shape_func
    procedure(monolis_domain_func), pointer, intent(out) :: edge_domain_func
    procedure(monolis_local_node_point_func), pointer, intent(out) :: edge_local_np_func
    procedure(monolis_shape_map_func), pointer, intent(out) :: edge_shape_map_func

    if(i_edge < 1 .or. 4 < i_edge)then
      n_edge_node = -1
      return
    endif

    n_edge_node = 2
    call monolis_alloc_I_1d(edge_node_ids, 2)
    edge_node_ids(1) = monolis_shape_2d_quad_1st_edge(1, i_edge)
    edge_node_ids(2) = monolis_shape_2d_quad_1st_edge(2, i_edge)

    edge_shape_func => monolis_shape_func_1d_line_1st
    edge_domain_func => monolis_domain_func_1d_line
    edge_local_np_func => monolis_local_node_position_1d_line_1st
    edge_shape_map_func => monolis_edge_map_func_2d_quad_1st
  end subroutine monolis_edge_data_func_2d_quad_1st

  !> 2次元四角形要素のエッジ要素の局所座標を親要素の局所座標にマップする関数
  subroutine monolis_edge_map_func_2d_quad_1st(i_edge, local_coord, local_coord_3d)
    use mod_monolis_utils_define_prm
    implicit none
    integer(kint), intent(in) :: i_edge
    real(kdouble), intent(in) :: local_coord(:)
    real(kdouble), intent(out) :: local_coord_3d(:)
    real(kdouble) :: u, f1, f2

    u = local_coord(1)
    f1 = 1.0d0 - (u + 1.0d0)/2.0d0
    f2 = (u + 1.0d0)/2.0d0

    select case(i_edge)
      case(1) ! 辺1-2
        local_coord_3d(1:2) = (/-1.0d0, -1.0d0/) * f1 + &
                              (/ 1.0d0, -1.0d0/) * f2
      case(2) ! 辺2-3
        local_coord_3d(1:2) = (/ 1.0d0, -1.0d0/) * f1 + &
                              (/ 1.0d0,  1.0d0/) * f2
      case(3) ! 辺3-4
        local_coord_3d(1:2) = (/ 1.0d0,  1.0d0/) * f1 + &
                              (/-1.0d0,  1.0d0/) * f2
      case(4) ! 辺4-1
        local_coord_3d(1:2) = (/-1.0d0,  1.0d0/) * f1 + &
                              (/-1.0d0, -1.0d0/) * f2
    end select
  end subroutine monolis_edge_map_func_2d_quad_1st
end module mod_monolis_shape_2d_quad_1st
