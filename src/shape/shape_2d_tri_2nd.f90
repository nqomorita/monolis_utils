module mod_monolis_shape_2d_tri_2nd
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
  use mod_monolis_def_shape
  use mod_monolis_shape_2d_tri_1st
  use mod_monolis_shape_1d_line_1st
  use mod_monolis_utils_alloc
  implicit none

  private

  real(kdouble), parameter :: gsp(2,4) = reshape([ &
    0.166666666666667d0, 0.166666666666667d0, &
    0.666666666666667d0, 0.166666666666667d0, &
    0.166666666666667d0, 0.666666666666667d0, &
    0.333333333333333d0, 0.333333333333333d0  &
    ], [2,4])

  real(kdouble), parameter :: np(2,6) = reshape([ &
     0.0d0, 0.0d0, &
     1.0d0, 0.0d0, &
     1.0d0, 0.0d0, &
     0.5d0, 0.0d0, &
     0.5d0, 0.5d0, &
     0.0d0, 0.5d0  &
    ], [2,6])

  real(kdouble), parameter :: weight(4) = [ &
     0.5d0, 0.5d0, 0.5d0, 0.5d0 &
    ]

  integer(kint), parameter :: monolis_shape_2d_tri_2nd_edge(3,3) = reshape([ &
     1, 4, 2, &
     2, 5, 3, &
     3, 6, 1  ], [3,3])

    public :: monolis_shape_2d_tri_2nd_num_gauss_point
    public :: monolis_shape_2d_tri_2nd_weight
    public :: monolis_shape_2d_tri_2nd_integral_point
    public :: monolis_shape_2d_tri_2nd_node_point
    public :: monolis_shape_2d_tri_2nd_is_inside_domain
    public :: monolis_shape_2d_tri_2nd_shapefunc
    public :: monolis_shape_2d_tri_2nd_shapefunc_deriv
    public :: monolis_shape_2d_tri_2nd_shapefunc_2nd_deriv
    public :: monolis_shape_2d_tri_2nd_get_global_position
    public :: monolis_shape_2d_tri_2nd_get_global_deriv
    public :: monolis_shape_2d_tri_2nd_edge
    !> 標準インターフェース用の関数を公開
    public :: monolis_shape_func_2d_tri_2nd
    public :: monolis_edge_data_func_2d_tri_2nd
    public :: monolis_edge_map_func_2d_tri_2nd

contains

  function monolis_shape_2d_tri_2nd_num_gauss_point()
    implicit none
    integer(kint) :: monolis_shape_2d_tri_2nd_num_gauss_point
    monolis_shape_2d_tri_2nd_num_gauss_point = 4
  end function monolis_shape_2d_tri_2nd_num_gauss_point

  function monolis_shape_2d_tri_2nd_weight(i)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble) :: monolis_shape_2d_tri_2nd_weight
    monolis_shape_2d_tri_2nd_weight = weight(i)
  end function monolis_shape_2d_tri_2nd_weight

  subroutine monolis_shape_2d_tri_2nd_integral_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(2)

    r(1) = gsp(1,i)
    r(2) = gsp(2,i)
  end subroutine monolis_shape_2d_tri_2nd_integral_point

  subroutine monolis_shape_2d_tri_2nd_node_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(2)

    r(1) = np(1,i)
    r(2) = np(2,i)
  end subroutine monolis_shape_2d_tri_2nd_node_point

  subroutine monolis_shape_2d_tri_2nd_is_inside_domain(local, is_inside)
    implicit none
    real(kdouble), intent(in) :: local(2)
    logical, intent(out) :: is_inside
    real(kdouble) :: func(3)

    is_inside = .false.
    call monolis_shape_2d_tri_1st_shapefunc(local, func)
    if(0.0d0 <= func(1) .and. func(1) <= 1.0d0 .and. &
       0.0d0 <= func(2) .and. func(2) <= 1.0d0 .and. &
       0.0d0 <= func(3) .and. func(3) <= 1.0d0)then 
      is_inside = .true.
    endif
  end subroutine monolis_shape_2d_tri_2nd_is_inside_domain

  subroutine monolis_shape_2d_tri_2nd_shapefunc(local, func)
    implicit none
    real(kdouble), intent(in) :: local(2)
    real(kdouble), intent(out) :: func(6)
    real(kdouble) :: xi, et, st

    st = 1.0d0 - local(1) - local(2)
    xi = local(1)
    et = local(2)

    func(1) = st*(2.0d0*st - 1.0d0)
    func(2) = xi*(2.0d0*xi - 1.0d0)
    func(3) = et*(2.0d0*et - 1.0d0)
    func(4) = 4.0d0*xi*st
    func(5) = 4.0d0*xi*et
    func(6) = 4.0d0*et*st
  end subroutine monolis_shape_2d_tri_2nd_shapefunc

  subroutine monolis_shape_2d_tri_2nd_shapefunc_deriv(local, func)
    implicit none
    real(kdouble), intent(in) :: local(2)
    real(kdouble), intent(out) :: func(6,2)
    real(kdouble) :: xi, et, st

    st = 1.0d0 - local(1) - local(2)
    xi = local(1)
    et = local(2)

    func(1,1) = 1.0d0 - 4.0d0*st
    func(2,1) = 4.0d0*xi - 1.0d0
    func(3,1) = 0.0d0
    func(4,1) = 4.0d0*(st - xi)
    func(5,1) = 4.0d0*et
    func(6,1) =-4.0d0*et

    func(1,2) = 1.0d0 - 4.0d0*st
    func(2,2) = 0.0d0
    func(3,2) = 4.0d0*et - 1.0d0
    func(4,2) =-4.0d0*xi
    func(5,2) = 4.0d0*xi
    func(6,2) = 4.0d0*(st - et)
  end subroutine monolis_shape_2d_tri_2nd_shapefunc_deriv

  subroutine monolis_shape_2d_tri_2nd_shapefunc_2nd_deriv(func)
    implicit none
    real(kdouble), intent(out) :: func(6,2,2)

    func(1,1,1) = 4.d0;  func(1,1,2) = 4.d0
    func(2,1,1) = 4.d0;  func(2,1,2) = 0.d0
    func(3,1,1) = 0.d0;  func(3,1,2) = 0.d0
    func(4,1,1) =-8.d0;  func(4,1,2) =-4.d0
    func(5,1,1) = 0.d0;  func(5,1,2) = 4.d0
    func(6,1,1) = 0.d0;  func(6,1,2) =-4.d0

    func(1,2,1) = 4.d0;  func(1,2,2) = 4.d0
    func(2,2,1) = 0.d0;  func(2,2,2) = 0.d0
    func(3,2,1) = 0.d0;  func(3,2,2) = 4.d0
    func(4,2,1) =-4.d0;  func(4,2,2) = 0.d0
    func(5,2,1) = 4.d0;  func(5,2,2) = 0.d0
    func(6,2,1) =-4.d0;  func(6,2,2) =-8.d0
  end subroutine monolis_shape_2d_tri_2nd_shapefunc_2nd_deriv

  subroutine monolis_shape_2d_tri_2nd_get_global_position(node, r, pos)
    implicit none
    real(kdouble), intent(in) :: node(2,6), r(2)
    real(kdouble), intent(out) :: pos(2)
    real(kdouble) :: func(6)

    call monolis_shape_2d_tri_2nd_shapefunc(r, func)
    pos = matmul(node, func)
  end subroutine monolis_shape_2d_tri_2nd_get_global_position

  subroutine monolis_shape_2d_tri_2nd_get_global_deriv(node, r, dndx, det)
    implicit none
    real(kdouble), intent(in) :: node(2,6)
    real(kdouble), intent(in) :: r(2)
    real(kdouble), intent(out) :: dndx(6,2)
    real(kdouble), intent(out) :: det
    real(kdouble) :: deriv(6,2), xj(2,2), inv(2,2)

    call monolis_shape_2d_tri_2nd_shapefunc_deriv(r, deriv)
    xj = matmul(node, deriv)
    call monolis_get_inverse_matrix_R_2d(xj, inv, det)
    dndx = matmul(deriv, inv)
  end subroutine monolis_shape_2d_tri_2nd_get_global_deriv

  !> 標準インターフェースによる形状関数
  subroutine monolis_shape_func_2d_tri_2nd(local_coord, N)
    implicit none
    real(kdouble), intent(in) :: local_coord(:)
    real(kdouble), intent(out) :: N(:)
    
    call monolis_shape_2d_tri_2nd_shapefunc(local_coord, N)
  end subroutine monolis_shape_func_2d_tri_2nd

  !> 標準インターフェースによるエッジ情報定義関数
  subroutine monolis_edge_data_func_2d_tri_2nd(i_edge, n_edge_node, edge_node_ids, &
    edge_shape_func, edge_domain_func, edge_local_np_func, edge_shape_map_func)
    implicit none
    integer(kint), intent(in) :: i_edge
    integer(kint), intent(out) :: n_edge_node
    integer(kint), intent(out), allocatable :: edge_node_ids(:)
    procedure(monolis_shape_func), pointer, intent(out) :: edge_shape_func
    procedure(monolis_domain_func), pointer, intent(out) :: edge_domain_func
    procedure(monolis_local_node_point_func), pointer, intent(out) :: edge_local_np_func
    procedure(monolis_shape_map_func), pointer, intent(out) :: edge_shape_map_func

    stop "monolis_edge_data_func_2d_tri_2nd"

    if(i_edge < 1 .or. 3 < i_edge)then
      n_edge_node = -1
      return
    endif

    n_edge_node = 3
    call monolis_alloc_I_1d(edge_node_ids, 3)
    edge_node_ids(1) = monolis_shape_2d_tri_2nd_edge(1, i_edge)
    edge_node_ids(2) = monolis_shape_2d_tri_2nd_edge(2, i_edge)
    edge_node_ids(3) = monolis_shape_2d_tri_2nd_edge(3, i_edge)

    edge_shape_func => null() !monolis_shape_func_1d_line_2nd
    edge_domain_func => monolis_domain_func_1d_line
    edge_local_np_func => null() !monolis_local_node_position_1d_line_2nd
    edge_shape_map_func => monolis_edge_map_func_2d_tri_2nd
  end subroutine monolis_edge_data_func_2d_tri_2nd

  !> 2D三角形要素の部分要素の局所座標を親要素の局所座標にマップする関数
  subroutine monolis_edge_map_func_2d_tri_2nd(i_edge, local_coord, local_coord_3d)
    use mod_monolis_utils_define_prm
    implicit none
    integer(kint), intent(in) :: i_edge
    real(kdouble), intent(in) :: local_coord(:)
    real(kdouble), intent(out) :: local_coord_3d(:)
    real(kdouble) :: u

    stop "monolis_edge_map_func_2d_tri_2nd"

    !u = 0.5d0 * (local_coord(1) + 1.0d0)

    !select case(i_edge)
    !  case(1) ! 辺1-2
    !    local_coord_3d(1) = u
    !    local_coord_3d(2) = 0.0d0
    !  case(2) ! 辺2-3
    !    local_coord_3d(1) = 1.0d0 - u
    !    local_coord_3d(2) = u
    !  case(3) ! 辺3-1
    !    local_coord_3d(1) = 0.0d0
    !    local_coord_3d(2) = 1.0d0 - u
    !end select
  end subroutine monolis_edge_map_func_2d_tri_2nd
end module mod_monolis_shape_2d_tri_2nd
