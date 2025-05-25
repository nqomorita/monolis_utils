module mod_monolis_shape_1d_line_1st
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
  implicit none

  private

  real(kdouble), parameter :: gsp(1,1) = reshape([ 0.0d0 ], [1,1])

  real(kdouble), parameter :: np(1,2) = reshape([ &
     -1.0d0, &
      1.0d0  &
    ], [1,2])

  real(kdouble), parameter :: weight(1) = [ 2.0d0 ]

  integer(kint), parameter :: monolis_shape_1d_line_1st_edge(1,2) = reshape([ &
     1, 2  ], [1,2])

    public :: monolis_shape_1d_line_1st_num_gauss_point
    public :: monolis_shape_1d_line_1st_weight
    public :: monolis_shape_1d_line_1st_integral_point
    public :: monolis_shape_1d_line_1st_node_point
    public :: monolis_shape_1d_line_1st_is_inside_domain
    public :: monolis_shape_1d_line_1st_shapefunc
    public :: monolis_shape_1d_line_1st_shapefunc_deriv
    public :: monolis_shape_1d_line_1st_get_global_position
    public :: monolis_shape_1d_line_1st_get_global_deriv
    ! 標準インターフェース用の関数を公開
    public :: monolis_shape_func_1d_line_1st
    public :: monolis_domain_func_1d_line
    ! 境界マッピング関数
    public :: monolis_shape_1d_line_1st_map_local_coord

contains

  function monolis_shape_1d_line_1st_num_gauss_point()
    implicit none
    integer(kint) :: monolis_shape_1d_line_1st_num_gauss_point
    monolis_shape_1d_line_1st_num_gauss_point = 1
  end function monolis_shape_1d_line_1st_num_gauss_point

  function monolis_shape_1d_line_1st_weight(i)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble) :: monolis_shape_1d_line_1st_weight
    monolis_shape_1d_line_1st_weight = weight(i)
  end function monolis_shape_1d_line_1st_weight

  subroutine monolis_shape_1d_line_1st_integral_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(1)
    r(1) = gsp(1,i)
  end subroutine monolis_shape_1d_line_1st_integral_point

  subroutine monolis_shape_1d_line_1st_node_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(1)
    r(1) = np(1,i)
  end subroutine monolis_shape_1d_line_1st_node_point

  subroutine monolis_shape_1d_line_1st_is_inside_domain(local, is_inside)
    implicit none
    real(kdouble), intent(in) :: local(:) 
    logical, intent(out) :: is_inside
    is_inside = .false.
    if(-1.0d0 <= local(1) .and. local(1) <= 1.0d0)then
      is_inside = .true.
    endif
  end subroutine monolis_shape_1d_line_1st_is_inside_domain

  subroutine monolis_shape_1d_line_1st_shapefunc(local, func)
    implicit none
    real(kdouble), intent(in) :: local(:)
    real(kdouble), intent(out) :: func(:)
    ! 1次線要素の形状関数 N1 = (1-xi)/2, N2 = (1+xi)/2
    func(1) = 0.5d0 * (1.0d0 - local(1))
    func(2) = 0.5d0 * (1.0d0 + local(1))
  end subroutine monolis_shape_1d_line_1st_shapefunc

  subroutine monolis_shape_1d_line_1st_shapefunc_deriv(local, func)
    implicit none
    real(kdouble), intent(in) :: local(:)
    real(kdouble), intent(out) :: func(:,:)
    ! 1次線要素の形状関数の微分 dN1/dxi = -0.5, dN2/dxi = 0.5
    func(1,1) = -0.5d0
    func(2,1) =  0.5d0
  end subroutine monolis_shape_1d_line_1st_shapefunc_deriv

  subroutine monolis_shape_1d_line_1st_get_global_position(node, r, pos)
    implicit none
    real(kdouble), intent(in) :: node(1,2)
    real(kdouble), intent(in) :: r(:)
    real(kdouble), intent(out) :: pos(1)
    real(kdouble) :: func(2)

    call monolis_shape_1d_line_1st_shapefunc(r, func)
    pos = matmul(node, func)
  end subroutine monolis_shape_1d_line_1st_get_global_position

  subroutine monolis_shape_1d_line_1st_get_global_deriv(node, r, dndx, det)
    implicit none
    real(kdouble), intent(in) :: node(1,2)
    real(kdouble), intent(in) :: r(:)
    real(kdouble), intent(out) :: dndx(2,1)
    real(kdouble), intent(out) :: det
    real(kdouble) :: deriv(2,1), xj(1,1), inv(1,1)

    call monolis_shape_1d_line_1st_shapefunc_deriv(r, deriv)
    xj = matmul(node, deriv)
    
    ! 1次元の場合、ヤコビアンの逆行列は単に逆数
    det = xj(1,1)
    inv(1,1) = 1.0d0 / xj(1,1)
    
    dndx = matmul(deriv, inv)
  end subroutine monolis_shape_1d_line_1st_get_global_deriv

  ! 標準インターフェースによる形状関数
  subroutine monolis_shape_func_1d_line_1st(local_coord, N)
    implicit none
    real(kdouble), intent(in) :: local_coord(:)
    real(kdouble), intent(out) :: N(:)
    
    call monolis_shape_1d_line_1st_shapefunc(local_coord, N)
  end subroutine monolis_shape_func_1d_line_1st

  ! 標準インターフェースによる定義域判定関数
  subroutine monolis_domain_func_1d_line(local_coord, is_inside)
    implicit none
    real(kdouble), intent(in) :: local_coord(:)
    logical, intent(out) :: is_inside
    
    call monolis_shape_1d_line_1st_is_inside_domain(local_coord, is_inside)
  end subroutine monolis_domain_func_1d_line

  !> 1D線要素の部分要素（端点）の局所座標を親要素の局所座標にマップする関数
  subroutine monolis_shape_1d_line_1st_map_local_coord(sub_dim, sub_id, sub_coord, parent_coord)
    implicit none
    integer(kint), intent(in) :: sub_dim    !> 部分要素次元（0:頂点）
    integer(kint), intent(in) :: sub_id     !> 部分要素ID (1-based)
    real(kdouble), intent(in) :: sub_coord(:) !> 部分要素での局所座標
    real(kdouble), intent(out) :: parent_coord(:) !> 親要素での対応する局所座標
    
    parent_coord = 0.0d0
    
    ! 1次元線要素の場合、部分要素はすべて頂点（0次元）のみ
    if (sub_dim == 0) then ! 頂点
      select case(sub_id)
        case(1) ! 最初の頂点 (-1.0)
          parent_coord(1) = -1.0d0
        case(2) ! 2番目の頂点 (1.0)
          parent_coord(1) = 1.0d0
        case default
          parent_coord(1) = 0.0d0 ! デフォルト値
      end select
    endif
  end subroutine monolis_shape_1d_line_1st_map_local_coord

end module mod_monolis_shape_1d_line_1st