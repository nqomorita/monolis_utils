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
    public :: monolis_local_node_position_1d_line_1st

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

  ! 標準インターフェースによる定義域判定関数
  subroutine monolis_local_node_position_1d_line_1st(i_node, local_positon)
    implicit none
    integer(kint), intent(in) :: i_node
    real(kdouble), intent(out) :: local_positon
    real(kdouble) :: r(1)
    
    call monolis_shape_1d_line_1st_node_point(i_node, r)
    local_positon = r(1)
  end subroutine monolis_local_node_position_1d_line_1st
end module mod_monolis_shape_1d_line_1st