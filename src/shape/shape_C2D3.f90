module mod_monolis_shape_c2d3
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
  implicit none

  private

  real(kdouble), parameter :: gsp(2,3) = reshape([ &
    0.166666666666667d0, 0.166666666666667d0, &
    0.666666666666667d0, 0.166666666666667d0, &
    0.166666666666667d0, 0.666666666666667d0  &
    ], [2,3])

  real(kdouble), parameter :: np(2,3) = reshape([ &
     0.0d0, 0.0d0, &
     1.0d0, 0.0d0, &
     0.0d0, 1.0d0  &
    ], [2,3])

  real(kdouble), parameter :: weight(3) = [ &
     0.166666666666666d0,0.166666666666666d0,0.166666666666666d0 &
    ]

    public :: monolis_C2D3_num_gauss_point
    public :: monolis_C2D3_weight
    public :: monolis_C2D3_integral_point
    public :: monolis_C2D3_node_point
    public :: monolis_C2D3_shapefunc
    public :: monolis_C2D3_shapefunc_deriv
    !public :: monolis_C2D3_surf
    !public :: monolis_C2D3_get_global_position
    public :: monolis_C2D3_get_global_deriv

contains

  function monolis_C2D3_num_gauss_point()
    implicit none
    integer(kint) :: monolis_C2D3_num_gauss_point
    monolis_C2D3_num_gauss_point = 3
  end function monolis_C2D3_num_gauss_point

  function monolis_C2D3_weight(i)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble) :: monolis_C2D3_weight
    monolis_C2D3_weight = weight(i)
  end function monolis_C2D3_weight

  subroutine monolis_C2D3_integral_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(2)

    r(1) = gsp(1,i)
    r(2) = gsp(2,i)
  end subroutine monolis_C2D3_integral_point

  subroutine monolis_C2D3_node_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(2)

    r(1) = np(1,i)
    r(2) = np(2,i)
  end subroutine monolis_C2D3_node_point

  subroutine monolis_C2D3_shapefunc(local, func)
    implicit none
    real(kdouble), intent(in) :: local(2)
    real(kdouble), intent(out) :: func(3)

    func(1) = 1.0d0 - local(1) - local(2)
    func(2) = local(1)
    func(3) = local(2)
  end subroutine monolis_C2D3_shapefunc

  subroutine monolis_C2D3_shapefunc_deriv(local, func)
    implicit none
    real(kdouble), intent(in) :: local(2)
    real(kdouble), intent(out) :: func(3,2)

    func(1,1) = -1.0d0
    func(2,1) =  1.0d0
    func(3,1) =  0.0d0

    func(1,2) = -1.0d0
    func(2,2) =  0.0d0
    func(3,2) =  1.0d0
  end subroutine monolis_C2D3_shapefunc_deriv

  subroutine monolis_C2D3_get_global_deriv(node, r, dndx, det)
    implicit none
    real(kdouble), intent(in) :: node(2,3)
    real(kdouble), intent(in) :: r(2)
    real(kdouble), intent(out) :: dndx(3,2)
    real(kdouble), intent(out) :: det
    real(kdouble) :: deriv(3,2), xj(2,2), inv(2,2)

    call monolis_C2D3_shapefunc_deriv(r, deriv)
    xj = matmul(node, deriv)
    call monolis_get_inverse_matrix_2d(xj, inv, det)
    dndx = matmul(deriv, inv)
  end subroutine monolis_C2D3_get_global_deriv

end module mod_monolis_shape_c2d3
