module mod_monolis_shape_c2d6
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
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

    public :: monolis_C2D6_num_gauss_point
    public :: monolis_C2D6_weight
    public :: monolis_C2D6_integral_point
    !public :: monolis_C2D6_node_point
    public :: monolis_C2D6_shapefunc
    public :: monolis_C2D6_shapefunc_deriv
    public :: monolis_C2D6_shapefunc_2nd_deriv
    !public :: monolis_C2D6_edge
    !public :: monolis_C2D6_get_global_position
    !public :: monolis_C2D6_get_global_deriv

contains

  function monolis_C2D6_num_gauss_point()
    implicit none
    integer(kint) :: monolis_C2D6_num_gauss_point
    monolis_C2D6_num_gauss_point = 4
  end function monolis_C2D6_num_gauss_point

  function monolis_C2D6_weight(i)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble) :: monolis_C2D6_weight
    monolis_C2D6_weight = weight(i)
  end function monolis_C2D6_weight

  subroutine monolis_C2D6_integral_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(2)

    r(1) = gsp(1,i)
    r(2) = gsp(2,i)
  end subroutine monolis_C2D6_integral_point

  !subroutine monolis_C2D6_node_point(i, r)
  !  implicit none
  !  integer(kint), intent(in) :: i
  !  real(kdouble), intent(out) :: r(2)

  !  r(1) = np(1,i)
  !  r(2) = np(2,i)
  !end subroutine monolis_C2D6_node_point

  subroutine monolis_C2D6_shapefunc(local, func)
    implicit none
    real(kdouble), intent(in) :: local(2)
    real(kdouble), intent(out) :: func(6)
    real(kdouble) :: xi, et, st

    xi = local(1)
    et = local(2)
    st = 1.0d0 - local(1) - local(2)

    func(1) = st*(2.0d0*st - 1.0d0)
    func(2) = xi*(2.0d0*xi - 1.0d0)
    func(3) = et*(2.0d0*et - 1.0d0)
    func(4) = 4.0d0*xi*st
    func(5) = 4.0d0*xi*et
    func(6) = 4.0d0*et*st
  end subroutine monolis_C2D6_shapefunc

  subroutine monolis_C2D6_shapefunc_deriv(local, func)
    implicit none
    real(kdouble), intent(in) :: local(2)
    real(kdouble), intent(out) :: func(6,2)
    real(kdouble) :: xi, et, st

    xi = local(1)
    et = local(2)
    st = 1.0d0 - local(1) - local(2)

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
  end subroutine monolis_C2D6_shapefunc_deriv

  subroutine monolis_C2D6_shapefunc_2nd_deriv(func)
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
  end subroutine monolis_C2D6_shapefunc_2nd_deriv
end module mod_monolis_shape_c2d6
