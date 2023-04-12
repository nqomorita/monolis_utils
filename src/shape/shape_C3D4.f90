module mod_monolis_shape_c3d4
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
  implicit none

  private

  real(kdouble), parameter :: gsp(3) = [ &
     0.25d0, 0.25d0, 0.25d0  &
    ]

  real(kdouble), parameter :: np(3,4) = reshape([ &
      0.0d0, 0.0d0, 0.0d0, &
      1.0d0, 0.0d0, 0.0d0, &
      0.0d0, 1.0d0, 0.0d0, &
      0.0d0, 0.0d0, 1.0d0  &
    ], [3,4])

  integer(kint), parameter :: monolis_C3D4_surf(3,4) = reshape([ &
     3, 2, 1, &
     1, 2, 4, &
     2, 3, 4, &
     3, 1, 4  ], [3,4])

  integer(kint), parameter :: monolis_C3D4_edge(2,6) = reshape([ &
     1, 2, &
     2, 3, &
     3, 1, &
     1, 4, &
     2, 4, &
     3, 4  ], [2,6])

    public :: monolis_C3D4_num_gauss_point
    public :: monolis_C3D4_weight
    public :: monolis_C3D4_integral_point
    public :: monolis_C3D4_node_point
    public :: monolis_C3D4_shapefunc
    public :: monolis_C3D4_shapefunc_deriv
    public :: monolis_C3D4_surf
    public :: monolis_C3D4_edge
    !public :: monolis_C3D4_get_global_position
    !public :: monolis_C3D4_get_global_deriv

contains

  function monolis_C3D4_num_gauss_point()
    implicit none
    integer(kint) :: monolis_C3D4_num_gauss_point
    monolis_C3D4_num_gauss_point = 1
  end function monolis_C3D4_num_gauss_point

  function monolis_C3D4_weight(i)
    implicit none
    integer(kint), optional, intent(in) :: i
    real(kdouble) :: monolis_C3D4_weight
    monolis_C3D4_weight = 0.166666666666666d0
  end function monolis_C3D4_weight

  subroutine monolis_C3D4_integral_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(3)

    r(1) = gsp(1)
    r(2) = gsp(2)
    r(3) = gsp(3)
  end subroutine monolis_C3D4_integral_point

  subroutine monolis_C3D4_node_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(3)

    r(1) = np(1,i)
    r(2) = np(2,i)
    r(3) = np(3,i)
  end subroutine monolis_C3D4_node_point

  subroutine monolis_C3D4_shapefunc(local, func)
    implicit none
    real(kdouble), intent(in) :: local(3)
    real(kdouble), intent(out) :: func(4)

    func(1) = 1.0d0 - local(1) - local(2) - local(3)
    func(2) = local(1)
    func(3) = local(2)
    func(4) = local(3)
  end subroutine monolis_C3D4_shapefunc

  subroutine monolis_C3D4_shapefunc_deriv(local, func)
    implicit none
    real(kdouble), intent(in) :: local(3)
    real(kdouble), intent(out) :: func(4,3)

    func(1,1) = -1.d0
    func(2,1) = 1.d0
    func(3,1) = 0.d0
    func(4,1) = 0.d0

    func(1,2) = -1.d0
    func(2,2) = 0.d0
    func(3,2) = 1.d0
    func(4,2) = 0.d0

    func(1,3) = -1.d0
    func(2,3) = 0.d0
    func(3,3) = 0.d0
    func(4,3) = 1.d0
  end subroutine monolis_C3D4_shapefunc_deriv
end module mod_monolis_shape_c3d4
