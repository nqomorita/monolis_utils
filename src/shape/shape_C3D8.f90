module mod_monolis_shape_c3d8
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
  implicit none

  private

  real(kdouble), parameter :: gsp(3,8) = reshape([ &
    -0.577350269189626d0,-0.577350269189626d0,-0.577350269189626d0, &
     0.577350269189626d0,-0.577350269189626d0,-0.577350269189626d0, &
    -0.577350269189626d0, 0.577350269189626d0,-0.577350269189626d0, &
     0.577350269189626d0, 0.577350269189626d0,-0.577350269189626d0, &
    -0.577350269189626d0,-0.577350269189626d0, 0.577350269189626d0, &
     0.577350269189626d0,-0.577350269189626d0, 0.577350269189626d0, &
    -0.577350269189626d0, 0.577350269189626d0, 0.577350269189626d0, &
     0.577350269189626d0, 0.577350269189626d0, 0.577350269189626d0  &
    ], [3,8])

  real(kdouble), parameter :: np(3,8) = reshape([ &
     -1.0d0, -1.0d0,-1.0d0, &
      1.0d0, -1.0d0,-1.0d0, &
      1.0d0,  1.0d0,-1.0d0, &
     -1.0d0,  1.0d0,-1.0d0, &
     -1.0d0, -1.0d0, 1.0d0, &
      1.0d0, -1.0d0, 1.0d0, &
      1.0d0,  1.0d0, 1.0d0, &
     -1.0d0,  1.0d0, 1.0d0  &
    ], [3,8])

  integer(kint), parameter :: monolis_C3D8_surf(4,6) = reshape([ &
     4, 3, 2, 1, &
     5, 6, 7, 8, &
     1, 2, 6, 5, &
     2, 3, 7, 6, &
     3, 4, 8, 7, &
     4, 1, 5, 8  ], [4,6])

  integer(kint), parameter :: monolis_C3D8_edge(2,12) = reshape([ &
     1, 2, &
     2, 3, &
     3, 4, &
     4, 1, &
     5, 6, &
     6, 7, &
     7, 8, &
     8, 5, &
     1, 5, &
     2, 6, &
     3, 7, &
     4, 8  ], [2,12])

    public :: monolis_C3D8_num_gauss_point
    public :: monolis_C3D8_weight
    public :: monolis_C3D8_integral_point
    public :: monolis_C3D8_node_point
    public :: monolis_C3D8_shapefunc
    public :: monolis_C3D8_shapefunc_deriv
    public :: monolis_C3D8_surf
    public :: monolis_C3D8_edge
    public :: monolis_C3D8_get_global_position
    public :: monolis_C3D8_get_global_deriv

contains

  function monolis_C3D8_num_gauss_point()
    implicit none
    integer(kint) :: monolis_C3D8_num_gauss_point
    monolis_C3D8_num_gauss_point = 8
  end function monolis_C3D8_num_gauss_point

  function monolis_C3D8_weight(i)
    implicit none
    integer(kint), optional, intent(in) :: i
    real(kdouble) :: monolis_C3D8_weight
    monolis_C3D8_weight = 1.0d0
  end function monolis_C3D8_weight

  subroutine monolis_C3D8_integral_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(3)

    r(1) = gsp(1,i)
    r(2) = gsp(2,i)
    r(3) = gsp(3,i)
  end subroutine monolis_C3D8_integral_point

  subroutine monolis_C3D8_node_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(3)

    r(1) = np(1,i)
    r(2) = np(2,i)
    r(3) = np(3,i)
  end subroutine monolis_C3D8_node_point

  subroutine monolis_C3D8_get_global_position(node, r, pos)
    implicit none
    real(kdouble), intent(in) :: node(3,8)
    real(kdouble), intent(in) :: r(3)
    real(kdouble), intent(out) :: pos(3)
    real(kdouble) :: func(8)

    call monolis_C3D8_shapefunc(r, func)
    pos = matmul(node, func)
  end subroutine monolis_C3D8_get_global_position

  subroutine monolis_C3D8_get_global_deriv(node, r, dndx, det)
    implicit none
    real(kdouble), intent(in) :: node(3,8)
    real(kdouble), intent(in) :: r(3)
    real(kdouble), intent(out) :: dndx(8,3)
    real(kdouble), intent(out) :: det
    real(kdouble) :: deriv(8,3), xj(3,3), inv(3,3)
    logical :: is_fail

    call monolis_C3D8_shapefunc_deriv(r, deriv)
    xj = matmul(node, deriv)
    call monolis_get_inverse_matrix_R_3d(xj, inv, det, is_fail)
    dndx = matmul(deriv, inv)
  end subroutine monolis_C3D8_get_global_deriv

  subroutine monolis_C3D8_shapefunc(local, func)
    implicit none
    real(kdouble), intent(in) :: local(3)
    real(kdouble), intent(out) :: func(8)

    func(1) = 0.125d0*(1.0d0-local(1))*(1.0d0-local(2))*(1.0d0-local(3))
    func(2) = 0.125d0*(1.0d0+local(1))*(1.0d0-local(2))*(1.0d0-local(3))
    func(3) = 0.125d0*(1.0d0+local(1))*(1.0d0+local(2))*(1.0d0-local(3))
    func(4) = 0.125d0*(1.0d0-local(1))*(1.0d0+local(2))*(1.0d0-local(3))
    func(5) = 0.125d0*(1.0d0-local(1))*(1.0d0-local(2))*(1.0d0+local(3))
    func(6) = 0.125d0*(1.0d0+local(1))*(1.0d0-local(2))*(1.0d0+local(3))
    func(7) = 0.125d0*(1.0d0+local(1))*(1.0d0+local(2))*(1.0d0+local(3))
    func(8) = 0.125d0*(1.0d0-local(1))*(1.0d0+local(2))*(1.0d0+local(3))
  end subroutine monolis_C3D8_shapefunc

  subroutine monolis_C3D8_shapefunc_deriv(local, func)
    implicit none
    real(kdouble), intent(in) :: local(3)
    real(kdouble), intent(out) :: func(8,3)

    func(1,1) = -0.125d0*(1.0d0-local(2))*(1.0d0-local(3))
    func(2,1) =  0.125d0*(1.0d0-local(2))*(1.0d0-local(3))
    func(3,1) =  0.125d0*(1.0d0+local(2))*(1.0d0-local(3))
    func(4,1) = -0.125d0*(1.0d0+local(2))*(1.0d0-local(3))
    func(5,1) = -0.125d0*(1.0d0-local(2))*(1.0d0+local(3))
    func(6,1) =  0.125d0*(1.0d0-local(2))*(1.0d0+local(3))
    func(7,1) =  0.125d0*(1.0d0+local(2))*(1.0d0+local(3))
    func(8,1) = -0.125d0*(1.0d0+local(2))*(1.0d0+local(3))

    func(1,2) = -0.125d0*(1.0d0-local(1))*(1.0d0-local(3))
    func(2,2) = -0.125d0*(1.0d0+local(1))*(1.0d0-local(3))
    func(3,2) =  0.125d0*(1.0d0+local(1))*(1.0d0-local(3))
    func(4,2) =  0.125d0*(1.0d0-local(1))*(1.0d0-local(3))
    func(5,2) = -0.125d0*(1.0d0-local(1))*(1.0d0+local(3))
    func(6,2) = -0.125d0*(1.0d0+local(1))*(1.0d0+local(3))
    func(7,2) =  0.125d0*(1.0d0+local(1))*(1.0d0+local(3))
    func(8,2) =  0.125d0*(1.0d0-local(1))*(1.0d0+local(3))

    func(1,3) = -0.125d0*(1.0d0-local(1))*(1.0d0-local(2))
    func(2,3) = -0.125d0*(1.0d0+local(1))*(1.0d0-local(2))
    func(3,3) = -0.125d0*(1.0d0+local(1))*(1.0d0+local(2))
    func(4,3) = -0.125d0*(1.0d0-local(1))*(1.0d0+local(2))
    func(5,3) =  0.125d0*(1.0d0-local(1))*(1.0d0-local(2))
    func(6,3) =  0.125d0*(1.0d0+local(1))*(1.0d0-local(2))
    func(7,3) =  0.125d0*(1.0d0+local(1))*(1.0d0+local(2))
    func(8,3) =  0.125d0*(1.0d0-local(1))*(1.0d0+local(2))
  end subroutine monolis_C3D8_shapefunc_deriv
end module mod_monolis_shape_c3d8
