module mod_monolis_shape_3d_tet_1st
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

  integer(kint), parameter :: monolis_shape_3d_tet_1st_surf(3,4) = reshape([ &
     3, 2, 1, &
     1, 2, 4, &
     2, 3, 4, &
     3, 1, 4  ], [3,4])

  integer(kint), parameter :: monolis_shape_3d_tet_1st_edge(2,6) = reshape([ &
     1, 2, &
     2, 3, &
     3, 1, &
     1, 4, &
     2, 4, &
     3, 4  ], [2,6])

    public :: monolis_shape_3d_tet_1st_num_gauss_point
    public :: monolis_shape_3d_tet_1st_weight
    public :: monolis_shape_3d_tet_1st_integral_point
    public :: monolis_shape_3d_tet_1st_node_point
    public :: monolis_shape_3d_tet_1st_shapefunc
    public :: monolis_shape_3d_tet_1st_shapefunc_deriv
    public :: monolis_shape_3d_tet_1st_surf
    public :: monolis_shape_3d_tet_1st_edge
    public :: monolis_shape_3d_tet_1st_get_global_position
    public :: monolis_shape_3d_tet_1st_get_global_deriv

contains

  function monolis_shape_3d_tet_1st_num_gauss_point()
    implicit none
    integer(kint) :: monolis_shape_3d_tet_1st_num_gauss_point
    monolis_shape_3d_tet_1st_num_gauss_point = 1
  end function monolis_shape_3d_tet_1st_num_gauss_point

  function monolis_shape_3d_tet_1st_weight(i)
    implicit none
    integer(kint), optional, intent(in) :: i
    real(kdouble) :: monolis_shape_3d_tet_1st_weight
    monolis_shape_3d_tet_1st_weight = 0.166666666666666d0
  end function monolis_shape_3d_tet_1st_weight

  subroutine monolis_shape_3d_tet_1st_integral_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(3)

    r(1) = gsp(1)
    r(2) = gsp(2)
    r(3) = gsp(3)
  end subroutine monolis_shape_3d_tet_1st_integral_point

  subroutine monolis_shape_3d_tet_1st_node_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(3)

    r(1) = np(1,i)
    r(2) = np(2,i)
    r(3) = np(3,i)
  end subroutine monolis_shape_3d_tet_1st_node_point

  subroutine monolis_shape_3d_tet_1st_shapefunc(local, func)
    implicit none
    real(kdouble), intent(in) :: local(3)
    real(kdouble), intent(out) :: func(4)

    func(1) = 1.0d0 - local(1) - local(2) - local(3)
    func(2) = local(1)
    func(3) = local(2)
    func(4) = local(3)
  end subroutine monolis_shape_3d_tet_1st_shapefunc

  subroutine monolis_shape_3d_tet_1st_shapefunc_deriv(local, func)
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
  end subroutine monolis_shape_3d_tet_1st_shapefunc_deriv

  subroutine monolis_shape_3d_tet_1st_get_global_position(node, r, pos)
    implicit none
    real(kdouble), intent(in) :: node(3,4)
    real(kdouble), intent(in) :: r(3)
    real(kdouble), intent(out) :: pos(3)
    real(kdouble) :: func(4)

    call monolis_shape_3d_tet_1st_shapefunc(r, func)
    pos = matmul(node, func)
  end subroutine monolis_shape_3d_tet_1st_get_global_position

  subroutine monolis_shape_3d_tet_1st_get_global_deriv(node, r, dndx, det)
    implicit none
    real(kdouble), intent(in) :: node(3,4)
    real(kdouble), intent(in) :: r(3)
    real(kdouble), intent(out) :: dndx(4,3)
    real(kdouble), intent(out) :: det
    real(kdouble) :: deriv(4,3), xj(3,3), inv(3,3)
    logical :: is_fail

    call monolis_shape_3d_tet_1st_shapefunc_deriv(r, deriv)
    xj = matmul(node, deriv)
    call monolis_get_inverse_matrix_R_3d(xj, inv, det, is_fail)
    dndx = matmul(deriv, inv)
  end subroutine monolis_shape_3d_tet_1st_get_global_deriv
end module mod_monolis_shape_3d_tet_1st
