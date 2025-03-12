module mod_monolis_shape_2d_tri_1st
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

  integer(kint), parameter :: monolis_shape_2d_tri_1st_edge(2,3) = reshape([ &
     1, 2, &
     2, 3, &
     3, 1  ], [2,3])

  !> [r_1, r_2, r_1 and r_2]
  real(kdouble), parameter :: monolis_shape_2d_tri_1st_edge_constraint_value(3,3) = reshape([ &
     0.0d0,-1.0d0, 0.0d0, &
     0.0d0, 0.0d0, 0.0d0, &
    -1.0d0, 0.0d0, 0.0d0  ], [3,3])

  !> [r_1, r_2, r_1 and r_2]
  logical, parameter :: monolis_shape_2d_tri_1st_edge_constraint_flag(3,3) = reshape([ &
     .false., .true. , .false., &
     .false., .false., .true. , &
     .true. , .false., .false.  ], [3,3])

    public :: monolis_shape_2d_tri_1st_num_gauss_point
    public :: monolis_shape_2d_tri_1st_weight
    public :: monolis_shape_2d_tri_1st_integral_point
    public :: monolis_shape_2d_tri_1st_node_point
    public :: monolis_shape_2d_tri_1st_is_inside_domain
    public :: monolis_shape_2d_tri_1st_shapefunc
    public :: monolis_shape_2d_tri_1st_shapefunc_deriv
    public :: monolis_shape_2d_tri_1st_get_global_position
    public :: monolis_shape_2d_tri_1st_get_global_deriv
    public :: monolis_shape_2d_tri_1st_edge
    public :: monolis_shape_2d_tri_1st_edge_constraint_value
    public :: monolis_shape_2d_tri_1st_edge_constraint_flag

contains

  function monolis_shape_2d_tri_1st_num_gauss_point()
    implicit none
    integer(kint) :: monolis_shape_2d_tri_1st_num_gauss_point
    monolis_shape_2d_tri_1st_num_gauss_point = 3
  end function monolis_shape_2d_tri_1st_num_gauss_point

  function monolis_shape_2d_tri_1st_weight(i)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble) :: monolis_shape_2d_tri_1st_weight
    monolis_shape_2d_tri_1st_weight = weight(i)
  end function monolis_shape_2d_tri_1st_weight

  subroutine monolis_shape_2d_tri_1st_integral_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(2)

    r(1) = gsp(1,i)
    r(2) = gsp(2,i)
  end subroutine monolis_shape_2d_tri_1st_integral_point

  subroutine monolis_shape_2d_tri_1st_node_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(2)

    r(1) = np(1,i)
    r(2) = np(2,i)
  end subroutine monolis_shape_2d_tri_1st_node_point

  subroutine monolis_shape_2d_tri_1st_is_inside_domain(local, is_inside)
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
  end subroutine monolis_shape_2d_tri_1st_is_inside_domain

  subroutine monolis_shape_2d_tri_1st_shapefunc(local, func)
    implicit none
    real(kdouble), intent(in) :: local(2)
    real(kdouble), intent(out) :: func(3)

    func(1) = 1.0d0 - local(1) - local(2)
    func(2) = local(1)
    func(3) = local(2)
  end subroutine monolis_shape_2d_tri_1st_shapefunc

  subroutine monolis_shape_2d_tri_1st_shapefunc_deriv(local, func)
    implicit none
    real(kdouble), intent(in) :: local(2)
    real(kdouble), intent(out) :: func(3,2)

    func(1,1) = -1.0d0
    func(2,1) =  1.0d0
    func(3,1) =  0.0d0

    func(1,2) = -1.0d0
    func(2,2) =  0.0d0
    func(3,2) =  1.0d0
  end subroutine monolis_shape_2d_tri_1st_shapefunc_deriv

  subroutine monolis_shape_2d_tri_1st_get_global_position(node, r, pos)
    implicit none
    real(kdouble), intent(in) :: node(2,3), r(2)
    real(kdouble), intent(out) :: pos(2)
    real(kdouble) :: func(3)

    call monolis_shape_2d_tri_1st_shapefunc(r, func)
    pos = matmul(node, func)
  end subroutine monolis_shape_2d_tri_1st_get_global_position

  subroutine monolis_shape_2d_tri_1st_get_global_deriv(node, r, dndx, det)
    implicit none
    real(kdouble), intent(in) :: node(2,3)
    real(kdouble), intent(in) :: r(2)
    real(kdouble), intent(out) :: dndx(3,2)
    real(kdouble), intent(out) :: det
    real(kdouble) :: deriv(3,2), xj(2,2), inv(2,2)

    call monolis_shape_2d_tri_1st_shapefunc_deriv(r, deriv)
    xj = matmul(node, deriv)
    call monolis_get_inverse_matrix_2d(xj, inv, det)
    dndx = matmul(deriv, inv)
  end subroutine monolis_shape_2d_tri_1st_get_global_deriv

end module mod_monolis_shape_2d_tri_1st
