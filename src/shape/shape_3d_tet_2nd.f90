module mod_monolis_shape_3d_tet_2nd
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
  use mod_monolis_shape_3d_tet_1st
  implicit none

  private

  real(kdouble), parameter :: gsp(3,4) = reshape([ &
     0.138196601125011d0, 0.138196601125011d0, 0.138196601125011d0, &
     0.585410196624968d0, 0.138196601125011d0, 0.138196601125011d0, &
     0.138196601125011d0, 0.585410196624968d0, 0.138196601125011d0, &
     0.138196601125011d0, 0.138196601125011d0, 0.585410196624968d0  &
    ], [3,4])

  !real(kdouble), parameter :: np(3,4) = reshape([ &
  !   -1.0d0, -1.0d0,-1.0d0, &
  !    1.0d0, -1.0d0,-1.0d0, &
  !    1.0d0,  1.0d0, 1.0d0, &
  !   -1.0d0,  1.0d0, 1.0d0  &
  !  ], [3,4])

  integer(kint), parameter :: monolis_shape_3d_tet_2nd_surf(6,4) = reshape([ &
     3, 2, 1, 6, 5, 7,&
     1, 2, 4, 5, 9, 8,&
     2, 3, 4, 6,10, 9,&
     3, 1, 4, 7, 8,10 ], [6,4])

  !> [r_1, r_2, r_3, r_1 and r_2, r_2 and r_3, r_1 and r_3, r_1 and r_2 and r_3]
  real(kdouble), parameter :: monolis_shape_3d_tet_2nd_surf_constraint_value(7,4) = reshape([ &
     0.0d0, 0.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     0.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 1.0d0, &
    -1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0  ], [7,4])

  !> [r_1, r_2, r_1 and r_2]
  logical, parameter :: monolis_shape_3d_tet_2nd_surf_constraint_flag(7,4) = reshape([ &
     .false., .false., .true. , .false., .false., .false., .false., &
     .false., .true. , .false., .false., .false., .false., .false., &
     .false., .false., .false., .false., .false., .false., .true., &
     .true. , .false., .false., .false., .false., .false., .false.  ], [7,4])

  integer(kint), parameter :: monolis_shape_3d_tet_2nd_edge(3,6) = reshape([ &
     1, 5, 2, &
     2, 6, 3, &
     3, 7, 1, &
     1, 8, 4, &
     2, 9, 4, &
     3,10, 4  ], [3,6])

  !> [r_1, r_2, r_3, r_1 and r_2, r_2 and r_3, r_1 and r_3, r_1 and r_2 and r_3]
  real(kdouble), parameter :: monolis_shape_3d_tet_2nd_edge_constraint_value(7,6) = reshape([ &
     0.0d0,-1.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     0.0d0, 0.0d0,-1.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, &
    -1.0d0, 0.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
    -1.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     0.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 1.0d0, 0.0d0, &
    -1.0d0, 0.0d0, 0.0d0, 0.0d0, 1.0d0, 0.0d0, 0.0d0  ], [7,6])

  !> [r_1, r_2, r_1 and r_2]
  logical, parameter :: monolis_shape_3d_tet_2nd_edge_constraint_flag(7,6) = reshape([ &
     .false., .true. , .true. , .false., .false., .false., .false., &
     .false., .false., .true. , .true. , .false., .false., .false., &
     .true. , .false., .true. , .false., .false., .false., .false., &
     .true. , .true. , .false., .false., .false., .false., .false., &
     .false., .true. , .false., .false., .false., .true. , .false., &
     .true. , .false., .false., .false., .true. , .false., .false.  ], [7,6])

    public :: monolis_shape_3d_tet_2nd_num_gauss_point
    public :: monolis_shape_3d_tet_2nd_weight
    public :: monolis_shape_3d_tet_2nd_integral_point
    !public :: monolis_shape_3d_tet_2nd_node_point
    public :: monolis_shape_3d_tet_2nd_is_inside_domain
    public :: monolis_shape_3d_tet_2nd_shapefunc
    public :: monolis_shape_3d_tet_2nd_shapefunc_deriv
    public :: monolis_shape_3d_tet_2nd_get_global_position
    public :: monolis_shape_3d_tet_2nd_get_global_deriv
    public :: monolis_shape_3d_tet_2nd_surf
    public :: monolis_shape_3d_tet_2nd_surf_constraint_value
    public :: monolis_shape_3d_tet_2nd_surf_constraint_flag
    public :: monolis_shape_3d_tet_2nd_edge
    public :: monolis_shape_3d_tet_2nd_edge_constraint_value
    public :: monolis_shape_3d_tet_2nd_edge_constraint_flag

contains

  function monolis_shape_3d_tet_2nd_num_gauss_point()
    implicit none
    integer(kint) :: monolis_shape_3d_tet_2nd_num_gauss_point
    monolis_shape_3d_tet_2nd_num_gauss_point = 4
  end function monolis_shape_3d_tet_2nd_num_gauss_point

  function monolis_shape_3d_tet_2nd_weight(i)
    implicit none
    integer(kint), optional, intent(in) :: i
    real(kdouble) :: monolis_shape_3d_tet_2nd_weight
    monolis_shape_3d_tet_2nd_weight = 0.041666666666667d0
  end function monolis_shape_3d_tet_2nd_weight

  subroutine monolis_shape_3d_tet_2nd_integral_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(3)

    r(1) = gsp(1,i)
    r(2) = gsp(2,i)
    r(3) = gsp(3,i)
  end subroutine monolis_shape_3d_tet_2nd_integral_point

  !subroutine monolis_shape_3d_tet_2nd_node_point(i, r)
  !  implicit none
  !  integer(kint), intent(in) :: i
  !  real(kdouble), intent(out) :: r(3)

  !  r(1) = np(1,i)
  !  r(2) = np(2,i)
  !  r(3) = np(3,i)
  !end subroutine monolis_shape_3d_tet_2nd_node_point

  subroutine monolis_shape_3d_tet_2nd_is_inside_domain(local, is_inside)
    implicit none
    real(kdouble), intent(in) :: local(3)
    logical, intent(out) :: is_inside
    real(kdouble) :: func(4)

    is_inside = .false.
    call monolis_shape_3d_tet_1st_shapefunc(local, func)
    if(0.0d0 <= func(1) .and. func(1) <= 1.0d0 .and. &
       0.0d0 <= func(2) .and. func(2) <= 1.0d0 .and. &
       0.0d0 <= func(3) .and. func(3) <= 1.0d0 .and. &
       0.0d0 <= func(4) .and. func(4) <= 1.0d0)then 
      is_inside = .true.
    endif
  end subroutine monolis_shape_3d_tet_2nd_is_inside_domain

  subroutine monolis_shape_3d_tet_2nd_shapefunc(local, func)
    implicit none
    real(kdouble), intent(in) :: local(3)
    real(kdouble), intent(out) :: func(10)
    real(kdouble) :: xi, et, st, a

    xi = local(1)
    et = local(2)
    st = local(3)
    a = 1.0d0 - xi - et - st

    func(1) = (2.0d0*a - 1.0d0)*a
    func(2) = xi*(2.0d0*xi - 1.0d0)
    func(3) = et*(2.0d0*et - 1.0d0)
    func(4) = st*(2.0d0*st - 1.0d0)
    func(5) = 4.0d0*xi*a
    func(6) = 4.0d0*xi*et
    func(7) = 4.0d0*et*a
    func(8) = 4.0d0*st*a
    func(9) = 4.0d0*xi*st
    func(10)= 4.0d0*et*st
  end subroutine monolis_shape_3d_tet_2nd_shapefunc

  subroutine monolis_shape_3d_tet_2nd_shapefunc_deriv(local, func)
    implicit none
    real(kdouble), intent(in) :: local(3)
    real(kdouble), intent(out) :: func(10,3)
    real(kdouble) :: xi, et, st, a

    xi = local(1)
    et = local(2)
    st = local(3)
    a = 1.0d0 - xi - et - st

    func(1,1) = 1.0d0 - 4.0d0*a
    func(2,1) = 4.0d0*xi - 1.0d0
    func(3,1) = 0.0d0
    func(4,1) = 0.0d0
    func(5,1) = 4.0d0*(1.0d0 - 2.0d0*xi - et - st)
    func(6,1) = 4.0d0*et
    func(7,1) =-4.0d0*et
    func(8,1) =-4.0d0*st
    func(9,1) = 4.0d0*st
    func(10,1)= 0.0d0

    func(1,2) = 1.0d0 - 4.0d0*a
    func(2,2) = 0.0d0
    func(3,2) = 4.0d0*et - 1.0d0
    func(4,2) = 0.0d0
    func(5,2) =-4.0d0*xi
    func(6,2) = 4.0d0*xi
    func(7,2) = 4.0d0*(1.0d0 - xi - 2.0d0*et - st)
    func(8,2) =-4.0d0*st
    func(9,2) = 0.0d0
    func(10,2)= 4.0d0*st

    func(1,3) = 1.0d0 - 4.0d0*a
    func(2,3) = 0.0d0
    func(3,3) = 0.0d0
    func(4,3) = 4.0d0*st - 1.0d0
    func(5,3) =-4.0d0*xi
    func(6,3) = 0.0d0
    func(7,3) =-4.0d0*et
    func(8,3) = 4.0d0*(1.0d0 - xi - et - 2.0d0*st)
    func(9,3) = 4.0d0*xi
    func(10,3)= 4.0d0*et
  end subroutine monolis_shape_3d_tet_2nd_shapefunc_deriv

  subroutine monolis_shape_3d_tet_2nd_get_global_position(node, r, pos)
    implicit none
    real(kdouble), intent(in) :: node(3,10)
    real(kdouble), intent(in) :: r(3)
    real(kdouble), intent(out) :: pos(3)
    real(kdouble) :: func(10)

    call monolis_shape_3d_tet_2nd_shapefunc(r, func)
    pos = matmul(node, func)
  end subroutine monolis_shape_3d_tet_2nd_get_global_position

  subroutine monolis_shape_3d_tet_2nd_get_global_deriv(node, r, dndx, det)
    implicit none
    real(kdouble), intent(in) :: node(3,10)
    real(kdouble), intent(in) :: r(3)
    real(kdouble), intent(out) :: dndx(10,3)
    real(kdouble), intent(out) :: det
    real(kdouble) :: deriv(10,3), xj(3,3), inv(3,3)

    call monolis_shape_3d_tet_2nd_shapefunc_deriv(r, deriv)
    xj = matmul(node, deriv)
    call monolis_get_inverse_matrix_R_3d(xj, inv, det)
    dndx = matmul(deriv, inv)
  end subroutine monolis_shape_3d_tet_2nd_get_global_deriv
end module mod_monolis_shape_3d_tet_2nd
