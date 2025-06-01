module mod_monolis_shape_3d_tet_2nd
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
  use mod_monolis_def_shape
  use mod_monolis_shape_3d_tet_1st
  use mod_monolis_shape_2d_tri_1st
  use mod_monolis_shape_2d_tri_2nd
  use mod_monolis_utils_alloc
  implicit none

  private

  real(kdouble), parameter :: gsp(3,4) = reshape([ &
    0.25d0, 0.25d0, 0.25d0, &
    0.5d0, 0.1666666666666667d0, 0.1666666666666667d0, &
    0.1666666666666667d0, 0.5d0, 0.1666666666666667d0, &
    0.1666666666666667d0, 0.1666666666666667d0, 0.5d0 &
    ], [3,4])

  real(kdouble), parameter :: np(3,10) = reshape([ &
    0.0d0, 0.0d0, 0.0d0, &
    1.0d0, 0.0d0, 0.0d0, &
    0.0d0, 1.0d0, 0.0d0, &
    0.0d0, 0.0d0, 1.0d0, &
    0.5d0, 0.0d0, 0.0d0, &
    0.5d0, 0.5d0, 0.0d0, &
    0.0d0, 0.5d0, 0.0d0, &
    0.0d0, 0.0d0, 0.5d0, &
    0.5d0, 0.0d0, 0.5d0, &
    0.0d0, 0.5d0, 0.5d0 &
    ], [3,10])

  integer(kint), parameter :: monolis_shape_3d_tet_2nd_surf(6,4) = reshape([ &
     3, 2, 1, 6, 5, 7,&
     1, 2, 4, 5, 9, 8,&
     2, 3, 4, 6,10, 9,&
     3, 1, 4, 7, 8,10 ], [6,4])

  integer(kint), parameter :: monolis_shape_3d_tet_2nd_edge(3,6) = reshape([ &
    1, 5, 2, &
    2, 6, 3, &
    3, 7, 1, &
    1, 8, 4, &
    2, 9, 4, &
    3, 10, 4 &
    ], [3,6])

    public :: monolis_shape_3d_tet_2nd_num_gauss_point
    public :: monolis_shape_3d_tet_2nd_weight
    public :: monolis_shape_3d_tet_2nd_integral_point
    public :: monolis_shape_3d_tet_2nd_node_point
    public :: monolis_shape_3d_tet_2nd_is_inside_domain
    public :: monolis_shape_3d_tet_2nd_shapefunc
    public :: monolis_shape_3d_tet_2nd_shapefunc_deriv
    public :: monolis_shape_3d_tet_2nd_get_global_position
    public :: monolis_shape_3d_tet_2nd_get_global_deriv
    public :: monolis_shape_3d_tet_2nd_surf
    public :: monolis_shape_3d_tet_2nd_edge
    ! 標準インターフェース用の関数
    public :: monolis_shape_func_3d_tet_2nd
    public :: monolis_surf_data_func_3d_tet_2nd
    public :: monolis_surf_map_func_3d_tet_2nd

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

  subroutine monolis_shape_3d_tet_2nd_node_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(3)

    r(1) = np(1,i)
    r(2) = np(2,i)
    r(3) = np(3,i)
  end subroutine monolis_shape_3d_tet_2nd_node_point

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

  !> 標準インターフェースによる形状関数
  subroutine monolis_shape_func_3d_tet_2nd(local_coord, N)
    implicit none
    real(kdouble), intent(in) :: local_coord(:)
    real(kdouble), intent(out) :: N(:)
    
    call monolis_shape_3d_tet_2nd_shapefunc(local_coord, N)
  end subroutine monolis_shape_func_3d_tet_2nd

  subroutine monolis_surf_data_func_3d_tet_2nd(i_face, n_face_node, face_node_ids, &
    face_shape_func, face_domain_func, n_face_edge, edge_data_func, face_shape_map_func)
    use mod_monolis_utils_define_prm
    implicit none
    integer(kint), intent(in) :: i_face
    integer(kint), intent(out) :: n_face_node
    integer(kint), intent(out) :: n_face_edge
    integer(kint), intent(out), allocatable :: face_node_ids(:)
    procedure(monolis_shape_func), pointer :: face_shape_func
    procedure(monolis_domain_func), pointer :: face_domain_func
    procedure(monolis_edge_data_func), pointer :: edge_data_func
    procedure(monolis_shape_map_func), pointer :: face_shape_map_func

    if(i_face < 1 .or. 6 < i_face)then
      n_face_node = -1
      return
    endif

    n_face_node = 6
    call monolis_alloc_I_1d(face_node_ids, 6)
    face_node_ids(1) = monolis_shape_3d_tet_2nd_surf(1, i_face)
    face_node_ids(2) = monolis_shape_3d_tet_2nd_surf(2, i_face)
    face_node_ids(3) = monolis_shape_3d_tet_2nd_surf(3, i_face)
    face_node_ids(4) = monolis_shape_3d_tet_2nd_surf(4, i_face)
    face_node_ids(5) = monolis_shape_3d_tet_2nd_surf(5, i_face)
    face_node_ids(6) = monolis_shape_3d_tet_2nd_surf(6, i_face)

    n_face_edge = 3

    face_shape_func => monolis_shape_func_2d_tri_2nd
    face_domain_func => monolis_domain_func_2d_tri
    edge_data_func => monolis_edge_data_func_2d_tri_2nd
    face_shape_map_func => monolis_surf_map_func_3d_tet_2nd
  end subroutine monolis_surf_data_func_3d_tet_2nd

  !> 2D六面体1次要素の部分要素の局所座標を親要素の局所座標にマップする関数
  subroutine monolis_surf_map_func_3d_tet_2nd(i_surf, local_coord, local_coord_3d)
    use mod_monolis_utils_define_prm
    implicit none
    integer(kint), intent(in) :: i_surf
    real(kdouble), intent(in) :: local_coord(:)
    real(kdouble), intent(out) :: local_coord_3d(:)
    real(kdouble) :: u, v

    stop "monolis_surf_map_func_3d_tet_2nd"

    u = local_coord(1)
    v = local_coord(2)

    select case(i_surf)
      case(1) ! 底面 (z=0)
        local_coord_3d(1) = u
        local_coord_3d(2) = v
        local_coord_3d(3) = 0.0d0
        local_coord_3d(4) = 0.0d0
        local_coord_3d(5) = 0.0d0
        local_coord_3d(6) = 0.0d0
      case(2) ! 側面1 (y=0)
        local_coord_3d(1) = u
        local_coord_3d(2) = 0.0d0
        local_coord_3d(3) = v
        local_coord_3d(4) = 0.0d0
        local_coord_3d(5) = 0.0d0
        local_coord_3d(6) = 0.0d0
      case(3) ! 側面2 (x=1-y-z)
        local_coord_3d(1) = 1.0d0 - v
        local_coord_3d(2) = u
        local_coord_3d(3) = v
        local_coord_3d(4) = 0.0d0
        local_coord_3d(5) = 0.0d0
        local_coord_3d(6) = 0.0d0
      case(4) ! 側面3 (x=0)
        local_coord_3d(1) = 0.0d0
        local_coord_3d(2) = u
        local_coord_3d(3) = v
        local_coord_3d(4) = 0.0d0
        local_coord_3d(5) = 0.0d0
        local_coord_3d(6) = 0.0d0
    end select
  end subroutine monolis_surf_map_func_3d_tet_2nd
end module mod_monolis_shape_3d_tet_2nd
