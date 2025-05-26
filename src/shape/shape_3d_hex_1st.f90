module mod_monolis_shape_3d_hex_1st
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
  use mod_monolis_def_shape
  use mod_monolis_shape_2d_quad_1st
  use mod_monolis_utils_alloc
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

  integer(kint), parameter :: monolis_shape_3d_hex_1st_surf(4,6) = reshape([ &
     4, 3, 2, 1, &
     5, 6, 7, 8, &
     1, 2, 6, 5, &
     2, 3, 7, 6, &
     3, 4, 8, 7, &
     4, 1, 5, 8  ], [4,6])

  integer(kint), parameter :: monolis_shape_3d_hex_1st_edge(2,12) = reshape([ &
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

    public :: monolis_shape_3d_hex_1st_num_gauss_point
    public :: monolis_shape_3d_hex_1st_weight
    public :: monolis_shape_3d_hex_1st_integral_point
    public :: monolis_shape_3d_hex_1st_node_point
    public :: monolis_shape_3d_hex_1st_is_inside_domain
    public :: monolis_shape_3d_hex_1st_shapefunc
    public :: monolis_shape_3d_hex_1st_shapefunc_deriv
    public :: monolis_shape_3d_hex_1st_get_global_position
    public :: monolis_shape_3d_hex_1st_get_global_deriv
    public :: monolis_shape_3d_hex_1st_surf
    public :: monolis_shape_3d_hex_1st_edge
    ! 標準インターフェース用の関数
    public :: monolis_shape_func_3d_hex_1st
    public :: monolis_domain_func_3d_hex
    public :: monolis_surf_data_func_3d_hex_1st
    public :: monolis_surf_map_func_3d_hex_1st

contains

  function monolis_shape_3d_hex_1st_num_gauss_point()
    implicit none
    integer(kint) :: monolis_shape_3d_hex_1st_num_gauss_point
    monolis_shape_3d_hex_1st_num_gauss_point = 8
  end function monolis_shape_3d_hex_1st_num_gauss_point

  function monolis_shape_3d_hex_1st_weight(i)
    implicit none
    integer(kint), optional, intent(in) :: i
    real(kdouble) :: monolis_shape_3d_hex_1st_weight
    monolis_shape_3d_hex_1st_weight = 1.0d0
  end function monolis_shape_3d_hex_1st_weight

  subroutine monolis_shape_3d_hex_1st_integral_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(3)

    r(1) = gsp(1,i)
    r(2) = gsp(2,i)
    r(3) = gsp(3,i)
  end subroutine monolis_shape_3d_hex_1st_integral_point

  subroutine monolis_shape_3d_hex_1st_node_point(i, r)
    implicit none
    integer(kint), intent(in) :: i
    real(kdouble), intent(out) :: r(3)

    r(1) = np(1,i)
    r(2) = np(2,i)
    r(3) = np(3,i)
  end subroutine monolis_shape_3d_hex_1st_node_point

  subroutine monolis_shape_3d_hex_1st_is_inside_domain(local, is_inside)
    implicit none
    real(kdouble), intent(in) :: local(3)
    logical, intent(out) :: is_inside

    is_inside = .false.
    if(0.0d0 <= local(1) .and. local(1) <= 1.0d0 .and. &
       0.0d0 <= local(2) .and. local(2) <= 1.0d0 .and. &
       0.0d0 <= local(3) .and. local(3) <= 1.0d0)then 
      is_inside = .true.
    endif
  end subroutine monolis_shape_3d_hex_1st_is_inside_domain

  subroutine monolis_shape_3d_hex_1st_shapefunc(local, func)
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
  end subroutine monolis_shape_3d_hex_1st_shapefunc

  subroutine monolis_shape_3d_hex_1st_shapefunc_deriv(local, func)
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
  end subroutine monolis_shape_3d_hex_1st_shapefunc_deriv

  subroutine monolis_shape_3d_hex_1st_get_global_position(node, r, pos)
    implicit none
    real(kdouble), intent(in) :: node(3,8)
    real(kdouble), intent(in) :: r(3)
    real(kdouble), intent(out) :: pos(3)
    real(kdouble) :: func(8)

    call monolis_shape_3d_hex_1st_shapefunc(r, func)
    pos = matmul(node, func)
  end subroutine monolis_shape_3d_hex_1st_get_global_position

  subroutine monolis_shape_3d_hex_1st_get_global_deriv(node, r, dndx, det)
    implicit none
    real(kdouble), intent(in) :: node(3,8)
    real(kdouble), intent(in) :: r(3)
    real(kdouble), intent(out) :: dndx(8,3)
    real(kdouble), intent(out) :: det
    real(kdouble) :: deriv(8,3), xj(3,3), inv(3,3)
    logical :: is_fail

    call monolis_shape_3d_hex_1st_shapefunc_deriv(r, deriv)
    xj = matmul(node, deriv)
    call monolis_get_inverse_matrix_R_3d(xj, inv, det, is_fail)
    dndx = matmul(deriv, inv)
  end subroutine monolis_shape_3d_hex_1st_get_global_deriv

  !> 標準インターフェースによる形状関数
  subroutine monolis_shape_func_3d_hex_1st(local_coord, N)
    implicit none
    real(kdouble), intent(in) :: local_coord(:)
    real(kdouble), intent(out) :: N(:)
    
    call monolis_shape_3d_hex_1st_shapefunc(local_coord, N)
  end subroutine monolis_shape_func_3d_hex_1st

  !> 標準インターフェースによる定義域判定関数
  subroutine monolis_domain_func_3d_hex(local_coord, is_inside)
    implicit none
    real(kdouble), intent(in) :: local_coord(:)
    logical, intent(out) :: is_inside
    
    call monolis_shape_3d_hex_1st_is_inside_domain(local_coord, is_inside)
  end subroutine monolis_domain_func_3d_hex

  !> 標準インターフェースによる面情報定義関数
  subroutine monolis_surf_data_func_3d_hex_1st(i_face, n_face_node, face_node_ids, &
    face_shape_func, face_domain_func, n_face_edge, edge_data_func, face_shape_map_func)
    use mod_monolis_utils_define_prm
    implicit none
    integer(kint), intent(in) :: i_face
    integer(kint), intent(out) :: n_face_node
    integer(kint), intent(out) :: n_face_edge
    integer(kint), intent(out), allocatable :: face_node_ids(:)
    procedure(monolis_shape_func), pointer :: face_shape_func
    procedure(monolis_domain_func), pointer :: face_domain_func
    procedure(monolis_local_node_point_func), pointer :: edge_local_np_fucn
    procedure(monolis_edge_data_func), pointer :: edge_data_func
    procedure(monolis_shape_map_func), pointer :: face_shape_map_func

    if(i_face < 1 .or. 6 < i_face)then
      n_face_node = -1
      return
    endif

    n_face_node = 4
    call monolis_alloc_I_1d(face_node_ids, 4)
    face_node_ids(1) = monolis_shape_3d_hex_1st_surf(1, i_face)
    face_node_ids(2) = monolis_shape_3d_hex_1st_surf(2, i_face)
    face_node_ids(3) = monolis_shape_3d_hex_1st_surf(3, i_face)
    face_node_ids(4) = monolis_shape_3d_hex_1st_surf(4, i_face)

    n_face_edge = 4

    face_shape_func => monolis_shape_func_2d_quad_1st
    face_domain_func => monolis_domain_func_2d_quad
    edge_data_func => monolis_edge_data_func_2d_quad_1st
    face_shape_map_func => monolis_surf_map_func_3d_hex_1st
  end subroutine monolis_surf_data_func_3d_hex_1st

  !> 2D六面体1次要素の部分要素の局所座標を親要素の局所座標にマップする関数
  subroutine monolis_surf_map_func_3d_hex_1st(i_surf, local_coord, local_coord_3d)
    use mod_monolis_utils_define_prm
    implicit none
    integer(kint), intent(in) :: i_surf
    real(kdouble), intent(in) :: local_coord(:)
    real(kdouble), intent(out) :: local_coord_3d(:)
    real(kdouble) :: u, v

    u = local_coord(1)
    v = local_coord(2)

    select case(i_surf)
      case(1) ! z = -1 面
        local_coord_3d(1) = u
        local_coord_3d(2) = v
        local_coord_3d(3) = -1.0d0
      case(2) ! z = 1 面
        local_coord_3d(1) = u
        local_coord_3d(2) = v
        local_coord_3d(3) = 1.0d0
      case(3) ! y = -1 面
        local_coord_3d(1) = u
        local_coord_3d(2) = -1.0d0
        local_coord_3d(3) = v
      case(4) ! x = 1 面
        local_coord_3d(1) = 1.0d0
        local_coord_3d(2) = u
        local_coord_3d(3) = v
      case(5) ! y = 1 面
        local_coord_3d(1) = u
        local_coord_3d(2) = 1.0d0
        local_coord_3d(3) = v
      case(6) ! x = -1 面
        local_coord_3d(1) = -1.0d0
        local_coord_3d(2) = u
        local_coord_3d(3) = v
    end select
  end subroutine monolis_surf_map_func_3d_hex_1st
end module mod_monolis_shape_3d_hex_1st
