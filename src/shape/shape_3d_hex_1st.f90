module mod_monolis_shape_3d_hex_1st
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
  use mod_monolis_def_shape
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

  !> [r_1, r_2, r_3, r_1 and r_2, r_2 and r_3, r_1 and r_3, r_1 and r_2 and r_3]
  real(kdouble), parameter :: monolis_shape_3d_hex_1st_surf_constraint_value(7,6) = reshape([ &
     0.0d0, 0.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     0.0d0, 0.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     0.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     0.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
    -1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0  ], [7,6])

  !> [r_1, r_2, r_1 and r_2]
  logical, parameter :: monolis_shape_3d_hex_1st_surf_constraint_flag(7,6) = reshape([ &
     .false., .false., .true. , .false., .false., .false., .false., &
     .false., .false., .true. , .false., .false., .false., .false., &
     .false., .true. , .false., .false., .false., .false., .false., &
     .true. , .false., .false., .false., .false., .false., .false., &
     .false., .true. , .false., .false., .false., .false., .false., &
     .true. , .false., .false., .false., .false., .false., .false.  ], [7,6])

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

  !> [r_1, r_2, r_3, r_1 and r_2, r_2 and r_3, r_1 and r_3, r_1 and r_2 and r_3]
  real(kdouble), parameter :: monolis_shape_3d_hex_1st_edge_constraint_value(7,12) = reshape([ &
     0.0d0,-1.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     1.0d0, 0.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     0.0d0, 1.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
    -1.0d0, 0.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     0.0d0,-1.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     1.0d0, 0.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     0.0d0, 1.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
    -1.0d0, 0.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
    -1.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     1.0d0,-1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
     1.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
    -1.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0  ], [7,12])

  !> [r_1, r_2, r_1 and r_2]
  logical, parameter :: monolis_shape_3d_hex_1st_edge_constraint_flag(7,12) = reshape([ &
     .false., .true. , .true. , .false., .false., .false., .false., &
     .true. , .false., .true. , .false., .false., .false., .false., &
     .false., .true. , .true. , .false., .false., .false., .false., &
     .true. , .false., .true. , .false., .false., .false., .false., &
     .false., .true. , .true. , .false., .false., .false., .false., &
     .true. , .false., .true. , .false., .false., .false., .false., &
     .false., .true. , .true. , .false., .false., .false., .false., &
     .true. , .false., .true. , .false., .false., .false., .false., &
     .true. , .true. , .false., .false., .false., .false., .false., &
     .true. , .true. , .false., .false., .false., .false., .false., &
     .true. , .true. , .false., .false., .false., .false., .false., &
     .true. , .true. , .false., .false., .false., .false., .false.  ], [7,12])

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
    public :: monolis_shape_3d_hex_1st_surf_constraint_value
    public :: monolis_shape_3d_hex_1st_surf_constraint_flag
    public :: monolis_shape_3d_hex_1st_edge
    public :: monolis_shape_3d_hex_1st_edge_constraint_value
    public :: monolis_shape_3d_hex_1st_edge_constraint_flag
    ! 標準インターフェース用の関数
    public :: monolis_shape_func_3d_hex_1st
    public :: monolis_domain_func_3d_hex
    public :: monolis_shape_3d_hex_1st_get_face_data
    public :: monolis_shape_3d_hex_1st_get_edge_data
    public :: monolis_shape_3d_hex_1st_is_on_boundary
    public :: monolis_shape_3d_hex_1st_map_local_coord

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

  !> 六面体1次要素の面情報を取得する関数
  subroutine monolis_shape_3d_hex_1st_get_face_data(face_id, face_nodes, face_type)
    use mod_monolis_def_shape, only: monolis_shape_2d_quad_1st
    implicit none
    integer(kint), intent(in) :: face_id
    integer(kint), allocatable, intent(out) :: face_nodes(:)
    integer(kint), intent(out) :: face_type
    
    if(face_id < 1 .or. face_id > 6) then
      face_type = -1
      return
    endif
    
    face_type = monolis_shape_2d_quad_1st
    allocate(face_nodes(4))
    face_nodes(1:4) = monolis_shape_3d_hex_1st_surf(1:4, face_id)
  end subroutine monolis_shape_3d_hex_1st_get_face_data

  !> 六面体1次要素のエッジ情報を取得する関数
  subroutine monolis_shape_3d_hex_1st_get_edge_data(edge_id, edge_nodes, edge_type)
    use mod_monolis_def_shape, only: monolis_shape_1d_line_1st
    implicit none
    integer(kint), intent(in) :: edge_id
    integer(kint), allocatable, intent(out) :: edge_nodes(:)
    integer(kint), intent(out) :: edge_type
    
    if(edge_id < 1 .or. edge_id > 12) then
      edge_type = -1
      return
    endif
    
    edge_type = monolis_shape_1d_line_1st
    allocate(edge_nodes(2))
    edge_nodes(1:2) = monolis_shape_3d_hex_1st_edge(1:2, edge_id)
  end subroutine monolis_shape_3d_hex_1st_get_edge_data

  !> 六面体1次要素の境界上にあるかを判定する関数
  subroutine monolis_shape_3d_hex_1st_is_on_boundary(local_coord, is_on_boundary)
    implicit none
    real(kdouble), intent(in) :: local_coord(:)
    logical, intent(out) :: is_on_boundary
    real(kdouble) :: eps

    eps = 1.0d-10
    is_on_boundary = .false.
    
    ! いずれかの面上にあるか確認
    if (abs(local_coord(1) - (-1.0d0)) < eps .or. &
        abs(local_coord(1) - 1.0d0) < eps .or. &
        abs(local_coord(2) - (-1.0d0)) < eps .or. &
        abs(local_coord(2) - 1.0d0) < eps .or. &
        abs(local_coord(3) - (-1.0d0)) < eps .or. &
        abs(local_coord(3) - 1.0d0) < eps) then
      is_on_boundary = .true.
    endif
  end subroutine monolis_shape_3d_hex_1st_is_on_boundary

  !> 六面体1次要素の境界座標マッピング関数 (部分要素から親要素へ)
  subroutine monolis_shape_3d_hex_1st_map_local_coord(sub_dim, sub_id, sub_coord, parent_coord)
    implicit none
    integer(kint), intent(in) :: sub_dim !> 部分要素次元（0:頂点, 1:エッジ, 2:面）
    integer(kint), intent(in) :: sub_id  !> 部分要素ID (1-based)
    real(kdouble), intent(in) :: sub_coord(:)  !> 部分要素での局所座標
    real(kdouble), intent(out) :: parent_coord(3) !> 親要素での対応する局所座標
    
    real(kdouble) :: u, v
    
    parent_coord = 0.0d0
    
    if (sub_dim == 2) then ! 面
      u = sub_coord(1)
      v = sub_coord(2)
      select case(sub_id)
        case(1) ! z = -1 面
          parent_coord(1) = u
          parent_coord(2) = v
          parent_coord(3) = -1.0d0
        case(2) ! z = 1 面
          parent_coord(1) = u
          parent_coord(2) = v
          parent_coord(3) = 1.0d0
        case(3) ! y = -1 面
          parent_coord(1) = u
          parent_coord(2) = -1.0d0
          parent_coord(3) = v
        case(4) ! x = 1 面
          parent_coord(1) = 1.0d0
          parent_coord(2) = u
          parent_coord(3) = v
        case(5) ! y = 1 面
          parent_coord(1) = u
          parent_coord(2) = 1.0d0
          parent_coord(3) = v
        case(6) ! x = -1 面
          parent_coord(1) = -1.0d0
          parent_coord(2) = u
          parent_coord(3) = v
      end select
    else if (sub_dim == 1) then ! エッジ
      u = sub_coord(1) ! エッジ上の局所座標 [-1, 1]
      select case(sub_id)
        case(1) ! エッジ1-2
          parent_coord = (/-1.0d0, -1.0d0, -1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                        (/ 1.0d0, -1.0d0, -1.0d0/) * ((u+1.0d0)/2.0d0)
        case(2) ! エッジ2-3
          parent_coord = (/ 1.0d0, -1.0d0, -1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                        (/ 1.0d0,  1.0d0, -1.0d0/) * ((u+1.0d0)/2.0d0)
        case(3) ! エッジ3-4
          parent_coord = (/ 1.0d0,  1.0d0, -1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                        (/-1.0d0,  1.0d0, -1.0d0/) * ((u+1.0d0)/2.0d0)
        case(4) ! エッジ4-1
          parent_coord = (/-1.0d0,  1.0d0, -1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                        (/-1.0d0, -1.0d0, -1.0d0/) * ((u+1.0d0)/2.0d0)
        case(5) ! エッジ5-6
          parent_coord = (/-1.0d0, -1.0d0,  1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                        (/ 1.0d0, -1.0d0,  1.0d0/) * ((u+1.0d0)/2.0d0)
        case(6) ! エッジ6-7
          parent_coord = (/ 1.0d0, -1.0d0,  1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                        (/ 1.0d0,  1.0d0,  1.0d0/) * ((u+1.0d0)/2.0d0)
        case(7) ! エッジ7-8
          parent_coord = (/ 1.0d0,  1.0d0,  1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                        (/-1.0d0,  1.0d0,  1.0d0/) * ((u+1.0d0)/2.0d0)
        case(8) ! エッジ8-5
          parent_coord = (/-1.0d0,  1.0d0,  1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                        (/-1.0d0, -1.0d0,  1.0d0/) * ((u+1.0d0)/2.0d0)
        case(9) ! エッジ1-5
          parent_coord = (/-1.0d0, -1.0d0, -1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                        (/-1.0d0, -1.0d0,  1.0d0/) * ((u+1.0d0)/2.0d0)
        case(10) ! エッジ2-6
          parent_coord = (/ 1.0d0, -1.0d0, -1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                        (/ 1.0d0, -1.0d0,  1.0d0/) * ((u+1.0d0)/2.0d0)
        case(11) ! エッジ3-7
          parent_coord = (/ 1.0d0,  1.0d0, -1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                        (/ 1.0d0,  1.0d0,  1.0d0/) * ((u+1.0d0)/2.0d0)
        case(12) ! エッジ4-8
          parent_coord = (/-1.0d0,  1.0d0, -1.0d0/) * (1.0d0 - (u+1.0d0)/2.0d0) + &
                        (/-1.0d0,  1.0d0,  1.0d0/) * ((u+1.0d0)/2.0d0)
      end select
    else if (sub_dim == 0) then ! 頂点
      select case(sub_id)
        case(1); parent_coord = (/-1.0d0, -1.0d0, -1.0d0/)
        case(2); parent_coord = (/ 1.0d0, -1.0d0, -1.0d0/)
        case(3); parent_coord = (/ 1.0d0,  1.0d0, -1.0d0/)
        case(4); parent_coord = (/-1.0d0,  1.0d0, -1.0d0/)
        case(5); parent_coord = (/-1.0d0, -1.0d0,  1.0d0/)
        case(6); parent_coord = (/ 1.0d0, -1.0d0,  1.0d0/)
        case(7); parent_coord = (/ 1.0d0,  1.0d0,  1.0d0/)
        case(8); parent_coord = (/-1.0d0,  1.0d0,  1.0d0/)
      end select
    endif
  end subroutine monolis_shape_3d_hex_1st_map_local_coord
end module mod_monolis_shape_3d_hex_1st
