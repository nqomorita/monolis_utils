module mod_monolis_def_shape
  use mod_monolis_utils_define_prm
  implicit none

  !> 2 次元三角形 1 次要素
  integer(kint), parameter :: monolis_shape_2d_tri_1st  = 1
  !> 2 次元三角形 2 次要素
  integer(kint), parameter :: monolis_shape_2d_tri_2nd  = 2
  !> 2 次元四角形 1 次要素
  integer(kint), parameter :: monolis_shape_2d_quad_1st = 3
  !> 3 次元四面体 1 次要素
  integer(kint), parameter :: monolis_shape_3d_tet_1st  = 4
  !> 3 次元四面体 2 次要素
  integer(kint), parameter :: monolis_shape_3d_tet_2nd  = 5
  !> 3 次元六面体 1 次要素
  integer(kint), parameter :: monolis_shape_3d_hex_1st  = 6

end module mod_monolis_def_shape
