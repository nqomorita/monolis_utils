!> k-d ツリーテストモジュール
module mod_monolis_utils_kdtree_test
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_error
  use mod_monolis_utils_aabb
  use mod_monolis_utils_std_test
  implicit none

contains

  !> main test subroutine
  subroutine monolis_utils_kdtree_test()
    implicit none

    call monolis_kdtree_init_test()
  end subroutine monolis_utils_kdtree_test

  !> unit test
  subroutine monolis_kdtree_init_test()
    implicit none

    call monolis_std_log_string("monolis_kdtree_init_test")

    !call monolis_aabb_init(monolis_aabb, BB, div)

    !call monolis_aabb_finalize(monolis_aabb)
  end subroutine monolis_kdtree_init_test
end module mod_monolis_utils_kdtree_test
