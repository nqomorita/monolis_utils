!> バウンディングボックステストモジュール
module mod_monolis_utils_aabb_test
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_error
  use mod_monolis_utils_std_test
  use mod_monolis_utils_aabb
  implicit none

contains

  !> main test subroutine
  subroutine monolis_utils_aabb_test()
    implicit none

    call monolis_aabb_init_test()
    call monolis_aabb_push_get_unit_test()
  end subroutine monolis_utils_aabb_test

  !> unit test
  subroutine monolis_aabb_init_test()
    implicit none

    call monolis_std_log_string("monolis_aabb_init_test")

    !call monolis_aabb_init(monolis_aabb, BB, div)

    !call monolis_aabb_finalize(monolis_aabb)
  end subroutine monolis_aabb_init_test

  subroutine monolis_aabb_push_get_unit_test()
    implicit none

    !call monolis_std_log_string("monolis_aabb_push_get_unit_test")

    !call monolis_aabb_push(monolis_aabb, BB, id)
  end subroutine monolis_aabb_push_get_unit_test
end module mod_monolis_utils_aabb_test
