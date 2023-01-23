program monolis_utils_test
  use mod_monolis_utils
  use mod_monolis_utils_alloc_test
  use mod_monolis_utils_std_test_test
  use mod_monolis_utils_std_sort_I_test
  use mod_monolis_utils_std_sort_R_test
  use mod_monolis_utils_std_algebra_test
  use mod_monolis_utils_hash_test
  use mod_monolis_utils_aabb_test
  use mod_monolis_utils_kdtree_test
  implicit none

  !> std test
  call monolis_utils_alloc_test()
  call monolis_utils_std_test_test()
  call monolis_utils_std_sort_I_test()
  call monolis_utils_std_sort_R_test()
  call monolis_utils_std_algebra_test()
  call monolis_utils_hash_test()
  call monolis_utils_aabb_test()
  call monolis_utils_kdtree_test()
end program monolis_utils_test
