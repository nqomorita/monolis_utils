program monolis_utils_test
  use mod_monolis_utils
  use mod_monolis_utils_alloc_test
  use mod_monolis_utils_std_test_test
  use mod_monolis_utils_std_sort_I_test
  implicit none

  !> std test
  call monolis_utils_alloc_test()
  call monolis_utils_std_test_test()
  call monolis_utils_std_sort_I_test()
end program monolis_utils_test
