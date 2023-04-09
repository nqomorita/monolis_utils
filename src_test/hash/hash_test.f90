!> ハッシュテストモジュール
module mod_monolis_utils_hash_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_utils_hash_test()
    implicit none

    call monolis_hash_init_test()
    call monolis_hash_get_key_I_test()
    call monolis_hash_push_get_unit_test()
    call monolis_hash_push_get_full_test()

    call monolis_std_global_log_string("monolis_hash_key")
    call monolis_std_global_log_string("monolis_hash_list_get")
    call monolis_std_global_log_string("monolis_hash_list_push")
    call monolis_std_global_log_string("monolis_hash_list_update")
    call monolis_std_global_log_string("monolis_hash_resize")
    call monolis_std_global_log_string("monolis_index_key")
  end subroutine monolis_utils_hash_test

  !> unit test
  subroutine monolis_hash_init_test()
    implicit none
    type(monolis_hash_structure) :: monolis_hash

    call monolis_std_global_log_string("monolis_hash_init")
    call monolis_std_global_log_string("monolis_hash_finalize")

    !> case 1
    call monolis_hash_init(monolis_hash, 5)

    call monolis_test_check_eq_I1("monolis_hash_init 1", monolis_hash%n_put, 0)
    call monolis_test_check_eq_I1("monolis_hash_init 2", monolis_hash%key_size, 5)
    call monolis_test_check_eq_I1("monolis_hash_init 3", monolis_hash%hash_size_id, 5)
    call monolis_test_check_eq_I1("monolis_hash_init 4", size(monolis_hash%bin), 16381)

    !> case 2
    call monolis_hash_finalize(monolis_hash)

    if(associated(monolis_hash%bin))then
      call monolis_test_assert_fail("monolis_hash_init", "")
    endif
  end subroutine monolis_hash_init_test

  subroutine monolis_hash_get_key_I_test()
    implicit none
    character :: key*5

    call monolis_std_global_log_string("monolis_hash_get_key_I")

    call monolis_hash_get_key_I(5, 1, key)

    if(key(1:5) /= "00001")then
      call monolis_test_assert_fail("monolis_hash_get_key_I", "")
    else
      call monolis_test_assert_pass("monolis_hash_get_key_I")
    endif
  end subroutine monolis_hash_get_key_I_test

  subroutine monolis_hash_push_get_unit_test()
    implicit none
    type(monolis_hash_structure) :: monolis_hash
    integer(kint) :: val
    character :: key*5
    logical :: is_pushed, is_exist

    call monolis_std_global_log_string("monolis_hash_push")
    call monolis_std_global_log_string("monolis_hash_get")
    call monolis_std_global_log_string("monolis_hash_get_key_I")

    call monolis_hash_init(monolis_hash, 5)

    !> case 1
    call monolis_hash_get_key_I(5, 1, key)
    val = 10
    call monolis_hash_push(monolis_hash, key, val, is_pushed, is_exist)

    call monolis_test_check_eq_L1("monolis_hash_push_get_unit_test 1", is_pushed, .true.)
    call monolis_test_check_eq_L1("monolis_hash_push_get_unit_test 1", is_exist , .false.)

    !> case 2
    call monolis_hash_get_key_I(5, 1, key)
    val = 0
    call monolis_hash_get(monolis_hash, key, val, is_exist)

    call monolis_test_check_eq_I1("monolis_hash_push_get_unit_test 2", val, 10)
    call monolis_test_check_eq_L1("monolis_hash_push_get_unit_test 2", is_exist , .true.)

    !> case 3
    call monolis_hash_get_key_I(5, 10, key)
    val = 0
    call monolis_hash_get(monolis_hash, key, val, is_exist)

    call monolis_test_check_eq_I1("monolis_hash_push_get_unit_test 3", val, 0)
    call monolis_test_check_eq_L1("monolis_hash_push_get_unit_test 3", is_exist , .false.)

    call monolis_hash_finalize(monolis_hash)
  end subroutine monolis_hash_push_get_unit_test

  subroutine monolis_hash_push_get_full_test()
    implicit none
    type(monolis_hash_structure) :: monolis_hash
    integer(kint) :: val, i
    character :: key*5
    logical :: is_pushed, is_exist

    call monolis_std_global_log_string("monolis_hash_get_key_I")
    call monolis_std_global_log_string("monolis_hash_push")
    call monolis_std_global_log_string("monolis_hash_get")

    call monolis_hash_init(monolis_hash, 5)

    !> case 1
    !> push
    do i = 1, 2000
      call monolis_hash_get_key_I(5, i, key)
      val = 2*i
      call monolis_hash_push(monolis_hash, key, val, is_pushed, is_exist)
      if(.not. is_pushed) call monolis_test_assert_fail("monolis_hash_push_get_full_test push", "")
      if(is_exist) call monolis_test_assert_fail("monolis_hash_push_get_full_test push", "")
    enddo

    call monolis_test_check_eq_I1("monolis_hash_push_get_full_test 1", monolis_hash%n_put, 2000)

    !> get
    do i = 1, 2000
      call monolis_hash_get_key_I(5, i, key)
      val = 0
      call monolis_hash_get(monolis_hash, key, val, is_exist)
      if(val /= 2*i) call monolis_test_assert_fail("monolis_hash_push_get_full_test get", "")
      if(.not. is_exist) call monolis_test_assert_fail("monolis_hash_push_get_full_test get", "")
    enddo

    !> case 2 over write
    !> push
    do i = 1, 2000
      call monolis_hash_get_key_I(5, i, key)
      val = 4*i
      call monolis_hash_push(monolis_hash, key, val, is_pushed, is_exist)
      if(.not. is_pushed) call monolis_test_assert_fail("monolis_hash_push_get_full_test push", "")
      if(.not. is_exist) call monolis_test_assert_fail("monolis_hash_push_get_full_test push", "")
    enddo

    call monolis_test_check_eq_I1("monolis_hash_push_get_full_test 2", monolis_hash%n_put, 2000)

    !> get
    do i = 1, 2000
      call monolis_hash_get_key_I(5, i, key)
      val = 0
      call monolis_hash_get(monolis_hash, key, val, is_exist)
      if(val /= 4*i) call monolis_test_assert_fail("monolis_hash_push_get_full_test get", "")
      if(.not. is_exist) call monolis_test_assert_fail("monolis_hash_push_get_full_test get", "")
    enddo

    call monolis_hash_finalize(monolis_hash)

    call monolis_test_assert_pass("monolis_hash_push_get_full_test 3")
  end subroutine monolis_hash_push_get_full_test
end module mod_monolis_utils_hash_test
