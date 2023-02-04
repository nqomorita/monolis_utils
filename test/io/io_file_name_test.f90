!> IO ファイル名モジュール
module mod_monolis_io_file_name_test
  use mod_monolis_utils_error
  use mod_monolis_utils_std_test
  use mod_monolis_io_file_name
  implicit none

contains

  subroutine monolis_io_file_name_test()
    implicit none

    call monolis_get_global_input_file_name_test()
    call monolis_get_local_input_file_name_test()
    call monolis_get_output_file_name_test()
  end subroutine monolis_io_file_name_test

  subroutine monolis_get_global_input_file_name_test()
    implicit none
    character(monolis_charlen) :: string, dirname, fname
    integer(kint) :: domain_id

    dirname = "parted"
    fname = "input.txt"
    domain_id = monolis_mpi_global_my_rank()
    string = monolis_get_global_input_file_name(dirname, fname, domain_id)

    if(monolis_mpi_global_comm_size() == 1)then
      if(trim(string) /= "input.txt")then
        call monolis_test_assert_fail("monolis_get_global_input_file_name_test", "")
      endif
    else
      if(domain_id == 0)then
        if(trim(string) /= "parted/input.txt.0")then
          call monolis_test_assert_fail("monolis_get_global_input_file_name_test", "")
        endif
      else
        if(trim(string) /= "parted/input.txt.1")then
          call monolis_test_assert_fail("monolis_get_global_input_file_name_test", "")
        endif
      endif
    endif

    call monolis_test_assert_pass("monolis_get_global_input_file_name_test")
  end subroutine monolis_get_global_input_file_name_test

  subroutine monolis_get_local_input_file_name_test()
    implicit none
    character(monolis_charlen) :: string, dirname, fname
    integer(kint) :: domain_id, comm

    dirname = "parted"
    fname = "input.txt"
    domain_id = monolis_mpi_global_my_rank()
    comm = monolis_mpi_global_comm()
    string = monolis_get_local_input_file_name(dirname, fname, domain_id, comm)

    if(monolis_mpi_global_comm_size() == 1)then
      if(trim(string) /= "input.txt")then
        call monolis_test_assert_fail("monolis_get_local_input_file_name_test", "")
      endif
    else
      if(domain_id == 0)then
        if(trim(string) /= "parted/input.txt.0")then
          call monolis_test_assert_fail("monolis_get_local_input_file_name_test", "")
        endif
      else
        if(trim(string) /= "parted/input.txt.1")then
          call monolis_test_assert_fail("monolis_get_local_input_file_name_test", "")
        endif
      endif
    endif

    call monolis_test_assert_pass("monolis_get_local_input_file_name_test")
  end subroutine monolis_get_local_input_file_name_test

  subroutine monolis_get_output_file_name_test()
    implicit none
    character(monolis_charlen) :: string, dirname, fname
    integer(kint) :: domain_id

    dirname = "parted"
    fname = "input.txt"
    domain_id = monolis_mpi_global_my_rank()
    string = monolis_get_output_file_name(dirname, fname, domain_id)

    if(domain_id == 0)then
      if(trim(string) /= "parted/input.txt.0")then
        call monolis_test_assert_fail("monolis_get_output_file_name_test", "")
      endif
    else
      if(trim(string) /= "parted/input.txt.1")then
        call monolis_test_assert_fail("monolis_get_output_file_name_test", "")
      endif
    endif

    call monolis_test_assert_pass("monolis_get_output_file_name_test")
  end subroutine monolis_get_output_file_name_test
end module mod_monolis_io_file_name_test
