!> IO ファイル名モジュール
module mod_monolis_io_file_name_test
  use mod_monolis_utils
  implicit none

contains

  subroutine monolis_io_file_name_test()
    implicit none

    call monolis_get_global_input_file_name_test()
    call monolis_get_local_input_file_name_test()
    call monolis_get_global_output_file_name_test()
    call monolis_get_local_output_file_name_test()
    call monolis_get_local_output_file_name_by_domain_id_test()
  end subroutine monolis_io_file_name_test

  subroutine monolis_get_global_input_file_name_test()
    implicit none
    character(monolis_charlen) :: string, topname, dirname, fname
    integer(kint) :: domain_id

    call monolis_std_global_log_string("monolis_get_global_input_file_name")

    topname = "."
    dirname = "parted"
    fname = "input.txt"
    domain_id = monolis_mpi_get_global_my_rank()

    string = monolis_get_global_input_file_name(topname, dirname, fname)

    if(monolis_mpi_get_global_comm_size() == 1)then
      if(trim(string) /= "./input.txt")then
        call monolis_test_assert_fail("monolis_get_global_input_file_name_test", "")
      endif
    else
      if(domain_id == 0)then
        if(trim(string) /= "./parted/input.txt.0")then
          call monolis_test_assert_fail("monolis_get_global_input_file_name_test", "")
        endif
      else
        if(trim(string) /= "./parted/input.txt.1")then
          call monolis_test_assert_fail("monolis_get_global_input_file_name_test", "")
        endif
      endif
    endif

    call monolis_test_assert_pass("monolis_get_global_input_file_name_test")
  end subroutine monolis_get_global_input_file_name_test

  subroutine monolis_get_local_input_file_name_test()
    implicit none
    character(monolis_charlen) :: string, topname, dirname, fname
    integer(kint) :: domain_id, comm

    call monolis_std_global_log_string("monolis_get_local_input_file_name")

    topname = "."
    dirname = "parted"
    fname = "input.txt"
    domain_id = monolis_mpi_get_global_my_rank()
    comm = monolis_mpi_get_global_comm()

    string = monolis_get_local_input_file_name(topname, dirname, fname, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      if(trim(string) /= "./input.txt")then
        call monolis_test_assert_fail("monolis_get_local_input_file_name_test", "")
      endif
    else
      if(domain_id == 0)then
        if(trim(string) /= "./parted/input.txt.0")then
          call monolis_test_assert_fail("monolis_get_local_input_file_name_test", "")
        endif
      else
        if(trim(string) /= "./parted/input.txt.1")then
          call monolis_test_assert_fail("monolis_get_local_input_file_name_test", "")
        endif
      endif
    endif

    call monolis_test_assert_pass("monolis_get_local_input_file_name_test")
  end subroutine monolis_get_local_input_file_name_test

  subroutine monolis_get_global_output_file_name_test()
    implicit none
    character(monolis_charlen) :: string, topname, dirname, fname
    integer(kint) :: domain_id

    call monolis_std_global_log_string("monolis_get_global_output_file_name")

    topname = "."
    dirname = "parted"
    fname = "input.txt"
    domain_id = monolis_mpi_get_global_my_rank()

    string = monolis_get_global_output_file_name(topname, dirname, fname)

    if(monolis_mpi_get_global_comm_size() == 1)then
      if(trim(string) /= "./input.txt")then
        call monolis_test_assert_fail("monolis_get_global_output_file_name_test", "")
      endif
    else
      if(domain_id == 0)then
        if(trim(string) /= "./parted/input.txt.0")then
          call monolis_test_assert_fail("monolis_get_global_output_file_name_test", "")
        endif
      else
        if(trim(string) /= "./parted/input.txt.1")then
          call monolis_test_assert_fail("monolis_get_global_output_file_name_test", "")
        endif
      endif
    endif

    call monolis_test_assert_pass("monolis_get_global_output_file_name_test")
  end subroutine monolis_get_global_output_file_name_test

  subroutine monolis_get_local_output_file_name_test()
    implicit none
    character(monolis_charlen) :: string, topname, dirname, fname
    integer(kint) :: domain_id, comm

    call monolis_std_global_log_string("monolis_get_local_output_file_name")

    topname = "."
    dirname = "parted"
    fname = "input.txt"
    domain_id = monolis_mpi_get_global_my_rank()
    comm = monolis_mpi_get_global_comm()

    string = monolis_get_local_output_file_name(topname, dirname, fname, comm)

    if(monolis_mpi_get_global_comm_size() == 1)then
      if(trim(string) /= "./input.txt")then
        call monolis_test_assert_fail("monolis_get_local_output_file_name_test", "")
      endif
    else
      if(domain_id == 0)then
        if(trim(string) /= "./parted/input.txt.0")then
          call monolis_test_assert_fail("monolis_get_local_output_file_name_test", "")
        endif
      else
        if(trim(string) /= "./parted/input.txt.1")then
          call monolis_test_assert_fail("monolis_get_local_output_file_name_test", "")
        endif
      endif
    endif

    call monolis_test_assert_pass("monolis_get_local_output_file_name_test")
  end subroutine monolis_get_local_output_file_name_test

  subroutine monolis_get_local_output_file_name_by_domain_id_test()
    implicit none
    character(monolis_charlen) :: string, topname, dirname, fname
    integer(kint) :: domain_id, comm

    call monolis_std_global_log_string("monolis_get_output_file_name_by_domain_id")

    topname = "."
    dirname = "parted"
    fname = "input.txt"
    domain_id = monolis_mpi_get_global_my_rank()
    comm = monolis_mpi_get_global_comm()

    string = monolis_get_output_file_name_by_domain_id(topname, dirname, fname, comm)

    if(domain_id == 0)then
      if(trim(string) /= "./parted/input.txt.0")then
        call monolis_test_assert_fail("monolis_get_local_output_file_name_test", "")
      endif
    else
      if(trim(string) /= "./parted/input.txt.1")then
        call monolis_test_assert_fail("monolis_get_local_output_file_name_test", "")
      endif
    endif

    call monolis_test_assert_pass("monolis_get_local_output_file_name_test")
  end subroutine monolis_get_local_output_file_name_by_domain_id_test
end module mod_monolis_io_file_name_test
