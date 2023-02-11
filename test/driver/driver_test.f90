module mod_monolis_driver_test
  use mod_monolis_utils
  implicit none

contains

  !> main test subroutine
  subroutine monolis_driver_test()
    implicit none

    call monolis_dbc_all_surf_hex_test()
    call monolis_dbc_all_surf_tet_test()
    call monolis_extract_all_surf_hex_test()
    call monolis_extract_all_surf_tet_test()
    call monolis_p_refiner_tet_test()
    call monolis_h_refiner_tet_test()
    call monolis_h_refiner_hex_test()
  end subroutine monolis_driver_test

  subroutine monolis_dbc_all_surf_hex_test()
    implicit none

    call monolis_std_log_string("monolis_dbc_all_surf_hex_test")
  end subroutine monolis_dbc_all_surf_hex_test

  subroutine monolis_dbc_all_surf_tet_test()
    implicit none

    call monolis_std_log_string("monolis_dbc_all_surf_tet_test")
  end subroutine monolis_dbc_all_surf_tet_test

  subroutine monolis_extract_all_surf_hex_test()
    implicit none

    call monolis_std_log_string("monolis_extract_all_surf_hex_test")
  end subroutine monolis_extract_all_surf_hex_test

  subroutine monolis_extract_all_surf_tet_test()
    implicit none

    call monolis_std_log_string("monolis_extract_all_surf_tet_test")
  end subroutine monolis_extract_all_surf_tet_test

  subroutine monolis_p_refiner_tet_test()
    implicit none

    call monolis_std_log_string("monolis_p_refiner_tet_test")
  end subroutine monolis_p_refiner_tet_test

  subroutine monolis_h_refiner_tet_test()
    implicit none

    call monolis_std_log_string("monolis_h_refiner_tet_test")
  end subroutine monolis_h_refiner_tet_test

  subroutine monolis_h_refiner_hex_test()
    implicit none

    call monolis_std_log_string("monolis_h_refiner_hex_test")
  end subroutine monolis_h_refiner_hex_test
end module mod_monolis_driver_test
