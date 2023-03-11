!> @ingroup driver
program monolis_extract_all_surf_hex
  use mod_monolis_utils
  use mod_monolis_driver_util
  use mod_monolis_extract_util
  implicit none
  !> 入力要素ファイル名
  character(monolis_charlen) :: finame
  !> 出力ファイル名
  character(monolis_charlen) :: foname
  !> 境界条件を設定する自由度数
  integer(kint) :: n_dof
  !> 境界条件の設定値
  real(kdouble), allocatable :: val(:)
  integer(kint) :: n_elem, n_base, n_surf_elem
  logical :: is_get
  integer(kint), allocatable :: elem(:,:), surf(:,:)

  call monolis_mpi_initialize()

  call monolis_std_log_string("monolis_extract_all_surf_hex")

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./monolis_extract_all_surf_hex {options}"
    write(*,"(a)")""
    write(*,"(a)")"options:"
    write(*,"(a)")"-i {input elem filename}: (defualt) elem.dat"
    write(*,"(a)")"-o {output filename}: (defualt) surf.dat"
    write(*,"(a)")"-h : help"
    stop monolis_success
  endif

  finame = "elem.dat"
  call monolis_get_arg_input_i_tag(finame, is_get)

  foname = "surf.dat"
  call monolis_get_arg_input_o_tag(foname, is_get)

  call monolis_input_elem(finame, n_elem, n_base, elem)

  if(n_base /= 8)then
    call monolis_std_error_string("please input 1st order hex mesh")
    call monolis_std_error_stop()
  endif

  call monolis_get_surf(n_elem, n_base, elem, 6, 4, n_surf_elem, surf)

  call monolis_output_elem(foname, n_surf_elem, 4, surf)

  call monolis_mpi_finalize()
end program monolis_extract_all_surf_hex
