!> @ingroup driver
program monolis_extract_all_surf_tet
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

  call monolis_std_log_string("monolis_extract_all_surf_tet")

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./monolis_extract_all_surf_tet {options}"
    write(*,"(a)")""
    write(*,"(a)")"options:"
    write(*,"(a)")"-in {input node filename}: (defualt) node.dat"
    write(*,"(a)")"-ie {input elem filename}: (defualt) elem.dat"
    write(*,"(a)")"-o  {output filename}: (defualt) surf.dat"
    write(*,"(a)")"-h  : help"
    stop monolis_success
  endif

  finame = "elem.dat"
  call monolis_get_arg_input_i_tag(finame, is_get)

  foname = "surf.dat"
  call monolis_get_arg_input_o_tag(foname, is_get)

  call monolis_input_elem(finame, n_elem, n_base, elem)

  if(n_base /= 4)then
    call monolis_std_error_string("please input 1st order tet mesh")
    call monolis_std_error_stop()
  endif

  call monolis_get_surf(n_elem, n_base, elem, 4, 3, n_surf_elem, surf)

  call monolis_output_elem(foname, n_surf_elem, 3, surf)

  call monolis_mpi_finalize()
end program monolis_extract_all_surf_tet
