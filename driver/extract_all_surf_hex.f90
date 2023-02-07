!> @ingroup driver
program monolis_extract_all_surf_hex
  use mod_monolis_utils
  use mod_monolis_driver_util
  use mod_monolis_extract_all_util
  implicit none
  !> 入力節点ファイル名
  character(monolis_charlen) :: fnname
  !> 入力要素ファイル名
  character(monolis_charlen) :: fename
  !> 出力ファイル名
  character(monolis_charlen) :: foname
  !> 境界条件を設定する自由度数
  integer(kint) :: n_dof
  !> 境界条件の設定値
  real(kdouble), allocatable :: val(:)
  integer(kint) :: n_node, n_elem, n_base, n_surf_node, n_surf_elem
  logical :: is_get
  integer(kint), allocatable :: elem(:,:), surf(:,:)
  integer(kint), allocatable :: i_bc(:,:)
  real(kdouble), allocatable :: node(:,:)
  real(kdouble), allocatable :: r_bc(:)

  call monolis_mpi_initialize()

  call monolis_std_debug_log_header("monolis_extract_all_surf_hex")

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./monolis_extract_all_surf_hex {options}"
    write(*,"(a)")""
    write(*,"(a)")"options:"
    write(*,"(a)")"-in {input node filename}: (defualt) node.dat"
    write(*,"(a)")"-ie {input elem filename}: (defualt) elem.dat"
    write(*,"(a)")"-o  {output filename}: (defualt) D_bc.dat"
    write(*,"(a)")"-h  : help"
    stop monolis_success
  endif

  call monolis_driver_get_arg_in(fnname)

  call monolis_driver_get_arg_ie(fename)

  foname = "D_bc.dat"
  call monolis_driver_get_arg_o(foname)

  call monolis_input_node(fnname, n_node, node)

  call monolis_input_elem(fename, n_elem, n_base, elem)

  if(n_base /= 8)then
    call monolis_std_error_string("please input 1st order hex mesh")
    call monolis_std_error_stop()
  endif

  call monolis_get_surf(n_node, n_elem, n_base, elem, 6, 4, n_surf_elem, surf)

  !call monolis_output_bc(foname, n_surf_node, n_dof, i_bc, r_bc)

  call monolis_mpi_finalize()
end program monolis_extract_all_surf_hex
