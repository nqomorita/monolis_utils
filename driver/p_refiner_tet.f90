!> @ingroup driver
program monolis_p_refiner_tet
  use mod_monolis_utils
  use mod_monolis_driver_util
  use mod_monolis_refiner_util
  implicit none
  !> 入力節点ファイル名
  character(monolis_charlen) :: finname
  !> 入力要素ファイル名
  character(monolis_charlen) :: fiename
  !> 出力節点ファイル名
  character(monolis_charlen) :: fonname
  !> 出力要素ファイル名
  character(monolis_charlen) :: foename
  integer(kint) :: n_node, n_elem, n_base, n_node_ref
  logical :: is_get, is_1_origin
  integer(kint), allocatable :: elem(:,:), elem_ref(:,:)
  real(kdouble), allocatable :: node(:,:), node_ref(:,:)

  call monolis_mpi_initialize()

  call monolis_std_log_string("monolis_p_refiner_tet")

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./monolis_p_refiner_tet {options}"
    write(*,"(a)")""
    write(*,"(a)")"-in {input node filename}: (defualt) node.dat"
    write(*,"(a)")"-ie {input elem filename}: (defualt) elem.dat"
    write(*,"(a)")"-on {output node filename}: (defualt) node.ref.dat"
    write(*,"(a)")"-oe {output elem filename}: (defualt) elem.ref.dat"
    write(*,"(a)")"-h  : help"
    stop monolis_success
  endif

  finname = "node.dat"
  call monolis_get_arg_input_in_tag(finname, is_get)

  fiename = "elem.dat"
  call monolis_get_arg_input_ie_tag(fiename, is_get)

  fonname = "node.ref.dat"
  call monolis_get_arg_input_S("-on", fonname, is_get)
  call monolis_std_log_string2("[output node file]", fonname)

  foename = "elem.ref.dat"
  call monolis_get_arg_input_S("-oe", foename, is_get)
  call monolis_std_log_string2("[output elem file]", foename)

  call monolis_input_node(finname, n_node, node)

  call monolis_input_elem(fiename, n_elem, n_base, elem)

  if(n_base /= 4)then
    call monolis_std_error_string("please input 1st order tet mesh")
    call monolis_std_error_stop()
  endif

  call monolis_check_fortran_1_origin_elem(elem, is_1_origin)

  if(.not. is_1_origin) elem = elem + 1

  call monolis_p_refine_tet(n_node, node, n_elem, elem, n_node_ref, node_ref, elem_ref)

  call monolis_output_node(fonname, n_node_ref, node_ref)

  if(.not. is_1_origin) elem_ref = elem_ref - 1

  call monolis_output_elem(foename, n_elem, 10, elem_ref)

  call monolis_mpi_finalize()
end program monolis_p_refiner_tet
