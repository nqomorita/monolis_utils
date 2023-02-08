!> @ingroup driver
program monolis_p_refiner_hex
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
  integer(kint) :: n_node, n_elem, n_base
  logical :: is_get
  integer(kint), allocatable :: elem(:,:)
  real(kdouble), allocatable :: node(:,:)

  call monolis_mpi_initialize()

  call monolis_std_debug_log_header("monolis_p_refiner_hex")

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./monolis_p_refiner_hex {options}"
    write(*,"(a)")""
    write(*,"(a)")"-in {input node filename}: (defualt) node.dat"
    write(*,"(a)")"-ie {input elem filename}: (defualt) elem.dat"
    write(*,"(a)")"-on {output node filename}: (defualt) node.ref.dat"
    write(*,"(a)")"-oe {output elem filename}: (defualt) elem.ref.dat"
    write(*,"(a)")"-h  : help"
    stop monolis_success
  endif

  finname = "node.dat"
  call monolis_get_arg_input_i_tag(finname)

  fiename = "elem.dat"
  call monolis_get_arg_input_i_tag(fiename)

  fonname = "node.ref.dat"
  call monolis_get_arg_input_S("-on", fonname, is_get)
  call monolis_std_log_string2("[output node file]", fonname)

  foename = "elem.ref.dat"
  call monolis_get_arg_input_S("-oe", foename, is_get)
  call monolis_std_log_string2("[output elem file]", foename)

  call monolis_input_node(finname, n_node, node)

  call monolis_input_elem(fiename, n_elem, n_base, elem)

  if(n_base /= 8)then
    call monolis_std_error_string("please input 1st order hex mesh")
    call monolis_std_error_stop()
  endif

  !call monolis_output_elem(foname, n_surf_elem, 3, surf)

  call monolis_mpi_finalize()
end program monolis_p_refiner_hex
