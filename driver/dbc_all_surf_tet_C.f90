!> @ingroup driver
program monolis_dbc_all_surf_tet_C
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
  complex(kdouble), allocatable :: val(:)
  integer(kint) :: n_elem, n_base, n_surf_node, n_surf_elem
  integer(kint) :: i, j, in
  logical :: is_get
  integer(kint), allocatable :: elem(:,:), surf(:,:)
  integer(kint), allocatable :: i_bc(:,:)
  integer(kint), allocatable :: node_id(:)
  complex(kdouble), allocatable :: c_bc(:)

  call monolis_mpi_initialize()

  call monolis_std_log_string("monolis_dbc_all_surf_tet_C")

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./monolis_dbc_all_surf_tet_C {options} {num of dof} {value}"
    write(*,"(a)")""
    write(*,"(a)")"options:"
    write(*,"(a)")"-i {input elem filename}: (defualt) elem.dat"
    write(*,"(a)")"-o {output filename}: (defualt) D_bc.dat"
    write(*,"(a)")"-h : help"
    stop monolis_success
  endif

  finame = "elem.dat"
  call monolis_get_arg_input_i_tag(finame, is_get)

  foname = "D_bc.dat"
  call monolis_get_arg_input_o_tag(foname, is_get)

  call monolis_driver_get_arg_dbc_all_C(n_dof, val, is_get)

  if(.not. is_get)then
    call monolis_std_error_string("input parameters are not set")
    call monolis_std_error_string("./monolis_dbc_all_surf_tet_C {options} {num of dof} {value}")
    stop monolis_fail
  endif

  call monolis_std_debug_log_I1("[n_dof]", n_dof)

  call monolis_input_elem(finame, n_elem, n_base, elem)

  if(n_base /= 4)then
    call monolis_std_error_string("please input 1st order tet mesh")
    call monolis_std_error_stop()
  endif

  call monolis_get_surf(n_elem, n_base, elem, 4, 3, n_surf_elem, surf)

  call monolis_get_surf_node(3, n_surf_elem, surf, n_surf_node, node_id)

  call monolis_alloc_I_2d(i_bc, 2, n_dof*n_surf_node)

  call monolis_alloc_C_1d(c_bc, n_dof*n_surf_node)

  do i = 1, n_surf_node
    do j = 1, n_dof
      in = n_dof*(i-1) + j
      i_bc(1,in) = node_id(i)
      i_bc(2,in) = j
      c_bc(in) = val(j)
    enddo
  enddo

  call monolis_output_bc_C(foname, n_dof*n_surf_node, n_dof, i_bc, c_bc)

  call monolis_mpi_finalize()
end program monolis_dbc_all_surf_tet_C
