!> IO テストモジュール
module mod_monolis_io_test
  use mod_monolis_utils_error
  use mod_monolis_utils_std_test
  use mod_monolis_io
  implicit none

contains

  subroutine monolis_io_test()
    implicit none

    call monolis_input_graph_test()
    call monolis_output_graph_test()
    call monolis_input_node_test()
    call monolis_output_node_test()
    call monolis_input_elem_test()
    call monolis_output_elem_test()
    call monolis_input_internal_vertex_number_test()
    call monolis_output_internal_vertex_number_test()
    call monolis_input_bc_test()
    call monolis_output_bc_test()
    call monolis_input_distval_i_test()
    call monolis_output_distval_i_test()
    call monolis_input_distval_r_test()
    call monolis_output_distval_r_test()
  end subroutine monolis_io_test

  subroutine monolis_input_graph_test()
    implicit none
    integer(kint) :: n_vertex
    integer(kint), allocatable :: vertex_id(:)
    integer(kint), allocatable :: index(:)
    integer(kint), allocatable :: item(:)

    call monolis_std_log_string("monolis_input_graph_test")

    call monolis_input_graph("io/input/graph.txt", n_vertex, vertex_id, index, item)
  end subroutine monolis_input_graph_test

  subroutine monolis_output_graph_test()
    implicit none

    call monolis_std_log_string("monolis_output_graph_test")

    !call monolis_output_graph(fname, n_vertex, vertex_id, index, item)
  end subroutine monolis_output_graph_test

  subroutine monolis_input_node_test()
    implicit none
    integer(kint) :: n_node
    real(kdouble), allocatable :: node(:,:)

    call monolis_std_log_string("monolis_input_node_test")

    call monolis_input_node("io/input/node.txt", n_node, node)
  end subroutine monolis_input_node_test

  subroutine monolis_output_node_test()
    implicit none

    call monolis_std_log_string("monolis_output_node_test")

    !call monolis_output_node(fname, n_node, node)
  end subroutine monolis_output_node_test

  subroutine monolis_input_elem_test()
    implicit none
    integer(kint) :: n_elem
    integer(kint) :: n_base
    integer(kint), allocatable :: elem(:,:)

    call monolis_std_log_string("monolis_input_elem_test")

    call monolis_input_elem("io/input/elem.txt", n_elem, n_base, elem)
  end subroutine monolis_input_elem_test

  subroutine monolis_output_elem_test()
    implicit none

    call monolis_std_log_string("monolis_output_elem_test")

    !call monolis_output_elem(fname, n_elem, n_base, elem)
  end subroutine monolis_output_elem_test

  subroutine monolis_input_internal_vertex_number_test()
    implicit none
    integer(kint) :: n_internal_vertex

    call monolis_std_log_string("monolis_input_internal_vertex_number_test")

    call monolis_input_internal_vertex_number("io/input/n_internal.txt", n_internal_vertex)
  end subroutine monolis_input_internal_vertex_number_test

  subroutine monolis_output_internal_vertex_number_test()
    implicit none
    integer(kint) :: n_internal_vertex

    call monolis_std_log_string("monolis_output_internal_vertex_number_test")

    !call monolis_output_internal_vertex_number(fname, n_internal_vertex)
  end subroutine monolis_output_internal_vertex_number_test

  subroutine monolis_input_bc_test()
    implicit none
    integer(kint) :: n_bc
    integer(kint) :: n_dof
    integer(kint), allocatable :: i_bc(:,:)
    real(kdouble), allocatable :: r_bc(:)

    call monolis_std_log_string("monolis_input_bc_test")

    call monolis_input_bc("io/input/bc.txt", n_bc, n_dof, i_bc, r_bc)
  end subroutine monolis_input_bc_test

  subroutine monolis_output_bc_test()
    implicit none

    call monolis_std_log_string("monolis_output_bc_test")

    !call monolis_output_bc(fname, n_bc, n_dof, i_bc, r_bc)
  end subroutine monolis_output_bc_test

  subroutine monolis_input_distval_i_test()
    implicit none
    character(monolis_charlen) :: label
    integer(kint) :: n_node
    integer(kint) :: n_dof
    integer(kint), allocatable :: val(:,:)

    call monolis_std_log_string("monolis_input_distval_i_test")

    call monolis_input_distval_i("io/input/distval_i.txt", label, n_node, n_dof, val)
  end subroutine monolis_input_distval_i_test

  subroutine monolis_output_distval_i_test()
    implicit none

    call monolis_std_log_string("monolis_output_distval_i_test")

    !call monolis_output_distval_i(fname, label, n_node, n_dof, val)
  end subroutine monolis_output_distval_i_test

  subroutine monolis_input_distval_r_test()
    implicit none
    character(monolis_charlen) :: label
    integer(kint) :: n_node
    integer(kint) :: n_dof
    real(kdouble), allocatable :: val(:,:)

    call monolis_std_log_string("monolis_input_distval_r_test")

    call monolis_input_distval_r("io/input/distval_r.txt", label, n_node, n_dof, val)
  end subroutine monolis_input_distval_r_test

  subroutine monolis_output_distval_r_test()
    implicit none

    call monolis_std_log_string("monolis_output_distval_r_test")

    !call monolis_output_distval_r(fname, label, n_node, n_dof, val)
  end subroutine monolis_output_distval_r_test
end module mod_monolis_io_test
