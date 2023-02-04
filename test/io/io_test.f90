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

    call monolis_std_log_string("monolis_input_graph_test")

    !call monolis_input_graph(fname, n_vertex, vertex_id, index, item)
  end subroutine monolis_input_graph_test

  subroutine monolis_output_graph_test()
    implicit none

    call monolis_std_log_string("monolis_output_graph_test")

    !call monolis_output_graph(fname, n_vertex, vertex_id, index, item)
  end subroutine monolis_output_graph_test

  subroutine monolis_input_node_test()
    implicit none

    call monolis_std_log_string("monolis_input_node_test")

    !call monolis_input_node(fname, n_node, node)
  end subroutine monolis_input_node_test

  subroutine monolis_output_node_test()
    implicit none

    call monolis_std_log_string("monolis_output_node_test")

    !call monolis_output_node(fname, n_node, node)
  end subroutine monolis_output_node_test

  subroutine monolis_input_elem_test()
    implicit none

    call monolis_std_log_string("monolis_input_elem_test")

    !call monolis_input_elem(fname, n_elem, n_base, elem)
  end subroutine monolis_input_elem_test

  subroutine monolis_output_elem_test()
    implicit none

    call monolis_std_log_string("monolis_output_elem_test")

    !call monolis_output_elem(fname, n_elem, n_base, elem)
  end subroutine monolis_output_elem_test

  subroutine monolis_input_internal_vertex_number_test()
    implicit none

    call monolis_std_log_string("monolis_input_internal_vertex_number_test")

    !call monolis_input_internal_vertex_number(fname, n_internal_vertex)
  end subroutine monolis_input_internal_vertex_number_test

  subroutine monolis_output_internal_vertex_number_test()
    implicit none

    call monolis_std_log_string("monolis_output_internal_vertex_number_test")

    !call monolis_output_internal_vertex_number(fname, n_internal_vertex)
  end subroutine monolis_output_internal_vertex_number_test

  subroutine monolis_input_bc_test()
    implicit none

    call monolis_std_log_string("monolis_input_bc_test")

    !call monolis_input_bc(fname, n_bc, n_dof, i_bc, r_bc)
  end subroutine monolis_input_bc_test

  subroutine monolis_output_bc_test()
    implicit none

    call monolis_std_log_string("monolis_output_bc_test")

    !call monolis_output_bc(fname, n_bc, n_dof, i_bc, r_bc)
  end subroutine monolis_output_bc_test

  subroutine monolis_input_distval_i_test()
    implicit none

    call monolis_std_log_string("monolis_input_distval_i_test")

    !call monolis_input_distval_i(fname, label, n_node, n_dof, val)
  end subroutine monolis_input_distval_i_test

  subroutine monolis_output_distval_i_test()
    implicit none

    call monolis_std_log_string("monolis_output_distval_i_test")

    !call monolis_output_distval_i(fname, label, n_node, n_dof, val)
  end subroutine monolis_output_distval_i_test

  subroutine monolis_input_distval_r_test()
    implicit none

    call monolis_std_log_string("monolis_input_distval_r_test")

    !call monolis_input_distval_r(fname, label, n_node, n_dof, val)
  end subroutine monolis_input_distval_r_test

  subroutine monolis_output_distval_r_test()
    implicit none

    call monolis_std_log_string("monolis_output_distval_r_test")

    !call monolis_output_distval_r(fname, label, n_node, n_dof, val)
  end subroutine monolis_output_distval_r_test
end module mod_monolis_io_test
