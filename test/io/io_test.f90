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
    integer(kint) :: i_ans(6)
    integer(kint), allocatable :: vertex_id(:)
    integer(kint), allocatable :: index(:)
    integer(kint), allocatable :: item(:)

    call monolis_std_log_string("monolis_input_graph_test")

    call monolis_input_graph("io/input/graph.txt", n_vertex, vertex_id, index, item)

    call monolis_test_check_eq_I1("monolis_input_graph_test 1", n_vertex, 3)

    i_ans(1) = 1
    i_ans(2) = 2
    i_ans(3) = 3
    call monolis_test_check_eq_I ("monolis_input_graph_test 2", vertex_id, i_ans(1:3))

    i_ans(1) = 0
    i_ans(2) = 2
    i_ans(3) = 4
    i_ans(4) = 6
    call monolis_test_check_eq_I ("monolis_input_graph_test 3", index, i_ans(1:4))

    i_ans(1) = 1
    i_ans(2) = 2
    i_ans(3) = 2
    i_ans(4) = 3
    i_ans(5) = 3
    i_ans(6) = 4
    call monolis_test_check_eq_I ("monolis_input_graph_test 4", item, i_ans(1:6))
  end subroutine monolis_input_graph_test

  subroutine monolis_output_graph_test()
    implicit none
    integer(kint) :: n_vertex, n_vertex_ans
    integer(kint) :: vertex_id(3)
    integer(kint) :: index(4)
    integer(kint) :: item(6)
    integer(kint), allocatable :: vertex_id_ans(:)
    integer(kint), allocatable :: index_ans(:)
    integer(kint), allocatable :: item_ans(:)

    call monolis_std_log_string("monolis_output_graph_test")

    n_vertex = 3
    vertex_id(1) = 1
    vertex_id(2) = 2
    vertex_id(3) = 3
    index(1) = 0
    index(2) = 2
    index(3) = 4
    index(4) = 6
    item(1) = 1
    item(2) = 2
    item(3) = 2
    item(4) = 3
    item(5) = 3
    item(6) = 4

    call monolis_output_graph("io/input/graph.txt.out", n_vertex, vertex_id, index, item)

    call monolis_input_graph("io/input/graph.txt.out", n_vertex_ans, vertex_id_ans, index_ans, item_ans)

    call monolis_test_check_eq_I1("monolis_output_graph_test 1", n_vertex, n_vertex_ans)

    call monolis_test_check_eq_I ("monolis_output_graph_test 2", vertex_id, vertex_id_ans)

    call monolis_test_check_eq_I ("monolis_output_graph_test 3", index, index_ans)

    call monolis_test_check_eq_I ("monolis_output_graph_test 4", item, item_ans)
  end subroutine monolis_output_graph_test

  subroutine monolis_input_node_test()
    implicit none
    integer(kint) :: n_node
    real(kdouble) :: r_ans(3)
    real(kdouble), allocatable :: node(:,:)

    call monolis_std_log_string("monolis_input_node_test")

    call monolis_input_node("io/input/node.txt", n_node, node)

    call monolis_test_check_eq_I1("monolis_input_node_test 1", n_node, 3)

    r_ans(1) = 1.0d0
    r_ans(2) = 2.0d0
    r_ans(3) = 3.0d0
    call monolis_test_check_eq_R ("monolis_input_node_test 2", node(:,1), r_ans)

    r_ans(1) = 4.0d0
    r_ans(2) = 5.0d0
    r_ans(3) = 6.0d0
    call monolis_test_check_eq_R ("monolis_input_node_test 3", node(:,2), r_ans)

    r_ans(1) = 7.0d0
    r_ans(2) = 8.0d0
    r_ans(3) = 9.0d0
    call monolis_test_check_eq_R ("monolis_input_node_test 4", node(:,3), r_ans)
  end subroutine monolis_input_node_test

  subroutine monolis_output_node_test()
    implicit none
    integer(kint) :: n_node, n_node_ans
    real(kdouble) :: node(3,3)
    real(kdouble), allocatable :: node_ans(:,:)

    call monolis_std_log_string("monolis_output_node_test")

    n_node = 3
    node(1,1) = 1.0d0; node(2,1) = 2.0d0; node(3,1) = 3.0d0
    node(1,2) = 4.0d0; node(2,2) = 5.0d0; node(3,2) = 6.0d0
    node(1,3) = 7.0d0; node(2,3) = 8.0d0; node(3,3) = 9.0d0

    call monolis_output_node("io/input/node.txt.out", n_node, node)

    call monolis_input_node("io/input/node.txt.out", n_node_ans, node_ans)

    call monolis_test_check_eq_I1("monolis_output_node_test 1", n_node, n_node_ans)

    call monolis_test_check_eq_R ("monolis_output_node_test 2", node(:,1), node_ans(:,1))

    call monolis_test_check_eq_R ("monolis_output_node_test 3", node(:,2), node_ans(:,2))

    call monolis_test_check_eq_R ("monolis_output_node_test 4", node(:,3), node_ans(:,3))
  end subroutine monolis_output_node_test

  subroutine monolis_input_elem_test()
    implicit none
    integer(kint) :: n_elem
    integer(kint) :: n_base
    integer(kint) :: i_ans(2)
    integer(kint), allocatable :: elem(:,:)

    call monolis_std_log_string("monolis_input_elem_test")

    call monolis_input_elem("io/input/elem.txt", n_elem, n_base, elem)

    call monolis_test_check_eq_I1("monolis_input_elem_test 1", n_elem, 3)

    call monolis_test_check_eq_I1("monolis_input_elem_test 2", n_base, 2)

    i_ans(1) = 1
    i_ans(2) = 2
    call monolis_test_check_eq_I ("monolis_input_elem_test 3", elem(:,1), i_ans)

    i_ans(1) = 2
    i_ans(2) = 3
    call monolis_test_check_eq_I ("monolis_input_elem_test 4", elem(:,2), i_ans)

    i_ans(1) = 3
    i_ans(2) = 4
    call monolis_test_check_eq_I ("monolis_input_elem_test 5", elem(:,3), i_ans)
  end subroutine monolis_input_elem_test

  subroutine monolis_output_elem_test()
    implicit none
    integer(kint) :: n_elem, n_elem_ans
    integer(kint) :: n_base, n_base_ans
    integer(kint) :: elem(2,3)
    integer(kint), allocatable :: elem_ans(:,:)

    call monolis_std_log_string("monolis_output_elem_test")

    n_elem = 3
    n_base = 2
    elem(1,1) = 1; elem(2,1) = 2;
    elem(1,2) = 2; elem(2,2) = 3;
    elem(1,3) = 3; elem(2,3) = 4;

    call monolis_output_elem("io/input/elem.txt.out", n_elem, n_base, elem)

    call monolis_input_elem("io/input/elem.txt.out", n_elem_ans, n_base_ans, elem_ans)

    call monolis_test_check_eq_I1("monolis_output_elem_test 1", n_elem, n_elem_ans)

    call monolis_test_check_eq_I1("monolis_output_elem_test 2", n_base, n_base_ans)

    call monolis_test_check_eq_I ("monolis_output_elem_test 3", elem(1,:), elem_ans(1,:))

    call monolis_test_check_eq_I ("monolis_output_elem_test 3", elem(2,:), elem_ans(2,:))
  end subroutine monolis_output_elem_test

  subroutine monolis_input_internal_vertex_number_test()
    implicit none
    integer(kint) :: n_internal_vertex

    call monolis_std_log_string("monolis_input_internal_vertex_number_test")

    call monolis_input_internal_vertex_number("io/input/n_internal.txt", n_internal_vertex)

    call monolis_test_check_eq_I1("monolis_input_internal_vertex_number_test 1", n_internal_vertex, 5)
  end subroutine monolis_input_internal_vertex_number_test

  subroutine monolis_output_internal_vertex_number_test()
    implicit none
    integer(kint) :: n_internal_vertex, n_internal_vertex_ans

    call monolis_std_log_string("monolis_output_internal_vertex_number_test")

    n_internal_vertex = 5
    call monolis_output_internal_vertex_number("io/input/n_internal.txt.out", n_internal_vertex)

    call monolis_input_internal_vertex_number("io/input/n_internal.txt.out", n_internal_vertex_ans)

    call monolis_test_check_eq_I1("monolis_output_internal_vertex_number_test 1", &
      & n_internal_vertex, n_internal_vertex_ans)
  end subroutine monolis_output_internal_vertex_number_test

  subroutine monolis_input_bc_test()
    implicit none
    integer(kint) :: n_bc
    integer(kint) :: n_dof
    integer(kint) :: i_ans(3)
    real(kdouble) :: r_ans(3)
    integer(kint), allocatable :: i_bc(:,:)
    real(kdouble), allocatable :: r_bc(:)

    call monolis_std_log_string("monolis_input_bc_test")

    call monolis_input_bc("io/input/bc.txt", n_bc, n_dof, i_bc, r_bc)

    call monolis_test_check_eq_I1("monolis_input_bc_test 1", n_bc, 3)

    call monolis_test_check_eq_I1("monolis_input_bc_test 2", n_dof, 2)

    i_ans(1) = 1
    i_ans(2) = 2
    i_ans(3) = 3
    call monolis_test_check_eq_I ("monolis_input_bc_test 3", i_bc(1,:), i_ans)

    i_ans(1) = 2
    i_ans(2) = 3
    i_ans(3) = 4
    call monolis_test_check_eq_I ("monolis_input_bc_test 4", i_bc(2,:), i_ans)

    r_ans(1) = 3.0d0
    r_ans(2) = 4.0d0
    r_ans(3) = 5.0d0
    call monolis_test_check_eq_R ("monolis_input_bc_test 5", r_bc, r_ans)
  end subroutine monolis_input_bc_test

  subroutine monolis_output_bc_test()
    implicit none
    integer(kint) :: n_bc, n_bc_ans
    integer(kint) :: n_dof, n_dof_ans
    integer(kint) :: i_bc(2,3)
    real(kdouble) :: r_bc(3)
    integer(kint), allocatable :: i_bc_ans(:,:)
    real(kdouble), allocatable :: r_bc_ans(:)

    call monolis_std_log_string("monolis_output_bc_test")

    n_bc = 3
    n_dof = 2
    i_bc(1,1) = 1; i_bc(2,1) = 2
    i_bc(1,2) = 2; i_bc(2,2) = 3
    i_bc(1,3) = 3; i_bc(2,3) = 4
    r_bc(1) = 3.0d0
    r_bc(2) = 4.0d0
    r_bc(3) = 5.0d0

    call monolis_output_bc("io/input/bc.txt.out", n_bc, n_dof, i_bc, r_bc)

    call monolis_input_bc("io/input/bc.txt.out", n_bc_ans, n_dof_ans, i_bc_ans, r_bc_ans)

    call monolis_test_check_eq_I1("monolis_output_bc_test 1", n_bc, n_bc_ans)

    call monolis_test_check_eq_I1("monolis_output_bc_test 2", n_dof, n_dof_ans)

    call monolis_test_check_eq_I ("monolis_output_bc_test 3", i_bc(1,:), i_bc_ans(1,:))

    call monolis_test_check_eq_I ("monolis_output_bc_test 4", i_bc(2,:), i_bc_ans(2,:))

    call monolis_test_check_eq_R ("monolis_output_bc_test 5", r_bc, r_bc_ans)
  end subroutine monolis_output_bc_test

  subroutine monolis_input_distval_i_test()
    implicit none
    character(monolis_charlen) :: label
    integer(kint) :: n_node
    integer(kint) :: n_dof
    integer(kint) :: i_ans(3)
    integer(kint), allocatable :: val(:,:)

    call monolis_std_log_string("monolis_input_distval_i_test")

    call monolis_input_distval_i("io/input/distval_i.txt", label, n_node, n_dof, val)

    if(trim(label) == "#val")then
      call monolis_test_assert_pass("monolis_input_distval_i_test 1")
    else
      call monolis_test_assert_fail("monolis_input_distval_i_test 1", "")
    endif

    call monolis_test_check_eq_I1("monolis_input_distval_i_test 2", n_node, 3)

    call monolis_test_check_eq_I1("monolis_input_distval_i_test 3", n_dof, 1)

    i_ans(1) = 3
    i_ans(2) = 4
    i_ans(3) = 5
    call monolis_test_check_eq_I ("monolis_input_distval_i_test 4", val(1,:), i_ans)
  end subroutine monolis_input_distval_i_test

  subroutine monolis_output_distval_i_test()
    implicit none
    character(monolis_charlen) :: label, label_ans
    integer(kint) :: n_node, n_node_ans
    integer(kint) :: n_dof, n_dof_ans
    integer(kint) :: val(1,3)
    integer(kint), allocatable :: val_ans(:,:)

    call monolis_std_log_string("monolis_output_distval_i_test")

    label = "#val"
    n_node = 3
    n_dof = 1
    val(1,1) = 3
    val(1,2) = 4
    val(1,3) = 5

    call monolis_output_distval_i("io/input/distval_i.txt.out", label, n_node, n_dof, val)

    call monolis_input_distval_i("io/input/distval_i.txt.out", label_ans, n_node_ans, n_dof_ans, val_ans)

    if(trim(label) == trim(label_ans))then
      call monolis_test_assert_pass("monolis_output_distval_i_test 1")
    else
      call monolis_test_assert_fail("monolis_output_distval_i_test 1", "")
    endif

    call monolis_test_check_eq_I1("monolis_output_distval_i_test 2", n_node, n_node_ans)

    call monolis_test_check_eq_I1("monolis_output_distval_i_test 3", n_dof, n_dof_ans)

    call monolis_test_check_eq_I ("monolis_output_distval_i_test 4", val(1,:), val_ans(1,:))
  end subroutine monolis_output_distval_i_test

  subroutine monolis_input_distval_r_test()
    implicit none
    character(monolis_charlen) :: label
    integer(kint) :: n_node
    integer(kint) :: n_dof
    real(kdouble) :: r_ans(3)
    real(kdouble), allocatable :: val(:,:)

    call monolis_std_log_string("monolis_input_distval_r_test")

    call monolis_input_distval_r("io/input/distval_r.txt", label, n_node, n_dof, val)

    if(trim(label) == "#val")then
      call monolis_test_assert_pass("monolis_input_distval_r_test 1")
    else
      call monolis_test_assert_fail("monolis_input_distval_r_test 1", "")
    endif

    call monolis_test_check_eq_I1("monolis_input_distval_r_test 2", n_node, 3)

    call monolis_test_check_eq_I1("monolis_input_distval_r_test 3", n_dof, 1)

    r_ans(1) = 3.0d0
    r_ans(2) = 4.0d0
    r_ans(3) = 5.0d0
    call monolis_test_check_eq_R ("monolis_input_distval_r_test 4", val(1,:), r_ans)
  end subroutine monolis_input_distval_r_test

  subroutine monolis_output_distval_r_test()
    implicit none
    character(monolis_charlen) :: label, label_ans
    integer(kint) :: n_node, n_node_ans
    integer(kint) :: n_dof, n_dof_ans
    real(kdouble) :: val(1,3)
    real(kdouble), allocatable :: val_ans(:,:)

    call monolis_std_log_string("monolis_output_distval_r_test")

    label = "#val"
    n_node = 3
    n_dof = 1
    val(1,1) = 3.0d0
    val(1,2) = 4.0d0
    val(1,3) = 5.0d0

    call monolis_output_distval_r("io/input/distval_r.txt.out", label, n_node, n_dof, val)

    call monolis_input_distval_r("io/input/distval_r.txt.out", label_ans, n_node_ans, n_dof_ans, val_ans)

    if(trim(label) == trim(label_ans))then
      call monolis_test_assert_pass("monolis_output_distval_r_test 1")
    else
      call monolis_test_assert_fail("monolis_output_distval_r_test 1", "")
    endif

    call monolis_test_check_eq_I1("monolis_output_distval_r_test 2", n_node, n_node_ans)

    call monolis_test_check_eq_I1("monolis_output_distval_r_test 3", n_dof, n_dof_ans)

    call monolis_test_check_eq_R ("monolis_output_distval_r_test 4", val(1,:), val_ans(1,:))
  end subroutine monolis_output_distval_r_test
end module mod_monolis_io_test
