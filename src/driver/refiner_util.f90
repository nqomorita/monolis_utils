module mod_monolis_refiner_util
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_hash
  use mod_monolis_utils_alloc
  use mod_monolis_shape_c3d4
  use mod_monolis_shape_c3d8
  implicit none

contains

  subroutine monolis_p_refine_tet(n_node, node, n_elem, elem, n_node_ref, node_ref, elem_ref)
    implicit none
    !> [in] 節点数
    integer(kint) :: n_node
    !> [in] 節点座標
    real(kdouble) :: node(:,:)
    !> [in] 要素の数
    integer(kint) :: n_elem
    !> [in] 要素
    integer(kint) :: elem(:,:)
    !> [out] リファイン後の節点数
    integer(kint) :: n_node_ref
    !> [out] リファイン後の節点
    real(kdouble), allocatable :: node_ref(:,:)
    !> [inout] リファイン後の要素
    integer(kint), allocatable :: elem_ref(:,:)
    type(monolis_hash_structure) :: hash_tree
    integer(kint) :: tmp, nid(6)
    integer(kint) :: i, i1, i2, eid, newid, conn(4)
    real(kdouble) :: pos(3,1)
    character :: ckey*18, ckey1*9, ckey2*9
    logical :: is_exist, is_pushed

    call monolis_hash_init(hash_tree, 18)

    call monolis_alloc_I_2d(elem_ref, 10, n_elem)
    call monolis_alloc_R_2d(node_ref, 3, n_node)
    node_ref = node

    newid = 0
    do eid = 1, n_elem
      conn = elem(:,eid)
      do i = 1, 6
        i1 = conn(monolis_C3D4_edge(1, i))
        i2 = conn(monolis_C3D4_edge(2, i))
        call monolis_hash_get_key_I(9, i1, ckey1)
        call monolis_hash_get_key_I(9, i2, ckey2)
        ckey = ckey1//ckey2

        is_exist = .false.
        call monolis_hash_get(hash_tree, ckey, tmp, is_exist)

        if(is_exist)then
          nid(i) = tmp
        else
          newid = newid + 1
          nid(i) = n_node + newid
          call monolis_hash_push(hash_tree, ckey, nid(i), is_pushed, is_exist)
          pos(:,1) = 0.5d0*(node(:,i1) + node(:,i2))
          call monolis_append_R_2d(node_ref, 1, pos)
        endif
      enddo
      elem_ref( 1,eid) = conn(1)
      elem_ref( 2,eid) = conn(2)
      elem_ref( 3,eid) = conn(3)
      elem_ref( 4,eid) = conn(4)
      elem_ref( 5,eid) = nid(1)
      elem_ref( 6,eid) = nid(2)
      elem_ref( 7,eid) = nid(3)
      elem_ref( 8,eid) = nid(4)
      elem_ref( 9,eid) = nid(5)
      elem_ref(10,eid) = nid(6)
    enddo

    n_node_ref = n_node + newid

    call monolis_hash_finalize(hash_tree)
  end subroutine monolis_p_refine_tet
end module mod_monolis_refiner_util
