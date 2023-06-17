module mod_monolis_refiner_util
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_hash
  use mod_monolis_utils_alloc
  use mod_monolis_shape_c3d4
  use mod_monolis_shape_c3d8
  use mod_monolis_extract_util
  implicit none

contains

  !> @ingroup dev_driver
  !> @brief 四面体一次要素の h 方向のリファイン
  subroutine monolis_h_refine_tet(n_node, node, n_elem, elem, n_node_ref, node_ref, n_elem_ref, elem_ref)
    implicit none
    !> [in] 節点数
    integer(kint), intent(in) :: n_node
    !> [in] 節点座標
    real(kdouble), intent(in) :: node(:,:)
    !> [in] 要素数
    integer(kint), intent(in) :: n_elem
    !> [in] 要素コネクティビティ
    integer(kint), intent(in) :: elem(:,:)
    !> [out] リファイン後の節点数
    integer(kint), intent(out) :: n_node_ref
    !> [out] リファイン後の節点座標
    real(kdouble), allocatable, intent(out) :: node_ref(:,:)
    !> [out] リファイン後の要素数
    integer(kint), intent(out) :: n_elem_ref
    !> [out] リファイン後の要素コネクティビティ
    integer(kint), allocatable, intent(out) :: elem_ref(:,:)
    type(monolis_hash_structure) :: hash_tree
    integer(kint) :: tmp, nid(6)
    integer(kint) :: i, i1, i2, i3, eid, newid, conn(4)
    real(kdouble) :: pos(3,1)
    character :: ckey*18, ckey1*9, ckey2*9
    logical :: is_exist, is_pushed

    call monolis_hash_init(hash_tree, 18)

    call monolis_alloc_I_2d(elem_ref, 4, 8*n_elem)
    call monolis_alloc_R_2d(node_ref, 3, n_node)
    node_ref = node
    n_elem_ref = 8*n_elem

    newid = 0
    do eid = 1, n_elem
      conn = elem(:,eid)
      do i = 1, 6
        i1 = conn(monolis_C3D4_edge(1, i))
        i2 = conn(monolis_C3D4_edge(2, i))

        if(i1 < i2)then
          i3 = i1
          i1 = i2
          i2 = i3
        endif

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

      !> elem 1
      elem_ref(1,8*eid - 7) = conn(1)
      elem_ref(2,8*eid - 7) = nid(1)
      elem_ref(3,8*eid - 7) = nid(3)
      elem_ref(4,8*eid - 7) = nid(4)
      !> elem 2
      elem_ref(1,8*eid - 6) = nid(1)
      elem_ref(2,8*eid - 6) = conn(2)
      elem_ref(3,8*eid - 6) = nid(2)
      elem_ref(4,8*eid - 6) = nid(5)
      !> elem 3
      elem_ref(1,8*eid - 5) = nid(3)
      elem_ref(2,8*eid - 5) = nid(2)
      elem_ref(3,8*eid - 5) = conn(3)
      elem_ref(4,8*eid - 5) = nid(6)
      !> elem 4
      elem_ref(1,8*eid - 4) = nid(4)
      elem_ref(2,8*eid - 4) = nid(5)
      elem_ref(3,8*eid - 4) = nid(6)
      elem_ref(4,8*eid - 4) = conn(4)
      !> elem 5
      elem_ref(1,8*eid - 3) = nid(1)
      elem_ref(2,8*eid - 3) = nid(5)
      elem_ref(3,8*eid - 3) = nid(3)
      elem_ref(4,8*eid - 3) = nid(4)
      !> elem 6
      elem_ref(1,8*eid - 2) = nid(5)
      elem_ref(2,8*eid - 2) = nid(6)
      elem_ref(3,8*eid - 2) = nid(3)
      elem_ref(4,8*eid - 2) = nid(4)
      !> elem 7
      elem_ref(1,8*eid - 1) = nid(1)
      elem_ref(2,8*eid - 1) = nid(2)
      elem_ref(3,8*eid - 1) = nid(3)
      elem_ref(4,8*eid - 1) = nid(5)
      !> elem 8
      elem_ref(1,8*eid    ) = nid(3)
      elem_ref(2,8*eid    ) = nid(2)
      elem_ref(3,8*eid    ) = nid(6)
      elem_ref(4,8*eid    ) = nid(5)
    enddo

    n_node_ref = n_node + newid

    call monolis_hash_finalize(hash_tree)
  end subroutine monolis_h_refine_tet

  !> @ingroup dev_driver
  !> @brief 六面体一次要素の h 方向のリファイン
  subroutine monolis_h_refine_hex(n_node, node, n_elem, elem, n_node_ref, node_ref, n_elem_ref, elem_ref)
    implicit none
    !> [in] 節点数
    integer(kint), intent(in) :: n_node
    !> [in] 節点座標
    real(kdouble), intent(in) :: node(:,:)
    !> [in] 要素数
    integer(kint), intent(in) :: n_elem
    !> [in] 要素コネクティビティ
    integer(kint), intent(in) :: elem(:,:)
    !> [out] リファイン後の節点数
    integer(kint), intent(out) :: n_node_ref
    !> [out] リファイン後の節点座標
    real(kdouble), allocatable, intent(out) :: node_ref(:,:)
    !> [out] リファイン後の要素数
    integer(kint), intent(out) :: n_elem_ref
    !> [out] リファイン後の要素コネクティビティ
    integer(kint), allocatable, intent(out) :: elem_ref(:,:)
    type(monolis_hash_structure) :: hash_tree
    integer(kint) :: tmp, nid(19)
    integer(kint) :: i, i1, i2, i3, j, eid, newid, conn(8)
    real(kdouble) :: pos(3,1)
    character :: ckey*27, ckey1*9, ckey2*9, ckey3*9
    logical :: is_exist, is_pushed

    call monolis_hash_init(hash_tree, 27)

    call monolis_alloc_I_2d(elem_ref, 8, 8*n_elem)
    call monolis_alloc_R_2d(node_ref, 3, n_node)
    node_ref = node
    n_elem_ref = 8*n_elem

    newid = 0
    do eid = 1, n_elem
      conn = elem(:,eid)
      nid = 0
      !> node on edge
      do i = 1, 12
        i1 = conn(monolis_C3D8_edge(1, i))
        i2 = conn(monolis_C3D8_edge(2, i))

        if(i1 < i2)then
          i3 = i1
          i1 = i2
          i2 = i3
        endif

        call monolis_hash_get_key_I(9, i1, ckey1)
        call monolis_hash_get_key_I(9, i2, ckey2)
        call monolis_hash_get_key_I(9,  0, ckey3)
        ckey = ckey1//ckey2//ckey3

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

      !> node on surf center
      do i = 1, 6
        ckey = get_key_surf(8, i, conn)

        is_exist = .false.
        call monolis_hash_get(hash_tree, ckey, tmp, is_exist)

        if(is_exist)then
          nid(i + 12) = tmp
        else
          newid = newid + 1
          nid(i + 12) = n_node + newid
          call monolis_hash_push(hash_tree, ckey, nid(i + 12), is_pushed, is_exist)
          pos = 0.0d0
          do j = 1, 4
            i1 = conn(monolis_C3D8_surf(j,i))
            pos(:,1) = pos(:,1) + 0.25d0*node(:,i1)
          enddo
          call monolis_append_R_2d(node_ref, 1, pos)
        endif
      enddo

      !> node on volume center
      newid = newid + 1
      nid(19) = n_node + newid
      pos = 0.0d0
      do j = 1, 8
        pos(:,1) = pos(:,1) + 0.125d0*node(:,conn(j))
      enddo
      call monolis_append_R_2d(node_ref, 1, pos)

      !> elem 1
      elem_ref(1,8*eid - 7) = conn(1)
      elem_ref(2,8*eid - 7) = nid(1)
      elem_ref(3,8*eid - 7) = nid(13)
      elem_ref(4,8*eid - 7) = nid(4)
      elem_ref(5,8*eid - 7) = nid(9)
      elem_ref(6,8*eid - 7) = nid(15)
      elem_ref(7,8*eid - 7) = nid(19)
      elem_ref(8,8*eid - 7) = nid(18)
      !> elem 2
      elem_ref(1,8*eid - 6) = nid(1)
      elem_ref(2,8*eid - 6) = conn(2)
      elem_ref(3,8*eid - 6) = nid(2)
      elem_ref(4,8*eid - 6) = nid(13)
      elem_ref(5,8*eid - 6) = nid(15)
      elem_ref(6,8*eid - 6) = nid(10)
      elem_ref(7,8*eid - 6) = nid(16)
      elem_ref(8,8*eid - 6) = nid(19)
      !> elem 3
      elem_ref(1,8*eid - 5) = nid(4)
      elem_ref(2,8*eid - 5) = nid(13)
      elem_ref(3,8*eid - 5) = nid(3)
      elem_ref(4,8*eid - 5) = conn(4)
      elem_ref(5,8*eid - 5) = nid(18)
      elem_ref(6,8*eid - 5) = nid(19)
      elem_ref(7,8*eid - 5) = nid(17)
      elem_ref(8,8*eid - 5) = nid(12)
      !> elem 4
      elem_ref(1,8*eid - 4) = nid(13)
      elem_ref(2,8*eid - 4) = nid(2)
      elem_ref(3,8*eid - 4) = conn(3)
      elem_ref(4,8*eid - 4) = nid(3)
      elem_ref(5,8*eid - 4) = nid(19)
      elem_ref(6,8*eid - 4) = nid(16)
      elem_ref(7,8*eid - 4) = nid(11)
      elem_ref(8,8*eid - 4) = nid(17)
      !> elem 5
      elem_ref(1,8*eid - 3) = nid(9)
      elem_ref(2,8*eid - 3) = nid(15)
      elem_ref(3,8*eid - 3) = nid(19)
      elem_ref(4,8*eid - 3) = nid(18)
      elem_ref(5,8*eid - 3) = conn(5)
      elem_ref(6,8*eid - 3) = nid(5)
      elem_ref(7,8*eid - 3) = nid(14)
      elem_ref(8,8*eid - 3) = nid(8)
      !> elem 6
      elem_ref(1,8*eid - 2) = nid(15)
      elem_ref(2,8*eid - 2) = nid(10)
      elem_ref(3,8*eid - 2) = nid(16)
      elem_ref(4,8*eid - 2) = nid(19)
      elem_ref(5,8*eid - 2) = nid(5)
      elem_ref(6,8*eid - 2) = conn(6)
      elem_ref(7,8*eid - 2) = nid(6)
      elem_ref(8,8*eid - 2) = nid(14)
      !> elem 7
      elem_ref(1,8*eid - 1) = nid(18)
      elem_ref(2,8*eid - 1) = nid(19)
      elem_ref(3,8*eid - 1) = nid(17)
      elem_ref(4,8*eid - 1) = nid(12)
      elem_ref(5,8*eid - 1) = nid(8)
      elem_ref(6,8*eid - 1) = nid(14)
      elem_ref(7,8*eid - 1) = nid(7)
      elem_ref(8,8*eid - 1) = conn(8)
      !> elem 8
      elem_ref(1,8*eid    ) = nid(19)
      elem_ref(2,8*eid    ) = nid(16)
      elem_ref(3,8*eid    ) = nid(11)
      elem_ref(4,8*eid    ) = nid(17)
      elem_ref(5,8*eid    ) = nid(14)
      elem_ref(6,8*eid    ) = nid(6)
      elem_ref(7,8*eid    ) = conn(7)
      elem_ref(8,8*eid    ) = nid(7)
    enddo

    n_node_ref = n_node + newid

    call monolis_hash_finalize(hash_tree)
  end subroutine monolis_h_refine_hex

  !> @ingroup dev_driver
  !> @brief 四面体一次要素の p 方向のリファイン
  subroutine monolis_p_refine_tet(n_node, node, n_elem, elem, n_node_ref, node_ref, elem_ref)
    implicit none
    !> [in] 節点数
    integer(kint), intent(in) :: n_node
    !> [in] 節点座標
    real(kdouble), intent(in) :: node(:,:)
    !> [in] 要素数
    integer(kint), intent(in) :: n_elem
    !> [in] 要素コネクティビティ
    integer(kint), intent(in) :: elem(:,:)
    !> [out] リファイン後の節点数
    integer(kint), intent(out) :: n_node_ref
    !> [out] リファイン後の節点座標
    real(kdouble), allocatable, intent(out) :: node_ref(:,:)
    !> [out] リファイン後の要素コネクティビティ
    integer(kint), allocatable, intent(out) :: elem_ref(:,:)
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
