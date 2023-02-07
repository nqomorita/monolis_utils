module mod_monolis_extract_all_util
  use mod_monolis_utils
  implicit none

  type(monolis_hash_structure) :: hash_tree

contains

  !> @ingroup dev_driver
  !> 表面要素を抽出
  subroutine monolis_get_surf(n_node, n_elem, n_base, elem, n_surf, &
    & n_base_out, n_elem_out, out)
    implicit none
    integer(kint) :: n_node, n_elem, n_base, conn(n_base)
    integer(kint) :: n_surf, n_node_out, n_elem_out, n_base_out
    integer(kint) :: elem(:,:)
    integer(kint) :: i, j, in, eid
    integer(kint), allocatable :: out(:,:)
    integer(kint), allocatable :: is_inner(:,:)

    call monolis_get_surf_main(n_elem, n_base, elem, n_surf, is_inner)

    n_elem_out = 0
    do eid = 1, n_elem
      do j = 1, n_surf
        if(is_inner(j,eid) == 0) n_elem_out = n_elem_out + 1
      enddo
    enddo

    call monolis_alloc_I_2d(out, n_base_out, n_elem_out)

    in = 0
    n_elem_out = 0
    do eid = 1, n_elem
      conn = elem(:,eid)
      do j = 1, n_surf
        if(is_inner(j,eid) == 0)then
          n_elem_out = n_elem_out + 1
          do i = 1, n_base_out
            if(n_base == 4)then
              in = conn(monolis_C3D4_surf(i,j))
            elseif(n_base == 8)then
              in = conn(monolis_C3D8_surf(i,j))
            endif
            out(i,j) = in
          enddo
        endif
      enddo
    enddo
  end subroutine monolis_get_surf

  !> @ingroup dev_driver
  !> 表面要素を抽出（メイン関数）
  subroutine monolis_get_surf_main(n_elem, n_base, elem, n_surf, is_inner)
    implicit none
    integer(kint) :: n_elem, n_base, conn(n_base)
    integer(kint) :: n_surf
    integer(kint) :: elem(:,:)
    integer(kint) :: i, in, eid
    character :: ckey*27
    logical :: is_exist, is_pushed
    integer(kint), allocatable :: is_inner(:,:)

    call monolis_hash_init(hash_tree, 27)

    do eid = 1, n_elem
      conn = elem(:,eid)
      do i = 1, n_surf
        ckey = get_key_surf(n_base, i, conn)
        is_exist = .false.
        call monolis_hash_get(hash_tree, ckey, in, is_exist)
        if(is_exist)then
          in = in + 1
        else
          in = 1
        endif
        call monolis_hash_push(hash_tree, ckey, in, is_pushed, is_exist)
      enddo
    enddo

    call monolis_alloc_I_2d(is_inner, n_surf, n_elem)

    do eid = 1, n_elem
      conn = elem(:,eid)
      do i = 1, n_surf
        ckey = get_key_surf(n_base, i, conn)
        is_exist = .false.
        call monolis_hash_get(hash_tree, ckey, in, is_exist)
        if(.not. is_exist) stop "error: monolis_get_surf_main"
        if(in == 2)then
          is_inner(i,eid) = 1
        endif
      enddo
    enddo

    call monolis_hash_finalize(hash_tree)
  end subroutine monolis_get_surf_main

  !> @ingroup dev_driver
  function get_key_surf(n_base, i, conn)
    implicit none
    integer(kint) :: n_base, i, conn(:), array(4)
    integer(kint) :: i1, i2, i3, i4
    character :: c1*9, c2*9, c3*9, get_key_surf*27

    if(n_base == 4)then
      i1 = conn(monolis_C3D4_surf(1,i))
      i2 = conn(monolis_C3D4_surf(2,i))
      i3 = conn(monolis_C3D4_surf(3,i))
      i4 = 2100000000
    elseif(n_base == 8)then
      i1 = conn(monolis_C3D8_surf(1,i))
      i2 = conn(monolis_C3D8_surf(2,i))
      i3 = conn(monolis_C3D8_surf(3,i))
      i4 = conn(monolis_C3D8_surf(4,i))
    else
      stop "error get_key_surf"
    endif

    array(1) = i1
    array(2) = i2
    array(3) = i3
    array(4) = i4
    call monolis_qsort_I_1d(array, 1, 4)

    write(c1,"(i9.9)")array(1)
    write(c2,"(i9.9)")array(2)
    write(c3,"(i9.9)")array(3)
    get_key_surf = c1//c2//c3
  end function get_key_surf
end module mod_monolis_extract_all_util
