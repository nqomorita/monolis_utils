module mod_monolis_extract_util
  use mod_monolis_utils_std_sort_I
  use mod_monolis_utils_alloc
  use mod_monolis_utils_hash
  use mod_monolis_shape_c3d4
  use mod_monolis_shape_c3d8
  implicit none

contains

  !> @ingroup dev_driver
  !> @brief 表面要素を抽出
  subroutine monolis_get_surf(n_elem, n_base, elem, n_surf, &
    & n_base_out, n_elem_out, out)
    implicit none
    !> [in] 要素数
    integer(kint), intent(in) :: n_elem
    !> [in] 要素を構成する形状関数の数
    integer(kint), intent(in) :: n_base
    !> [in] 要素コネクティビティ
    integer(kint), intent(in) :: elem(:,:)
    !> [in] 入力要素の面の数
    integer(kint), intent(in) :: n_surf
    !> [in] 入力要素の面の基底の数
    integer(kint), intent(in) :: n_base_out
    !> [out] 抽出された要素の要素数
    integer(kint), intent(out) :: n_elem_out
    !> [out] 抽出された要素
    integer(kint), allocatable, intent(out) :: out(:,:)
    integer(kint) :: conn(n_base)
    integer(kint) :: i, j, in, eid
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
            out(i,n_elem_out) = in
          enddo
        endif
      enddo
    enddo
  end subroutine monolis_get_surf

  !> @ingroup dev_driver
  !> @brief 表面要素を構成する節点番号を抽出
  subroutine monolis_get_surf_node(n_base, n_surf, surf, n_node, node_id)
    implicit none
    !> [in] 要素を構成する形状関数の数
    integer(kint), intent(in) :: n_base
    !> [in] 入力要素の面の数
    integer(kint), intent(in) :: n_surf
    !> [in] 抽出された要素
    integer(kint), intent(in) :: surf(:,:)
    !> [out] 節点数
    integer(kint), intent(out) :: n_node
    !> [out] 節点番号
    integer(kint), allocatable, intent(out) :: node_id(:)
    integer(kint) :: i, j
    integer(kint), allocatable :: tmp(:)

    call monolis_alloc_I_1d(tmp, n_base*n_surf)

    do i = 1, n_surf
      do j = 1, n_base
        tmp(n_base*(i-1) + j) = surf(j,i)
      enddo
    enddo

    call monolis_qsort_I_1d(tmp, 1, n_base*n_surf)
    call monolis_get_uniq_array_I(tmp, n_base*n_surf, n_node)

    call monolis_alloc_I_1d(node_id, n_node)

    do i = 1, n_node
      node_id(i) = tmp(i)
    enddo
  end subroutine monolis_get_surf_node

  !> @ingroup dev_driver
  !> @brief 表面要素を抽出（メイン関数）
  subroutine monolis_get_surf_main(n_elem, n_base, elem, n_surf, is_inner)
    implicit none
    !> [in]  要素数
    integer(kint), intent(in) :: n_elem
    !> [in] 要素を構成する形状関数の数
    integer(kint), intent(in) :: n_base
    !> [in] 要素コネクティビティ
    integer(kint), intent(in) :: elem(:,:)
    !> [in] 入力要素の面の数
    integer(kint), intent(in) :: n_surf
    !> [out] 要素の表面判定フラグ
    integer(kint), allocatable, intent(out) :: is_inner(:,:)
    type(monolis_hash_structure) :: hash_tree
    integer(kint) :: conn(n_base)
    integer(kint) :: i, in, eid
    character :: ckey*27
    logical :: is_exist, is_pushed

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
  !> @brief ハッシュキーの生成
  function get_key_surf(n_base, i, conn)
    implicit none
    !> [out] ハッシュキー
    character :: get_key_surf*27
    !> [in] 要素を構成する形状関数の数
    integer(kint), intent(in) :: n_base
    !> [in] 入力要素の面番号
    integer(kint), intent(in) :: i
    !> [in] 要素コネクティビティ
    integer(kint), intent(in) :: conn(:)
    integer(kint) :: array(4)
    integer(kint) :: i1, i2, i3, i4
    character :: c1*9, c2*9, c3*9

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
end module mod_monolis_extract_util
