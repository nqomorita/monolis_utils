!> k-d ツリーモジュール
module mod_monolis_utils_kdtree
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_error
  use mod_monolis_utils_aabb
  use mod_monolis_utils_alloc

  implicit none

  private

  public :: monolis_kdtree_structure
  public :: monolis_kdtree_init_by_BB

  !> k-d ツリー構造体
  type monolis_kdtree_structure_main
    !> 評価・分割する軸番号（1:x 軸、2:y 軸、3:z 軸）
    integer(kint) :: dim
    !> 格納する id 番号
    integer(kint) :: id
    !> バウンディングボックス（x_min, x_max, y_min, y_max, z_min, z_max の順に格納）
    real(kdouble) :: BB(6)
    !> BB の中間座標
    real(kdouble) :: mid_coord(3)
    !> 中間点より座標が小さい集合の k-d ツリー構造体
    type(monolis_kdtree_structure_main), pointer :: left
    !> 中間点より座標が大きい集合の k-d ツリー構造体
    type(monolis_kdtree_structure_main), pointer :: right
  end type monolis_kdtree_structure_main

  !> k-d ツリー構造体
  type monolis_kdtree_structure
    !> k-d ツリー構造体（メイン）
    type(monolis_kdtree_structure_main), pointer :: kdtree => null()
  end type monolis_kdtree_structure

contains

  !> @ingroup kdtree
  !> バウンディングボックス情報を入力して k-d ツリーを構築
  subroutine monolis_kdtree_init_by_BB(monolis_kdtree, n_BB, BB_id, BB)
    implicit none
    !> k-d ツリー構造体
    type(monolis_kdtree_structure) :: monolis_kdtree
    !> [in] バウンディングボックスの入力数
    integer(kint), intent(in) :: n_BB
    !> [in,out] バウンディングボックスの id（サイズ [n_BB]）
    integer(kint), intent(inout) :: BB_id(:)
    !> [in,out] バウンディングボックスの入力座標（サイズ [6, n_BB]、x_min, x_max, y_min, y_max, z_min, z_max の順に格納）
    real(kdouble), intent(inout) :: BB(:,:)

    if(associated(monolis_kdtree%kdtree))then
      call monolis_std_error_string("monolis_kdtree_init_by_BB")
      call monolis_std_error_string("monolis_kdtree is already allocated or initialized")
      call monolis_std_error_stop()
    endif

    call monolis_kdtree_build_by_BB(monolis_kdtree%kdtree, n_BB, BB_id, BB, 1)
  end subroutine monolis_kdtree_init_by_BB

  recursive subroutine monolis_kdtree_build_by_BB(kdtree, n_BB, BB_id, BB, depth)
    implicit none
    !> k-d ツリー構造体
    type(monolis_kdtree_structure_main), pointer :: kdtree
    !> [in,out] バウンディングボックスの入力数
    integer(kint) :: n_BB
    !> [in,out] バウンディングボックスの id（サイズ [n_BB]）
    integer(kint) :: BB_id(:)
    !> [in,out] バウンディングボックスの入力座標（サイズ [6, n_BB]、x_min, x_max, y_min, y_max, z_min, z_max の順に格納）
    real(kdouble) :: BB(:,:)
    !> [in,out] k-d ツリーの深さ
    integer(kint) :: depth
    integer(kint) :: i, mid_id, iSl, iEl, iSr, iEr
    integer(kint), allocatable :: perm(:)
    real(kdouble), allocatable :: mid_coord(:,:)

    if(n_BB <= 0) return

    allocate(kdtree)

    !> 評価軸の決定
    kdtree%dim = 3 - mod(depth, 3)

    !> 保持している全てのデータ全体の BB を取得
    call monolis_get_aabb(BB, kdtree%BB)

    !> 保持しているデータごとに中間座標を取得しソート
    call monolis_alloc_R_2d(mid_coord, 3, n_BB)

    do i = 1, n_BB
      mid_coord(1,i) = 0.5d0*(BB(1,i) + BB(2,i))
      mid_coord(2,i) = 0.5d0*(BB(3,i) + BB(4,i))
      mid_coord(3,i) = 0.5d0*(BB(5,i) + BB(6,i))
    enddo

    call monolis_get_sequence_array_I(perm, n_BB, 1, 1)
    call monolis_qsort_R_1d_I_1d(mid_coord(kdtree%dim,:), perm, 1, n_BB)

    !> BB id、BB の置換
    call monolis_perm_array_I(BB_id, perm, n_BB)
    call monolis_perm_array_R(BB(1,:), perm, n_BB)
    call monolis_perm_array_R(BB(2,:), perm, n_BB)
    call monolis_perm_array_R(BB(3,:), perm, n_BB)
    call monolis_perm_array_R(BB(4,:), perm, n_BB)
    call monolis_perm_array_R(BB(5,:), perm, n_BB)
    call monolis_perm_array_R(BB(6,:), perm, n_BB)

    mid_id = n_BB / 2
    kdtree%id = mid_id

    iSl = 1
    iEl = mid_id - 1
    iSr = mid_id + 1
    iEr = n_BB

    call monolis_kdtree_build_by_BB(kdtree%left,  iEl - iSl + 1, BB_id(iSl:iEl), BB(:,iSl:iEl), depth + 1)
    call monolis_kdtree_build_by_BB(kdtree%right, iEr - iSr + 1, BB_id(iSr:iEr), BB(:,iSr:iEr), depth + 1)
  end subroutine monolis_kdtree_build_by_BB
end module mod_monolis_utils_kdtree
