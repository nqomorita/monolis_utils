!> std ソートモジュール（整数型）
module mod_monolis_utils_std_sort_I
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  implicit none

contains

  !> @ingroup std
  !> クイックソート（1次元整数配列）
  recursive subroutine monolis_qsort_I_1d(array, iS, iE)
    implicit none
    !> [in,out] 整数配列
    integer(kint), intent(inout) :: array(:)
    !> [in] ソートする開始位置
    integer(kint), intent(in) :: iS
    !> [in] ソートする終了位置
    integer(kint), intent(in) :: iE
    integer(kint) :: pivot, center, left, right, tmp

    if (iS >= iE) return

    center = (iS + iE) / 2
    pivot = array(center)
    left = iS
    right = iE

    do
      do while (array(left) < pivot)
        left = left + 1
      enddo
      do while (pivot < array(right))
        right = right - 1
      enddo

      if (left >= right) exit

      tmp = array(left)
      array(left) = array(right)
      array(right) = tmp

      left = left + 1
      right = right - 1
    enddo

    if(iS < left - 1)  call monolis_qsort_I_1d(array, iS, left - 1)
    if(right + 1 < iE) call monolis_qsort_I_1d(array, right + 1, iE)
  end subroutine monolis_qsort_I_1d

  !> @ingroup std
  !> クイックソート（2次元整数配列）
  !> @details 第一引数をソート
  recursive subroutine monolis_qsort_I_2d(array1, array2, iS, iE)
    implicit none
    !> [in,out] ソートされる整数配列
    integer(kint), intent(inout) :: array1(:)
    !> [in,out] ソートに従属する整数配列
    integer(kint), intent(inout) :: array2(:)
    !> [in] ソートする開始位置
    integer(kint), intent(in) :: iS
    !> [in] ソートする終了位置
    integer(kint), intent(in) :: iE
    integer(kint) :: pivot, center, left, right, tmp

    if (iS >= iE) return

    center = (iS + iE) / 2
    pivot = array1(center)
    left = iS
    right = iE

    do
      do while (array1(left) < pivot)
        left = left + 1
      enddo
      do while (pivot < array1(right))
        right = right - 1
      enddo

      if (left >= right) exit

      tmp = array1(left)
      array1(left) = array1(right)
      array1(right) = tmp

      tmp = array2(left)
      array2(left) = array2(right)
      array2(right) = tmp

      left = left + 1
      right = right - 1
    enddo

    if(iS < left - 1)  call monolis_qsort_I_2d(array1, array2, iS, left - 1)
    if(right + 1 < iE) call monolis_qsort_I_2d(array1, array2, right + 1, iE)
  end subroutine monolis_qsort_I_2d

  !> @ingroup std
  !> 整数配列の二分探索
  subroutine monolis_bsearch_I(array, iS, iE, val, idx)
    implicit none
    !> [in] 整数配列
    integer(kint), intent(in) :: array(:)
    !> [in] ソートする開始位置
    integer(kint), intent(in) :: iS
    !> [in] ソートする終了位置
    integer(kint), intent(in) :: iE
    !> [in] 検索する整数値
    integer(kint), intent(in) :: val
    !> [out] 検索した結果の位置（検索結果がない場合 -1 を返す）
    integer(kint), intent(out) :: idx
    integer(kint) :: center, left, right, pivot

    left = iS
    right = iE

    do
      if (left > right) then
        idx = -1
        exit
      endif
      center = (left + right) / 2
      pivot = array(center)
      if (val < pivot) then
        right = center - 1
        cycle
      elseif (pivot < val) then
        left = center + 1
        cycle
      else
        idx = center
        exit
      endif
    enddo
  end subroutine monolis_bsearch_I

  !> @ingroup std
  !> 整数の等差数列を生成
  subroutine monolis_get_sequence_array_I(array, n, origin, difference)
    implicit none
    !> [out] 整数配列
    integer(kint), intent(out) :: array(:)
    !> [in] 整数配列のサイズ
    integer(kint), intent(in) :: n
    !> [in] 初項
    integer(kint), intent(in) :: origin
    !> [in] 交差
    integer(kint), intent(in) :: difference
    integer(kint) :: i

    do i = 1, n
      array(i) = origin + (i - 1)*difference
    enddo
  end subroutine monolis_get_sequence_array_I

  !> @ingroup std
  !> 整数配列の重複要素の削除
  subroutine monolis_get_uniq_array_I(array, len, newlen)
    implicit none
    !> [in,out] 整数配列
    integer(kint), intent(inout) :: array(:)
    !> [in] 整数配列のサイズ
    integer(kint), intent(in) :: len
    !> [out] 重複を省いた要素数
    integer(kint), intent(out) :: newlen
    integer(kint) :: i, ndup

    ndup = 0
    do i = 2, len
      if(array(i) == array(i - 1 - ndup))then
        ndup = ndup + 1
      elseif(ndup > 0)then
        array(i - ndup) = array(i)
      endif
    end do
    newlen = len - ndup
  end subroutine monolis_get_uniq_array_I

  !> @ingroup std
  !> 整数配列の置換
  subroutine monolis_perm_array_I(array, perm, n)
    implicit none
    !> [in,out] 整数配列
    integer(kint), intent(inout) :: array(:)
    !> [in] 置換ベクトル
    integer(kint), intent(in) :: perm(:)
    !> [in] ベクトルサイズ
    integer(kint), intent(in) :: n
    integer(kint) :: i, in
    integer(kint), allocatable :: temp(:)

    call monolis_alloc_I_1d(temp, n)

    temp = array

    do i = 1, n
      in = perm(i)
      array(i) = temp(in)
    enddo
  end subroutine monolis_perm_array_I
end module mod_monolis_utils_std_sort_I
