!> std ソートモジュール（実数型）
module mod_monolis_utils_std_sort_R
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  implicit none

contains

  !> @ingroup std
  !> クイックソート（1次元実数配列）
  recursive subroutine monolis_qsort_R_1d(array, iS, iE)
    implicit none
    !> [in,out] 整数配列
    real(kdouble), intent(inout) :: array(:)
    !> [in] ソートする開始位置
    integer(kint), intent(in) :: iS
    !> [in] ソートする終了位置
    integer(kint), intent(in) :: iE
    integer(kint) :: center, left, right
    real(kdouble) :: pivot, tmp

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

    if(iS < left - 1)  call monolis_qsort_R_1d(array, iS, left - 1)
    if(right + 1 < iE) call monolis_qsort_R_1d(array, right + 1, iE)
  end subroutine monolis_qsort_R_1d

  !> @ingroup std
  !> クイックソート（1次元実数配列）
  recursive subroutine monolis_qsort_R_1d_I_1d(array1, array2, iS, iE)
    implicit none
    !> [in,out] 実数配列
    real(kdouble), intent(inout) :: array1(:)
    !> [in,out] ソートに従属する整数配列
    integer(kint), intent(inout) :: array2(:)
    !> [in] ソートする開始位置
    integer(kint), intent(in) :: iS
    !> [in] ソートする終了位置
    integer(kint), intent(in) :: iE
    integer(kint) :: center, left, right, itemp
    real(kdouble) :: pivot, tmp

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

      itemp = array2(left)
      array2(left) = array2(right)
      array2(right) = itemp

      left = left + 1
      right = right - 1
    enddo

    if(iS < left - 1)  call monolis_qsort_R_1d_I_1d(array1, array2, iS, left - 1)
    if(right + 1 < iE) call monolis_qsort_R_1d_I_1d(array1, array2, right + 1, iE)
  end subroutine monolis_qsort_R_1d_I_1d

  !> @ingroup std
  !> クイックソート（2次元実数配列）
  !> @details 第一引数をソート
  recursive subroutine monolis_qsort_R_2d(array1, array2, iS, iE)
    implicit none
    !> [in,out] ソートされる実数配列
    real(kdouble), intent(inout) :: array1(:)
    !> [in,out] ソートに従属する実数配列
    real(kdouble), intent(inout) :: array2(:)
    !> [in] ソートする開始位置
    integer(kint), intent(in) :: iS
    !> [in] ソートする終了位置
    integer(kint), intent(in) :: iE
    integer(kint) :: center, left, right
    real(kdouble) :: pivot, tmp

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

    if(iS < left - 1)  call monolis_qsort_R_2d(array1, array2, iS, left - 1)
    if(right + 1 < iE) call monolis_qsort_R_2d(array1, array2, right + 1, iE)
  end subroutine monolis_qsort_R_2d

  !> @ingroup std
  !> 実数配列の二分探索
  subroutine monolis_bsearch_R(array, iS, iE, val, idx)
    implicit none
    !> [in] 実数配列
    real(kdouble), intent(in) :: array(:)
    !> [in] ソートする開始位置
    integer(kint), intent(in) :: iS
    !> [in] ソートする終了位置
    integer(kint), intent(in) :: iE
    !> [in] 検索する実数値
    real(kdouble), intent(in) :: val
    !> [out] 検索した結果の位置（検索結果がない場合 -1 を返す）
    integer(kint), intent(out) :: idx
    integer(kint) :: center, left, right
    real(kdouble) :: pivot

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
  end subroutine monolis_bsearch_R

  !> @ingroup std
  !> 実数の等差数列を生成
  subroutine monolis_get_sequence_array_R(array, n, origin, difference)
    implicit none
    !> [out] 整数配列
    real(kdouble), intent(out) :: array(:)
    !> [in] 整数配列のサイズ
    integer(kint), intent(in) :: n
    !> [in] 初項
    real(kdouble), intent(in) :: origin
    !> [in] 交差
    real(kdouble), intent(in) :: difference
    real(kdouble) :: i

    do i = 1, n
      array(i) = origin + (i - 1)*difference
    enddo
  end subroutine monolis_get_sequence_array_R

  !> @ingroup std
  !> 整数配列の置換
  subroutine monolis_perm_array_R(array, perm, n)
    implicit none
    !> [in,out] 整数配列
    real(kdouble), intent(inout) :: array(:)
    !> [in] 置換ベクトル
    integer(kint), intent(in) :: perm(:)
    !> [in] ベクトルサイズ
    integer(kint), intent(in) :: n
    integer(kint) :: i, in
    real(kdouble), allocatable :: temp(:)

    call monolis_alloc_R_1d(temp, n)

    temp = array

    do i = 1, n
      in = perm(i)
      array(i) = temp(in)
    enddo
  end subroutine monolis_perm_array_R
end module mod_monolis_utils_std_sort_R
