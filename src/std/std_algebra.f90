!> std 代数計算モジュール
module mod_monolis_utils_std_algebra
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_error
  implicit none

contains

  !> @ingroup std_algebra
  !> 行列からベクトル配列に変換（実数型）
  subroutine monolis_mat_to_vec_R(N, M, MAT, X)
    implicit none
    !> 行数
    integer(kint) :: N
    !> 列数
    integer(kint) :: M
    !> 行列
    real(kdouble) :: MAT(N,M)
    !> ベクトル
    real(kdouble) :: X(N*M)
    integer(kint) :: i, j

    do i = 1, M
      do j = 1, N
        X(N*(i-1) + j) = MAT(j,i)
      enddo
    enddo
  end subroutine monolis_mat_to_vec_R

  !> @ingroup std_algebra
  !> ベクトル配列から行列に変換（実数型）
  subroutine monolis_vec_to_mat_R(N, M, X, MAT)
    implicit none
    !> 行数
    integer(kint) :: N
    !> 列数
    integer(kint) :: M
    !> ベクトル
    real(kdouble) :: X(N*M)
    !> 行列
    real(kdouble) :: MAT(N,M)
    integer(kint) :: i, j

    do i = 1, M
      do j = 1, N
        MAT(j,i) = X(N*(i-1) + j)
      enddo
    enddo
  end subroutine monolis_vec_to_mat_R

  !> @ingroup std_algebra
  !> 逆行列の取得（n x n 行列、実数型）
  !> @details ピボットが 0 であればエラーストップ
  subroutine monolis_get_inverse_matrix_R(n, a, inv)
    implicit none
    !> [in] 行列の大きさ
    integer(kint), intent(in) :: n
    !> [in] 入力行列（サイズ [n, n]）
    real(kdouble), intent(in) :: a(n,n)
    !> [out] 逆行列（サイズ [n, n]）
    real(kdouble), intent(out) :: inv(n,n)
    integer(kint) :: i, j, k
    real(kdouble) :: b(n,n), tmp

    b = a

    inv = 0.0d0
    do i = 1, n
      inv(i,i) = 1.0d0
    enddo

    do i = 1, n
      if(b(i,i) == 0.0d0)then
        call monolis_std_error_string("monolis_get_inverse_matrix_R")
        call monolis_std_error_string("diagonal component is 0")
        call monolis_std_error_stop()
      endif

      tmp = 1.0d0/b(i,i)

      do j = 1, n
          b(j,i) =   b(j,i) * tmp
        inv(j,i) = inv(j,i) * tmp
      enddo

      do j = 1, n
        if(i /= j) then
          tmp = b(i,j)
          do k = 1, n
              b(k,j) =   b(k,j) -   b(k,i) * tmp
            inv(k,j) = inv(k,j) - inv(k,i) * tmp
          enddo
        endif
      enddo
    enddo
  end subroutine monolis_get_inverse_matrix_R

  !> @ingroup std_algebra
  !> 逆行列の取得（2 x 2 行列、実数型）
  !> @details 行列式が 0 以下であればエラーストップ
  !> @details `is_fail` 引数が渡されている場合、行列式が 0 以下であれば `true` が戻る
  subroutine monolis_get_inverse_matrix_R_2d(a, inv, det, is_fail)
    implicit none
    !> [in] 入力行列（サイズ [2, 2]）
    real(kdouble), intent(in) :: a(2,2)
    !> [out] 逆行列（サイズ [2, 2]）
    real(kdouble), intent(out) :: inv(2,2)
    !> [out] 行列式
    real(kdouble), intent(out) :: det
    !> [out] 行列式が 0 以下であれば `true` となるフラグ
    logical, optional, intent(out) :: is_fail
    real(kdouble) :: detinv

    if(present(is_fail)) is_fail = .false.

    det = a(1,1) * a(2,2) &
        - a(2,1) * a(1,2)

    if(det <= 0.0d0)then
      if(present(is_fail))then
        is_fail = .true.
      else
        call monolis_std_error_string("monolis_get_inverse_matrix_R_2d")
        call monolis_std_error_string("determinant is less than 0")
        call monolis_std_error_stop()
      endif
    endif

    detinv = 1.0d0/det
    inv(1,1) =  a(2,2)*detinv
    inv(1,2) = -a(1,2)*detinv
    inv(2,1) = -a(2,1)*detinv
    inv(2,2) =  a(1,1)*detinv
  end subroutine monolis_get_inverse_matrix_R_2d

  !> @ingroup std_algebra
  !> 逆行列の取得（3 x 3 行列、実数型）
  !> @details 行列式が 0 以下であればエラーストップ
  !> @details `is_fail` 引数が渡されている場合、行列式が 0 以下であれば `true` が戻る
  subroutine monolis_get_inverse_matrix_R_3d(a, inv, det, is_fail)
    implicit none
    !> [in] 入力行列（サイズ [3, 3]）
    real(kdouble), intent(in) :: a(3,3)
    !> [out] 逆行列（サイズ [3, 3]）
    real(kdouble), intent(out) :: inv(3,3)
    !> [out] 行列式
    real(kdouble), intent(out) :: det
    !> [out] 行列式が 0 以下であれば `true` となるフラグ
    logical, optional, intent(out) :: is_fail
    real(kdouble) :: detinv

    if(present(is_fail)) is_fail = .false.

    det = a(1,1) * a(2,2) * a(3,3) &
        + a(2,1) * a(3,2) * a(1,3) &
        + a(3,1) * a(1,2) * a(2,3) &
        - a(3,1) * a(2,2) * a(1,3) &
        - a(2,1) * a(1,2) * a(3,3) &
        - a(1,1) * a(3,2) * a(2,3)

    if(det <= 0.0d0)then
      if(present(is_fail))then
        is_fail = .true.
      else
        call monolis_std_error_string("monolis_get_inverse_matrix_R_3d")
        call monolis_std_error_string("determinant is less than 0")
        call monolis_std_error_stop()
      endif
    endif

    detinv = 1.0d0/det
    inv(1,1) = detinv * ( a(2,2)*a(3,3) - a(3,2)*a(2,3))
    inv(1,2) = detinv * (-a(1,2)*a(3,3) + a(3,2)*a(1,3))
    inv(1,3) = detinv * ( a(1,2)*a(2,3) - a(2,2)*a(1,3))
    inv(2,1) = detinv * (-a(2,1)*a(3,3) + a(3,1)*a(2,3))
    inv(2,2) = detinv * ( a(1,1)*a(3,3) - a(3,1)*a(1,3))
    inv(2,3) = detinv * (-a(1,1)*a(2,3) + a(2,1)*a(1,3))
    inv(3,1) = detinv * ( a(2,1)*a(3,2) - a(3,1)*a(2,2))
    inv(3,2) = detinv * (-a(1,1)*a(3,2) + a(3,1)*a(1,2))
    inv(3,3) = detinv * ( a(1,1)*a(2,2) - a(2,1)*a(1,2))
  end subroutine monolis_get_inverse_matrix_R_3d

  !> @ingroup std_algebra
  !> 3 次元の外積計算（正規化あり、実数型）
  subroutine monolis_normalize_cross_product_R_3d(v1, v2, v3)
    implicit none
    !> [in] 入力ベクトル v1（サイズ [3]）
    real(kdouble), intent(in) :: v1(3)
    !> [in] 入力ベクトル v2（サイズ [3]）
    real(kdouble), intent(in) :: v2(3)
    !> [out] 出力ベクトル v3 = v1 x v2（サイズ [3]）
    real(kdouble), intent(out) :: v3(3)
    real(kdouble) :: s(3), vec(3)

    s = v1
    vec(1) = s(2)*v2(3) - s(3)*v2(2)
    vec(2) = s(3)*v2(1) - s(1)*v2(3)
    vec(3) = s(1)*v2(2) - s(2)*v2(1)

    call monolis_normalize_vector_R(3, vec, v3)
  end subroutine monolis_normalize_cross_product_R_3d

  !> @ingroup std_algebra
  !> 3 次元の外積計算（正規化なし、実数型）
  subroutine monolis_cross_product_R_3d(v1, v2, v3)
    implicit none
    !> [in] 入力ベクトル v1（サイズ [3]）
    real(kdouble), intent(in) :: v1(3)
    !> [in] 入力ベクトル v2（サイズ [3]）
    real(kdouble), intent(in) :: v2(3)
    !> [out] 出力ベクトル v3 = v1 x v2（サイズ [3]）
    real(kdouble), intent(out) :: v3(3)
    real(kdouble) :: s(3)

    s = v1
    v3(1) = s(2)*v2(3) - s(3)*v2(2)
    v3(2) = s(3)*v2(1) - s(1)*v2(3)
    v3(3) = s(1)*v2(2) - s(2)*v2(1)
  end subroutine monolis_cross_product_R_3d

  !> @ingroup std_algebra
  !> ベクトルの正規化（実数型）
  subroutine monolis_normalize_vector_R(n, v1, v2)
    implicit none
    !> [in] ベクトルのサイズ
    integer(kint), intent(in) :: n
    !> [in] 入力ベクトル（サイズ [n]）
    real(kdouble), intent(in) :: v1(n)
    !> [out] 出力ベクトル（サイズ [n]）
    real(kdouble), intent(out) :: v2(n)
    real(kdouble) :: norm

    call monolis_get_l2_norm_R(n, v1, norm)

    if(norm == 0.0d0)then
      v2 = 0.0d0
      return
    endif

    v2 = v1/norm
  end subroutine monolis_normalize_vector_R

  !> @ingroup std_algebra
  !> ベクトルのノルム計算（実数型）
  subroutine monolis_get_l2_norm_R(n, v1, norm)
    implicit none
    !> [in] ベクトルのサイズ
    integer(kint), intent(in) :: n
    !> [in] 入力ベクトル（サイズ [n]）
    real(kdouble), intent(in) :: v1(n)
    !> [out] 出力ノルム
    real(kdouble), intent(out) :: norm
    integer(kint) :: i
    real(kdouble) :: l2

    l2 = 0.0d0
    do i = 1, n
      l2 = l2 + v1(i)*v1(i)
    enddo
    norm = dsqrt(l2)
  end subroutine monolis_get_l2_norm_R

  !> @ingroup std_algebra
  !> ランダムベクトルの取得（実数型）
  subroutine monolis_get_rundom_number_R(n, x, seed)
    implicit none
    !> [in] ベクトルサイズ
    integer(kint), intent(in) :: n
    !> [out] ベクトル
    real(kdouble), intent(out) :: x(n)
    !> [in] シフト量
    integer(kint), intent(in) :: seed
    integer(kint), parameter :: m = 1664501
    integer(kint), parameter :: lambda = 1229
    integer(kint), parameter :: u = 351750
    integer(kint) :: i, t
    real(kdouble) :: inv

    t = 0
    inv = 1.0d0/m
    do i = 1, seed
      t = mod(lambda*t + u, m)
    enddo
    do i = seed + 1, seed + n
      t = mod(lambda*t + u, m)
      x(i - seed) = inv*t
    enddo
  end subroutine monolis_get_rundom_number_R
end module mod_monolis_utils_std_algebra
