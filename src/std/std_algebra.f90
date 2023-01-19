!> std algebra モジュール
module mod_monolis_utils_std_algebra
  use mod_monolis_utils_define_prm
  implicit none

contains

  !> @ingroup std_algebra
  !> 逆行列の取得（n x n 行列）
  !> @details ピボットが 0 であればエラーストップ
  subroutine monolis_get_inverse_matrix(n, a, inv)
    implicit none
    !> [in] 行列の大きさ
    integer(kint), intent(in) :: n
    !> [in] 入力行列
    real(kdouble), intent(in) :: a(n,n)
    !> [out] 逆行列
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
        call monolis_std_error_string("monolis_get_inverse_matrix")
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
  end subroutine monolis_get_inverse_matrix

  !> @ingroup std_algebra
  !> 逆行列の取得（2 x 2 行列）
  !> @details 行列式が 0 以下であればエラーストップ
  !> @details `is_fail` 引数が渡されている場合、行列式が 0 以下であれば `true` が戻る
  subroutine monolis_get_inverse_matrix_2d(a, inv, det, is_fail)
    implicit none
    !> [in] 入力行列
    real(kdouble), intent(in) :: a(2,2)
    !> [out] 逆行列
    real(kdouble), intent(out) :: inv(2,2)
    !> [out] 行列式
    real(kdouble), intent(out) :: det
    !> [out] 行列式が 0 以下であれば `true` となるフラグ
    logical, optional, intent(out) :: is_fail
    real(kdouble) :: detinv

    is_fail = .false.

    det = a(1,1) * a(2,2) &
        - a(2,1) * a(1,2)

    if(det <= 0.0d0)then
      if(present(is_fail))then
        is_fail = .true.
      else
        call monolis_std_error_string("monolis_get_inverse_matrix_2d")
        call monolis_std_error_string("determinant is less than 0")
        call monolis_std_error_stop()
      endif
    endif

    detinv = 1.0d0/det
    inv(1,1) =  a(2,2)*detinv
    inv(1,2) = -a(1,2)*detinv
    inv(2,1) = -a(2,1)*detinv
    inv(2,2) =  a(1,1)*detinv
  end subroutine monolis_get_inverse_matrix_2d

  !> @ingroup std_algebra
  !> 逆行列の取得（3 x 3 行列）
  !> @details 行列式が 0 以下であればエラーストップ
  !> @details `is_fail` 引数が渡されている場合、行列式が 0 以下であれば `true` が戻る
  subroutine monolis_get_inverse_matrix_3d(a, inv, det, is_fail)
    implicit none
    !> [in] 入力行列
    real(kdouble), intent(in) :: a(3,3)
    !> [out] 逆行列
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
        call monolis_std_error_string("monolis_get_inverse_matrix_3d")
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
  end subroutine monolis_get_inverse_matrix_3d

  subroutine monolis_normalize_cross_product_3d(a, b, vec)
    implicit none
    real(kdouble) :: vec(3), a(3), b(3), norm(3), l2

    norm(1) = a(2)*b(3) - a(3)*b(2)
    norm(2) = a(3)*b(1) - a(1)*b(3)
    norm(3) = a(1)*b(2) - a(2)*b(1)
    l2 = dsqrt(norm(1)*norm(1) + norm(2)*norm(2) + norm(3)*norm(3))
    if(l2 == 0.0d0)then
      vec = 0.0d0
      return
    endif
    vec = norm/l2
  end subroutine monolis_normalize_cross_product_3d

  subroutine monolis_normalize_vector(n, a)
    implicit none
    integer(kint) :: i, n
    real(kdouble) :: vec(n), a(n), l2

    l2 = 0.0d0
    do i = 1, n
      l2 = l2 + a(i)*a(i)
    enddo
    l2 = dsqrt(l2)
    if(l2 == 0.0d0)then
      vec = 0.0d0
      return
    endif
    vec = a/l2
  end subroutine monolis_normalize_vector

  subroutine monolis_get_l2_norm(n, a)
    implicit none
    integer(kint) :: i, n
    real(kdouble) :: norm, a(n), l2

    l2 = 0.0d0
    do i = 1, n
      l2 = l2 + a(i)*a(i)
    enddo
    norm = dsqrt(l2)
  end subroutine monolis_get_l2_norm

end module mod_monolis_utils_std_algebra
