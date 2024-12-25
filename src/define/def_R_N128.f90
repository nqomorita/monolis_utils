!> 擬似四倍精度データモジュール
module mod_monolis_utils_define_R_N128
  use mod_monolis_utils_define_prm
!  use mod_monolis_utils_alloc
!  use mod_monolis_utils_palloc
!  use mod_monolis_mpi_util
!  use mod_monolis_utils_define_com
  implicit none

  !> @ingroup nhp
  type, public :: monolis_R_N128
    !> 擬似四倍精度の高次精度部分
    real(kdouble) :: hi
    !> 擬似四倍精度の低次精度部分
    real(kdouble) :: lo
  end type monolis_R_N128

  !> 擬似四倍精度演算構造体の MPI 用整数ラベル
  integer(kint), save :: monolis_mpi_type_R_N128

  !> 擬似四倍精度演算の MPI 用整数ラベル
  integer(kint), save :: monolis_mpi_add_R_N128

contains

  !> @ingroup nhp
  !> 擬似四倍精度演算を MPI ライブラリに登録する初期化関数
  subroutine monolis_add_R_N128_MPI_init()
    implicit none
    integer(kint) :: n_kdouble
    integer(kint) :: ierr

#ifndef NO_MPI
    n_kdouble = 2
    call MPI_Type_contiguous(n_kdouble, MPI_DOUBLE_PRECISION, monolis_mpi_type_R_N128, ierr)
    call MPI_Type_commit(monolis_mpi_type_R_N128, ierr)

    call MPI_Op_create(monolis_add_R_N128_MPI, .true., monolis_mpi_add_R_N128, ierr)
#endif
  end subroutine monolis_add_R_N128_MPI_init

  !> @ingroup nhp
  !> 擬似四倍精度演算を MPI ライブラリに登録する初期化関数
  subroutine monolis_add_R_N128_MPI_finalize()
    implicit none
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_Type_free(monolis_mpi_type_R_N128, ierr)
    call MPI_Op_free(monolis_add_R_N128_MPI, ierr)
#endif
  end subroutine monolis_add_R_N128_MPI_finalize

  !> @ingroup nhp
  !> 加算 c = a + b（擬似四倍精度実数型）
  !> @details Cary-Add 方式（下位パートの誤差を厳密に計算しない）
  subroutine monolis_add_R_N128(a, b, c)
    implicit none
    !> [in] 入力値（擬似四倍精度実数型）
    type(monolis_R_N128), intent(in) :: a
    !> [in] 入力値（擬似四倍精度実数型）
    type(monolis_R_N128), intent(in) :: b
    !> [out] 出力値（擬似四倍精度実数型）
    type(monolis_R_N128), intent(out) :: c
    real(kdouble) :: th, tl, f, e

    !# 上位部の加算（Two_Sum）
    th = a%hi + b%hi
    e  = th - a%hi
    f  = (b%hi - e) + (a%hi - (th - e))

    !# 下位部の加算
    tl = f + a%lo + b%lo

    !# 上位と下位の加算結果を分けて保持（Fast_Two_Sum）
    c%hi = th + tl
    c%lo = tl - (c%hi - th)
  end subroutine monolis_add_R_N128

  !> @ingroup nhp
  !> 加算 c = a + b（擬似四倍精度実数型, MPI コールバック関数）
  subroutine monolis_add_R_N128_MPI(in, inout, len, data_type)
    implicit none
    !> [in] 入力値（擬似四倍精度実数型）
    type(monolis_R_N128), intent(in) :: in(*)
    !> [inout] 入力値（擬似四倍精度実数型）
    type(monolis_R_N128), intent(inout) :: inout(*)
    !> [in] 配列長さ
    integer(kint) :: len
    !> [in] データ構造体のラベル
    integer(kint) :: data_type
    !> ローカル変数
    real(kdouble) :: i

    do i = 1, len
       call monolis_add_R_N128(inout(i), in(i), inout(i))
    enddo
  end subroutine monolis_add_R_N128_MPI

  !> @ingroup nhp
  !> 倍精度型から擬似四倍精度実数型の変換
  function monolis_conv_R_to_R_N128(x) result(a)
    implicit none
    !> [in] 入出力値（整数型）
    real(kdouble), intent(in) :: x
    !> [out] 出力値（擬似四倍精度実数型）
    type(monolis_R_N128) :: a

    a%hi = x
    a%lo = 0.0d0
  end function monolis_conv_R_to_R_N128

  !> @ingroup nhp
  !> 倍精度型から擬似四倍精度実数型の変換
  function monolis_conv_R_N128_to_R(a) result(x)
    implicit none
    !> [in] 入力値（擬似四倍精度実数型）
    type(monolis_R_N128), intent(in) :: a
    !> [out] 出力値（倍精度実数型）
    real(kdouble) :: x
    x = a%hi + a%lo
  end function monolis_conv_R_N128_to_R

  !> @ingroup nhp
  !> 擬似四倍精度実数型のコピー
  function monolis_copy_R_N128(a) result(b)
    implicit none
    !> [in] 入力値（擬似四倍精度実数型）
    type(monolis_R_N128), intent(in) :: a
    !> [out] 出力値（倍精度実数型）
    type(monolis_R_N128) :: b

    b%hi = a%hi
    b%lo = a%lo
  end function monolis_copy_R_N128
end module mod_monolis_utils_define_R_N128