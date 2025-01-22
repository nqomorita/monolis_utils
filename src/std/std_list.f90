!> list モジュール
module mod_monolis_utils_std_list
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  implicit none

  !> リスト構造体配列（実数型）
  type monolis_list_R
    integer(kint) :: n
    real(kdouble), allocatable :: array(:)
  end type monolis_list_R

  !> リスト構造体配列（実数型）
  type monolis_list_I
    integer(kint) :: n
    integer(kint), allocatable :: array(:)
  end type monolis_list_I

  !> リスト構造体配列（複素数型）
  type monolis_list_C
    integer(kint) :: n
    complex(kdouble), allocatable :: array(:)
  end type monolis_list_C

contains

  !> @ingroup list
  !> リスト構造体の初期化（実数型）
  subroutine monolis_list_initialize_R(list_struct_R, n)
    implicit none
    !> リスト構造体配列
    type(monolis_list_R), intent(inout) :: list_struct_R(:)
    !> リスト構造体配列の長さ
    integer(kint), intent(in) :: n
    integer(kint) :: i

    do i = 1, n
      list_struct_R(i)%n = 0
      call monolis_dealloc_R_1d(list_struct_R(i)%array)
    enddo
  end subroutine monolis_list_initialize_R

  !> @ingroup list
  !> リスト構造体の初期化（整数型）
  subroutine monolis_list_initialize_I(list_struct_I, n)
    implicit none
    !> リスト構造体配列
    type(monolis_list_I), intent(inout) :: list_struct_I(:)
    !> リスト構造体配列の長さ
    integer(kint), intent(in) :: n
    integer(kint) :: i

    do i = 1, n
      list_struct_I(i)%n = 0
      call monolis_dealloc_I_1d(list_struct_I(i)%array)
    enddo
  end subroutine monolis_list_initialize_I

  !> @ingroup list
  !> リスト構造体の初期化（複素数型）
  subroutine monolis_list_initialize_C(list_struct_C, n)
    implicit none
    !> リスト構造体配列
    type(monolis_list_C), intent(inout) :: list_struct_C(:)
    !> リスト構造体配列の長さ
    integer(kint), intent(in) :: n
    integer(kint) :: i

    do i = 1, n
      list_struct_C(i)%n = 0
      call monolis_dealloc_C_1d(list_struct_C(i)%array)
    enddo
  end subroutine monolis_list_initialize_C

  !> @ingroup list
  !> リスト構造体の終了処理（実数型）
  subroutine monolis_list_finalize_R(list_struct_R)
    implicit none
    !> リスト構造体配列
    type(monolis_list_R), intent(inout) :: list_struct_R(:)
    integer(kint) :: i

    do i = 1, size(list_struct_R)
      list_struct_R(i)%n = 0
      call monolis_dealloc_R_1d(list_struct_R(i)%array)
    enddo
  end subroutine monolis_list_finalize_R

  !> @ingroup list
  !> リスト構造体の終了処理（整数型）
  subroutine monolis_list_finalize_I(list_struct_I)
    implicit none
    !> リスト構造体配列
    type(monolis_list_I), intent(inout) :: list_struct_I(:)
    integer(kint) :: i

    do i = 1, size(list_struct_I)
      list_struct_I(i)%n = 0
      call monolis_dealloc_I_1d(list_struct_I(i)%array)
    enddo
  end subroutine monolis_list_finalize_I

  !> @ingroup list
  !> リスト構造体の終了処理（複素数型）
  subroutine monolis_list_finalize_C(list_struct_C)
    implicit none
    !> リスト構造体配列
    type(monolis_list_C), intent(inout) :: list_struct_C(:)
    integer(kint) :: i

    do i = 1, size(list_struct_C)
      list_struct_C(i)%n = 0
      call monolis_dealloc_C_1d(list_struct_C(i)%array)
    enddo
  end subroutine monolis_list_finalize_C

  !> @ingroup list
  !> リスト構造体へ値の設定（実数型）
  subroutine monolis_list_set_R(list_struct_R, id, n, array)
    implicit none
    !> リスト構造体配列
    type(monolis_list_R), intent(inout) :: list_struct_R(:)
    !> 配列番号
    integer(kint), intent(in) :: id
    !> 配列番号 id に登録する配列サイズ
    integer(kint), intent(in) :: n
    !> 登録する配列
    real(kdouble), intent(in) :: array(:)

    if(n /= size(array))then
      call monolis_std_error_string("monolis_liset_set_R")
      call monolis_std_error_string("n is not equal as size(array)")
      call monolis_std_error_stop()
    endif

    list_struct_R(id)%n = n

    call monolis_dealloc_R_1d(list_struct_R(id)%array)
    call monolis_alloc_R_1d(list_struct_R(id)%array, n)
    list_struct_R(id)%array(:) = array(:)
  end subroutine monolis_list_set_R

  !> @ingroup list
  !> リスト構造体へ値の設定（整数型）
  subroutine monolis_list_set_I(list_struct_I, id, n, array)
    implicit none
    !> リスト構造体配列
    type(monolis_list_I), intent(inout) :: list_struct_I(:)
    !> 配列番号
    integer(kint), intent(in) :: id
    !> 配列番号 id に登録する配列サイズ
    integer(kint), intent(in) :: n
    !> 登録する配列
    integer(kint), intent(in) :: array(:)

    if(n /= size(array))then
      call monolis_std_error_string("monolis_liset_set_I")
      call monolis_std_error_string("n is not equal as size(array)")
      call monolis_std_error_stop()
    endif

    list_struct_I(id)%n = n

    call monolis_dealloc_I_1d(list_struct_I(id)%array)
    call monolis_alloc_I_1d(list_struct_I(id)%array, n)
    list_struct_I(id)%array(:) = array(:)
  end subroutine monolis_list_set_I

  !> @ingroup list
  !> リスト構造体へ値の設定（複素数型）
  subroutine monolis_list_set_C(list_struct_C, id, n, array)
    implicit none
    !> リスト構造体配列
    type(monolis_list_C), intent(inout) :: list_struct_C(:)
    !> 配列番号
    integer(kint), intent(in) :: id
    !> 配列番号 id に登録する配列サイズ
    integer(kint), intent(in) :: n
    !> 登録する配列
    complex(kdouble), intent(in) :: array(:)

    if(n /= size(array))then
      call monolis_std_error_string("monolis_liset_set_C")
      call monolis_std_error_string("n is not equal as size(array)")
      call monolis_std_error_stop()
    endif

    list_struct_C(id)%n = n

    call monolis_dealloc_C_1d(list_struct_C(id)%array)
    call monolis_alloc_C_1d(list_struct_C(id)%array, n)
    list_struct_C(id)%array(:) = array(:)
  end subroutine monolis_list_set_C

  !> @ingroup list
  !> リスト構造体へ値の取得（実数型）
  subroutine monolis_list_get_R(list_struct_R, id, array)
    implicit none
    !> リスト構造体配列
    type(monolis_list_R), intent(in) :: list_struct_R(:)
    !> 配列番号
    integer(kint), intent(in) :: id
    !> 参照する配列
    real(kdouble), allocatable, intent(inout) :: array(:)

    call monolis_dealloc_R_1d(array)
    call monolis_alloc_R_1d(array, list_struct_R(id)%n)
    array(:) = list_struct_R(id)%array(:)
  end subroutine monolis_list_get_R

  !> @ingroup list
  !> リスト構造体へ値の取得（整数型）
  subroutine monolis_list_get_I(list_struct_I, id, array)
    implicit none
    !> リスト構造体配列
    type(monolis_list_I), intent(in) :: list_struct_I(:)
    !> 配列番号
    integer(kint), intent(in) :: id
    !> 参照する配列
    integer(kint), allocatable, intent(inout) :: array(:)

    call monolis_dealloc_I_1d(array)
    call monolis_alloc_I_1d(array, list_struct_I(id)%n)
    array(:) = list_struct_I(id)%array(:)
  end subroutine monolis_list_get_I

  !> @ingroup list
  !> リスト構造体へ値の取得（複素数型）
  subroutine monolis_list_get_C(list_struct_C, id, array)
    implicit none
    !> リスト構造体配列
    type(monolis_list_C), intent(in) :: list_struct_C(:)
    !> 配列番号
    integer(kint), intent(in) :: id
    !> 参照する配列
    complex(kdouble), allocatable, intent(inout) :: array(:)

    call monolis_dealloc_C_1d(array)
    call monolis_alloc_C_1d(array, list_struct_C(id)%n)
    array(:) = list_struct_C(id)%array(:)
  end subroutine monolis_list_get_C

end module mod_monolis_utils_std_list
