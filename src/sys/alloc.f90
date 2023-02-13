!> メモリ確保モジュール
!# I_1d
!# subroutine monolis_alloc_I_1d(var, i)
!# subroutine monolis_dealloc_I_1d(var)
!# subroutine monolis_realloc_I_1d(var, i)
!# subroutine monolis_append_I_1d(var, n_add, var_add)
!#
!# I_2d
!# subroutine monolis_alloc_I_2d(var, i, j)
!# subroutine monolis_dealloc_I_2d(var)
!#
!# R_1d
!# subroutine monolis_alloc_R_1d(var, i)
!# subroutine monolis_dealloc_R_1d(var)
!#
!# R_2d
!# subroutine monolis_alloc_R_2d(var, i, j)
!# subroutine monolis_dealloc_R_2d(var)
!# subroutine monolis_realloc_R_2d(var, m, n)
!# subroutine monolis_append_R_2d(var, n_add, var_add)
!#
!# C_1d
!# subroutine monolis_alloc_C_1d(var, i)
!# subroutine monolis_dealloc_C_1d(var)
!#
!# C_2d
!# subroutine monolis_alloc_C_2d(var, i, j)
!# subroutine monolis_dealloc_C_2d(var)
!#
!# L_1d
!# subroutine monolis_alloc_L_1d(var, i)
!# subroutine monolis_dealloc_L_1d(var)
!#
!# L_2d
!# subroutine monolis_alloc_L_2d(var, i, j)
!# subroutine monolis_dealloc_L_2d(var)
module mod_monolis_utils_alloc
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_error
  implicit none

contains

  !> @ingroup alloc
  !> 1 次元整数配列のメモリ確保
  !> @details 初期値 0 でメモリ確保がなされる。
  subroutine monolis_alloc_I_1d(var, i)
    implicit none
    !> [out] メモリ確保する配列
    integer(kint), intent(out), allocatable :: var(:)
    !> [in] 配列サイズ
    integer(kint), intent(in) :: i
    integer(kint) :: ierr

    if(allocated(var))then
      call monolis_std_error_string("monolis_alloc_I_1d")
      call monolis_std_error_string("input arg. is already allocated")
      call monolis_std_error_stop()
    endif

    allocate(var(i), source = 0, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_alloc_I_1d")
      call monolis_std_error_string("allocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_alloc_I_1d

  !> @ingroup alloc
  !> 1 次元整数配列のメモリ開放
  subroutine monolis_dealloc_I_1d(var)
    implicit none
    !> [in] メモリ開放する配列
    integer(kint), allocatable :: var(:)
    integer(kint) :: ierr

    if(.not. allocated(var))then
      return
    endif

    deallocate(var, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_dealloc_I_1d")
      call monolis_std_error_string("deallocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_dealloc_I_1d

  !> @ingroup alloc
  !> 1 次元整数配列のメモリ再確保
  !> @details 再確保で増えた配列部分は初期値 0 でメモリ確保がなされる。
  subroutine monolis_realloc_I_1d(var, i)
    implicit none
    !> [in,out] メモリ確保する配列
    integer(kint), allocatable :: var(:)
    !> [in] 再確保後の配列サイズ
    integer(kint), intent(in) :: i
    integer(kint), allocatable :: temp(:)
    integer(kint) :: j, iold

    if(.not. allocated(var))then
      call monolis_alloc_I_1d(var, i)
      return
    endif

    iold = size(var)

    call monolis_alloc_I_1d(temp, iold)

    temp(:) = var(:)

    call monolis_dealloc_I_1d(var)

    call monolis_alloc_I_1d(var, i)

    do j = 1, min(iold, i)
      var(j) = temp(j)
    enddo
  end subroutine monolis_realloc_I_1d

  !> @ingroup alloc
  !> 1 次元整数配列の末尾にデータ配列を追加
  subroutine monolis_append_I_1d(var, n_add, var_add)
    implicit none
    !> [in,out] 元の配列
    integer(kint), allocatable :: var(:)
    !> [in] 追加する配列サイズ
    integer(kint), intent(in) :: n_add
    !> [in] 追加する配列
    integer(kint), intent(in) :: var_add(:)
    integer(kint) :: n_all, n_old, i

    if(.not. allocated(var))then
      n_old = 0
    else
      n_old = size(var)
    endif

    n_all = n_old + n_add

    call monolis_realloc_I_1d(var, n_all)

    do i = n_old + 1, n_all
      var(i) = var_add(i - n_old)
    enddo
  end subroutine monolis_append_I_1d

  !> @ingroup alloc
  !> 2 次元整数配列のメモリ確保
  !> @details 配列サイズは var(i, j) として確保される。
  subroutine monolis_alloc_I_2d(var, i, j)
    implicit none
    !> [out] メモリ確保する配列（サイズ [i, j]）
    integer(kint), intent(out), allocatable :: var(:,:)
    !> [in] 配列サイズ
    integer(kint), intent(in) :: i
    !> [in] 配列サイズ
    integer(kint), intent(in) :: j
    integer(kint) :: ierr

    if(allocated(var))then
      call monolis_std_error_string("monolis_alloc_I_2d")
      call monolis_std_error_string("input arg. is already allocated")
      call monolis_std_error_stop()
    endif

    allocate(var(i,j), source = 0, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_alloc_I_2d")
      call monolis_std_error_string("allocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_alloc_I_2d

  !> @ingroup alloc
  !> 2 次元整数配列のメモリ開放
  subroutine monolis_dealloc_I_2d(var)
    implicit none
    !> [in] メモリ開放する配列
    integer(kint), allocatable :: var(:,:)
    integer(kint) :: ierr

    if(.not. allocated(var))then
      return
    endif

    deallocate(var, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_dealloc_I_2d")
      call monolis_std_error_string("deallocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_dealloc_I_2d

  !> @ingroup alloc
  !> 1 次元浮動小数点配列のメモリ確保
  subroutine monolis_alloc_R_1d(var, i)
    implicit none
    !> [out] メモリ確保する配列
    real(kdouble), intent(out), allocatable :: var(:)
    !> [in] 配列サイズ
    integer(kint), intent(in) :: i
    integer(kint) :: ierr

    if(allocated(var))then
      call monolis_std_error_string("monolis_alloc_R_1d")
      call monolis_std_error_string("input arg. is already allocated")
      call monolis_std_error_stop()
    endif

    allocate(var(i), source = 0.0d0, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_alloc_R_1d")
      call monolis_std_error_string("allocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_alloc_R_1d

  !> @ingroup alloc
  !> 1 次元浮動小数点配列のメモリ開放
  subroutine monolis_dealloc_R_1d(var)
    implicit none
    !> [in] メモリ開放する配列
    real(kdouble), allocatable :: var(:)
    integer(kint) :: ierr

    if(.not. allocated(var))then
      return
    endif

    deallocate(var, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_dealloc_R_1d")
      call monolis_std_error_string("deallocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_dealloc_R_1d

  !> @ingroup alloc
  !> 2 次元浮動小数点配列のメモリ確保
  !> @details 配列サイズは var(i, j) として確保される。
  subroutine monolis_alloc_R_2d(var, i, j)
    implicit none
    !> [out] メモリ確保する配列（サイズ [i, j]）
    real(kdouble), intent(out), allocatable :: var(:,:)
    !> [in] 配列サイズ
    integer(kint), intent(in) :: i
    !> [in] 配列サイズ
    integer(kint), intent(in) :: j
    integer(kint) :: ierr

    if(allocated(var))then
      call monolis_std_error_string("monolis_alloc_R_2d")
      call monolis_std_error_string("input arg. is already allocated")
      call monolis_std_error_stop()
    endif

    allocate(var(i,j), source = 0.0d0, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_alloc_R_2d")
      call monolis_std_error_string("allocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_alloc_R_2d

  !> @ingroup alloc
  !> 2 次元浮動小数点配列のメモリ開放
  subroutine monolis_dealloc_R_2d(var)
    implicit none
    !> [in] メモリ開放する配列
    real(kdouble), allocatable :: var(:,:)
    integer(kint) :: ierr

    if(.not. allocated(var))then
      return
    endif

    deallocate(var, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_dealloc_R_2d")
      call monolis_std_error_string("deallocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_dealloc_R_2d

  !> @ingroup alloc
  !> 2 次元実数配列のメモリ再確保
  !> @details 再確保で増えた配列部分は初期値 0 でメモリ確保がなされる。
  subroutine monolis_realloc_R_2d(var, m, n)
    implicit none
    !> [in,out] メモリ確保する配列
    real(kdouble), allocatable :: var(:,:)
    !> [in] 再確保後の配列サイズ（1 次元目）
    integer(kint), intent(in) :: m
    !> [in] 再確保後の配列サイズ（2 次元目）
    integer(kint), intent(in) :: n
    real(kdouble), allocatable :: temp(:,:)
    integer(kint) :: i, j, nold, mold

    if(.not. allocated(var))then
      call monolis_alloc_R_2d(var, m, n)
      return
    endif

    mold = size(var, 1)
    nold = size(var, 2)

    call monolis_alloc_R_2d(temp, mold, nold)

    temp = var

    call monolis_dealloc_R_2d(var)

    call monolis_alloc_R_2d(var, m, n)

    do j = 1, min(nold, n)
      do i = 1, min(mold, m)
        var(i,j) = temp(i,j)
      enddo
    enddo
  end subroutine monolis_realloc_R_2d

  !> @ingroup alloc
  !> 2 次元実数配列の末尾にデータ配列を追加
  subroutine monolis_append_R_2d(var, n_add, var_add)
    implicit none
    !> [in,out] 元の配列
    real(kdouble), allocatable :: var(:,:)
    !> [in] 追加する配列サイズ
    integer(kint), intent(in) :: n_add
    !> [in] 追加する配列
    real(kdouble), intent(in) :: var_add(:,:)
    integer(kint) :: n_all, n_old, i, m

    if(.not. allocated(var))then
      n_old = 0
    else
      n_old = size(var, 2)
    endif

    m = size(var, 1)
    n_all = n_old + n_add

    call monolis_realloc_R_2d(var, m, n_all)

    do i = n_old + 1, n_all
      var(:,i) = var_add(:,i - n_old)
    enddo
  end subroutine monolis_append_R_2d

  !> @ingroup alloc
  !> 1 次元複素数型配列のメモリ確保
  subroutine monolis_alloc_C_1d(var, i)
    implicit none
    !> [out] メモリ確保する配列
    complex(kdouble), intent(out), allocatable :: var(:)
    !> [in] 配列サイズ
    integer(kint), intent(in) :: i
    integer(kint) :: ierr

    if(allocated(var))then
      call monolis_std_error_string("monolis_alloc_C_1d")
      call monolis_std_error_string("input arg. is already allocated")
      call monolis_std_error_stop()
    endif

    allocate(var(i), source = (0.0d0, 0.0d0), stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_alloc_C_1d")
      call monolis_std_error_string("allocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_alloc_C_1d

  !> @ingroup alloc
  !> 1 次元浮動小数点配列のメモリ開放
  subroutine monolis_dealloc_C_1d(var)
    implicit none
    !> [in] メモリ開放する配列
    complex(kdouble), allocatable :: var(:)
    integer(kint) :: ierr

    if(.not. allocated(var))then
      return
    endif

    deallocate(var, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_dealloc_C_1d")
      call monolis_std_error_string("deallocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_dealloc_C_1d

  !> @ingroup alloc
  !> 2 次元浮動小数点配列のメモリ確保
  !> @details 配列サイズは var(i, j) として確保される。
  subroutine monolis_alloc_C_2d(var, i, j)
    implicit none
    !> [out] メモリ確保する配列（サイズ [i, j]）
    complex(kdouble), intent(out), allocatable :: var(:,:)
    !> [in] 配列サイズ
    integer(kint), intent(in) :: i
    !> [in] 配列サイズ
    integer(kint), intent(in) :: j
    integer(kint) :: ierr

    if(allocated(var))then
      call monolis_std_error_string("monolis_alloc_C_2d")
      call monolis_std_error_string("input arg. is already allocated")
      call monolis_std_error_stop()
    endif

    allocate(var(i,j), source = (0.0d0, 0.0d0), stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_alloc_C_2d")
      call monolis_std_error_string("allocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_alloc_C_2d

  !> @ingroup alloc
  !> 2 次元浮動小数点配列のメモリ開放
  subroutine monolis_dealloc_C_2d(var)
    implicit none
    !> [in] メモリ開放する配列
    complex(kdouble), allocatable :: var(:,:)
    integer(kint) :: ierr

    if(.not. allocated(var))then
      return
    endif

    deallocate(var, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_dealloc_C_2d")
      call monolis_std_error_string("deallocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_dealloc_C_2d

  !> @ingroup alloc
  !> 1 次元論理型配列のメモリ確保
  subroutine monolis_alloc_L_1d(var, i)
    implicit none
    !> [out] メモリ確保する配列
    logical, intent(out), allocatable :: var(:)
    !> [in] 配列サイズ
    integer(kint), intent(in) :: i
    integer(kint) :: ierr

    if(allocated(var))then
      call monolis_std_error_string("monolis_alloc_L_1d")
      call monolis_std_error_string("input arg. is already allocated")
      call monolis_std_error_stop()
    endif

    allocate(var(i), source = .false., stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_alloc_L_1d")
      call monolis_std_error_string("allocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_alloc_L_1d

  !> @ingroup alloc
  !> 1 次元論理型配列のメモリ開放
  subroutine monolis_dealloc_L_1d(var)
    implicit none
    !> [in] メモリ開放する配列
    logical, allocatable :: var(:)
    integer(kint) :: ierr

    if(.not. allocated(var))then
      return
    endif

    deallocate(var, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_dealloc_L_1d")
      call monolis_std_error_string("deallocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_dealloc_L_1d

  !> @ingroup alloc
  !> 2 次元論理型配列のメモリ確保
  !> @details 配列サイズは var(i, j) として確保される。
  subroutine monolis_alloc_L_2d(var, i, j)
    implicit none
    !> [out] メモリ確保する配列（サイズ [i, j]）
    logical, intent(out), allocatable :: var(:,:)
    !> [in] 配列サイズ
    integer(kint), intent(in) :: i
    !> [in] 配列サイズ
    integer(kint), intent(in) :: j
    integer(kint) :: ierr

    if(allocated(var))then
      call monolis_std_error_string("monolis_alloc_L_2d")
      call monolis_std_error_string("input arg. is already allocated")
      call monolis_std_error_stop()
    endif

    allocate(var(i,j), source = .false., stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_alloc_L_2d")
      call monolis_std_error_string("allocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_alloc_L_2d

  !> @ingroup alloc
  !> 2 次元論理型配列のメモリ開放
  subroutine monolis_dealloc_L_2d(var)
    implicit none
    !> [in] メモリ開放する配列
    logical, allocatable :: var(:,:)
    integer(kint) :: ierr

    if(.not. allocated(var))then
      return
    endif

    deallocate(var, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_dealloc_L_2d")
      call monolis_std_error_string("deallocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_dealloc_L_2d
end module mod_monolis_utils_alloc
