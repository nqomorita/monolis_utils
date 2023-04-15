!> メモリ確保モジュール
!# I_1d
!# subroutine monolis_palloc_I_1d(var, size)
!# subroutine monolis_pdealloc_I_1d(var)
!#
!# R_1d
!# subroutine monolis_palloc_R_1d(var, size)
!# subroutine monolis_pdealloc_R_1d(var)
!#
!# C_1d
!# subroutine monolis_palloc_C_1d(var, size)
!# subroutine monolis_pdealloc_C_1d(var)
module mod_monolis_utils_palloc
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_error
  implicit none

contains

  !> @ingroup alloc
  !> 1 次元整数ポインタのメモリ確保
  !> @details 初期値 0 でメモリ確保がなされる。
  subroutine monolis_palloc_I_1d(var, size)
    implicit none
    !> [in,out] メモリ確保するポインタ
    integer(kint), pointer, intent(inout) :: var(:)
    !> [in] ポインタサイズ
    integer(kint), intent(in) :: size
    integer(kint) :: ierr

    if(associated(var))then
      call monolis_std_error_string("monolis_palloc_I_1d")
      call monolis_std_error_string("input arg. is already allocated")
      call monolis_std_error_stop()
    endif

    allocate(var(size), source = 0, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_palloc_I_1d")
      call monolis_std_error_string("allocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_palloc_I_1d

  !> @ingroup alloc
  !> 1 次元整数ポインタのメモリ開放
  subroutine monolis_pdealloc_I_1d(var)
    implicit none
    !> [in,out] メモリ開放するポインタ
    integer(kint), pointer, intent(inout) :: var(:)
    integer(kint) :: ierr

    if(.not. associated(var))then
      return
    endif

    deallocate(var, stat = ierr)
    var => null()

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_pdealloc_I_1d")
      call monolis_std_error_string("deallocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_pdealloc_I_1d

  !> @ingroup alloc
  !> 1 次元浮動小数点ポインタのメモリ確保
  subroutine monolis_palloc_R_1d(var, size)
    implicit none
    !> [in,out] メモリ確保するポインタ
    real(kdouble), pointer, intent(inout) :: var(:)
    !> [in] ポインタサイズ
    integer(kint), intent(in) :: size
    integer(kint) :: ierr

    if(associated(var))then
      call monolis_std_error_string("monolis_palloc_R_1d")
      call monolis_std_error_string("input arg. is already allocated")
      call monolis_std_error_stop()
    endif

    allocate(var(size), source = 0.0d0, stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_palloc_R_1d")
      call monolis_std_error_string("allocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_palloc_R_1d

  !> @ingroup alloc
  !> 1 次元浮動小数点ポインタのメモリ開放
  subroutine monolis_pdealloc_R_1d(var)
    implicit none
    !> [in,out] メモリ開放するポインタ
    real(kdouble), pointer, intent(inout) :: var(:)
    integer(kint) :: ierr

    if(.not. associated(var))then
      return
    endif

    deallocate(var, stat = ierr)
    var => null()

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_pdealloc_R_1d")
      call monolis_std_error_string("deallocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_pdealloc_R_1d

  !> @ingroup alloc
  !> 1 次元複素数型ポインタのメモリ確保
  subroutine monolis_palloc_C_1d(var, size)
    implicit none
    !> [in,out] メモリ確保するポインタ
    complex(kdouble), pointer, intent(inout) :: var(:)
    !> [in] ポインタサイズ
    integer(kint), intent(in) :: size
    integer(kint) :: ierr

    if(associated(var))then
      call monolis_std_error_string("monolis_palloc_C_1d")
      call monolis_std_error_string("input arg. is already allocated")
      call monolis_std_error_stop()
    endif

    allocate(var(size), source = (0.0d0, 0.0d0), stat = ierr)

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_palloc_C_1d")
      call monolis_std_error_string("allocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_palloc_C_1d

  !> @ingroup alloc
  !> 1 次元浮動小数点ポインタのメモリ開放
  subroutine monolis_pdealloc_C_1d(var)
    implicit none
    !> [in,out] メモリ開放するポインタ
    complex(kdouble), pointer, intent(inout) :: var(:)
    integer(kint) :: ierr

    if(.not. associated(var))then
      return
    endif

    deallocate(var, stat = ierr)
    var => null()

    if(ierr /= 0)then
      call monolis_std_error_string("monolis_pdealloc_C_1d")
      call monolis_std_error_string("deallocation is failed")
      call monolis_std_error_stop()
    endif
  end subroutine monolis_pdealloc_C_1d
end module mod_monolis_utils_palloc
