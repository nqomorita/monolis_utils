module mod_monolis_driver_util
  use mod_monolis_utils_alloc
  implicit none

contains

  !> @ingroup dev_driver
  !> @brief 出力境界条件値を取得
  subroutine monolis_driver_get_arg_dbc_all_R(n_dof, val, is_get)
    implicit none
    !> [out] 計算点が持つ自由度
    integer(kint), intent(out) :: n_dof
    !> [out] 入力値
    real(kdouble), allocatable, intent(out) :: val(:)
    !> [out] 引数の取得判定
    logical, intent(out) :: is_get
    integer(kint) :: i, j, count
    character(monolis_charlen) :: argc1

    is_get = .false.

    j = 0
    count = iargc()
    do i = 1, count/2
      j = i
      call getarg(2*i-1, argc1)
      if(trim(argc1(1:1)) == "-")then
      else
        exit
      endif
    enddo

    if(2*j+1 > count) return

    call getarg(2*j-1, argc1)

    read(argc1,*) n_dof

    if(n_dof < 1) return

    call monolis_alloc_R_1d(val, n_dof)

    do i = 1, n_dof
      call getarg(2*j-1 + i, argc1)
      read(argc1,*) val(i)
    enddo

    is_get = .true.
  end subroutine monolis_driver_get_arg_dbc_all_R

  !> @ingroup dev_driver
  !> @brief 出力境界条件値を取得
  subroutine monolis_driver_get_arg_dbc_all_C(n_dof, val, is_get)
    implicit none
    !> [out] 計算点が持つ自由度
    integer(kint), intent(out) :: n_dof
    !> [out] 入力値
    complex(kdouble), allocatable, intent(out) :: val(:)
    !> [out] 引数の取得判定
    logical, intent(out) :: is_get
    integer(kint) :: i, j, count
    real(kdouble) :: r(2)
    character(monolis_charlen) :: argc1

    is_get = .false.

    j = 0
    count = iargc()
    do i = 1, count/2
      j = i
      call getarg(2*i-1, argc1)
      if(trim(argc1(1:1)) == "-")then
      else
        exit
      endif
    enddo

    if(2*j+3 > count) return

    call getarg(2*j-1, argc1)

    read(argc1,*) n_dof

    if(n_dof < 1) return

    call monolis_alloc_C_1d(val, n_dof)

    do i = 1, n_dof
      call getarg(2*j - 1 + 2*i - 1, argc1)
      read(argc1,*) r(1)
      call getarg(2*j - 1 + 2*i    , argc1)
      read(argc1,*) r(2)

      val(i) = cmplx(r(1), r(2))
    enddo

    is_get = .true.
  end subroutine monolis_driver_get_arg_dbc_all_C
end module mod_monolis_driver_util
