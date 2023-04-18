!> IO 行列モジュール
module mod_monolis_io_mtx
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  implicit none

contains

  !> @ingroup io
  !> matrix market 形式の行列データを取得（実数型）
  subroutine monolis_input_mtx_R(fname, N, NZ, elem, coef)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [out] 行列の自由度
    integer(kint), intent(out) :: N
    !> [out] 行列の非零要素数
    integer(kint), intent(out) :: NZ
    !> [out] 行列の非零要素数
    integer(kint), allocatable, intent(out) :: elem(:,:)
    !> [out] 行列の非零要素数
    real(kdouble), allocatable, intent(out) :: coef(:)
    integer(kint) :: i, in, ierr
    character(monolis_charlen) :: ctemp
    logical :: is_first

    !> count lines
    is_first = .true.
    open(20, file = trim(fname), status = "old")
    in = 0

    do
      read(20, "(a)", iostat = ierr) ctemp
      if(0 /= ierr) exit

      if(ctemp(1:1) == "%") cycle

      if(is_first)then
        backspace(20)
        read(20,*) N, i, NZ

        call monolis_alloc_I_2d(elem, 2, NZ)
        call monolis_alloc_R_1d(coef, NZ)
        is_first = .false.
      else
        in = in + 1
        backspace(20)
        read(20,*) elem(1,in), elem(2,in), coef(in)
      endif
      if(in == NZ) exit
    enddo
    close(20)
  end subroutine monolis_input_mtx_R

  !> @ingroup io
  !> matrix market 形式の行列データを取得（複素数型用）
  subroutine monolis_input_mtx_C(fname, N, NZ, elem, coef)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [out] 行列の自由度
    integer(kint), intent(out) :: N
    !> [out] 行列の非零要素数
    integer(kint), intent(out) :: NZ
    !> [out] 行列の非零要素数
    integer(kint), allocatable, intent(out) :: elem(:,:)
    !> [out] 行列の非零要素数
    complex(kdouble), allocatable, intent(out) :: coef(:)
    integer(kint) :: i, in, ierr
    real(kdouble) :: r1, r2
    character(monolis_charlen) :: ctemp
    logical :: is_first

    !> count lines
    is_first = .true.
    open(20, file = trim(fname), status = "old")
    in = 0

    do
      read(20, "(a)", iostat = ierr) ctemp
      if(0 /= ierr) exit

      if(ctemp(1:1) == "%") cycle

      if(is_first)then
        backspace(20)
        read(20,*) N, i, NZ

        call monolis_alloc_I_2d(elem, 2, NZ)
        call monolis_alloc_C_1d(coef, NZ)
        is_first = .false.
      else
        in = in + 1
        backspace(20)
        read(20,*) elem(1,in), elem(2,in), r1, r2
        coef(in) = cmplx(r1, r2)
      endif
      if(in == NZ) exit
    enddo
    close(20)
  end subroutine monolis_input_mtx_C
end module mod_monolis_io_mtx
