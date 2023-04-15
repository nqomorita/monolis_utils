!> std テストモジュール
!# monolis_test_check_eq_I1(header, a, b)
!# monolis_test_check_eq_R1(header, a, b)
!# monolis_test_check_eq_C1(header, a, b)
!# monolis_test_check_eq_L1(header, a, b)
!# monolis_test_check_eq_I(header, a, b)
!# monolis_test_check_eq_R(header, a, b)
!# monolis_test_check_eq_C(header, a, b)
!# monolis_test_check_eq_L(header, a, b)
!# monolis_test_assert_pass(header)
!# monolis_test_assert_fail(header, reason)
!#
!# !> develop use
!# monolis_test_check_eq_I_main(a, b, is_eq)
!# monolis_test_check_eq_R_main(a, b, is_eq)
!# monolis_test_check_eq_C_main(a, b, is_eq)
!# monolis_test_check_eq_L_main(a, b, is_eq)
module mod_monolis_utils_std_test
  use mod_monolis_utils_define_prm
  implicit none

contains

  !> @ingroup dev_test
  !> 整数値の比較（メイン関数）
  subroutine monolis_test_check_eq_I_main(a, b, is_eq)
    implicit none
    !> [in] 入力配列 a
    integer(kint), intent(in) :: a
    !> [in] 入力配列 b
    integer(kint), intent(in) :: b
    !> [out] 一致判定（一致していれば .ture.）
    logical, intent(out) :: is_eq

    if(a - b /= 0)then
      is_eq = .false.
    else
      is_eq = .true.
    endif
  end subroutine monolis_test_check_eq_I_main

  !> @ingroup dev_test
  !> 実数値の比較（メイン関数）
  subroutine monolis_test_check_eq_R_main(a, b, is_eq)
    implicit none
    !> [in] 入力配列 a
    real(kdouble), intent(in) :: a
    !> [in] 入力配列 b
    real(kdouble), intent(in) :: b
    !> [out] 一致判定（一致していれば .ture.）
    logical, intent(out) :: is_eq

    if(abs(a) < 1.0d-20)then
      if(abs(a - b) > monolis_test_ths)then
        is_eq = .false.
      else
        is_eq = .true.
      endif
    elseif(abs(a - b)/abs(a) > monolis_test_ths)then
      is_eq = .false.
    else
      is_eq = .true.
    endif
  end subroutine monolis_test_check_eq_R_main

  !> @ingroup dev_test
  !> 複素数値の比較（メイン関数）
  subroutine monolis_test_check_eq_C_main(a, b, is_eq)
    implicit none
    !> [in] 入力配列 a
    complex(kdouble), intent(in) :: a
    !> [in] 入力配列 b
    complex(kdouble), intent(in) :: b
    !> [out] 一致判定（一致していれば .ture.）
    logical, intent(out) :: is_eq

    if(abs(real(a)) < 1.0d-20 .or. abs(dimag(a)) < 1.0d-20)then
      if(abs(real(a) - real(b)) + abs(dimag(a) - dimag(b)) > monolis_test_ths)then
        is_eq = .false.
      else
        is_eq = .true.
      endif
    elseif(abs(real(a) - real(b))/abs(real(a)) + abs(dimag(a) - dimag(b))/abs(dimag(a)) &
      & > monolis_test_ths)then
      is_eq = .false.
    else
      is_eq = .true.
    endif
  end subroutine monolis_test_check_eq_C_main

  !> @ingroup dev_test
  !> 複素数値の比較（メイン関数）
  subroutine monolis_test_check_eq_L_main(a, b, is_eq)
    implicit none
    !> [in] 入力配列 a
    logical, intent(in) :: a
    !> [in] 入力配列 b
    logical, intent(in) :: b
    !> [out] 一致判定（一致していれば .ture.）
    logical, intent(out) :: is_eq

    if(a .eqv. b)then
      is_eq = .true.
    else
      is_eq = .false.
    endif
  end subroutine monolis_test_check_eq_L_main

  !> @ingroup test
  !> 整数値（スカラ）の比較
  subroutine monolis_test_check_eq_I1(header, a, b)
    implicit none
    !> [in] テスト内容を示す文字列
    character(*), intent(in)  :: header
    !> [in] 入力配列 a
    integer(kint), intent(in) :: a
    !> [in] 入力配列 b
    integer(kint), intent(in) :: b
    logical :: is_eq

    call monolis_test_check_eq_I_main(a, b, is_eq)

    if(is_eq)then
      call monolis_test_assert_pass(header)
    else
      call monolis_test_assert_fail(header, "value mismatch")
    endif
  end subroutine monolis_test_check_eq_I1

  !> @ingroup test
  !> 実数値（スカラ）の比較
  subroutine monolis_test_check_eq_R1(header, a, b)
    implicit none
    !> [in] テスト内容を示す文字列
    character(*), intent(in)  :: header
    !> [in] 入力配列 a
    real(kdouble), intent(in) :: a
    !> [in] 入力配列 b
    real(kdouble), intent(in) :: b
    logical :: is_eq

    call monolis_test_check_eq_R_main(a, b, is_eq)

    if(is_eq)then
      call monolis_test_assert_pass(header)
    else
      call monolis_test_assert_fail(header, "value mismatch")
    endif
  end subroutine monolis_test_check_eq_R1

  !> @ingroup test
  !> 複素数値（スカラ）の比較
  subroutine monolis_test_check_eq_C1(header, a, b)
    implicit none
    !> [in] テスト内容を示す文字列
    character(*), intent(in)  :: header
    !> [in] 入力配列 a
    complex(kdouble), intent(in) :: a
    !> [in] 入力配列 b
    complex(kdouble), intent(in) :: b
    logical :: is_eq

    call monolis_test_check_eq_C_main(a, b, is_eq)

    if(is_eq)then
      call monolis_test_assert_pass(header)
    else
      call monolis_test_assert_fail(header, "value mismatch")
    endif
  end subroutine monolis_test_check_eq_C1

  !> @ingroup test
  !> 論理型（スカラ）の比較
  subroutine monolis_test_check_eq_L1(header, a, b)
    implicit none
    !> [in] テスト内容を示す文字列
    character(*), intent(in)  :: header
    !> [in] 入力配列 a
    logical, intent(in) :: a
    !> [in] 入力配列 b
    logical, intent(in) :: b
    logical :: is_eq

    call monolis_test_check_eq_L_main(a, b, is_eq)

    if(is_eq)then
      call monolis_test_assert_pass(header)
    else
      call monolis_test_assert_fail(header, "value mismatch")
    endif
  end subroutine monolis_test_check_eq_L1

  !> @ingroup test
  !> 整数値（ベクトル）の比較
  subroutine monolis_test_check_eq_I(header, a, b)
    implicit none
    !> [in] テスト内容を示す文字列
    character(*), intent(in)  :: header
    !> [in] 入力配列 a
    integer(kint), intent(in) :: a(:)
    !> [in] 入力配列 b
    integer(kint), intent(in) :: b(:)
    integer(kint) :: i
    logical :: is_eq

    if(size(a) /= size(b))then
      call monolis_test_assert_fail(header, "size mismatch")
    endif

    do i = 1, size(a)
      call monolis_test_check_eq_I_main(a(i), b(i), is_eq)
      if(.not. is_eq) call monolis_test_assert_fail(header, "value mismatch")
    enddo

    call monolis_test_assert_pass(header)
  end subroutine monolis_test_check_eq_I

  !> @ingroup test
  !> 実数値（ベクトル）の比較
  subroutine monolis_test_check_eq_R(header, a, b)
    implicit none
    !> [in] テスト内容を示す文字列
    character(*), intent(in)  :: header
    !> [in] 入力配列 a
    real(kdouble), intent(in) :: a(:)
    !> [in] 入力配列 b
    real(kdouble), intent(in) :: b(:)
    integer(kint) :: i
    logical :: is_eq

    if(size(a) /= size(b))then
      call monolis_test_assert_fail(header, "size mismatch")
    endif

    do i = 1, size(a)
      call monolis_test_check_eq_R_main(a(i), b(i), is_eq)
      if(.not. is_eq) call monolis_test_assert_fail(header, "value mismatch")
    enddo

    call monolis_test_assert_pass(header)
  end subroutine monolis_test_check_eq_R

  !> @ingroup test
  !> 複素数値（ベクトル）の比較
  subroutine monolis_test_check_eq_C(header, a, b)
    implicit none
    !> [in] テスト内容を示す文字列
    character(*), intent(in)  :: header
    !> [in] 入力配列 a
    complex(kdouble), intent(in) :: a(:)
    !> [in] 入力配列 b
    complex(kdouble), intent(in) :: b(:)
    integer(kint) :: i
    logical :: is_eq

    if(size(a) /= size(b))then
      call monolis_test_assert_fail(header, "size mismatch")
    endif

    do i = 1, size(a)
      call monolis_test_check_eq_C_main(a(i), b(i), is_eq)
      if(.not. is_eq) call monolis_test_assert_fail(header, "value mismatch")
    enddo

    call monolis_test_assert_pass(header)
  end subroutine monolis_test_check_eq_C

  !> @ingroup test
  !> 複素数値（ベクトル）の比較
  subroutine monolis_test_check_eq_L(header, a, b)
    implicit none
    !> [in] テスト内容を示す文字列
    character(*), intent(in)  :: header
    !> [in] 入力配列 a
    logical, intent(in) :: a(:)
    !> [in] 入力配列 b
    logical, intent(in) :: b(:)
    integer(kint) :: i
    logical :: is_eq

    if(size(a) /= size(b))then
      call monolis_test_assert_fail(header, "size mismatch")
    endif

    do i = 1, size(a)
      call monolis_test_check_eq_L_main(a(i), b(i), is_eq)
      if(.not. is_eq) call monolis_test_assert_fail(header, "value mismatch")
    enddo

    call monolis_test_assert_pass(header)
  end subroutine monolis_test_check_eq_L

  !> @ingroup test
  !> テストパス時の標準出力
  subroutine monolis_test_assert_pass(header)
    use mod_monolis_mpi_util
    implicit none
    !> [in] テスト内容を示す文字列
    character(*), intent(in) :: header
    character :: esc*1 = char(27)

    if(monolis_mpi_get_global_my_rank() == 0)then
      write(*,"(a,a)")esc//"[32m"//"[ PASSED ] "//trim(header)//esc//"[0m"
    endif
  end subroutine monolis_test_assert_pass

  !> @ingroup test
  !> テストエラー時の標準出力・エラーストップ
  subroutine monolis_test_assert_fail(header, reason)
    implicit none
    !> [in] テスト内容を示す文字列
    character(*), intent(in) :: header
    !> [in] エラー内容を示す文字列
    character(*), intent(in) :: reason
    character :: esc*1 = char(27)

    write(*,"(a,a)")esc//"[31m"//"[ FAILED ] "//trim(header)//esc//"[0m"//" : "//trim(reason)
    error stop monolis_fail
  end subroutine monolis_test_assert_fail
end module mod_monolis_utils_std_test
