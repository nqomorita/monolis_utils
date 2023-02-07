!> IO 引数モジュール
module mod_monolis_io_arg
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  implicit none

contains

  !> @ingroup io
  !> 実行ファイルの引数設定のチェック
  subroutine monolis_check_arg_input(tag, is_get)
    implicit none
    !> [in] 引数のタグ
    character(*) :: tag
    !> [out] 引数の取得判定
    logical :: is_get
    integer(kint) :: i, count
    character(monolis_charlen) :: argc1

    is_get = .false.

    count = iargc()

    do i = 1, count
      call getarg(i, argc1)
      if(trim(argc1) == trim(tag))then
        is_get = .true.
        return
      endif
    enddo
  end subroutine monolis_check_arg_input

  !> @ingroup io
  !> 実行ファイルの整数型引数の取得
  subroutine monolis_get_arg_input_I(tag, var, is_get)
    implicit none
    !> [in] 引数のタグ
    character(*) :: tag
    !> [out] 分割数
    integer(kint) :: var
    !> [out] 引数の取得判定
    logical :: is_get
    integer(kint) :: i, count
    character(monolis_charlen) :: argc1
    character(monolis_charlen) :: argc2

    is_get = .false.

    count = iargc()
    !if(mod(count,2) /= 0)then
    !  write(*,"(a)") "* monolis input arg error"
    !  call monolis_error_stop()
    !endif

    do i = 1, count/2
      call getarg(2*i-1, argc1)
      call getarg(2*i  , argc2)
      if(trim(argc1) == trim(tag))then
        read(argc2,*) var
        is_get = .true.
      endif
    enddo
  end subroutine monolis_get_arg_input_I

  !> @ingroup io
  !> 実行ファイルの実数型引数の取得
  subroutine monolis_get_arg_input_R(tag, var, is_get)
    implicit none
    !> [in] 引数のタグ
    character(*) :: tag
    !> [out] 分割数
    real(kdouble) :: var
    !> [out] 引数の取得判定
    logical :: is_get
    integer(kint) :: i, count
    character(monolis_charlen) :: argc1
    character(monolis_charlen) :: argc2

    is_get = .false.

    count = iargc()
    !if(mod(count,2) /= 0)then
    !  write(*,"(a)") "* monolis input arg error"
    !  call monolis_error_stop()
    !endif

    do i = 1, count/2
      call getarg(2*i-1, argc1)
      call getarg(2*i  , argc2)
      if(trim(argc1) == trim(tag))then
        read(argc2,*) var
        is_get = .true.
      endif
    enddo
  end subroutine monolis_get_arg_input_R

  !> @ingroup io
  !> 実行ファイルの文字列型引数の取得
  subroutine monolis_get_arg_input_S(tag, var, is_get)
    implicit none
    !> [in] 引数のタグ
    character(*) :: tag
    !> [out] 文字列型
    character(monolis_charlen) :: var
    !> [out] 引数の取得判定
    logical :: is_get
    integer(kint) :: i, count
    character(monolis_charlen) :: argc1
    character(monolis_charlen) :: argc2

    is_get = .false.

    count = iargc()
    !if(mod(count,2) /= 0)then
    !  write(*,"(a)") "* monolis input arg error"
    !  call monolis_error_stop()
    !endif

    do i = 1, count/2
      call getarg(2*i-1, argc1)
      call getarg(2*i  , argc2)

      if(trim(argc1) == trim(tag))then
        read(argc2,"(a)") var
        is_get = .true.
      endif
    enddo
  end subroutine monolis_get_arg_input_S

  !> @ingroup io
  !> 入力節点ファイル名を取得
  subroutine monolis_get_arg_input_in_tag(fnname)
    implicit none
    !> 入力節点ファイル名
    character(monolis_charlen) :: fnname
    logical :: is_get
    fnname = "node.dat"
    call monolis_get_arg_input_S("-in", fnname, is_get)
    call monolis_std_log_string2("[input node file]", fnname)
  end subroutine monolis_get_arg_input_in_tag

  !> @ingroup io
  !> 入力要素ファイル名を取得
  subroutine monolis_get_arg_input_ie_tag(fename)
    implicit none
    !> 入力要素ファイル名
    character(monolis_charlen) :: fename
    logical :: is_get
    fename = "elem.dat"
    call monolis_get_arg_input_S("-ie", fename, is_get)
    call monolis_std_log_string2("[input elem file]", fename)
  end subroutine monolis_get_arg_input_ie_tag

  !> @ingroup io
  !> 入力ファイル名を取得
  subroutine monolis_get_arg_input_i_tag(finame)
    implicit none
    !> 出力ファイル名
    character(monolis_charlen) :: finame
    logical :: is_get
    call monolis_get_arg_input_S("-i", finame, is_get)
    call monolis_std_log_string2("[input file]", finame)
  end subroutine monolis_get_arg_input_i_tag

  !> @ingroup io
  !> 出力ファイル名を取得
  subroutine monolis_get_arg_input_o_tag(foname)
    implicit none
    !> 出力ファイル名
    character(monolis_charlen) :: foname
    logical :: is_get
    call monolis_get_arg_input_S("-o", foname, is_get)
    call monolis_std_log_string2("[output file]", foname)
  end subroutine monolis_get_arg_input_o_tag
end module mod_monolis_io_arg
