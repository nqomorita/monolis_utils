!> ハッシュモジュール
module mod_monolis_utils_hash
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_error

  implicit none

  private

  public :: monolis_hash_structure
  public :: monolis_hash_init
  public :: monolis_hash_finalize
  public :: monolis_hash_get
  public :: monolis_hash_push
  public :: monolis_hash_get_key_I

  !> デフォルトハッシュサイズ id
  integer(kint), parameter :: monolis_init_hash_size_id = 5

  !> ハッシュサイズの定義
  integer(kint), parameter :: monolis_hash_size(22) = (/&
  &       1021,       2039,      4093,      8191,     16381, &
  &      32749,      65521,    131071,    262139,    524287, &
  &    1048573,    2097143,   4194301,   8388593,  16777213, &
  &   33554393,   67108859, 134217689, 268435399, 536870909, &
  & 1073741789, 2147483647/)

  !> リスト構造体
  type type_monolis_hash_list
    !> ハッシュ
    integer(kint) :: hash = 0
    !> 登録値
    integer(kint) :: val  = 0
    !> キー
    character(:), allocatable :: key
  end type type_monolis_hash_list

  !> ビン構造体
  type type_monolis_hash_bin
    !> リストサイズ
    integer(kint) :: n = 0
    !> リスト配列
    type(type_monolis_hash_list), pointer :: list(:)
  end type type_monolis_hash_bin

  !> ハッシュ構造体
  type monolis_hash_structure
    !> ハッシュサイズ id
    integer(kint) :: hash_size_id = 1
    !> キーサイズ
    integer(kint) :: key_size = 1
    !> 登録データ数
    integer(kint) :: n_put = 0
    !> ビン配列
    type(type_monolis_hash_bin), pointer :: bin(:)
  end type monolis_hash_structure

contains

  !> @ingroup hash
  !> @brief ハッシュ構造体の初期化関数
  subroutine monolis_hash_init(monolis_hash, key_size)
    implicit none
    !> [in,out] ハッシュ構造体
    type(monolis_hash_structure), intent(inout) :: monolis_hash
    !> [in] キーサイズ
    integer(kint), intent(in) :: key_size
    type(type_monolis_hash_bin), pointer :: bin(:)
    integer(kint) :: hash_size

    monolis_hash%n_put = 0
    monolis_hash%key_size = key_size
    monolis_hash%hash_size_id = monolis_init_hash_size_id

    hash_size = monolis_hash_size(monolis_hash%hash_size_id)
    allocate(bin(hash_size))
    monolis_hash%bin => bin

    nullify(bin)
  end subroutine monolis_hash_init

  !> @ingroup hash
  !> @brief ハッシュ構造体の終了関数
  subroutine monolis_hash_finalize(monolis_hash)
    implicit none
    !> [in,out] ハッシュ構造体
    type(monolis_hash_structure), intent(inout) :: monolis_hash
    type(type_monolis_hash_list), pointer :: list(:)
    integer(kint) :: i, j, hash_size

    hash_size = monolis_hash_size(monolis_hash%hash_size_id)
    do i = 1, hash_size
      if(0 < monolis_hash%bin(i)%n)then
        list => monolis_hash%bin(i)%list
        do j = 1, monolis_hash%bin(i)%n
          deallocate(list(j)%key)
        enddo
        deallocate(monolis_hash%bin(i)%list)
      endif
    enddo

    deallocate(monolis_hash%bin)
    nullify(list)
  end subroutine monolis_hash_finalize

  !> @ingroup hash
  !> @brief 整数値からキーを取得
  subroutine monolis_hash_get_key_I(key_size, i, key)
    implicit none
    !> [in] キーサイズ
    integer(kint), intent(in) :: key_size
    !> [in] 入力整数値
    integer(kint), intent(in) :: i
    !> [out] キー
    character(*), intent(out) :: key
    character(monolis_charlen) :: ctmp
    character(monolis_charlen) :: cstyle

    if(key_size < len(key))then
      call monolis_std_error_string("monolis_hash_get_key_I")
      call monolis_std_error_string("input key_size is larger than actural key length")
      call monolis_std_error_stop()
    endif

    write(cstyle,"(a,i0,a,i0,a)")'(i', key_size, ".", key_size, ')'
    write(ctmp,trim(cstyle)) i
    key = ctmp(1:key_size)
  end subroutine monolis_hash_get_key_I

  !> @ingroup hash
  !> @brief キーから登録値の取得
  subroutine monolis_hash_get(monolis_hash, key, val, is_exist)
    implicit none
    !> [in] ハッシュ構造体
    type(monolis_hash_structure), intent(in) :: monolis_hash
    !> [in] キー
    character(*), intent(in) :: key
    !> [out] 取得値
    integer(kint), intent(out) :: val
    !> [out] データ取得フラグ（正常にデータ取得：.true.）
    logical, intent(out) :: is_exist
    integer(kint) :: hash

    val = 0
    is_exist = .false.
    call monolis_hash_key(key, monolis_hash%key_size, hash)
    call monolis_hash_list_get(monolis_hash, key, hash, val, is_exist)
  end subroutine monolis_hash_get

  !> @ingroup hash
  !> @brief ハッシュ構造体へ値の登録
  subroutine monolis_hash_push(monolis_hash, key, val, is_pushed, is_exist)
    implicit none
    !> [in,out] ハッシュ構造体
    type(monolis_hash_structure), intent(inout) :: monolis_hash
    !> [in] キー
    character(*), intent(in) :: key
    !> [in] 入力値
    integer(kint), intent(in) :: val
    !> [out] キーの登録フラグ（正常に登録した場合：.true.）
    logical, intent(out) :: is_pushed
    !> [out] キーの存在フラグ（既に同じキーで登録済の場合：.true.）
    logical, intent(out) :: is_exist
    integer(kint) :: hash, tmp

    is_pushed = .false.
    is_exist  = .false.

    if(0.75d0*dble(monolis_hash_size(monolis_hash%hash_size_id)) < dble(monolis_hash%n_put))then
      call monolis_hash_resize(monolis_hash)
    endif

    call monolis_hash_key(key, monolis_hash%key_size, hash)
    call monolis_hash_list_get(monolis_hash, key, hash, tmp, is_exist)

    if(.not. is_exist)then
      call monolis_hash_list_push(monolis_hash, key, hash, val)
      monolis_hash%n_put = monolis_hash%n_put + 1
      is_pushed = .true.
    else
      call monolis_hash_list_update(monolis_hash, key, hash, val)
      is_pushed = .true.
    endif
  end subroutine monolis_hash_push

  !> @ingroup dev_hash
  !> @brief ハッシュのリサイズ
  subroutine monolis_hash_resize(monolis_hash)
    implicit none
    !> [in,out] ハッシュ構造体
    type(monolis_hash_structure), intent(inout) :: monolis_hash
    integer(kint) :: i, j, hash, val
    integer(kint) :: new_size, old_size
    type(type_monolis_hash_bin), pointer :: new_bin(:), old_bin(:), temp_bin
    type(type_monolis_hash_list), pointer :: list(:)
    character(:), allocatable :: key

    old_size = monolis_hash_size(monolis_hash%hash_size_id)
    if(monolis_hash%hash_size_id < 22)then
      monolis_hash%hash_size_id = monolis_hash%hash_size_id + 1
    endif
    new_size = monolis_hash_size(monolis_hash%hash_size_id)

    allocate(new_bin(new_size))
    old_bin => monolis_hash%bin
    monolis_hash%bin => new_bin

    do i = 1, old_size
      temp_bin => old_bin(i)
      temp_bin%list => old_bin(i)%list
      do j = 1, old_bin(i)%n
        hash = temp_bin%list(j)%hash
        key  = temp_bin%list(j)%key
        val  = temp_bin%list(j)%val
        call monolis_hash_list_push(monolis_hash, key, hash, val)
      enddo
    enddo

    do i = 1, old_size
      list => old_bin(i)%list
      if(associated(list)) deallocate(list)
    enddo

    deallocate(old_bin)
    nullify(temp_bin)
    nullify(old_bin)
    nullify(new_bin)
  end subroutine monolis_hash_resize

  !> @ingroup dev_hash
  !> @brief キーから登録値の取得（メイン関数）
  subroutine monolis_hash_list_get(monolis_hash, key, hash, val, is_exist)
    implicit none
    !> [in] ハッシュ構造体
    type(monolis_hash_structure), intent(in) :: monolis_hash
    !> [in] キー
    character(*), intent(in) :: key
    !> [in] ハッシュ
    integer(kint), intent(in) :: hash
    !> [out] 取得値
    integer(kint), intent(out) :: val
    !> [out] データ取得フラグ（正常にデータ取得：.true.）
    logical, intent(out) :: is_exist
    integer(kint) :: n, i
    integer(kint) :: idx, hash_size

    is_exist = .false.
    hash_size = monolis_hash_size(monolis_hash%hash_size_id)
    call monolis_index_key(hash, hash_size, idx)
    n = monolis_hash%bin(idx)%n
    do i = 1, n
      if(monolis_hash%bin(idx)%list(i)%key == key)then
        val = monolis_hash%bin(idx)%list(i)%val
        is_exist = .true.
        return
      endif
    enddo
  end subroutine monolis_hash_list_get

  !> @ingroup dev_hash
  !> @brief ハッシュ構造体へ値の登録（メイン関数）
  subroutine monolis_hash_list_push(monolis_hash, key, hash, val)
    implicit none
    !> [in,out] ハッシュ構造体
    type(monolis_hash_structure), intent(inout) :: monolis_hash
    !> [in] キー
    character(*), intent(in) :: key
    !> [in] ハッシュ
    integer(kint), intent(in) :: hash
    !> [in] 入力値
    integer(kint), intent(in) :: val
    integer(kint) :: i, iold, inew
    integer(kint) :: index, hash_size
    type(type_monolis_hash_list), pointer :: old_list(:), new_list(:)

    hash_size = monolis_hash_size(monolis_hash%hash_size_id)
    call monolis_index_key(hash, hash_size, index)
    iold = monolis_hash%bin(index)%n
    old_list => monolis_hash%bin(index)%list

    inew = iold + 1
    allocate(new_list(inew))
    do i = 1, iold
      new_list(i)%hash = old_list(i)%hash
      new_list(i)%key  = old_list(i)%key
      new_list(i)%val  = old_list(i)%val
    enddo

    new_list(inew)%hash = hash
    new_list(inew)%key  = key
    new_list(inew)%val  = val

    monolis_hash%bin(index)%n = inew
    monolis_hash%bin(index)%list => new_list
    if(associated(old_list)) deallocate(old_list)
    nullify(old_list)
    nullify(new_list)
  end subroutine monolis_hash_list_push

  !> @ingroup dev_hash
  !> @brief ハッシュの登録値の更新
  subroutine monolis_hash_list_update(monolis_hash, key, hash, val)
    implicit none
    !> [in,out] ハッシュ構造体
    type(monolis_hash_structure), intent(inout) :: monolis_hash
    !> [in] キー
    character(*), intent(in) :: key
    !> [in] ハッシュ
    integer(kint), intent(in) :: hash
    !> [in] 入力値
    integer(kint), intent(in) :: val
    integer(kint) :: i, n
    integer(kint) :: index, hash_size

    hash_size = monolis_hash_size(monolis_hash%hash_size_id)
    call monolis_index_key(hash, hash_size, index)

    n = monolis_hash%bin(index)%n
    do i = 1, n
      if(monolis_hash%bin(index)%list(i)%key == key)then
        monolis_hash%bin(index)%list(i)%val = val
      endif
    enddo
  end subroutine monolis_hash_list_update

  !> @ingroup dev_hash
  !> @brief BJD2 hash function
  subroutine monolis_hash_key(key, key_size, hash)
    implicit none
    !> [in] キー
    character(*), intent(in) :: key
    !> [in] キーサイズ
    integer(kint), intent(in) :: key_size
    !> [out] ハッシュ
    integer(kint), intent(out) :: hash
    integer(kint) :: i, t

    hash = 5381
    do i = 1, key_size
      t = mod(hash*33, 65536_4)
      hash = mod(t + iachar(key(i:i)), 65536_4)
    enddo
  end subroutine monolis_hash_key

  !> @ingroup dev_hash
  !> @brief ハッシュインデックス取得
  subroutine monolis_index_key(hash, hash_size, index)
    implicit none
    !> [in] ハッシュ
    integer(kint), intent(in) :: hash
    !> [in] ハッシュサイズ
    integer(kint), intent(in) :: hash_size
    !> [out] インデックス
    integer(kint), intent(out) :: index

    index = mod(hash, hash_size) + 1
  end subroutine monolis_index_key
end module mod_monolis_utils_hash