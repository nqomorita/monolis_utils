!> パラメータモジュール
module mod_monolis_utils_define_prm
  implicit none

  !> integer 変数の精度
  integer(4), parameter :: kint    = 4
  !> real 変数の精度
  integer(4), parameter :: kdouble = 8

  !> monolis ライブラリにおける正常処理フラグ
  integer(kint), parameter :: monolis_success = 0
  !> monolis ライブラリにおける異常処理フラグ
  integer(kint), parameter :: monolis_fail = 1
  !> monolis ライブラリにおける文字列長さ
  integer(kint), parameter :: monolis_charlen = 256
  !> monolis ライブラリにおける整数型の true フラグ
  integer(kint), parameter :: monolis_I_true = 1
  !> monolis ライブラリにおける整数型の false フラグ
  integer(kint), parameter :: monolis_I_false = 0

  !> monolis ライブラリにおけるテスト機能の一致判定閾値
  real(kdouble), parameter :: monolis_test_ths = 1.0d-8

  !> monolis ライブラリにおけるデバッグ出力フラグ
  logical, save :: monolis_debug_log_write = .false.
end module mod_monolis_utils_define_prm