!> MPI モジュール
module mod_monolis_mpi
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_define_R_N128
  use mod_monolis_mpi_util
  use mod_monolis_utils_alloc
  use mod_monolis_utils_define_com
  use mod_monolis_utils_std_sort_I
  implicit none

  !> MPI 演算タグ（和）
  integer(kint), parameter :: monolis_mpi_sum = 1
  !> MPI 演算タグ（最大値）
  integer(kint), parameter :: monolis_mpi_max = 2
  !> MPI 演算タグ（最小値）
  integer(kint), parameter :: monolis_mpi_min = 3

contains

  !> @ingroup mpi
  !> allreduce 関数（整数型）
  subroutine monolis_allreduce_I1(val, tag, comm)
    implicit none
    !> [in,out] 入出力値（整数型）
    integer(kint), intent(inout) :: val
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(kint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint)  :: n, ierr, in(1), out(1)

#ifndef NO_MPI
    in = val
    out = 0
    n = 1
    if(tag == monolis_mpi_sum)then
      call MPI_allreduce(in, out, n, monolis_mpi_int, MPI_SUM, comm, ierr)
    elseif(tag == monolis_mpi_max)then
      call MPI_allreduce(in, out, n, monolis_mpi_int, MPI_MAX, comm, ierr)
    elseif(tag == monolis_mpi_min)then
      call MPI_allreduce(in, out, n, monolis_mpi_int, MPI_MIN, comm, ierr)
    endif
    val = out(1)
#endif
  end subroutine monolis_allreduce_I1

  !> @ingroup mpi
  !> allreduce 関数（整数配列型）
  subroutine monolis_allreduce_I(n, val, tag, comm)
    implicit none
    !> [in] 配列サイズ
    integer(kint), intent(in) :: n
    !> [in,out] 入出力値（整数型）
    integer(kint), intent(inout) :: val(n)
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(kint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint)  :: ierr, temp(n)

#ifndef NO_MPI
    temp = 0
    if(tag == monolis_mpi_sum)then
      call MPI_allreduce(val, temp, n, monolis_mpi_int, MPI_SUM, comm, ierr)
    elseif(tag == monolis_mpi_max)then
      call MPI_allreduce(val, temp, n, monolis_mpi_int, MPI_MAX, comm, ierr)
    elseif(tag == monolis_mpi_min)then
      call MPI_allreduce(val, temp, n, monolis_mpi_int, MPI_MIN, comm, ierr)
    endif
    val = temp
#endif
  end subroutine monolis_allreduce_I

  !> @ingroup mpi
  !> allreduce 関数（浮動小数点型）
  subroutine monolis_allreduce_R1(val, tag, comm)
    implicit none
    !> [in,out] 入出力値（浮動小数点型）
    real(kdouble), intent(inout) :: val
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(kint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: n, ierr
    real(kdouble) :: in(1), out(1)

#ifndef NO_MPI
    in = val
    out = 0.0d0
    n = 1
    if(tag == monolis_mpi_sum)then
      call MPI_allreduce(in, out, n, MPI_REAL8, MPI_SUM, comm, ierr)
    elseif(tag == monolis_mpi_max)then
      call MPI_allreduce(in, out, n, MPI_REAL8, MPI_MAX, comm, ierr)
    elseif(tag == monolis_mpi_min)then
      call MPI_allreduce(in, out, n, MPI_REAL8, MPI_MIN, comm, ierr)
    endif
    val = out(1)
#endif
  end subroutine monolis_allreduce_R1

  !> @ingroup mpi
  !> allreduce 関数（浮動小数点配列型）
  subroutine monolis_allreduce_R(n, val, tag, comm)
    implicit none
    !> [in] 配列サイズ
    integer(kint), intent(in) :: n
    !> [in,out] 入出力値（浮動小数点配列型）
    real(kdouble), intent(inout) :: val(n)
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(kint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr
    real(kdouble) :: temp(n)

#ifndef NO_MPI
    temp = 0.0d0
    if(tag == monolis_mpi_sum)then
      call MPI_allreduce(val, temp, n, MPI_REAL8, MPI_SUM, comm, ierr)
    elseif(tag == monolis_mpi_max)then
      call MPI_allreduce(val, temp, n, MPI_REAL8, MPI_MAX, comm, ierr)
    elseif(tag == monolis_mpi_min)then
      call MPI_allreduce(val, temp, n, MPI_REAL8, MPI_MIN, comm, ierr)
    endif
    val = temp
#endif
  end subroutine monolis_allreduce_R

  !> @ingroup mpi
  !> allreduce 関数（擬似四倍精度実数型）
  subroutine monolis_allreduce_R1_N128(val, tag, comm)
    implicit none
    !> [in,out] 入出力値（浮動小数点型）
    type(monolis_R_N128), intent(inout) :: val
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(kint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr, n
    type(monolis_R_N128) :: in(1), out(1)

#ifndef NO_MPI
    n = 1
    in(1) = monolis_copy_R_N128(val)
    if(tag == monolis_mpi_sum)then
      call MPI_allreduce(in, out, n, monolis_mpi_type_R_N128, monolis_mpi_add_R_N128, comm, ierr)
    elseif(tag == monolis_mpi_max)then
      stop "monolis_allreduce_R1_N128 with max op. is not enabled"
      !call MPI_allreduce(in, out, n, MPI_REAL8, MPI_MAX, comm, ierr)
    elseif(tag == monolis_mpi_min)then
      stop "monolis_allreduce_R1_N128 with min op. is not enabled"
      !call MPI_allreduce(in, out, n, MPI_REAL8, MPI_MIN, comm, ierr)
    endif
    val = monolis_copy_R_N128(out(1))
#endif
  end subroutine monolis_allreduce_R1_N128

  !> @ingroup mpi
  !> allreduce 関数（擬似四倍精度実数型）
  subroutine monolis_allreduce_R_N128(n, val, tag, comm)
    implicit none
    !> [in] 配列サイズ
    integer(kint), intent(in) :: n
    !> [in,out] 入出力値（浮動小数点型）
    type(monolis_R_N128), intent(inout) :: val(n)
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(kint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: i, ierr
    type(monolis_R_N128) :: temp(n)

#ifndef NO_MPI
    if(tag == monolis_mpi_sum)then
      call MPI_allreduce(val, temp, n, monolis_mpi_type_R_N128, monolis_mpi_add_R_N128, comm, ierr)
    elseif(tag == monolis_mpi_max)then
      stop "monolis_allreduce_R_N128 with max op. is not enabled"
      !call MPI_allreduce(val, temp, n, MPI_REAL8, MPI_MAX, comm, ierr)
    elseif(tag == monolis_mpi_min)then
      stop "monolis_allreduce_R_N128 with min op. is not enabled"
      !call MPI_allreduce(val, temp, n, MPI_REAL8, MPI_MIN, comm, ierr)
    endif
    do i = 1, n
      val(i) = monolis_copy_R_N128(temp(i))
    enddo
#endif
  end subroutine monolis_allreduce_R_N128

  !> @ingroup mpi
  !> allreduce 関数（複素数型）
  subroutine monolis_allreduce_C1(val, tag, comm)
    implicit none
    !> [in,out] 入出力値（浮動小数点型）
    complex(kdouble), intent(inout) :: val
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(kint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: n, ierr
    complex(kdouble) :: in(1), out(1)

#ifndef NO_MPI
    in = val
    out = 0.0d0
    n = 1
    if(tag == monolis_mpi_sum)then
      call MPI_allreduce(in, out, n, MPI_DOUBLE_COMPLEX, MPI_SUM, comm, ierr)
    elseif(tag == monolis_mpi_max)then
      call MPI_allreduce(in, out, n, MPI_DOUBLE_COMPLEX, MPI_MAX, comm, ierr)
    elseif(tag == monolis_mpi_min)then
      call MPI_allreduce(in, out, n, MPI_DOUBLE_COMPLEX, MPI_MIN, comm, ierr)
    endif
    val = out(1)
#endif
  end subroutine monolis_allreduce_C1

  !> @ingroup mpi
  !> allreduce 関数（複素数型）
  subroutine monolis_allreduce_C(n, val, tag, comm)
    implicit none
    !> [in] 配列サイズ
    integer(kint), intent(in) :: n
    !> [in,out] 入出力値（浮動小数点配列型）
    complex(kdouble), intent(inout) :: val(n)
    !> [in] MPI 演算タグ（monolis_mpi_sum, monolis_mpi_max, monolis_mpi_min）
    integer(kint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr
    complex(kdouble) :: temp(n)

#ifndef NO_MPI
    temp = 0.0d0
    if(tag == monolis_mpi_sum)then
      call MPI_allreduce(val, temp, n, MPI_DOUBLE_COMPLEX, MPI_SUM, comm, ierr)
    elseif(tag == monolis_mpi_max)then
      call MPI_allreduce(val, temp, n, MPI_DOUBLE_COMPLEX, MPI_MAX, comm, ierr)
    elseif(tag == monolis_mpi_min)then
      call MPI_allreduce(val, temp, n, MPI_DOUBLE_COMPLEX, MPI_MIN, comm, ierr)
    endif
    val = temp
#endif
  end subroutine monolis_allreduce_C

  !> @ingroup mpi
  !> Isend 関数（整数配列型）
  subroutine monolis_Isend_I(n, ws, pe_id, comm, req)
    implicit none
    !> [in] 配列サイズ
    integer(kint), intent(in) :: n
    !> [in] 送信値
    integer(kint), intent(in) :: ws(:)
    !> [in] 送信先 MPI ランク
    integer(kint), intent(in) :: pe_id
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] MPI リクエスト
    integer(kint), intent(in) :: req
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_Isend(ws, n, MPI_INTEGER, pe_id, 0, comm, req, ierr)
#endif
  end subroutine monolis_Isend_I

  !> @ingroup mpi
  !> Irecv 関数（整数配列型）
  subroutine monolis_Irecv_I(n, ws, pe_id, comm, req)
    implicit none
    !> [in] 配列サイズ
    integer(kint), intent(in) :: n
    !> [out] 受信値
    integer(kint), intent(out) :: ws(:)
    !> [in] 送信元 MPI ランク
    integer(kint), intent(in) :: pe_id
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] MPI リクエスト
    integer(kint), intent(in) :: req
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_Irecv(ws, n, MPI_INTEGER, pe_id, 0, comm, req, ierr)
#endif
  end subroutine monolis_Irecv_I

  !> @ingroup mpi
  !> Isend 関数（浮動小数点配列型）
  subroutine monolis_Isend_R(n, ws, pe_id, comm, req)
    implicit none
    !> [in] 配列サイズ
    integer(kint), intent(in) :: n
    !> [in] 送信値
    real(kdouble), intent(in) :: ws(:)
    !> [in] 送信先 MPI ランク
    integer(kint), intent(in) :: pe_id
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] MPI リクエスト
    integer(kint), intent(in) :: req
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_Isend(ws, n, MPI_REAL8, pe_id, 0, comm, req, ierr)
#endif
  end subroutine monolis_Isend_R

  !> @ingroup mpi
  !> Irecv 関数（浮動小数点配列型）
  subroutine monolis_Irecv_R(n, ws, pe_id, comm, req)
    implicit none
    !> [in] 配列サイズ
    integer(kint), intent(in) :: n
    !> [out] 受信値
    real(kdouble), intent(out) :: ws(:)
    !> [in] 送信元 MPI ランク
    integer(kint), intent(in) :: pe_id
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] MPI リクエスト
    integer(kint), intent(in) :: req
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_Irecv(ws, n, MPI_REAL8, pe_id, 0, comm, req, ierr)
#endif
  end subroutine monolis_Irecv_R

  !> @ingroup mpi
  !> Isend 関数（複素数型）
  subroutine monolis_Isend_C(n, ws, pe_id, comm, req)
    implicit none
    !> [in] 配列サイズ
    integer(kint), intent(in) :: n
    !> [in] 送信値
    complex(kdouble), intent(in) :: ws(:)
    !> [in] 送信先 MPI ランク
    integer(kint), intent(in) :: pe_id
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] MPI リクエスト
    integer(kint), intent(in) :: req
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_Isend(ws, n, MPI_DOUBLE_COMPLEX, pe_id, 0, comm, req, ierr)
#endif
  end subroutine monolis_Isend_C

  !> @ingroup mpi
  !> Irecv 関数（複素数型）
  subroutine monolis_Irecv_C(n, ws, pe_id, comm, req)
    implicit none
    !> [in] 配列サイズ
    integer(kint), intent(in) :: n
    !> [out] 受信値
    complex(kdouble), intent(out) :: ws(:)
    !> [in] 送信元 MPI ランク
    integer(kint), intent(in) :: pe_id
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> [in] MPI リクエスト
    integer(kint), intent(in) :: req
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_Irecv(ws, n, MPI_DOUBLE_COMPLEX, pe_id, 0, comm, req, ierr)
#endif
  end subroutine monolis_Irecv_C

  !> @ingroup mpi
  !> gatherv 関数（整数配列型）
  subroutine monolis_gather_V_I(sbuf, sc, rbuf, rc, disp, root, comm)
    implicit none
    !> [in] 送信データ配列
    integer(kint), intent(in) :: sbuf(:)
    !> [in] 送信データ個数
    integer(kint), intent(in) :: sc
    !> [out] 受信データ配列
    integer(kint), intent(out) :: rbuf(:)
    !> [in] 各ランクのデータ個数リスト
    integer(kint), intent(in) :: rc(:)
    !> [in] 各ランクのデータ格納位置リスト
    integer(kint), intent(in) :: disp(:)
    !> [in] データを格納する MPI ランク
    integer(kint), intent(in) :: root
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_gatherv(sbuf, sc, monolis_mpi_int, rbuf, rc, disp, monolis_mpi_int, root, comm, ierr)
#else
    rbuf(1:sc) = sbuf(1:sc)
#endif
  end subroutine monolis_gather_V_I

  !> @ingroup mpi
  !> scatterv 関数（整数配列型）
  subroutine monolis_scatter_V_I(sbuf, sc, disp, rbuf, rc, root, comm)
    implicit none
    !> [in] 送信データ配列
    integer(kint), intent(in) :: sbuf(:)
    !> [in] 送信データ個数
    integer(kint), intent(in) :: sc(:)
    !> [in] 各ランクのデータ格納位置リスト
    integer(kint), intent(in) :: disp(:)
    !> [out] 受信データ配列
    integer(kint), intent(out) :: rbuf(:)
    !> [in] 各ランクのデータ個数リスト
    integer(kint), intent(in) :: rc
    !> [in] データを送信する MPI ランク
    integer(kint), intent(in) :: root
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_scatterv(sbuf, sc, disp, monolis_mpi_int, rbuf, rc, monolis_mpi_int, root, comm, ierr)
#else
    rbuf(1:rc) = sbuf(1:rc)
#endif
  end subroutine monolis_scatter_V_I

  !> @ingroup mpi
  !> gatherv 関数（実数配列型）
  subroutine monolis_gather_V_R(sbuf, sc, rbuf, rc, disp, root, comm)
    implicit none
    !> [in] 送信データ配列
    real(kdouble), intent(in) :: sbuf(:)
    !> [in] 送信データ個数
    integer(kint), intent(in) :: sc
    !> [out] 受信データ配列
    real(kdouble), intent(out) :: rbuf(:)
    !> [in] 各ランクのデータ個数リスト
    integer(kint), intent(in) :: rc(:)
    !> [in] 各ランクのデータ格納位置リスト
    integer(kint), intent(in) :: disp(:)
    !> [in] データを格納する MPI ランク
    integer(kint), intent(in) :: root
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_gatherv(sbuf, sc, MPI_REAL8, rbuf, rc, disp, MPI_REAL8, root, comm, ierr)
#else
    rbuf(1:sc) = sbuf(1:sc)
#endif
  end subroutine monolis_gather_V_R

  !> @ingroup mpi
  !> scatterv 関数（実数配列型）
  subroutine monolis_scatter_V_R(sbuf, sc, disp, rbuf, rc, root, comm)
    implicit none
    !> [in] 送信データ配列
    real(kdouble), intent(in) :: sbuf(:)
    !> [in] 送信データ個数
    integer(kint), intent(in) :: sc(:)
    !> [in] 各ランクのデータ格納位置リスト
    integer(kint), intent(in) :: disp(:)
    !> [out] 受信データ配列
    real(kdouble), intent(out) :: rbuf(:)
    !> [in] 各ランクのデータ個数リスト
    integer(kint), intent(in) :: rc
    !> [in] データを送信する MPI ランク
    integer(kint), intent(in) :: root
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_scatterv(sbuf, sc, disp, MPI_REAL8, rbuf, rc, MPI_REAL8, root, comm, ierr)
#else
    rbuf(1:rc) = sbuf(1:rc)
#endif
  end subroutine monolis_scatter_V_R

  !> @ingroup mpi
  !> gatherv 関数（実数配列型）
  subroutine monolis_gather_V_C(sbuf, sc, rbuf, rc, disp, root, comm)
    implicit none
    !> [in] 送信データ配列
    complex(kdouble), intent(in) :: sbuf(:)
    !> [in] 送信データ個数
    integer(kint), intent(in) :: sc
    !> [out] 受信データ配列
    complex(kdouble), intent(out) :: rbuf(:)
    !> [in] 各ランクのデータ個数リスト
    integer(kint), intent(in) :: rc(:)
    !> [in] 各ランクのデータ格納位置リスト
    integer(kint), intent(in) :: disp(:)
    !> [in] データを格納する MPI ランク
    integer(kint), intent(in) :: root
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_gatherv(sbuf, sc, MPI_DOUBLE_COMPLEX, rbuf, rc, disp, MPI_DOUBLE_COMPLEX, root, comm, ierr)
#else
    rbuf(1:sc) = sbuf(1:sc)
#endif
  end subroutine monolis_gather_V_C

  !> @ingroup mpi
  !> scatterv 関数（実数配列型）
  subroutine monolis_scatter_V_C(sbuf, sc, disp, rbuf, rc, root, comm)
    implicit none
    !> [in] 送信データ配列
    complex(kdouble), intent(in) :: sbuf(:)
    !> [in] 送信データ個数
    integer(kint), intent(in) :: sc(:)
    !> [in] 各ランクのデータ格納位置リスト
    integer(kint), intent(in) :: disp(:)
    !> [out] 受信データ配列
    complex(kdouble), intent(out) :: rbuf(:)
    !> [in] 各ランクのデータ個数リスト
    integer(kint), intent(in) :: rc
    !> [in] データを送信する MPI ランク
    integer(kint), intent(in) :: root
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_scatterv(sbuf, sc, disp, MPI_DOUBLE_COMPLEX, rbuf, rc, MPI_DOUBLE_COMPLEX, root, comm, ierr)
#else
    rbuf(1:rc) = sbuf(1:rc)
#endif
  end subroutine monolis_scatter_V_C

  !> @ingroup mpi
  !> allgather 関数（整数型）
  subroutine monolis_allgather_I1(sval, rbuf, comm)
    implicit none
    !> [in] 送信データ
    integer(kint), intent(in) :: sval
    !> [out] 受信データ
    integer(kint), intent(out) :: rbuf(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_allgather(sval, 1, monolis_mpi_int, rbuf, 1, monolis_mpi_int, comm, ierr)
#else
    rbuf(1) = sval
#endif
  end subroutine monolis_allgather_I1

  !> @ingroup mpi
  !> allgatherv 関数（整数型）
  subroutine monolis_allgather_V_I(n, sval, rbuf, counts, displs, comm)
    implicit none
    !> [in] データ送信個数
    integer(kint), intent(in) :: n
    !> [in] 送信データ
    integer(kint), intent(in) :: sval(:)
    !> [out] 受信データ
    integer(kint), intent(out) :: rbuf(:)
    !> [in] プロセスごとの送信データ長さ
    integer(kint), intent(in) :: counts(:)
    !> [in] プロセスごとの送信データ開始位置
    integer(kint), intent(in) :: displs(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call mpi_allgatherv(sval, n, monolis_mpi_int, rbuf, counts, displs, monolis_mpi_int, comm, ierr)
#else
    rbuf = sval
#endif
  end subroutine monolis_allgather_V_I

  !> @ingroup mpi
  !> allgather 関数（実数型）
  subroutine monolis_allgather_R1(sval, rbuf, comm)
    implicit none
    !> [in] 送信データ
    real(kdouble), intent(in) :: sval
    !> [out] 受信データ
    real(kdouble), intent(out) :: rbuf(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_allgather(sval, 1, MPI_REAL8, rbuf, 1, MPI_REAL8, comm, ierr)
#else
    rbuf(1) = sval
#endif
  end subroutine monolis_allgather_R1

  !> @ingroup mpi
  !> allgatherv 関数（実数型）
  subroutine monolis_allgather_V_R(n, sval, rbuf, counts, displs, comm)
    implicit none
    !> [in] データ送信個数
    integer(kint), intent(in) :: n
    !> [in] 送信データ
    real(kdouble), intent(in) :: sval(:)
    !> [out] 受信データ
    real(kdouble), intent(out) :: rbuf(:)
    !> [in] プロセスごとの送信データ長さ
    integer(kint), intent(in) :: counts(:)
    !> [in] プロセスごとの送信データ開始位置
    integer(kint), intent(in) :: displs(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call mpi_allgatherv(sval, n, MPI_REAL8, rbuf, counts, displs, MPI_REAL8, comm, ierr)
#else
    rbuf = sval
#endif
  end subroutine monolis_allgather_V_R

  !> @ingroup mpi
  !> allgather 関数（実数型）
  subroutine monolis_allgather_C1(sval, rbuf, comm)
    implicit none
    !> [in] 送信データ
    complex(kdouble), intent(in) :: sval
    !> [out] 受信データ
    complex(kdouble), intent(out) :: rbuf(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call MPI_allgather(sval, 1, MPI_DOUBLE_COMPLEX, rbuf, 1, MPI_DOUBLE_COMPLEX, comm, ierr)
#else
    rbuf(1) = sval
#endif
  end subroutine monolis_allgather_C1

  !> @ingroup mpi
  !> allgatherv 関数（実数型）
  subroutine monolis_allgather_V_C(n, sval, rbuf, counts, displs, comm)
    implicit none
    !> [in] データ送信個数
    integer(kint), intent(in) :: n
    !> [in] 送信データ
    complex(kdouble), intent(in) :: sval(:)
    !> [out] 受信データ
    complex(kdouble), intent(out) :: rbuf(:)
    !> [in] プロセスごとの送信データ長さ
    integer(kint), intent(in) :: counts(:)
    !> [in] プロセスごとの送信データ開始位置
    integer(kint), intent(in) :: displs(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call mpi_allgatherv(sval, n, MPI_DOUBLE_COMPLEX, rbuf, counts, displs, MPI_DOUBLE_COMPLEX, comm, ierr)
#else
    rbuf = sval
#endif
  end subroutine monolis_allgather_V_C

  !> @ingroup mpi
  !> alltoall 関数（整数型）
  subroutine monolis_alltoall_I1(n, sbuf, comm)
    implicit none
    !> [in] データ送信個数
    integer(kint), intent(in) :: n
    !> [in,out] 送信データ
    integer(kint), intent(inout) :: sbuf(n)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !> 受信データ
    integer(kint) :: rbuf(n)
    integer(kint) :: ierr

#ifndef NO_MPI
    call mpi_alltoall(sbuf, 1, monolis_mpi_int, rbuf, 1, monolis_mpi_int, comm, ierr)
    sbuf = rbuf
#endif
  end subroutine monolis_alltoall_I1

  !> @ingroup mpi
  !> alltoallv 関数（整数型）
  subroutine monolis_alltoall_V_I(sbuf, scounts, sdispls, rbuf, rcounts, rdispls, comm)
    implicit none
    !> [in] 送信データ配列
    integer(kint), intent(in) :: sbuf(:)
    !> [in] 各領域ごとの送信データの個数
    integer(kint), intent(in) :: scounts(:)
    !> [in] 各領域へ送信するデータ配列の開始位置
    integer(kint), intent(in) :: sdispls(:)
    !> [in] 受信データ配列
    integer(kint), intent(out) :: rbuf(:)
    !> [in] 各領域ごとの受信データの個数
    integer(kint), intent(in) :: rcounts(:)
    !> [in] 各領域から受信するデータ配列の開始位置
    integer(kint), intent(in) :: rdispls(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call mpi_alltoallv(sbuf, scounts, sdispls, monolis_mpi_int, &
                     & rbuf, rcounts, rdispls, monolis_mpi_int, comm, ierr)
#endif
  end subroutine monolis_alltoall_V_I

  !> @ingroup mpi
  !> alltoallv 関数（実数型）
  subroutine monolis_alltoall_V_R(sbuf, scounts, sdispls, rbuf, rcounts, rdispls, comm)
    implicit none
    !> [in] 送信データ配列
    real(kdouble), intent(in) :: sbuf(:)
    !> [in] 各領域ごとの送信データの個数
    integer(kint), intent(in) :: scounts(:)
    !> [in] 各領域へ送信するデータ配列の開始位置
    integer(kint), intent(in) :: sdispls(:)
    !> [in] 受信データ配列
    real(kdouble), intent(out) :: rbuf(:)
    !> [in] 各領域ごとの受信データの個数
    integer(kint), intent(in) :: rcounts(:)
    !> [in] 各領域から受信するデータ配列の開始位置
    integer(kint), intent(in) :: rdispls(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: ierr

#ifndef NO_MPI
    call mpi_alltoallv(sbuf, scounts, sdispls, MPI_REAL8, &
                     & rbuf, rcounts, rdispls, MPI_REAL8, comm, ierr)
#endif
  end subroutine monolis_alltoall_V_R

  !> @ingroup mpi
  !> 隣接領域に定義された任意本数ベクトルのベクトル数の取得関数
  !> @detail 分割領域ごとに任意本数のベクトルが定義されている。
  subroutine monolis_mpi_get_n_neib_vector(monoCOM, n_vec, n_neib_vec)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 自領域が持つベクトル数
    integer(kint), intent(in) :: n_vec
    !> [in] 自領域と隣接領域のベクトル数の合計
    integer(kint), intent(out) :: n_neib_vec
    integer(kint) :: i, in, comm_size, my_rank
    integer(kint), allocatable :: n_neib_send(:)

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0)then
      n_neib_vec = n_vec
      return
    endif

    !# 送信ベクトル数の共有
    my_rank = monolis_mpi_get_local_my_rank(monoCOM%comm)
    comm_size = monolis_mpi_get_local_comm_size(monoCOM%comm)
    call monolis_alloc_I_1d(n_neib_send, comm_size)
    n_neib_send = n_vec
    call monolis_alltoall_I1(comm_size, n_neib_send, monoCOM%comm)

    n_neib_vec = n_vec
    do i = 1, monoCOM%recv_n_neib
      in = monoCOM%recv_neib_pe(i)
      n_neib_vec = n_neib_vec + n_neib_send(in + 1)
    enddo
  end subroutine monolis_mpi_get_n_neib_vector

  !> @ingroup mpi
  !> Wait 関数（単一リクエスト）
  subroutine monolis_mpi_wait(req, ierr)
    implicit none
    !> [in] MPI リクエスト
    integer(kint), intent(in) :: req
    !> [out] エラーコード
    integer(kint), intent(out) :: ierr

#ifndef NO_MPI
    call MPI_Wait(req, MPI_STATUS_IGNORE, ierr)
#else
    ierr = 0
#endif
  end subroutine monolis_mpi_wait

  !> @ingroup mpi
  !> Waitall 関数（複数リクエスト）
  subroutine monolis_mpi_waitall(n, reqs, ierr)
    implicit none
    !> [in] リクエスト数
    integer(kint), intent(in) :: n
    !> [in] MPI リクエスト配列
    integer(kint), intent(in) :: reqs(n)
    !> [out] エラーコード
    integer(kint), intent(out) :: ierr

#ifndef NO_MPI
    call MPI_Waitall(n, reqs, MPI_STATUSES_IGNORE, ierr)
#else
    ierr = 0
#endif
  end subroutine monolis_mpi_waitall

  !> @ingroup mpi
end module mod_monolis_mpi
