!> MPI モジュール
module mod_monolis_mpi
  use mod_monolis_utils_define_prm
  use mod_monolis_mpi_util
  use mod_monolis_utils_alloc
  use mod_monolis_utils_define_com
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
      call MPI_allreduce(in, out, n, MPI_INTEGER, MPI_SUM, comm, ierr)
    elseif(tag == monolis_mpi_max)then
      call MPI_allreduce(in, out, n, MPI_INTEGER, MPI_MAX, comm, ierr)
    elseif(tag == monolis_mpi_min)then
      call MPI_allreduce(in, out, n, MPI_INTEGER, MPI_MIN, comm, ierr)
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
      call MPI_allreduce(val, temp, n, MPI_INTEGER, MPI_SUM, comm, ierr)
    elseif(tag == monolis_mpi_max)then
      call MPI_allreduce(val, temp, n, MPI_INTEGER, MPI_MAX, comm, ierr)
    elseif(tag == monolis_mpi_min)then
      call MPI_allreduce(val, temp, n, MPI_INTEGER, MPI_MIN, comm, ierr)
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
  subroutine monolis_gatherv_I(sbuf, sc, rbuf, rc, disp, root, comm)
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
    call MPI_gatherv(sbuf, sc, MPI_INTEGER, rbuf, rc, disp, MPI_INTEGER, root, comm, ierr)
#else
    rbuf(1:sc) = sbuf(1:sc)
#endif
  end subroutine monolis_gatherv_I

  !> @ingroup mpi
  !> scatterv 関数（整数配列型）
  subroutine monolis_scatterv_I(sbuf, sc, disp, rbuf, rc, root, comm)
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
    call MPI_scatterv(sbuf, sc, disp, MPI_INTEGER, rbuf, rc, MPI_INTEGER, root, comm, ierr)
#else
    rbuf(1:rc) = sbuf(1:rc)
#endif
  end subroutine monolis_scatterv_I

  !> @ingroup mpi
  !> gatherv 関数（実数配列型）
  subroutine monolis_gatherv_R(sbuf, sc, rbuf, rc, disp, root, comm)
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
  end subroutine monolis_gatherv_R

  !> @ingroup mpi
  !> scatterv 関数（実数配列型）
  subroutine monolis_scatterv_R(sbuf, sc, disp, rbuf, rc, root, comm)
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
  end subroutine monolis_scatterv_R

  !> @ingroup mpi
  !> gatherv 関数（実数配列型）
  subroutine monolis_gatherv_C(sbuf, sc, rbuf, rc, disp, root, comm)
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
  end subroutine monolis_gatherv_C

  !> @ingroup mpi
  !> scatterv 関数（実数配列型）
  subroutine monolis_scatterv_C(sbuf, sc, disp, rbuf, rc, root, comm)
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
  end subroutine monolis_scatterv_C

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
    call MPI_allgather(sval, 1, MPI_INTEGER, rbuf, 1, MPI_INTEGER, comm, ierr)
#else
    rbuf(1) = sval
#endif
  end subroutine monolis_allgather_I1

  !> @ingroup mpi
  !> allgatherv 関数（整数型）
  subroutine monolis_allgatherv_I(n, sval, rbuf, counts, displs, comm)
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
    call mpi_allgatherv(sval, n, MPI_INTEGER, rbuf, counts, displs, MPI_INTEGER, comm, ierr)
#else
    rbuf = sval
#endif
  end subroutine monolis_allgatherv_I

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
  subroutine monolis_allgatherv_R(n, sval, rbuf, counts, displs, comm)
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
  end subroutine monolis_allgatherv_R

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
  subroutine monolis_allgatherv_C(n, sval, rbuf, counts, displs, comm)
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
  end subroutine monolis_allgatherv_C

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
    call mpi_alltoall(sbuf, 1, MPI_INTEGER, rbuf, 1, MPI_INTEGER, comm, ierr)
    sbuf = rbuf
#endif
  end subroutine monolis_alltoall_I1

  !> @ingroup mpi
  !> 通信テーブルを用いた send recv 関数（浮動小数点型）
  subroutine monolis_SendRecv_R(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    & send_index, send_item, recv_index, recv_item, &
    & val_in, val_out, ndof, comm)
    implicit none
    !> [in] send する隣接領域数
    integer(kint), intent(in) :: send_n_neib
    !> [in] send する隣接領域 id
    integer(kint), intent(in) :: send_neib_pe(:)
    !> [in] recv する隣接領域数
    integer(kint), intent(in) :: recv_n_neib
    !> [in] recv する隣接領域 id
    integer(kint), intent(in) :: recv_neib_pe(:)
    !> [in] send の index 配列
    integer(kint), intent(in) :: send_index(:)
    !> [in] send の item 配列（送信する節点番号データ）
    integer(kint), intent(in) :: send_item (:)
    !> [in] recv の index 配列
    integer(kint), intent(in) :: recv_index(:)
    !> [in] recv の item 配列（受信する節点番号データ）
    integer(kint), intent(in) :: recv_item (:)
    !> [in,out] 送信データ配列
    real(kdouble), intent(inout) :: val_in(:)
    !> [in,out] 受信データ配列
    real(kdouble), intent(inout) :: val_out(:)
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: i, iS, in, j, k, ierr, ns, nr
    integer(kint) :: sta1(monolis_mpi_status_size, send_n_neib)
    integer(kint) :: sta2(monolis_mpi_status_size, recv_n_neib)
    integer(kint) :: req1(send_n_neib)
    integer(kint) :: req2(recv_n_neib)
    real(kdouble), allocatable :: ws(:)
    real(kdouble), allocatable :: wr(:)

#ifndef NO_MPI
    ns = send_index(send_n_neib + 1)
    nr = recv_index(recv_n_neib + 1)

    call monolis_alloc_R_1d(ws, ndof*ns)
    call monolis_alloc_R_1d(wr, ndof*nr)

    do i = 1, send_n_neib
      iS = send_index(i)
      in = send_index(i + 1) - iS
      if(in == 0) cycle
      l1:do j = iS + 1, iS + in
        if(send_item(j) == -1) cycle l1
        do k = 1, ndof
          ws(ndof*(j - 1) + k) = val_in(ndof*(send_item(j) - 1) + k)
        enddo
      enddo l1
      call monolis_Isend_R(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), send_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
      if(in == 0) cycle
      call monolis_Irecv_R(ndof*in, wr(ndof*iS + 1:ndof*iS + ndof*in), recv_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
      l2:do j = iS + 1, iS + in
        if(recv_item(j) == -1) cycle l2
        do k = 1, ndof
          val_out(ndof*(recv_item(j) - 1) + k) = wr(ndof*(j - 1) + k)
        enddo
      enddo l2
    enddo

    call MPI_waitall(send_n_neib, req1, sta1, ierr)
#endif
  end subroutine monolis_SendRecv_R

  !> @ingroup mpi
  !> 通信テーブルを用いた send recv 関数（整数型）
  subroutine monolis_SendRecv_I(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    & send_index, send_item, recv_index, recv_item, &
    & val_in, val_out, ndof, comm)
    implicit none
    !> [in] send する隣接領域数
    integer(kint), intent(in) :: send_n_neib
    !> [in] send する隣接領域 id
    integer(kint), intent(in) :: send_neib_pe(:)
    !> [in] recv する隣接領域数
    integer(kint), intent(in) :: recv_n_neib
    !> [in] recv する隣接領域 id
    integer(kint), intent(in) :: recv_neib_pe(:)
    !> [in] send の index 配列
    integer(kint), intent(in) :: send_index(:)
    !> [in] send の item 配列（送信する節点番号データ）
    integer(kint), intent(in) :: send_item (:)
    !> [in] recv の index 配列
    integer(kint), intent(in) :: recv_index(:)
    !> [in] recv の item 配列（受信する節点番号データ）
    integer(kint), intent(in) :: recv_item (:)
    !> [in,out] 送信データ配列
    integer(kint), intent(inout) :: val_in(:)
    !> [in,out] 受信データ配列
    integer(kint), intent(inout) :: val_out(:)
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: i, iS, in, j, k, ierr, ns, nr
    integer(kint) :: sta1(monolis_mpi_status_size, send_n_neib)
    integer(kint) :: sta2(monolis_mpi_status_size, recv_n_neib)
    integer(kint) :: req1(send_n_neib)
    integer(kint) :: req2(recv_n_neib)
    integer(kint), allocatable :: ws(:)
    integer(kint), allocatable :: wr(:)

#ifndef NO_MPI
    ns = send_index(send_n_neib + 1)
    nr = recv_index(recv_n_neib + 1)

    call monolis_alloc_I_1d(ws, ndof*ns)
    call monolis_alloc_I_1d(wr, ndof*nr)

    do i = 1, send_n_neib
      iS = send_index(i)
      in = send_index(i + 1) - iS
      if(in == 0) cycle
      l1:do j = iS + 1, iS + in
        if(send_item(j) == -1) cycle l1
        do k = 1, ndof
          ws(ndof*(j - 1) + k) = val_in(ndof*(send_item(j) - 1) + k)
        enddo
      enddo l1
      call monolis_Isend_I(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), send_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
      if(in == 0) cycle
      call monolis_Irecv_I(ndof*in, wr(ndof*iS + 1:ndof*iS + ndof*in), recv_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
      l2:do j = iS + 1, iS + in
        if(recv_item(j) == -1) cycle l2
        do k = 1, ndof
          val_out(ndof*(recv_item(j) - 1) + k) = wr(ndof*(j - 1) + k)
        enddo
      enddo l2
    enddo

    call MPI_waitall(send_n_neib, req1, sta1, ierr)
#endif
  end subroutine monolis_SendRecv_I

  !> @ingroup mpi
  !> 通信テーブルを用いた send recv 関数（複素数型）
  subroutine monolis_SendRecv_C(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    & send_index, send_item, recv_index, recv_item, &
    & val_in, val_out, ndof, comm)
    implicit none
    !> [in] send する隣接領域数
    integer(kint), intent(in) :: send_n_neib
    !> [in] send する隣接領域 id
    integer(kint), intent(in) :: send_neib_pe(:)
    !> [in] recv する隣接領域数
    integer(kint), intent(in) :: recv_n_neib
    !> [in] recv する隣接領域 id
    integer(kint), intent(in) :: recv_neib_pe(:)
    !> [in] send の index 配列
    integer(kint), intent(in) :: send_index(:)
    !> [in] send の item 配列（送信する節点番号データ）
    integer(kint), intent(in) :: send_item (:)
    !> [in] recv の index 配列
    integer(kint), intent(in) :: recv_index(:)
    !> [in] recv の item 配列（受信する節点番号データ）
    integer(kint), intent(in) :: recv_item (:)
    !> [in,out] 送信データ配列
    complex(kdouble), intent(inout) :: val_in(:)
    !> [in,out] 受信データ配列
    complex(kdouble), intent(inout) :: val_out(:)
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: i, iS, in, j, k, ierr, ns, nr
    integer(kint) :: sta1(monolis_mpi_status_size, send_n_neib)
    integer(kint) :: sta2(monolis_mpi_status_size, recv_n_neib)
    integer(kint) :: req1(send_n_neib)
    integer(kint) :: req2(recv_n_neib)
    complex(kdouble), allocatable :: ws(:)
    complex(kdouble), allocatable :: wr(:)

#ifndef NO_MPI
    ns = send_index(send_n_neib + 1)
    nr = recv_index(recv_n_neib + 1)

    call monolis_alloc_C_1d(ws, ndof*ns)
    call monolis_alloc_C_1d(wr, ndof*nr)

    do i = 1, send_n_neib
      iS = send_index(i)
      in = send_index(i + 1) - iS
      if(in == 0) cycle
      l1:do j = iS + 1, iS + in
        if(send_item(j) == -1) cycle l1
        do k = 1, ndof
          ws(ndof*(j - 1) + k) = val_in(ndof*(send_item(j) - 1) + k)
        enddo
      enddo l1
      call monolis_Isend_C(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), send_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
      if(in == 0) cycle
      call monolis_Irecv_C(ndof*in, wr(ndof*iS + 1:ndof*iS + ndof*in), recv_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
      l2:do j = iS + 1, iS + in
        if(recv_item(j) == -1) cycle l2
        do k = 1, ndof
          val_out(ndof*(recv_item(j) - 1) + k) = wr(ndof*(j - 1) + k)
        enddo
      enddo l2
    enddo

    call MPI_waitall(send_n_neib, req1, sta1, ierr)
#endif
  end subroutine monolis_SendRecv_C

  !> @ingroup mpi
  !> ベクトルのアップデート関数（実数型）
  subroutine monolis_mpi_update_R(monoCOM, ndof, X, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in,out] 入出力ベクトル
    real(kdouble), intent(inout) :: X(:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    real(kdouble) :: t1, t2

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0) return

    t1 = monolis_get_time()
    call monolis_SendRecv_R(monoCOM%send_n_neib, monoCOM%send_neib_pe, &
       & monoCOM%recv_n_neib, monoCOM%recv_neib_pe, &
       & monoCOM%send_index, monoCOM%send_item, &
       & monoCOM%recv_index, monoCOM%recv_item, &
       & X, X, ndof, monoCOM%comm)
    t2 = monolis_get_time()

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_update_R

  !> @ingroup mpi
  !> ベクトルのアップデート関数（整数型）
  subroutine monolis_mpi_update_I(monoCOM, ndof, X, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in,out] 入出力ベクトル
    integer(kint), intent(inout) :: X(:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    real(kdouble) :: t1, t2

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0) return

    t1 = monolis_get_time()
    call monolis_SendRecv_I(monoCOM%send_n_neib, monoCOM%send_neib_pe, &
       & monoCOM%recv_n_neib, monoCOM%recv_neib_pe, &
       & monoCOM%send_index, monoCOM%send_item, &
       & monoCOM%recv_index, monoCOM%recv_item, &
       & X, X, ndof, monoCOM%comm)
    t2 = monolis_get_time()

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_update_I

  !> @ingroup mpi
  !> ベクトルのアップデート関数（複素数型）
  subroutine monolis_mpi_update_C(monoCOM, ndof, X, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in,out] 入出力ベクトル
    complex(kdouble), intent(inout) :: X(:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    real(kdouble) :: t1, t2

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0) return

    t1 = monolis_get_time()
    call monolis_SendRecv_C(monoCOM%send_n_neib, monoCOM%send_neib_pe, &
       & monoCOM%recv_n_neib, monoCOM%recv_neib_pe, &
       & monoCOM%send_index, monoCOM%send_item, &
       & monoCOM%recv_index, monoCOM%recv_item, &
       & X, X, ndof, monoCOM%comm)
    t2 = monolis_get_time()

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_update_C
end module mod_monolis_mpi
