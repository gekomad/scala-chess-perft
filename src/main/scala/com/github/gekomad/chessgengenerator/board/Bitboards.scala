package com.github.gekomad.chessgengenerator.board
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import com.github.gekomad.chessgengenerator.core.Def._
object Bitboards {

  //Kindergarten

  import Board._

  trait Bitboard {
    def getRankFile(position: Int, allpieces: Long): Long
    def getDiagonalAntiDiagonal(position: Int, allpieces: Long): Long
  }

  def apply(): Bitboard = {
    val MAGIC_KEY_DIAG_ANTIDIAG: Long = 0x101010101010101L
    val MAGIC_KEY_FILE_RANK: Long = 0x102040810204080L

    val BITBOARD_DIAGONAL: Array[Array[Long]] = Array.ofDim[Long](64, 256)
    val BITBOARD_ANTIDIAGONAL: Array[Array[Long]] = Array.ofDim[Long](64, 256)
    val BITBOARD_FILE: Array[Array[Long]] = Array.ofDim[Long](64, 256)
    val BITBOARD_RANK: Array[Array[Long]] = Array.ofDim[Long](64, 256)

    case class _Ttmp() {
      var MASK_BIT_SET_NOBOUND_TMP: Array[Array[Long]] =
        Array.ofDim[Long](64, 64)
      var MASK_BIT_SET_NOBOUND_COUNT_TMP: Array[Array[Long]] =
        Array.ofDim[Long](64, 64)
    }

    val tmp_struct: _Ttmp = _Ttmp()

    val MASK_BIT_SET: Array[Array[Long]] = Array.ofDim[Long](64, 64)

    (0 until 64).foreach { i =>
      (0 until 64).foreach { j =>
        MASK_BIT_SET(i)(i) = 0
        (Math.min(i, j) to Math.max(i, j)).foreach { e =>
          val r: Long = (RANK(i) | POW2(i)) & (RANK(j) | POW2(j))
          if (r != 0)
            MASK_BIT_SET(i)(j) |= (POW2(e) & r)
          else {
            val r = (FILE_(i) | POW2(i)) & (FILE_(j) | POW2(j))
            if (r != 0)
              MASK_BIT_SET(i)(j) |= (POW2(e) & r)
            else {
              val r = (DIAGONAL(i) | POW2(i)) & (DIAGONAL(j) | POW2(j))
              if (r != 0)
                MASK_BIT_SET(i)(j) |= (POW2(e) & r)
              else {
                val r = (ANTIDIAGONAL(i) | POW2(i)) & (ANTIDIAGONAL(j) | POW2(
                  j))
                if (r != 0) {
                  MASK_BIT_SET(i)(j) |= (POW2(e) & r)
                }
              }
            }
          }
        }

        if (i == j)
          MASK_BIT_SET(i)(i) &= NOTPOW2(i)
      }
    }

    (0 until 64).foreach { i =>
      (0 until 64).foreach { j =>
        tmp_struct.MASK_BIT_SET_NOBOUND_TMP(i)(j) = MASK_BIT_SET(i)(j)
        tmp_struct.MASK_BIT_SET_NOBOUND_TMP(i)(j) &= NOTPOW2(i)
        tmp_struct.MASK_BIT_SET_NOBOUND_TMP(i)(j) &= NOTPOW2(j)
        MASK_BIT_SET(i)(j) &= NOTPOW2(i)
      }
    }

    (0 until 64).foreach { i =>
      (0 until 64).foreach { j =>
        tmp_struct.MASK_BIT_SET_NOBOUND_COUNT_TMP(i)(j) =
          bitCount(tmp_struct.MASK_BIT_SET_NOBOUND_TMP(i)(j))
      }
    }

    def rankIdx(position: Int, allpieces: Long): Char =
      ((allpieces >>> RANK_ATX8(position)) & 0xff).toChar

    def fileIdx(position: Int, allpieces: Long): Char =
      ((((allpieces & FILE_(position)) * MAGIC_KEY_FILE_RANK) >>> 56) & 0xff).toChar

    def diagonalIdx(position: Int, allpieces: Long): Char =
      ((((allpieces & DIAGONAL(position)) * MAGIC_KEY_DIAG_ANTIDIAG) >>> 56) & 0xff).toChar

    def antiDiagonalIdx(position: Int, allpieces: Long): Char =
      ((((allpieces & ANTIDIAGONAL(position)) * MAGIC_KEY_DIAG_ANTIDIAG) >>> 56) & 0xff).toChar

    def combinations1(elems: Array[Long],
                      len: Int,
                      pos: Array[Int],
                      depth: Int,
                      margin: Int): Array[Long] = {
      val res = ArrayBuffer.empty[Long]
      if (depth >= len) {
        pos.indices.foreach(ii => res += elems(pos(ii)))
        res.toArray
      } else {
        if ((elems.length - margin) < (len - depth)) {
          res.toArray
        } else {
          (margin until elems.length).foreach { ii =>
            pos(depth) = ii
            val v = combinations1(elems, len, pos, depth + 1, ii + 1)
            v.foreach { t1 =>
              res += t1
            }
          }
          res.toArray
        }
      }
    }

    def combinations(elems: Array[Long], len: Int): Array[Long] =
      combinations1(elems, len, new Array[Int](len), 0, 0)

    def getCombinationArr(elements: Array[Long]): Array[Long] = {
      val res = ArrayBuffer.empty[Long]

      var bits: Long = 0L
      (1 to elements.length).foreach { len =>
        var k = 0
        combinations(elements, len).foreach { rr =>
          bits = bits | POW2(rr.toInt)
          k = k + 1
          if (k == len) {
            res += bits
            bits = 0
            k = 0
          }
        }
      }
      res.toArray
    }

    def performDiagCapture(position: Int, allpieces: Long): Long = {
      val q: Long = allpieces & MASK_BIT_UNSET_LEFT_UP(position)
      if (q != 0) {
        val bound = BITScanReverse(q)
        if ((allpieces & POW2(bound)) != 0)
          POW2(bound)
        else 0l
      } else 0l
    } | {
      val q = allpieces & MASK_BIT_UNSET_LEFT_DOWN(position)
      if (q != 0) {
        val bound = BITScanForward(q)
        if ((allpieces & POW2(bound)) != 0)
          POW2(bound)
        else 0l
      } else 0l
    }

    def performDiagShift(position: Int, allpieces: Long): Long = {
      val q1: Long = allpieces & MASK_BIT_UNSET_LEFT_UP(position)
      val k: Long =
        if (q1 != 0)
          tmp_struct.MASK_BIT_SET_NOBOUND_TMP(position)(BITScanReverse(q1))
        else MASK_BIT_SET_LEFT_LOWER(position)
      val q2 = allpieces & MASK_BIT_UNSET_LEFT_DOWN(position)
      k | (if (q2 != 0)
             tmp_struct.MASK_BIT_SET_NOBOUND_TMP(position)(BITScanForward(q2))
           else MASK_BIT_SET_LEFT_UPPER(position))
    }

    def performColumnShift(position: Int, allpieces: Long): Long = {
      val q1: Long = allpieces & MASK_BIT_UNSET_UP(position)
      val k: Long =
        if (q1 != 0)
          tmp_struct.MASK_BIT_SET_NOBOUND_TMP(position)(BITScanReverse(q1))
        else MASK_BIT_SET_VERT_LOWER(position)
      val q2 = allpieces & MASK_BIT_UNSET_DOWN(position)
      k | (if (q2 != 0)
             tmp_struct.MASK_BIT_SET_NOBOUND_TMP(position)(BITScanForward(q2))
           else MASK_BIT_SET_VERT_UPPER(position))

    }

    def performRankShift(position: Int, allpieces: Long): Long = {
      val q1: Long = allpieces & MASK_BIT_UNSET_RIGHT(position)
      val k: Long =
        if (q1 != 0l)
          tmp_struct.MASK_BIT_SET_NOBOUND_TMP(position)(BITScanForward(q1))
        else MASK_BIT_SET_ORIZ_LEFT(position)
      val q2 = allpieces & MASK_BIT_UNSET_LEFT(position)
      k | (if (q2 != 0l)
             tmp_struct.MASK_BIT_SET_NOBOUND_TMP(position)(BITScanReverse(q2))
           else MASK_BIT_SET_ORIZ_RIGHT(position))

    }

    def performAntiDiagShift(position: Int, allpieces: Long): Long = {
      val q1: Long = allpieces & MASK_BIT_UNSET_RIGHT_UP(position)
      val k: Long =
        if (q1 != 0)
          tmp_struct.MASK_BIT_SET_NOBOUND_TMP(position)(BITScanReverse(q1))
        else MASK_BIT_SET_RIGHT_LOWER(position)
      val q2: Long = allpieces & MASK_BIT_UNSET_RIGHT_DOWN(position)
      k | (if (q2 != 0)
             tmp_struct.MASK_BIT_SET_NOBOUND_TMP(position)(BITScanForward(q2))
           else MASK_BIT_SET_RIGHT_UPPER(position))
    }

    def getCombination(e: Long): Array[Long] = {
      val elements = e
      val res = ArrayBuffer.empty[Long]

      @tailrec
      def w(elements: Long): Unit =
        if (elements == 0) ()
        else {
          val o = BITScanForward(elements)
          res += o.toLong
          w(resetLSB(elements))
        }
      w(elements)
      getCombinationArr(res.toArray)
    }

    def performRankCapture(position: Int, allpieces: Long): Long = {
      val x = allpieces & RANK(position)
      val q: Long = x & MASK_BIT_UNSET_LEFT(position)
      if (q != 0 && ((allpieces & POW2(BITScanReverse(q))) != 0))
        POW2(BITScanReverse(q))
      else 0l
    } | {
      val x = allpieces & RANK(position)
      val q = x & MASK_BIT_UNSET_RIGHT(position)
      if (q != 0 && ((allpieces & POW2(BITScanForward(q))) != 0))
        POW2(BITScanForward(q))
      else 0l
    }

    def performColumnCapture(position: Int, allpieces: Long): Long = {
      val x = allpieces & FILE_(position)
      val q: Long = x & MASK_BIT_UNSET_UP(position)
      if (q != 0 && ((allpieces & POW2(BITScanReverse(q))) != 0))
        POW2(BITScanReverse(q))
      else 0l
    } | {
      val x = allpieces & FILE_(position)
      val q = x & MASK_BIT_UNSET_DOWN(position)
      if (q != 0 && ((allpieces & POW2(BITScanForward(q))) != 0))
        POW2(BITScanForward(q))
      else 0l
    }

    def performAntiDiagCapture(position: Int, allpieces: Long): Long = {
      val q: Long = allpieces & MASK_BIT_UNSET_RIGHT_UP(position)
      if (q != 0) {
        val bound = BITScanReverse(q)
        if ((allpieces & POW2(bound)) != 0)
          POW2(bound)
        else 0l
      } else 0l
    } | {
      val q = allpieces & MASK_BIT_UNSET_RIGHT_DOWN(position)
      if (q != 0) {
        val bound = BITScanForward(q)
        if ((allpieces & POW2(bound)) != 0)
          POW2(bound)
        else 0l

      } else 0l
    }

    (0 until 64).foreach { pos =>
      getCombination(DIAGONAL(pos)).foreach { allpieces =>
        BITBOARD_DIAGONAL(pos)(diagonalIdx(pos, allpieces)) = performDiagShift(
          pos,
          allpieces) | performDiagCapture(pos, allpieces)
      }
    }

    (0 until 64).foreach { pos =>
      getCombination(FILE_(pos)).foreach { allpieces =>
        BITBOARD_FILE(pos)(fileIdx(pos, allpieces)) = performColumnShift(
          pos,
          allpieces) | performColumnCapture(pos, allpieces)
      }
    }

    (0 until 64).foreach { pos =>
      getCombination(RANK(pos)).foreach { allpieces =>
        BITBOARD_RANK(pos)(rankIdx(pos, allpieces)) = performRankShift(
          pos,
          allpieces) | performRankCapture(pos, allpieces)
      }
    }

    (0 until 64).foreach { pos =>
      getCombination(ANTIDIAGONAL(pos)).foreach { allpieces =>
        BITBOARD_ANTIDIAGONAL(pos)(antiDiagonalIdx(pos, allpieces)) = performAntiDiagShift(
          pos,
          allpieces) | performAntiDiagCapture(pos, allpieces)
      }
    }

    val b = new Bitboard {
      override def getRankFile(position: Int, allpieces: Long): Long =
        //    ........            00000000
        //    ...q....            00010000
        //    ........            00010000
        //    ...r.p..    --->    11101100
        //    ........            00010000
        //    ........            00010000
        //    ...Q....            00010000
        //    ........            00000000
        BITBOARD_FILE(position)(fileIdx(position, allpieces)) | BITBOARD_RANK(
          position)(rankIdx(position, allpieces))

      override def getDiagonalAntiDiagonal(position: Int,
                                           allpieces: Long): Long =
        //    ........            00010000
        //    q.......            10100000
        //    .B......            00000000
        //    R.......    --->    10100000
        //    ........            00010000
        //    ........            00001000
        //    ........            00000100
        //    ........            00000010
        BITBOARD_DIAGONAL(position)(diagonalIdx(position, allpieces)) |
          BITBOARD_ANTIDIAGONAL(position)(antiDiagonalIdx(position, allpieces))
    }

    //clean tmp structs
    tmp_struct.MASK_BIT_SET_NOBOUND_COUNT_TMP = Array.ofDim[Long](0, 0)
    tmp_struct.MASK_BIT_SET_NOBOUND_TMP = Array.ofDim[Long](0, 0)
    b
  }

}
