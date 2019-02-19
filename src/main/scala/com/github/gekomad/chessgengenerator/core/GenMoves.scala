package com.github.gekomad.chessgengenerator.core

import java.util

import com.github.gekomad.chessgengenerator.board.Board._
import Def._
import ChessBoard.{BitmapPosition, Position, Side}
import GenMoves.{_Tmove, _TmoveP}

import scala.annotation.tailrec

object GenMoves {

  private val MAX_REP_COUNT = 1024

  val MAX_MOVE = 130
  val MAX_PLY = 96

  trait _TmoveP {
    val moveList: Array[_Tmove] = Array.fill[_Tmove](MAX_MOVE)(new _Tmove {})
    var size: Int = 0
  }

  trait _Tmove {
    private def clear(): Unit = {
      promotionPiece = 0
      pieceFrom = 0
      capturedPiece = 0
      from = 0
      to = 0
      side = 0
      type1 = 0
      score = 0
    }

    var promotionPiece: Char = 0
    var pieceFrom: Char = 0
    var capturedPiece: Char = 0
    var from: Char = 0
    var to: Char = 0
    var side: Char = 0
    var type1: Char = 0
    var score: Int = 0

    override def toString: String = {
      val f = ChessBoard.decodeBoardinv(type1, from, side)
      val t = ChessBoard.decodeBoardinv(type1, to, side)
      s"_Tmove (promotionPiece: ${ChessBoard.decodePiece(promotionPiece.toInt)}, pieceFrom: ${ChessBoard
        .decodePiece(pieceFrom.toInt)}," +
        s"capturedPiece: ${ChessBoard.decodePiece(capturedPiece.toInt)}, from: $f, to: $t, side: ${ChessBoard
          .decodeSide(side.toLong)}, " +
        s"type1: ${type1.toInt} ${ChessBoard.decodeType(type1.toInt)}, score: $score)"
    }
  }

  def apply(fen: String, perftMode: Boolean): Option[GenMoves] =
    ChessBoard(fen).map(new GenMoves(_, perftMode))
}

import GenMoves._

case class GenMoves private (chessBoard: ChessBoard, perftMode: Boolean) {

  private var running: Int = 0
  private val forceCheck = false

  private var numMoves = 0l
  private var numMovesq = 0l
  private var repetitionMapCount = 0

  def zobristKey: BitmapPosition = chessBoard.zobristKey

  val genList: Array[_TmoveP] = Array.fill[_TmoveP](MAX_PLY)(new _TmoveP {})

  val repetitionMap: Array[Long] = Array.ofDim[Long](MAX_REP_COUNT)

  val killerHeuristic: Array[Array[Position]] = Array.ofDim[Int](64, 64)

  def getSide: Side = chessBoard.chessboard(SIDETOMOVE_IDX).toInt

  def generate(listId: Int, side: Side): Seq[_Tmove] = {
    resetList(listId)
    val friends = chessBoard.getBitmap(side)
    val enemies = chessBoard.getBitmap(side ^ 1)

    assert(chessBoard.chessboard(KING_BLACK) != 0)
    assert(chessBoard.chessboard(KING_WHITE) != 0)
    generateCaptures(listId, side, enemies, friends)

    generateMoves(listId, side, friends | enemies)

    val l = genList(listId)
    l.moveList.slice(0, l.size).toList
  }

  def generateMoves(listId: Int, side: Side, allpieces: Long): Unit = {
    assert(side == 0 || side == 1)
    assert(chessBoard.chessboard(KING_BLACK) != 0)
    assert(chessBoard.chessboard(KING_WHITE) != 0)
    tryAllCastle(listId, side, allpieces)
    performDiagShift(listId, BISHOP_BLACK + side, side, allpieces)
    performRankFileShift(listId, ROOK_BLACK + side, side, allpieces)
    performRankFileShift(listId, QUEEN_BLACK + side, side, allpieces)
    performDiagShift(listId, QUEEN_BLACK + side, side, allpieces)
    performPawnShift(listId, side, ~allpieces)
    performKnightShiftCapture(listId, KNIGHT_BLACK + side, ~allpieces, side)
    performKingShiftCapture(listId, side, ~allpieces)
  }

  def generateCaptures(listId: Int,
                       side: Side,
                       enemies: Long,
                       friends: Long): Boolean = {
    assert(side == 0 || side == 1)
    assert(chessBoard.chessboard(KING_BLACK) != 0)
    assert(chessBoard.chessboard(KING_WHITE) != 0)
    val allpieces = enemies | friends
    if (performPawnCapture(listId, side, enemies))
      true
    else if (performKingShiftCapture(listId, side, enemies))
      true
    else if (performKnightShiftCapture(listId,
                                       KNIGHT_BLACK + side,
                                       enemies,
                                       side))
      true
    else if (performDiagCapture(listId,
                                BISHOP_BLACK + side,
                                enemies,
                                side,
                                allpieces))
      true
    else if (performRankFileCapture(listId,
                                    ROOK_BLACK + side,
                                    enemies,
                                    side,
                                    allpieces))
      true
    else if (performRankFileCapture(listId,
                                    QUEEN_BLACK + side,
                                    enemies,
                                    side,
                                    allpieces))
      true
    else if (performDiagCapture(listId,
                                QUEEN_BLACK + side,
                                enemies,
                                side,
                                allpieces))
      true
    else
      false
  }

  val getForceCheck: Boolean = forceCheck

  private def getDiagCapture(position: Position,
                             allpieces: Long,
                             enemies: Long): Long = {
    assert(position >= 0 && position < 64)
    chessBoard.bitboards.getDiagonalAntiDiagonal(position, allpieces) & enemies
  }

  private def getDiagShiftAndCapture(position: Position,
                                     enemies: Long,
                                     allpieces: Long): Long = {
    assert(position >= 0 && position < 64)
    val nuovo: Long =
      chessBoard.bitboards.getDiagonalAntiDiagonal(position, allpieces)
    (nuovo & enemies) | (nuovo & ~allpieces)
  }

  private def getDiagShiftCount(position: Position, allpieces: Long): Int = {
    assert(position >= 0 && position < 64)
    bitCount(
      chessBoard.bitboards
        .getDiagonalAntiDiagonal(position, allpieces) & ~allpieces)
  }

  private def performPawnCapture(listId: Int,
                                 side: Side,
                                 enemies: Long): Boolean = {
    if (chessBoard.chessboard(side) == 0) {
      if (chessBoard.chessboard(ENPASSANT_IDX) != NO_ENPASSANT) {
        chessBoard.updateZobristKey(13,
                                    chessBoard.chessboard(ENPASSANT_IDX).toInt)
      }
      chessBoard.chessboard(ENPASSANT_IDX) = NO_ENPASSANT
      false
    } else {

      val r = if (side != 0) {
        ((chessBoard.chessboard(side) << 7) & TABCAPTUREPAWN_LEFT & enemies, -7)
      } else {
        ((chessBoard.chessboard(side) >> 7) & TABCAPTUREPAWN_RIGHT & enemies, 7)
      }
      @tailrec
      def w(x: Long, of: Int): Boolean =
        if (x == 0) false
        else {
          val o = BITScanForward(x)
          if ((side != 0 && o > 55) || (side == 0 && o < 8)) { //PROMOTION
            if (pushmove(listId,
                         PROMOTION_MOVE_MASK,
                         o + of,
                         o,
                         side,
                         QUEEN_BLACK + side,
                         side))
              true //queen
            else if (perftMode) {
              if (pushmove(listId,
                           PROMOTION_MOVE_MASK,
                           o + of,
                           o,
                           side,
                           KNIGHT_BLACK + side,
                           side))
                true //knight
              else if (pushmove(listId,
                                PROMOTION_MOVE_MASK,
                                o + of,
                                o,
                                side,
                                ROOK_BLACK + side,
                                side))
                true //rock
              else if (pushmove(listId,
                                PROMOTION_MOVE_MASK,
                                o + of,
                                o,
                                side,
                                BISHOP_BLACK + side,
                                side))
                true //bishop
            } else false
          } else if (pushmove(listId,
                              STANDARD_MOVE_MASK,
                              o + of,
                              o,
                              side,
                              NO_PROMOTION,
                              side)) {
            true
          }
          w(resetLSB(x), of)
        }
      if (w(r._1, r._2)) true
      else {
        val r2 =
          if (side != 0)
            ((chessBoard
               .chessboard(side) << 9) & TABCAPTUREPAWN_RIGHT & enemies,
             -9)
          else
            ((chessBoard.chessboard(side) >> 9) & TABCAPTUREPAWN_LEFT & enemies,
             9)
        @tailrec
        def w2(x2: Long, of: Int): Boolean =
          if (x2 == 0) false
          else {
            val o = BITScanForward(x2)
            if ((side != 0 && o > 55) || (side == 0 && o < 8)) { //PROMOTION
              if (pushmove(listId,
                           PROMOTION_MOVE_MASK,
                           o + of,
                           o,
                           side,
                           QUEEN_BLACK + side,
                           side))
                true //queen
              else if (perftMode) {
                if (pushmove(listId,
                             PROMOTION_MOVE_MASK,
                             o + of,
                             o,
                             side,
                             KNIGHT_BLACK + side,
                             side))
                  true //knight
                else if (pushmove(listId,
                                  PROMOTION_MOVE_MASK,
                                  o + of,
                                  o,
                                  side,
                                  BISHOP_BLACK + side,
                                  side))
                  true //bishop
                else if (pushmove(listId,
                                  PROMOTION_MOVE_MASK,
                                  o + of,
                                  o,
                                  side,
                                  ROOK_BLACK + side,
                                  side))
                  true //rock

              } else false
            } else if (pushmove(listId,
                                STANDARD_MOVE_MASK,
                                o + of,
                                o,
                                side,
                                NO_PROMOTION,
                                side))
              true

            w2(resetLSB(x2), of)
          }
        if (w2(r2._1, r2._2)) true
        else {
          //ENPASSANT
          if (chessBoard.chessboard(ENPASSANT_IDX) != NO_ENPASSANT) {
            val x3 = ENPASSANT_MASK(side ^ 1)(
              chessBoard.chessboard(ENPASSANT_IDX).toInt) & chessBoard
              .chessboard(side)
            @tailrec
            def go(x: Long): Unit = if (x != 0) {
              val o = BITScanForward(x)
              val k = (if (side != 0) chessBoard.chessboard(ENPASSANT_IDX) + 8
                       else chessBoard.chessboard(ENPASSANT_IDX) - 8).toInt
              pushmove(listId,
                       ENPASSANT_MOVE_MASK,
                       o,
                       k,
                       side,
                       NO_PROMOTION,
                       side)
              go(resetLSB(x))
            }
            go(x3)
            chessBoard.updateZobristKey(
              13,
              chessBoard.chessboard(ENPASSANT_IDX).toInt)
            chessBoard.chessboard(ENPASSANT_IDX) = NO_ENPASSANT
          }
          false
        }
      }
    }
  }

  private def performPawnShift(listId: Int,
                               side: Side,
                               xallpieces: Long): Unit = {
    val x1 = chessBoard.chessboard(side)
    if ((x1 & PAWNS_JUMP(side)) != 0) {
      checkJumpPawn(listId, side, x1, xallpieces)
    }
    val (tt, x) = if (side != 0) {
      (-8, x1 << 8)
    } else {
      (8, x1 >> 8)
    }

    @tailrec
    def go(x: Long): Unit = if (x != 0) {
      val o = BITScanForward(x)
      assert(chessBoard.getPieceAt(side, POW2(o + tt)) != SQUARE_FREE)
      assert((chessBoard.getBitmap(side) & POW2(o + tt)) != 0)
      if (o > 55 || o < 8) {
        pushmove(listId,
                 PROMOTION_MOVE_MASK,
                 o + tt,
                 o,
                 side,
                 QUEEN_BLACK + side,
                 side)
        if (perftMode) {
          pushmove(listId,
                   PROMOTION_MOVE_MASK,
                   o + tt,
                   o,
                   side,
                   KNIGHT_BLACK + side,
                   side)
          pushmove(listId,
                   PROMOTION_MOVE_MASK,
                   o + tt,
                   o,
                   side,
                   BISHOP_BLACK + side,
                   side)
          pushmove(listId,
                   PROMOTION_MOVE_MASK,
                   o + tt,
                   o,
                   side,
                   ROOK_BLACK + side,
                   side)
        }
      } else {
        pushmove(listId,
                 STANDARD_MOVE_MASK,
                 o + tt,
                 o,
                 side,
                 NO_PROMOTION,
                 side)
      }
      go(resetLSB(x))
    }
    go(x & xallpieces)
  }

  def getListSize(listId: Int): Int = genList(listId).size

  private def pushStackMove(): Unit =
    pushStackMove(chessBoard.chessboard(ZOBRISTKEY_IDX))

  private def resetList(listId: Int): Unit = genList(listId).size = 0

  private def incKillerHeuristic(from: Position,
                                 to: Position,
                                 value: Int): Unit = {
    if (getRunning != 0) {
      assert(from > 0 && from < 64)
      assert(to > 0 && to < 64)
      assert(killerHeuristic(from)(to) <= killerHeuristic(from)(to) + value)
      killerHeuristic(from)(to) += value
    }
  }

  private def isAttacked(side: Side,
                         position: Position,
                         allpieces: Long): Boolean =
    getAttackers(side, exitOnFirst = true, position, allpieces) != 0

  private def getAllAttackers(side: Side,
                              position: Position,
                              allpieces: Long): Long =
    getAttackers(side, exitOnFirst = false, position, allpieces)

  private def inCheck(side: Side,
                      type1: Char,
                      from: Position,
                      to: Position,
                      pieceFrom: Position,
                      pieceTo: Position,
                      promotionPiece: Int): Boolean = {

    assert(from >= 0 && from < 64)
    assert(to >= 0 && to < 64)
    assert(pieceFrom >= 0 && pieceFrom < 12)
    assert(side == 0 || side == 1)
    assert(perftMode || forceCheck)
    assert((type1 & 0xc) == 0)

    val result1 = type1 & 0x3 match {

      case STANDARD_MOVE_MASK =>
        assert(pieceFrom != SQUARE_FREE)
        assert(pieceTo != KING_BLACK)
        assert(pieceTo != KING_WHITE)
        val from1 = chessBoard.chessboard(pieceFrom)
        val to1 = if (pieceTo != SQUARE_FREE) {
          val to1 = chessBoard.chessboard(pieceTo)
          chessBoard.chessboard(pieceTo) &= NOTPOW2(to)
          to1
        } else -1l
        chessBoard.chessboard(pieceFrom) &= NOTPOW2(from)
        chessBoard.chessboard(pieceFrom) |= POW2(to)
        assert(chessBoard.chessboard(KING_BLACK) != 0)
        assert(chessBoard.chessboard(KING_WHITE) != 0)

        val result = isAttacked(
          side,
          BITScanForward(chessBoard.chessboard(KING_BLACK + side)),
          chessBoard.getBitmap(BLACK) | chessBoard.getBitmap(WHITE))
        chessBoard.chessboard(pieceFrom) = from1
        if (pieceTo != SQUARE_FREE) chessBoard.chessboard(pieceTo) = to1
        result
      case PROMOTION_MOVE_MASK =>
        val to1 =
          if (pieceTo != SQUARE_FREE) chessBoard.chessboard(pieceTo) else 0l
        val from1 = chessBoard.chessboard(pieceFrom)
        val p1 = chessBoard.chessboard(promotionPiece)
        chessBoard.chessboard(pieceFrom) &= NOTPOW2(from)
        if (pieceTo != SQUARE_FREE)
          chessBoard.chessboard(pieceTo) &= NOTPOW2(to)

        chessBoard.chessboard(promotionPiece) = chessBoard.chessboard(
          promotionPiece) | POW2(to)
        val result = isAttacked(
          side,
          BITScanForward(chessBoard.chessboard(KING_BLACK + side)),
          chessBoard.getBitmap(BLACK) | chessBoard.getBitmap(WHITE))
        if (pieceTo != SQUARE_FREE) chessBoard.chessboard(pieceTo) = to1

        chessBoard.chessboard(pieceFrom) = from1
        chessBoard.chessboard(promotionPiece) = p1
        result
      case ENPASSANT_MOVE_MASK =>
        val to1 = chessBoard.chessboard(side ^ 1)
        val from1 = chessBoard.chessboard(side)
        chessBoard.chessboard(side) &= NOTPOW2(from)
        chessBoard.chessboard(side) |= POW2(to)
        if (side != 0)
          chessBoard.chessboard(side ^ 1) &= NOTPOW2(to - 8)
        else
          chessBoard.chessboard(side ^ 1) &= NOTPOW2(to + 8)

        val result = isAttacked(
          side,
          BITScanForward(chessBoard.chessboard(KING_BLACK + side)),
          chessBoard.getBitmap(BLACK) | chessBoard.getBitmap(WHITE))
        chessBoard.chessboard(side ^ 1) = to1
        chessBoard.chessboard(side) = from1
        result
      case _ => ???
    }

    result1
  }

  private def pushmove(listId: Int,
                       type1: Char,
                       from: Position,
                       to: Position,
                       side: Side,
                       promotionPiece: Int,
                       pieceFrom: Int): Boolean = {

    assert(chessBoard.chessboard(KING_BLACK) != 0)
    assert(chessBoard.chessboard(KING_WHITE) != 0)
    var piece_captured = SQUARE_FREE
    var res = false
    if (((type1 & 0x3) != ENPASSANT_MOVE_MASK) && 0 == (type1 & 0xc)) {
      piece_captured =
        if (side != 0) chessBoard.getPieceAt(BLACK, POW2(to))
        else chessBoard.getPieceAt(WHITE, POW2(to))
      if (piece_captured == KING_BLACK + (side ^ 1)) {
        res = true
      }
    } else if ((type1 & 0xc) == 0) { //no castle
      piece_captured = side ^ 1
    }
    val i = if ((type1 & 0xc) == 0 && (forceCheck || perftMode)) { //no castle

      if (inCheck(side,
                  type1,
                  from,
                  to,
                  pieceFrom,
                  piece_captured,
                  promotionPiece))
        false
      else true
    } else true

    if (!i) false
    else {

      assert(listId >= 0 && listId < MAX_PLY, listId)
      assert(getListSize(listId) < MAX_MOVE)
      val mos: _Tmove = genList(listId).moveList(getListSize(listId))
      genList(listId).size += 1
      mos.type1 =
        (chessBoard.chessboard(RIGHT_CASTLE_IDX).toInt | type1.toInt).toChar
      mos.side = side.toChar
      mos.capturedPiece = piece_captured.toChar
      if ((type1 & 0x3) != 0) {
        mos.from = from.toChar
        mos.to = to.toChar
        mos.pieceFrom = pieceFrom.toChar
        mos.promotionPiece = promotionPiece.toChar
        if (!perftMode) {
          if (res) {
            mos.score = _INFINITE
          } else {
            assert(pieceFrom >= 0 && pieceFrom < 12)

            assert(from >= 0 && from < 64, from)
            assert(to >= 0 && to < 64)
            mos.score = killerHeuristic(from)(to)

            mos.score += (if (PIECES_VALUE(piece_captured) >= PIECES_VALUE(
                                pieceFrom))
                            (PIECES_VALUE(piece_captured) - PIECES_VALUE(
                              pieceFrom)) * 2
                          else PIECES_VALUE(piece_captured))

          }
        }
      } else if ((type1 & 0xc) != 0) { //castle
        assert(chessBoard.chessboard(RIGHT_CASTLE_IDX) != 0)
        mos.score = 100
      }

      assert(getListSize(listId) < MAX_MOVE)
      res
    }
  }

  private def getMove(listId: Int, i: Int): _Tmove = genList(listId).moveList(i)

  private def setRunning(t: Int): Unit = running = t

  private def getRunning: Int = running

  private def inCheck(side: Side): Boolean =
    isAttacked(side,
               BITScanForward(chessBoard.chessboard(KING_BLACK + side)),
               chessBoard.getBitmap(BLACK) | chessBoard.getBitmap(WHITE))

  private def setKillerHeuristic(from: Position,
                                 to: Position,
                                 value: Int): Unit = {
    if (getRunning != 0) {
      assert(from > 0 && from < 64)
      assert(to > 0 && to < 64)
      killerHeuristic(from)(to) = value
    }
  }

  private def checkJumpPawn(listId: Int,
                            side: Side,
                            x1: Long,
                            xallpieces: Long): Unit = {
    val y = x1 & TABJUMPPAWN
    val x =
      if (side != 0)
        (((y << 8) & xallpieces) << 8) & xallpieces
      else
        (((y >> 8) & xallpieces) >> 8) & xallpieces

    @tailrec
    def go(x: Long): Unit = if (x != 0) {
      val o: Position = BITScanForward(x)
      pushmove(listId,
               STANDARD_MOVE_MASK,
               o + (if (side != 0) -16 else 16),
               o,
               side,
               NO_PROMOTION,
               side)
      go(resetLSB(x))
    }

    go(x)

  }

  private def popStackMove(): Unit = {
    assert(repetitionMapCount > 0)
    repetitionMapCount -= 1
    if (repetitionMapCount != 0 && repetitionMap(repetitionMapCount - 1) == 0) {
      repetitionMapCount -= 1
    }
  }

  private def pushStackMove(key: Long): Unit = {
    assert(repetitionMapCount < MAX_REP_COUNT - 1)
    repetitionMap(repetitionMapCount) = key
    repetitionMapCount += 1
  }

  private def performRankFileCapture(listId: Int,
                                     piece: Int,
                                     enemies: Long,
                                     side: Int,
                                     allpieces: Long): Boolean = {
    assert(piece >= 0 && piece < 12)
    assert(side == 0 || side == 1)

    @tailrec
    def w0(x2: Long): Boolean =
      if (x2 == 0) false
      else {
        val position = BITScanForward(x2)

        @tailrec
        def w(rankFile: Long): Boolean =
          if (rankFile == 0) false
          else {
            if (pushmove(listId,
                         STANDARD_MOVE_MASK,
                         position,
                         BITScanForward(rankFile),
                         side,
                         NO_PROMOTION,
                         piece))
              true
            else
              w(resetLSB(rankFile))
          }

        if (w(chessBoard.bitboards.getRankFile(position, allpieces) & enemies))
          true
        else
          w0(resetLSB(x2))
      }
    w0(chessBoard.chessboard(piece))

  }

  private def performRankFileCaptureAndShiftCount(position: Position,
                                                  enemies: Long,
                                                  allpieces: Long): Int = {
    assert(position >= 0 && position < 64)
    val rankFile2 = chessBoard.bitboards.getRankFile(position, allpieces)
    val rankFile1 = (rankFile2 & enemies) | (rankFile2 & ~allpieces)
    bitCount(rankFile1)
  }

  private def performDiagCapture(listId: Int,
                                 piece: Int,
                                 enemies: Long,
                                 side: Side,
                                 allpieces: Long): Boolean = {
    assert(piece >= 0 && piece < 12)
    assert(side == 0 || side == 1)

    @tailrec
    def w2(x2: Long): Boolean =
      if (x2 == 0) false
      else {
        val position = BITScanForward(x2)

        @tailrec
        def w(diag: Long): Boolean =
          if (diag == 0) false
          else if (pushmove(listId,
                            STANDARD_MOVE_MASK,
                            position,
                            BITScanForward(diag),
                            side,
                            NO_PROMOTION,
                            piece))
            true
          else w(resetLSB(diag))
        if (w(chessBoard.bitboards
              .getDiagonalAntiDiagonal(position, allpieces) & enemies)) true
        else
          w2(resetLSB(x2))
      }
    w2(chessBoard.chessboard(piece))

  }

  private def performRankFileShift(listId: Int,
                                   piece: Int,
                                   side: Side,
                                   allpieces: Long): Unit = {
    assert(piece >= 0 && piece < 12)
    assert(side == 0 || side == 1)
    @tailrec
    def go(x2: Long): Unit = if (x2 != 0) {
      val position = BITScanForward(x2)
      val rankFile = chessBoard.bitboards
        .getRankFile(position, allpieces) & ~allpieces
      @tailrec
      def go2(rankFile: Long): Unit = if (rankFile != 0) {

        pushmove(listId,
                 STANDARD_MOVE_MASK,
                 position,
                 BITScanForward(rankFile),
                 side,
                 NO_PROMOTION,
                 piece)
        go2(resetLSB(rankFile))
      }
      go2(rankFile)
      go(resetLSB(x2))
    }

    go(chessBoard.chessboard(piece))

  }

  private def performDiagShift(listId: Int,
                               piece: Int,
                               side: Side,
                               allpieces: Long): Unit = {
    assert(piece >= 0 && piece < 12)
    assert(side == 0 || side == 1)

    @tailrec
    def go(x2: Long): Unit = if (x2 != 0) {

      val position = BITScanForward(x2)
      val diag = chessBoard.bitboards
        .getDiagonalAntiDiagonal(position, allpieces) & ~allpieces

      @tailrec
      def go2(diag: Long): Unit = if (diag != 0) {
        pushmove(listId,
                 STANDARD_MOVE_MASK,
                 position,
                 BITScanForward(diag),
                 side,
                 NO_PROMOTION,
                 piece)
        go2(resetLSB(diag))
      }
      go2(diag)
      go(resetLSB(x2))
    }
    go(chessBoard.chessboard(piece))
  }

  private def generateMoves(listId: Int,
                            piece: Int,
                            side: Side,
                            allpieces: Long): Unit = {
    assert(side == 0 || side == 1)
    if (side != 0) generateMoves(listId, WHITE, allpieces)
    else generateMoves(listId, BLACK, allpieces)
  }

  private def getMobilityPawns(side: Side,
                               ep: Int,
                               ped_friends: Long,
                               enemies: Long,
                               xallpieces: Long): Int = {
    assert(side == 0 || side == 1)
    if (ep == NO_ENPASSANT) 0
    else if (bitCount(ENPASSANT_MASK(side ^ 1)(ep) & chessBoard.chessboard(
               side)) + side == WHITE)
      bitCount((ped_friends << 8) & xallpieces) + bitCount(
        ((((ped_friends & TABJUMPPAWN) << 8) & xallpieces) << 8) & xallpieces) + bitCount(
        (chessBoard.chessboard(side) << 7) & TABCAPTUREPAWN_LEFT & enemies
      ) + bitCount(
        (chessBoard.chessboard(side) << 9) & TABCAPTUREPAWN_RIGHT & enemies)
    else
      bitCount((ped_friends >> 8) & xallpieces) + bitCount(
        ((((ped_friends & TABJUMPPAWN) >> 8) & xallpieces) >> 8) & xallpieces) + bitCount(
        (chessBoard.chessboard(side) >> 7) & TABCAPTUREPAWN_RIGHT & enemies
      ) + bitCount(
        (chessBoard.chessboard(side) >> 9) & TABCAPTUREPAWN_LEFT & enemies)
  }

  private def getMobilityQueen(position: Position,
                               enemies: Long,
                               allpieces: Long): Int = {
    assert(position >= 0 && position < 64)
    performRankFileCaptureAndShiftCount(position, enemies, allpieces) +
      bitCount(getDiagShiftAndCapture(position, enemies, allpieces))
  }

  private def getMobilityRook(position: Position,
                              enemies: Long,
                              friends: Long): Int = {
    assert(position >= 0 && position < 64)
    performRankFileCaptureAndShiftCount(position, enemies, enemies | friends)
  }

  private def clearKillerHeuristic(): Unit =
    killerHeuristic.foreach(a => util.Arrays.fill(a, 0))

  private def performCastle(side: Side, type1: Char): Unit = {
    assert(side == 0 || side == 1)
    if (side == WHITE) {
      if ((type1 & KING_SIDE_CASTLE_MOVE_MASK) != 0) {
        assert(chessBoard.getPieceAt(side, POW2_3) == KING_WHITE)
        assert(chessBoard.getPieceAt(side, POW2_1) == SQUARE_FREE)
        assert(chessBoard.getPieceAt(side, POW2_2) == SQUARE_FREE)
        assert(chessBoard.getPieceAt(side, POW2_0) == ROOK_WHITE)
        chessBoard.updateZobristKey(KING_WHITE, 3)
        chessBoard.updateZobristKey(KING_WHITE, 1)
        chessBoard.chessboard(KING_WHITE) = (chessBoard.chessboard(KING_WHITE) | POW2_1) & NOTPOW2_3
        chessBoard.updateZobristKey(ROOK_WHITE, 2)
        chessBoard.updateZobristKey(ROOK_WHITE, 0)
        chessBoard.chessboard(ROOK_WHITE) = (chessBoard.chessboard(ROOK_WHITE) | POW2_2) & NOTPOW2_0
      } else {
        assert((type1 & QUEEN_SIDE_CASTLE_MOVE_MASK) != 0)
        assert(chessBoard.getPieceAt(side, POW2_3) == KING_WHITE)
        assert(chessBoard.getPieceAt(side, POW2_4) == SQUARE_FREE)
        assert(chessBoard.getPieceAt(side, POW2_5) == SQUARE_FREE)
        assert(chessBoard.getPieceAt(side, POW2_6) == SQUARE_FREE)
        assert(chessBoard.getPieceAt(side, POW2_7) == ROOK_WHITE)
        chessBoard.chessboard(KING_WHITE) = (chessBoard.chessboard(KING_WHITE) | POW2_5) & NOTPOW2_3
        chessBoard.updateZobristKey(KING_WHITE, 5)
        chessBoard.updateZobristKey(KING_WHITE, 3)
        chessBoard.chessboard(ROOK_WHITE) = (chessBoard.chessboard(ROOK_WHITE) | POW2_4) & NOTPOW2_7
        chessBoard.updateZobristKey(ROOK_WHITE, 4)
        chessBoard.updateZobristKey(ROOK_WHITE, 7)
      }
    } else {
      if ((type1 & KING_SIDE_CASTLE_MOVE_MASK) != 0) {
        assert(chessBoard.getPieceAt(side, POW2_59) == KING_BLACK)
        assert(chessBoard.getPieceAt(side, POW2_58) == SQUARE_FREE)
        assert(chessBoard.getPieceAt(side, POW2_57) == SQUARE_FREE)
        assert(chessBoard.getPieceAt(side, POW2_56) == ROOK_BLACK)
        chessBoard.chessboard(KING_BLACK) = (chessBoard.chessboard(KING_BLACK) | POW2_57) & NOTPOW2_59
        chessBoard.updateZobristKey(KING_BLACK, 57)
        chessBoard.updateZobristKey(KING_BLACK, 59)
        chessBoard.chessboard(ROOK_BLACK) = (chessBoard.chessboard(ROOK_BLACK) | POW2_58) & NOTPOW2_56
        chessBoard.updateZobristKey(ROOK_BLACK, 58)
        chessBoard.updateZobristKey(ROOK_BLACK, 56)
      } else {
        assert((type1 & QUEEN_SIDE_CASTLE_MOVE_MASK) != 0)
        assert(chessBoard.getPieceAt(side, POW2_59) == KING_BLACK)
        assert(chessBoard.getPieceAt(side, POW2_60) == SQUARE_FREE)
        assert(chessBoard.getPieceAt(side, POW2_61) == SQUARE_FREE)
        assert(chessBoard.getPieceAt(side, POW2_62) == SQUARE_FREE)
        assert(chessBoard.getPieceAt(side, POW2_63) == ROOK_BLACK)
        chessBoard.chessboard(KING_BLACK) = (chessBoard.chessboard(KING_BLACK) | POW2_61) & NOTPOW2_59
        chessBoard.updateZobristKey(KING_BLACK, 61)
        chessBoard.updateZobristKey(KING_BLACK, 59)
        chessBoard.chessboard(ROOK_BLACK) = (chessBoard.chessboard(ROOK_BLACK) | POW2_60) & NOTPOW2_63
        chessBoard.updateZobristKey(ROOK_BLACK, 60)
        chessBoard.updateZobristKey(ROOK_BLACK, 63)
      }
    }
  }

  private def getMobilityCastle(side: Side, allpieces: Long): Int = {
    assert(side == 0 || side == 1)
    var count = 0
    if (side == WHITE) {
      if ((POW2_3 & chessBoard.chessboard(KING_WHITE)) != 0 && ((allpieces & 0x6L) == 0) && (chessBoard
            .chessboard(RIGHT_CASTLE_IDX) & RIGHT_KING_CASTLE_WHITE_MASK) != 0
          && (chessBoard.chessboard(ROOK_WHITE) & POW2_0) != 0 && !isAttacked(
            WHITE,
            1,
            allpieces) && !isAttacked(WHITE, 2, allpieces) && !isAttacked(
            WHITE,
            3,
            allpieces)) {
        count += 1
      }
      if ((POW2_3 & chessBoard.chessboard(KING_WHITE)) != 0 && ((allpieces & 0x70L) == 0) && (chessBoard
            .chessboard(RIGHT_CASTLE_IDX) & RIGHT_QUEEN_CASTLE_WHITE_MASK) != 0
          && (chessBoard.chessboard(ROOK_WHITE) & POW2_7) != 0 && !isAttacked(
            WHITE,
            3,
            allpieces) && !isAttacked(WHITE, 4, allpieces) && !isAttacked(
            WHITE,
            5,
            allpieces)) {
        count += 1
      }
    } else {
      if ((POW2_59 & chessBoard.chessboard(KING_BLACK)) != 0 && (chessBoard.chessboard(
            RIGHT_CASTLE_IDX) & RIGHT_KING_CASTLE_BLACK_MASK) != 0 && ((allpieces & 0x600000000000000L) == 0)
          && (chessBoard.chessboard(ROOK_BLACK) & POW2_56) != 0 && !isAttacked(
            BLACK,
            57,
            allpieces) && !isAttacked(BLACK, 58, allpieces) && !isAttacked(
            BLACK,
            59,
            allpieces)) {
        count += 1
      }
      if ((POW2_59 & chessBoard.chessboard(KING_BLACK)) != 0 && (chessBoard.chessboard(
            RIGHT_CASTLE_IDX) & RIGHT_QUEEN_CASTLE_BLACK_MASK) != 0 && ((allpieces & 0x7000000000000000L) == 0)
          && (chessBoard.chessboard(ROOK_BLACK) & POW2_63) != 0 && !isAttacked(
            BLACK,
            59,
            allpieces) && !isAttacked(BLACK, 60, allpieces) && !isAttacked(
            BLACK,
            61,
            allpieces)) {
        count += 1
      }
    }
    count
  }

  private def tryAllCastle(listId: Int, side: Side, allpieces: Long): Unit = {

    assert(side == 0 || side == 1)
    if (side == WHITE) {
      if ((POW2_3 & chessBoard.chessboard(KING_WHITE)) != 0 && 0 == (allpieces & 0x6L) && (chessBoard
            .chessboard(RIGHT_CASTLE_IDX) & RIGHT_KING_CASTLE_WHITE_MASK) != 0
          && (chessBoard.chessboard(ROOK_WHITE) & POW2_0) != 0 && !isAttacked(
            WHITE,
            1,
            allpieces) && !isAttacked(WHITE, 2, allpieces) &&
          !isAttacked(WHITE, 3, allpieces)) {
        pushmove(listId,
                 KING_SIDE_CASTLE_MOVE_MASK,
                 -1,
                 -1,
                 WHITE,
                 NO_PROMOTION,
                 -1)
      }
      if ((POW2_3 & chessBoard.chessboard(KING_WHITE)) != 0 && 0 == (allpieces & 0x70L) && (chessBoard
            .chessboard(RIGHT_CASTLE_IDX) & RIGHT_QUEEN_CASTLE_WHITE_MASK) != 0
          && (chessBoard.chessboard(ROOK_WHITE) & POW2_7) != 0 && !isAttacked(
            WHITE,
            3,
            allpieces) && !isAttacked(WHITE, 4, allpieces) &&
          !isAttacked(WHITE, 5, allpieces)) {
        pushmove(listId,
                 QUEEN_SIDE_CASTLE_MOVE_MASK,
                 -1,
                 -1,
                 WHITE,
                 NO_PROMOTION,
                 -1)
      }
    } else {
      if ((POW2_59 & chessBoard.chessboard(KING_BLACK)) != 0 && (chessBoard
            .chessboard(RIGHT_CASTLE_IDX) & RIGHT_KING_CASTLE_BLACK_MASK) != 0 &&
          0 == (allpieces & 0x600000000000000L) && (chessBoard.chessboard(
            ROOK_BLACK) & POW2_56) != 0 && !isAttacked(BLACK, 57, allpieces) &&
          !isAttacked(BLACK, 58, allpieces) && !isAttacked(BLACK,
                                                           59,
                                                           allpieces)) {
        pushmove(listId,
                 KING_SIDE_CASTLE_MOVE_MASK,
                 -1,
                 -1,
                 BLACK,
                 NO_PROMOTION,
                 -1)
      }
      if ((POW2_59 & chessBoard.chessboard(KING_BLACK)) != 0 && (chessBoard
            .chessboard(RIGHT_CASTLE_IDX) & RIGHT_QUEEN_CASTLE_BLACK_MASK) != 0
          && 0 == (allpieces & 0x7000000000000000L) && (chessBoard.chessboard(
            ROOK_BLACK) & POW2_63) != 0 && !isAttacked(BLACK, 59, allpieces)
          && !isAttacked(BLACK, 60, allpieces) && !isAttacked(BLACK,
                                                              61,
                                                              allpieces)) {
        pushmove(listId,
                 QUEEN_SIDE_CASTLE_MOVE_MASK,
                 -1,
                 -1,
                 BLACK,
                 NO_PROMOTION,
                 -1)
      }
    }

  }

  private def performKnightShiftCapture(listId: Int,
                                        piece: Int,
                                        enemies: Long,
                                        side: Side): Boolean = {
    assert(piece >= 0 && piece < 12)
    assert(side == 0 || side == 1)

    @tailrec
    def w(x: Long): Boolean =
      if (x == 0) false
      else {
        val pos = BITScanForward(x)

        @tailrec
        def w1(x1: Long): Boolean =
          if (x1 == 0) false
          else if (pushmove(listId,
                            STANDARD_MOVE_MASK,
                            pos,
                            BITScanForward(x1),
                            side,
                            NO_PROMOTION,
                            piece))
            true
          else w1(resetLSB(x1))
        if (w1(enemies & KNIGHT_MASK(pos))) true
        else
          w(resetLSB(x))
      }

    w(chessBoard.chessboard(piece))

  }

  private def performKingShiftCapture(listId: Int,
                                      side: Side,
                                      enemies: Long): Boolean = {
    assert(side == 0 || side == 1)
    val pos = BITScanForward(chessBoard.chessboard(KING_BLACK + side))
    assert(pos != -1)

    @tailrec
    def w1(x1: Long): Boolean =
      if (x1 == 0) false
      else if (pushmove(listId,
                        STANDARD_MOVE_MASK,
                        pos,
                        BITScanForward(x1),
                        side,
                        NO_PROMOTION,
                        KING_BLACK + side))
        true
      else w1(resetLSB(x1))

    w1(enemies & NEAR_MASK1(pos))

  }

  private def unPerformCastle(side: Side, type1: Char): Unit = {
    assert(side == 0 || side == 1)
    if (side == WHITE) {
      if ((type1 & KING_SIDE_CASTLE_MOVE_MASK) != 0) {
        assert(chessBoard.getPieceAt(side, POW2_1) == KING_WHITE)
        assert(chessBoard.getPieceAt(side, POW2_0) == 12)
        assert(chessBoard.getPieceAt(side, POW2_3) == 12)
        assert(chessBoard.getPieceAt(side, POW2_2) == ROOK_WHITE)
        chessBoard.chessboard(KING_WHITE) = (chessBoard.chessboard(KING_WHITE) | POW2_3) & NOTPOW2_1
        chessBoard.chessboard(ROOK_WHITE) = (chessBoard.chessboard(ROOK_WHITE) | POW2_0) & NOTPOW2_2
      } else {
        chessBoard.chessboard(KING_WHITE) = (chessBoard.chessboard(KING_WHITE) | POW2_3) & NOTPOW2_5
        chessBoard.chessboard(ROOK_WHITE) = (chessBoard.chessboard(ROOK_WHITE) | POW2_7) & NOTPOW2_4
      }
    } else {
      if ((type1 & KING_SIDE_CASTLE_MOVE_MASK) != 0) {
        chessBoard.chessboard(KING_BLACK) = (chessBoard.chessboard(KING_BLACK) | POW2_59) & NOTPOW2_57
        chessBoard.chessboard(ROOK_BLACK) = (chessBoard.chessboard(ROOK_BLACK) | POW2_56) & NOTPOW2_58
      } else {
        chessBoard.chessboard(KING_BLACK) = (chessBoard.chessboard(KING_BLACK) | POW2_59) & NOTPOW2_61
        chessBoard.chessboard(ROOK_BLACK) = (chessBoard.chessboard(ROOK_BLACK) | POW2_63) & NOTPOW2_60
      }
    }
  }

  def takeback(move: _Tmove, oldkey: Long, rep: Boolean): Unit = {
    if (rep) {
      popStackMove()
    }
    chessBoard.chessboard(ZOBRISTKEY_IDX) = oldkey
    chessBoard.chessboard(ENPASSANT_IDX) = NO_ENPASSANT

    chessBoard.chessboard(RIGHT_CASTLE_IDX) = move.type1 & 0xf0
    if ((move.type1 & 0x3) == STANDARD_MOVE_MASK || (move.type1 & 0x3) == ENPASSANT_MOVE_MASK) {
      val posTo = move.to
      val posFrom = move.from
      val movecapture = move.capturedPiece
      assert(posFrom >= 0 && posFrom < 64)
      assert(posTo >= 0 && posTo < 64)
      val pieceFrom = move.pieceFrom
      chessBoard.chessboard(pieceFrom) = (chessBoard.chessboard(pieceFrom) & NOTPOW2(
        posTo)) | POW2(posFrom)
      if (movecapture != SQUARE_FREE) {
        if ((move.type1 & 0x3) != ENPASSANT_MOVE_MASK) {
          chessBoard.chessboard(movecapture) |= POW2(posTo)
        } else {
          assert(movecapture == (move.side ^ 1))
          if (move.side != 0) {
            chessBoard.chessboard(movecapture) |= POW2(posTo - 8)
          } else {
            chessBoard.chessboard(movecapture) |= POW2(posTo + 8)
          }
        }
      }
    } else if ((move.type1 & 0x3) == PROMOTION_MOVE_MASK) {
      val posTo = move.to
      val posFrom = move.from
      val movecapture = move.capturedPiece
      assert(posTo >= 0 && move.side >= 0 && move.promotionPiece >= 0)
      chessBoard.chessboard(move.side) |= POW2(posFrom)
      chessBoard.chessboard(move.promotionPiece) &= NOTPOW2(posTo)
      if (movecapture != SQUARE_FREE) {
        chessBoard.chessboard(movecapture) |= POW2(posTo)
      }
    } else if ((move.type1 & 0xc) != 0) { //castle
      unPerformCastle(move.side, move.type1)
    }
  }

  def makemove(move: _Tmove, rep: Boolean, checkInCheck: Boolean): Boolean = {

    assert(
      bitCount(chessBoard.chessboard(KING_WHITE)) == 1 && bitCount(
        chessBoard.chessboard(KING_BLACK)) == 1)
    var pieceFrom = SQUARE_FREE
    var movecapture = ' '

    val rightCastleOld = chessBoard.chessboard(RIGHT_CASTLE_IDX)

    if (0 == (move.type1 & 0xc)) { //no castle
      val posTo = move.to
      val posFrom = move.from
      movecapture = move.capturedPiece

      assert(posFrom >= 0 && posFrom < 64, posFrom.toInt)
      assert(posTo >= 0 && posTo < 64, posTo.toInt)
      pieceFrom = move.pieceFrom
      if ((move.type1 & 0x3) == PROMOTION_MOVE_MASK) {
        chessBoard.chessboard(pieceFrom) &= NOTPOW2(posFrom)
        chessBoard.updateZobristKey(pieceFrom, posFrom)
        assert(move.promotionPiece >= 0)
        chessBoard.chessboard(move.promotionPiece) |= POW2(posTo)
        chessBoard.updateZobristKey(move.promotionPiece, posTo)
      } else {
        chessBoard.chessboard(pieceFrom) = (chessBoard.chessboard(pieceFrom) | POW2(
          posTo)) & NOTPOW2(posFrom)
        chessBoard.updateZobristKey(pieceFrom, posFrom)
        chessBoard.updateZobristKey(pieceFrom, posTo)
      }
      if (movecapture != SQUARE_FREE) {
        if ((move.type1 & 0x3) != ENPASSANT_MOVE_MASK) {
          chessBoard.chessboard(movecapture) &= NOTPOW2(posTo)
          chessBoard.updateZobristKey(movecapture, posTo)
        } else { //en passant
          assert(movecapture == (move.side ^ 1))
          if (move.side != 0) {
            chessBoard.chessboard(movecapture) &= NOTPOW2(posTo - 8)
            chessBoard.updateZobristKey(movecapture, posTo - 8)
          } else {
            chessBoard.chessboard(movecapture) &= NOTPOW2(posTo + 8)
            chessBoard.updateZobristKey(movecapture, posTo + 8)
          }
        }
      }
      //lost castle right
      pieceFrom match {
        case KING_WHITE =>
          chessBoard.chessboard(RIGHT_CASTLE_IDX) &= 0xcf

        case KING_BLACK =>
          chessBoard.chessboard(RIGHT_CASTLE_IDX) &= 0x3f

        case ROOK_WHITE =>
          if (posFrom == 0) {
            chessBoard.chessboard(RIGHT_CASTLE_IDX) &= 0xef
          } else if (posFrom == 7) {
            chessBoard.chessboard(RIGHT_CASTLE_IDX) &= 0xdf
          }

        case ROOK_BLACK =>
          if (posFrom == 56) {
            chessBoard.chessboard(RIGHT_CASTLE_IDX) &= 0xbf
          } else if (posFrom == 63) {
            chessBoard.chessboard(RIGHT_CASTLE_IDX) &= 0x7f
          }

        //en passant
        case PAWN_WHITE =>
          if (((RANK_1 & POW2(posFrom)) != 0) && ((RANK_3 & POW2(posTo)) != 0)) {
            chessBoard.chessboard(ENPASSANT_IDX) = posTo
            chessBoard.updateZobristKey(
              13,
              chessBoard.chessboard(ENPASSANT_IDX).toInt)
          }

        case PAWN_BLACK =>
          if (((RANK_6 & POW2(posFrom)) != 0) && ((RANK_4 & POW2(posTo)) != 0)) {
            chessBoard.chessboard(ENPASSANT_IDX) = posTo
            chessBoard.updateZobristKey(
              13,
              chessBoard.chessboard(ENPASSANT_IDX).toInt)
          }
        case _ =>
      }
    } else { //castle
      performCastle(move.side, move.type1)
      if (move.side == WHITE) {
        chessBoard.chessboard(RIGHT_CASTLE_IDX) &= 0xcf
      } else {
        chessBoard.chessboard(RIGHT_CASTLE_IDX) &= 0x3f
      }
    }
    val x2 = rightCastleOld ^ chessBoard.chessboard(RIGHT_CASTLE_IDX)
    @tailrec
    def go(x2: Long): Unit = if (x2 != 0) {
      val position = BITScanForward(x2)
      chessBoard.updateZobristKey(14, position)
      go(resetLSB(x2))
    }
    go(x2)
    if (rep) {
      if (movecapture != SQUARE_FREE || pieceFrom == WHITE || pieceFrom == BLACK || (move.type1 & 0xc) != 0) {
        pushStackMove(0)
      }
      pushStackMove(chessBoard.chessboard(ZOBRISTKEY_IDX))
    }
    if ((forceCheck || (checkInCheck && !perftMode)) && ((move.side == WHITE && inCheck(
          WHITE)) || (move.side == BLACK && inCheck(BLACK))))
      false
    else
      true
  }

  private def init(): Unit = {
    numMoves = 0
    numMovesq = 0
    //listId = 0
  }

  private def getTotMoves: Long = numMoves + numMovesq

  private def setRepetitionMapCount(i: Int): Unit = repetitionMapCount = i

  private def getAttackers(side: Side,
                           exitOnFirst: Boolean,
                           position: Position,
                           allpieces: Long): Long = {
    assert(position >= 0 && position < 64)
    assert(side == 0 || side == 1)

    ///knight
    var attackers = KNIGHT_MASK(position) & chessBoard.chessboard(
      KNIGHT_BLACK + (side ^ 1))
    if (exitOnFirst && attackers != 0) 1
    else {
      ///king
      attackers |= NEAR_MASK1(position) & chessBoard.chessboard(
        KING_BLACK + (side ^ 1))
      if (exitOnFirst && attackers != 0) 1
      else {
        ///pawn
        attackers |= PAWN_FORK_MASK(side)(position) & chessBoard.chessboard(
          PAWN_BLACK + (side ^ 1))
        if (exitOnFirst && attackers != 0) 1
        else {
          ///bishop queen
          var enemies = chessBoard.chessboard(BISHOP_BLACK + (side ^ 1)) | chessBoard
            .chessboard(QUEEN_BLACK + (side ^ 1))

          @tailrec
          def w2(nuovo: Long): Int =
            if (nuovo == 0) 0
            else {
              val bound = BITScanForward(nuovo)
              attackers |= POW2(bound)
              if (exitOnFirst && attackers != 0) 1
              else
                w2(resetLSB(nuovo))
            }

          w(
            chessBoard.bitboards
              .getDiagonalAntiDiagonal(position, allpieces) & enemies)
          enemies = chessBoard.chessboard(ROOK_BLACK + (side ^ 1)) | chessBoard
            .chessboard(QUEEN_BLACK + (side ^ 1))

          @tailrec
          def w(nuovo: Long): Int =
            if (nuovo == 0) 0
            else {
              val bound = BITScanForward(nuovo)
              attackers |= POW2(bound)
              if (exitOnFirst && attackers != 0) 1
              else
                w(resetLSB(nuovo))
            }
          if (w(chessBoard.bitboards.getRankFile(position, allpieces) & enemies) == 1)
            1
          else
            attackers
        }
      }
    }
  }

  def display(): Unit = chessBoard.display()
}
