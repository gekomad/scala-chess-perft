package com.github.gekomad.chessgengenerator.core

import Def._
import com.github.gekomad.chessgengenerator.board.Board._
import ChessBoard.{BitmapPosition, Position, Side}
import com.github.gekomad.chessgengenerator.board.Bitboards

import scala.annotation.tailrec

case class ChessBoard private () {

  val bitboards                         = Bitboards()
  val chessboard: Array[BitmapPosition] = Array.ofDim[Long](16)

  def getBitmapNoPawns(side: Side): Long =
    chessboard(ROOK_BLACK + side) | chessboard(BISHOP_BLACK + side) | chessboard(KNIGHT_BLACK + side) | chessboard(
      KING_BLACK + side
    ) | chessboard(QUEEN_BLACK + side)

  def zobristKey: BitmapPosition = chessboard(ZOBRISTKEY_IDX)

  def getNpiecesNoPawnNoKing(side: Side): Int = bitCount(
    chessboard(ROOK_BLACK + side) | chessboard(BISHOP_BLACK + side) | chessboard(KNIGHT_BLACK + side) | chessboard(
      QUEEN_BLACK + side
    )
  )

  def getBitmap(side: Side): Long =
    chessboard(PAWN_BLACK + side) | chessboard(ROOK_BLACK + side) | chessboard(BISHOP_BLACK + side) | chessboard(
      KNIGHT_BLACK + side
    ) | chessboard(KING_BLACK + side) | chessboard(QUEEN_BLACK + side)

  def getPieceAt(side: Side, bitmapPos: BitmapPosition): Int =
    if ((bitmapPos & chessboard(PAWN_BLACK + side)) != 0) PAWN_BLACK + side
    else if ((bitmapPos & chessboard(ROOK_BLACK + side)) != 0) ROOK_BLACK + side
    else if ((bitmapPos & chessboard(BISHOP_BLACK + side)) != 0) BISHOP_BLACK + side
    else if ((bitmapPos & chessboard(KNIGHT_BLACK + side)) != 0) KNIGHT_BLACK + side
    else if ((bitmapPos & chessboard(QUEEN_BLACK + side)) != 0) QUEEN_BLACK + side
    else if ((bitmapPos & chessboard(KING_BLACK + side)) != 0) KING_BLACK + side
    else SQUARE_FREE

  def updateZobristKey(piece: Int, position: Position): Unit =
    chessboard(ZOBRISTKEY_IDX) ^= Random.RANDOM_KEY(piece)(position)

  private def loadFen(fen: String): Option[Side] = {

    val iss       = fen.split(" ")
    val pos       = iss(0)
    val side      = iss(1)
    val castle    = iss(2)
    val enpassant = iss(3)
    var ix        = 0
    val s         = Array.ofDim[Int](64)

    pos.foreach { ch =>
      if (ch != '/') {
        if (INV_FEN(ch) != 0xFF) {
          s(ix) = INV_FEN(ch)
          ix += 1
        } else if (ch > 47 && ch < 58) { // 0 1 2 3 4 5 6 7 8 9
          (0 until ch - 48).foreach { _ =>
            s(ix) = SQUARE_FREE
            ix += 1
          }
        } else {
          println("err1")
          None
        }
      }
    }
    if (ix != 64) {
      println("err2")
      None
    }
    if (side == "b")
      chessboard(SIDETOMOVE_IDX) = BLACK
    else if (side == "w")
      chessboard(SIDETOMOVE_IDX) = WHITE
    else {
      println("err3")
      None
    }
    (0 until 64).foreach { i =>
      val p = s(63 - i)
      if (p != SQUARE_FREE) {
        updateZobristKey(p, i)
        chessboard(p) |= POW2(i)
      } else {
        chessboard(p) &= NOTPOW2(i)
      }
    }
    (0 until castle.length).foreach { e =>
      castle(e) match {
        case 'K' =>
          updateZobristKey(RIGHT_CASTLE_IDX, 4)
          assert(BITScanForward(if (4 == RIGHT_KING_CASTLE_WHITE_MASK) 1 else 0) != 0)
          chessboard(RIGHT_CASTLE_IDX) |= RIGHT_KING_CASTLE_WHITE_MASK

        case 'k' =>
          updateZobristKey(RIGHT_CASTLE_IDX, 6)

          assert(BITScanForward(if (6 == RIGHT_KING_CASTLE_BLACK_MASK) 1 else 0) != 0)
          chessboard(RIGHT_CASTLE_IDX) |= RIGHT_KING_CASTLE_BLACK_MASK

        case 'Q' =>
          updateZobristKey(RIGHT_CASTLE_IDX, 5)
          assert(BITScanForward(if (5 == RIGHT_QUEEN_CASTLE_WHITE_MASK) 1 else 0) != 0)

          chessboard(RIGHT_CASTLE_IDX) |= RIGHT_QUEEN_CASTLE_WHITE_MASK

        case 'q' =>
          updateZobristKey(RIGHT_CASTLE_IDX, 7)
          assert(BITScanForward(if (7 == RIGHT_QUEEN_CASTLE_BLACK_MASK) 1 else 0) != 0)

          chessboard(RIGHT_CASTLE_IDX) |= RIGHT_QUEEN_CASTLE_BLACK_MASK

        case _ =>
      }
    }
    chessboard(ENPASSANT_IDX) = NO_ENPASSANT
    (0 until 64).find(enpassant == BOARD(_)).foreach { i =>
      chessboard(ENPASSANT_IDX) = i
      if (chessboard(SIDETOMOVE_IDX) != 0) {
        chessboard(ENPASSANT_IDX) -= 8
      } else {
        chessboard(ENPASSANT_IDX) += 8
      }
      updateZobristKey(ENPASSANT_IDX, chessboard(ENPASSANT_IDX).toInt)
    }
    Some(chessboard(SIDETOMOVE_IDX).toInt)
  }

  def display(): Unit = {

    print("\n     a   b   c   d   e   f   g   h")
    (0 until 64).foreach { t =>
      var x = ' '
      if (t % 8 == 0) {
        print("\n   ----+---+---+---+---+---+---+----\n")
        print(s"${8 - RANK_AT(t)} | ")
      }
      val o = FEN_PIECE(getPieceAt(WHITE, POW2(63 - t)))

      var k = ' '
      if (o != '-') { k = o } else {
        k = FEN_PIECE(getPieceAt(BLACK, POW2(63 - t)))
      }
      if (k == '-') { x = ' ' } else { x = k }

      if (x != ' ') { print(x) } else {
        if ((POW2(t) & WHITE_SQUARES) != 0) { print(" ") } else { print(".") }
      }
      print(" | ")
    }
    println("\n   ----+---+---+---+---+---+---+----")
    println("     a   b   c   d   e   f   g   h\n\n")
    println(s"fen: $boardToFen")

    println(s"zobrist key: ${chessboard(ZOBRISTKEY_IDX)}")
    println(
      "en passant position: " + (if (chessboard(ENPASSANT_IDX) == NO_ENPASSANT)
                                   "None"
                                 else chessboard(ENPASSANT_IDX))
    )
    println(s"right castle: ${chessboard(RIGHT_CASTLE_IDX)}")

    println("side to move: " + ChessBoard.decodeSide(chessboard(SIDETOMOVE_IDX)))
  }

  def getSide: Side = chessboard(SIDETOMOVE_IDX).toInt

  private def makeZobristKey(): Unit = {
    chessboard(ZOBRISTKEY_IDX) = 0
    (0 until 12).foreach { u =>
      @tailrec
      def go(c: Long): Unit = if (c != 0) {
        val position: Position = BITScanForward(c)
        updateZobristKey(u, position)
        go(resetLSB(c))
      }
      go(chessboard(u))
    }
    if (chessboard(ENPASSANT_IDX) != NO_ENPASSANT) {
      updateZobristKey(ENPASSANT_IDX, chessboard(ENPASSANT_IDX).toInt)
    }

    @tailrec
    def go2(x2: Long): Unit = if (x2 != 0) {
      val position: Position = BITScanForward(x2)
      updateZobristKey(14, position)
      go2(resetLSB(x2))
    }
    go2(chessboard(RIGHT_CASTLE_IDX))

  }

  def boardToFen: String = {
    var fen: String = ""
    (0 until 8).foreach { y =>
      var l   = 0
      var row = ""
      (0 until 8).foreach { x =>
        var q = getPieceAt(BLACK, POW2(63 - ((y * 8) + x)))
        if (q == SQUARE_FREE) {
          q = getPieceAt(WHITE, POW2(63 - ((y * 8) + x)))
        }
        if (q == SQUARE_FREE) {
          l = l + 1
        } else {
          if (l > 0) {
            row = row + (l + 48).toChar
          }
          l = 0
          row = row + FEN_PIECE(q)
        }
      }
      if (l > 0) {
        row = row + (l + 48).toChar
      }
      fen = fen + row
      if (y < 7) {
        fen = fen + "/"
      }
    }
    if (chessboard(SIDETOMOVE_IDX) == BLACK) {
      fen += " b "
    } else {
      fen += " w "
    }
    var cst = 0
    if ((chessboard(RIGHT_CASTLE_IDX) & RIGHT_KING_CASTLE_WHITE_MASK) != 0) {
      fen += "K"
      cst += 1
    }
    if ((chessboard(RIGHT_CASTLE_IDX) & RIGHT_QUEEN_CASTLE_WHITE_MASK) != 0) {
      fen += "Q"
      cst += 1
    }
    if ((chessboard(RIGHT_CASTLE_IDX) & RIGHT_KING_CASTLE_BLACK_MASK) != 0) {
      fen += "k"
      cst += 1
    }
    if ((chessboard(RIGHT_CASTLE_IDX) & RIGHT_QUEEN_CASTLE_BLACK_MASK) != 0) {
      fen += "q"
      cst += 1
    }
    if (cst == 0) {
      fen += "-"
    }
    if (chessboard(ENPASSANT_IDX) == NO_ENPASSANT) {
      fen += " -"
    } else {
      fen += " "
      fen += (if (chessboard(SIDETOMOVE_IDX) != 0)
                BOARD(chessboard(ENPASSANT_IDX).toInt + 8)
              else BOARD(chessboard(ENPASSANT_IDX).toInt - 8))

    }
    fen += " 0 1"
    fen
  }
}

object ChessBoard {
  def decodeSide(side: Long): String = if (side == 0) "Black" else "White"

  def apply(fen: String): Option[ChessBoard] = {
    val o = new ChessBoard()
    o.loadFen(fen)
      .fold {
        println(s"Bad FEN position format $fen")
        None: Option[ChessBoard]
      } { _ =>
        o.makeZobristKey()
        Some(o)
      }
  }

  def apply(): Option[ChessBoard] = ChessBoard(STARTPOS)

  type Side           = Int
  type Position       = Int
  type BitmapPosition = Long

  private def getPieceByChar(c: Char): Option[Position] = (0 until 12).find(c == FEN_PIECE(_))

  private def decodeBoard(a: String): Option[Char] = (0 until 64).find(i => 0 == a.compare(BOARD(i))).map(_.toChar)

  def decodeType(p: Int): String = p & 0xf match {
    case STANDARD_MOVE_MASK          => "STANDARD_MOVE"
    case ENPASSANT_MOVE_MASK         => "ENPASSANT_MOVE"
    case PROMOTION_MOVE_MASK         => "PROMOTION_MOVE"
    case KING_SIDE_CASTLE_MOVE_MASK  => "KING_SIDE_CASTLE_MOVE"
    case QUEEN_SIDE_CASTLE_MOVE_MASK => "QUEEN_SIDE_CASTLE_MOVE"
    case _                           => "?"
  }

  def decodePiece(p: Int): String = p match {
    case PAWN_BLACK   => "PAWN_BLACK  "
    case PAWN_WHITE   => "PAWN_WHITE  "
    case ROOK_BLACK   => "ROOK_BLACK  "
    case ROOK_WHITE   => "ROOK_WHITE  "
    case BISHOP_BLACK => "BISHOP_BLACK"
    case BISHOP_WHITE => "BISHOP_WHITE"
    case KNIGHT_BLACK => "KNIGHT_BLACK"
    case KNIGHT_WHITE => "KNIGHT_WHITE"
    case KING_BLACK   => "KING_BLACK  "
    case KING_WHITE   => "KING_WHITE  "
    case QUEEN_BLACK  => "QUEEN_BLACK "
    case QUEEN_WHITE  => "QUEEN_WHITE "
    case _            => "None"
  }

  def decodeBoardinv(type_ : Char, a: Position, side: Side): String = {
    assert(a >= 0 && a < 64)
    if ((type_ & QUEEN_SIDE_CASTLE_MOVE_MASK) != 0 && side == WHITE)
      "e1c1"
    else if ((type_ & KING_SIDE_CASTLE_MOVE_MASK) != 0 && side == WHITE)
      "e1g1"
    else if ((type_ & QUEEN_SIDE_CASTLE_MOVE_MASK) != 0 && side == BLACK)
      "e8c8"
    else if ((type_ & KING_SIDE_CASTLE_MOVE_MASK) != 0 && side == BLACK)
      "e8g8"
    else BOARD(a)
  }
}
