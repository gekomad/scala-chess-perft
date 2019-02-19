package com.github.gekomad.chessgengenerator.perft

import com.github.gekomad.chessgengenerator.core.{ChessBoard, GenMoves}

object Perft {

  import com.github.gekomad.chessgengenerator.board.Board._
  import com.github.gekomad.chessgengenerator.core.ChessBoard._

  private def calculate(idListPre: Int,
                        g: GenMoves,
                        side: Int,
                        useHash: Boolean,
                        smp: Boolean,
                        depth: Int): Long = {
    assert(side == 0 || side == 1)
    if (depth == 0) 1
    else {

      val idList = idListPre + 1
      g.generate(idList, side)
        .map { move =>
          val keyold = g.zobristKey
          g.makemove(move, rep = false, checkInCheck = false)
          val nPerft = calculate(idList, g, side ^ 1, useHash, smp, depth - 1)
          g.takeback(move, keyold, rep = false)
          nPerft
        }
        .sum
    }
  }

  def perft(fen: String, depth: Int, printPartialResult: Boolean = false)
    : Option[Seq[(String, BitmapPosition)]] =
    GenMoves(fen, true).flatMap { g =>
      g.display()

      val side = g.getSide
      val idList = 0
      val fhash = false
      val smp = false

      val res = g.generate(idList, side).map { move =>
        val keyold = g.zobristKey
        g.makemove(move, rep = false, checkInCheck = false)

        val nPerft =
          calculate(idList, g, side ^ 1, fhash, smp, depth = depth - 1)

        g.takeback(move, keyold, false)

        val h =
          if (ChessBoard
                .decodeBoardinv(move.type1, move.to, side)
                .length() > 2) {
            //castle
            ChessBoard.decodeBoardinv(move.type1, move.to, side)

          } else {
            ChessBoard
              .decodeBoardinv(move.type1, move.from, side) + (if (move.capturedPiece != SQUARE_FREE)
                                                                '*'
                                                              else '-') +
              ChessBoard.decodeBoardinv(move.type1, move.to, side)
          }
        if (printPartialResult) println(h + " " + nPerft)
        (h, nPerft)

      }

      Some(res)
    }

}
