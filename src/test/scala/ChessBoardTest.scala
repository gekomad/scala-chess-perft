import com.github.gekomad.chessgengenerator.board.Board._
import com.github.gekomad.chessgengenerator.core.ChessBoard
import org.scalatest.FunSuite

class ChessBoardTest extends FunSuite {

  test("ChessBoard") {
    val c = ChessBoard()

    c.map { ch =>
        ch.display()
        assert(ch.getBitmapNoPawns(WHITE) == 0xffL)
        assert(ch.getNpiecesNoPawnNoKing(WHITE) == 7)
        assert(ch.getBitmap(WHITE) == 0xffffl)
        assert(ch.getBitmap(BLACK) == 0xffff000000000000l)
        assert(ch.getPieceAt(WHITE, 1l) == 3)
        assert(ch.getSide == WHITE)
        assert(ch.boardToFen == "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

      }
      .getOrElse(assert(false))
  }

  test("ChessBoard 2") {
    val c = ChessBoard("8/PPP4k/8/8/8/8/4Kppp/8 b - - 0 1")

    c.map { ch =>
        ch.display()
        assert(ch.getBitmapNoPawns(WHITE) == 0x800L)
        assert(ch.getNpiecesNoPawnNoKing(WHITE) == 0)
        assert(ch.getBitmap(WHITE) == 0xe0000000000800l)
        assert(ch.getBitmap(BLACK) == 0x1000000000700l)
        assert(ch.getPieceAt(WHITE, 1l) == 12)
        assert(ch.getSide == BLACK)
        assert(ch.boardToFen == "8/PPP4k/8/8/8/8/4Kppp/8 b - - 0 1")

      }
      .getOrElse(assert(false))
  }
}
