import com.github.gekomad.chessgengenerator.board.Board._
import com.github.gekomad.chessgengenerator.core.GenMoves
import com.github.gekomad.chessgengenerator.core.GenMoves.{_Tmove, _TmoveP}
import org.scalatest.funsuite.AnyFunSuite

class GenMovesTest extends AnyFunSuite {

  case class _TmoveTest(
    promotionPiece: Int,
    pieceFrom: Int,
    capturedPiece: Int,
    from: Int,
    to: Int,
    side: Int,
    type1: Int,
    score: Int
  )

  object _TmoveTest {
    def apply(m: _Tmove): _TmoveTest =
      new _TmoveTest(m.promotionPiece, m.pieceFrom, m.capturedPiece, m.from, m.to, m.side, m.type1, m.score)
  }

  test("8/PPP4k/8/8/8/8/4Kppp/8 w - - 0 1") {
    val idList = 0
    val g      = GenMoves("8/PPP4k/8/8/8/8/4Kppp/8 w - - 0 1", true).getOrElse(???)
    g.display()
    assert(g.getListSize(idList) == 0)
    val friends = g.chessBoard.getBitmap(WHITE)
    val enemies = g.chessBoard.getBitmap(BLACK)
    g.generateCaptures(idList, WHITE, enemies, friends)
    assert(g.getListSize(idList) == 1)
    g.generateMoves(idList, WHITE, friends | enemies)

    assert(g.getListSize(idList) == 18)

    val moveP: _TmoveP      = g.genList(0)
    val list: Array[_Tmove] = moveP.moveList
    val listcount: Int      = moveP.size

    val o = (0 until listcount).map { i =>
      val o: _Tmove = list(i)
      println(o)
      _TmoveTest(o)
    }

    val res = List(
      _TmoveTest(65535, 9, 0, 11, 10, 1, 3, 0),
      _TmoveTest(11, 1, 12, 53, 61, 1, 2, 0),
      _TmoveTest(7, 1, 12, 53, 61, 1, 2, 0),
      _TmoveTest(5, 1, 12, 53, 61, 1, 2, 0),
      _TmoveTest(3, 1, 12, 53, 61, 1, 2, 0),
      _TmoveTest(11, 1, 12, 54, 62, 1, 2, 0),
      _TmoveTest(7, 1, 12, 54, 62, 1, 2, 0),
      _TmoveTest(5, 1, 12, 54, 62, 1, 2, 0),
      _TmoveTest(3, 1, 12, 54, 62, 1, 2, 0),
      _TmoveTest(11, 1, 12, 55, 63, 1, 2, 0),
      _TmoveTest(7, 1, 12, 55, 63, 1, 2, 0),
      _TmoveTest(5, 1, 12, 55, 63, 1, 2, 0),
      _TmoveTest(3, 1, 12, 55, 63, 1, 2, 0),
      _TmoveTest(65535, 9, 12, 11, 4, 1, 3, 0),
      _TmoveTest(65535, 9, 12, 11, 12, 1, 3, 0),
      _TmoveTest(65535, 9, 12, 11, 18, 1, 3, 0),
      _TmoveTest(65535, 9, 12, 11, 19, 1, 3, 0),
      _TmoveTest(65535, 9, 12, 11, 20, 1, 3, 0)
    )

    assert(o.toList == res)

  }

  test("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1") {

    val g: GenMoves =
      GenMoves("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", true).getOrElse(???)
    val idList  = 0
    val friends = g.chessBoard.getBitmap(WHITE)
    val enemies = g.chessBoard.getBitmap(BLACK)
    g.generateCaptures(idList, WHITE, enemies, friends)
    assert(g.getListSize(idList) == 8)
    g.generateMoves(idList, WHITE, friends | enemies)

    assert(g.getListSize(idList) == 48)

    val moveP: _TmoveP      = g.genList(0)
    val list: Array[_Tmove] = moveP.moveList
    val listcount: Int      = moveP.size

    val o = (0 until listcount).map { i =>
      val o = list(i)
      println(o)
      _TmoveTest(o)
    }

    val res = List(
      _TmoveTest(65535, 1, 0, 9, 16, 1, 243, 0),
      _TmoveTest(65535, 1, 0, 36, 43, 1, 243, 0),
      _TmoveTest(65535, 7, 0, 35, 41, 1, 243, 0),
      _TmoveTest(65535, 7, 0, 35, 50, 1, 243, 0),
      _TmoveTest(65535, 7, 0, 35, 52, 1, 243, 0),
      _TmoveTest(65535, 5, 4, 11, 47, 1, 243, 0),
      _TmoveTest(65535, 11, 0, 18, 16, 1, 243, 0),
      _TmoveTest(65535, 11, 6, 18, 42, 1, 243, 0),
      _TmoveTest(0, 0, 12, 0, 0, 1, 244, 100),
      _TmoveTest(0, 0, 12, 0, 0, 1, 248, 100),
      _TmoveTest(65535, 5, 12, 11, 2, 1, 243, 0),
      _TmoveTest(65535, 5, 12, 11, 4, 1, 243, 0),
      _TmoveTest(65535, 5, 12, 11, 20, 1, 243, 0),
      _TmoveTest(65535, 5, 12, 11, 29, 1, 243, 0),
      _TmoveTest(65535, 5, 12, 11, 38, 1, 243, 0),
      _TmoveTest(65535, 5, 12, 12, 5, 1, 243, 0),
      _TmoveTest(65535, 5, 12, 12, 19, 1, 243, 0),
      _TmoveTest(65535, 5, 12, 12, 26, 1, 243, 0),
      _TmoveTest(65535, 5, 12, 12, 33, 1, 243, 0),
      _TmoveTest(65535, 5, 12, 12, 40, 1, 243, 0),
      _TmoveTest(65535, 3, 12, 0, 1, 1, 243, 0),
      _TmoveTest(65535, 3, 12, 0, 2, 1, 243, 0),
      _TmoveTest(65535, 3, 12, 7, 4, 1, 243, 0),
      _TmoveTest(65535, 3, 12, 7, 5, 1, 243, 0),
      _TmoveTest(65535, 3, 12, 7, 6, 1, 243, 0),
      _TmoveTest(65535, 11, 12, 18, 17, 1, 243, 0),
      _TmoveTest(65535, 11, 12, 18, 19, 1, 243, 0),
      _TmoveTest(65535, 11, 12, 18, 20, 1, 243, 0),
      _TmoveTest(65535, 11, 12, 18, 26, 1, 243, 0),
      _TmoveTest(65535, 11, 12, 18, 34, 1, 243, 0),
      _TmoveTest(65535, 11, 12, 18, 25, 1, 243, 0),
      _TmoveTest(65535, 11, 12, 18, 32, 1, 243, 0),
      _TmoveTest(65535, 1, 12, 9, 25, 1, 243, 0),
      _TmoveTest(65535, 1, 12, 15, 31, 1, 243, 0),
      _TmoveTest(65535, 1, 12, 9, 17, 1, 243, 0),
      _TmoveTest(65535, 1, 12, 14, 22, 1, 243, 0),
      _TmoveTest(65535, 1, 12, 15, 23, 1, 243, 0),
      _TmoveTest(65535, 1, 12, 36, 44, 1, 243, 0),
      _TmoveTest(65535, 7, 12, 21, 4, 1, 243, 0),
      _TmoveTest(65535, 7, 12, 21, 6, 1, 243, 0),
      _TmoveTest(65535, 7, 12, 21, 31, 1, 243, 0),
      _TmoveTest(65535, 7, 12, 21, 38, 1, 243, 0),
      _TmoveTest(65535, 7, 12, 35, 20, 1, 243, 0),
      _TmoveTest(65535, 7, 12, 35, 25, 1, 243, 0),
      _TmoveTest(65535, 7, 12, 35, 29, 1, 243, 0),
      _TmoveTest(65535, 7, 12, 35, 45, 1, 243, 0),
      _TmoveTest(65535, 9, 12, 3, 2, 1, 243, 0),
      _TmoveTest(65535, 9, 12, 3, 4, 1, 243, 0)
    )

    assert(o.toList == res)

  }
}
