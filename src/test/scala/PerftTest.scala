import com.github.gekomad.chessgengenerator.perft.Perft._
import org.scalatest.funsuite.AnyFunSuite
import com.github.gekomad.chessgengenerator.core.ChessBoard.BitmapPosition
import com.github.gekomad.chessgengenerator.board.Board._
import com.github.gekomad.chessgengenerator.util.PrintAndSum.sumAndPrint

class PerftTest extends AnyFunSuite {

  test("perft 1") {
    val l = perft(STARTPOS, 1)
    assert(sumAndPrint(l).getOrElse(-1) == 20)
  }

  test("perft 2") {
    val l = perft(STARTPOS, 2)
    assert(sumAndPrint(l).getOrElse(-1) == 400)
  }

  test("perft 3") {

    val l = perft(STARTPOS, 3)

    assert(sumAndPrint(l).getOrElse(-1) == 8902)
  }

  test("perft 4") {

    val l = perft(STARTPOS, 4)
    assert(sumAndPrint(l).getOrElse(-1) == 197281)
  }

  test("perft 5") {

    val l = perft(STARTPOS, 5)
    assert(sumAndPrint(l).getOrElse(-1) == 4865609)
  }

  test("perft 6") {

    val l = perft(STARTPOS, 6)
    assert(sumAndPrint(l).getOrElse(-1) == 119060324)
  }

  test("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 - 3") {

    val l = perft("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 3)
    assert(sumAndPrint(l).getOrElse(-1) == 97862)
  }

  test("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 - 5") {

    val l = perft("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 5)
    assert(sumAndPrint(l).getOrElse(-1) == 193690690)
  }

  test("8/PPP4k/8/8/8/8/4Kppp/8 w - - 0 1 - 1") {

    val l = perft("8/PPP4k/8/8/8/8/4Kppp/8 w - - 0 1", 1)
    assert(sumAndPrint(l).getOrElse(-1) == 18)
  }

  test("8/PPP4k/8/8/8/8/4Kppp/8 w - - 0 1 - 2") {

    val l = perft("8/PPP4k/8/8/8/8/4Kppp/8 w - - 0 1", 2)
    assert(sumAndPrint(l).getOrElse(-1) == 290)
  }

  test("8/PPP4k/8/8/8/8/4Kppp/8 w - - 0 1 - 5") {

    val l = perft("8/PPP4k/8/8/8/8/4Kppp/8 w - - 0 1", 5)
    assert(sumAndPrint(l).getOrElse(-1) == 1745545)
  }

  test("8/PPP4k/8/8/8/8/4Kp2/6qq w - - 0 1 - 1") {

    val l = perft("8/PPP4k/8/8/8/8/4Kp2/6qq w - - 0 1", 1)
    assert(sumAndPrint(l).getOrElse(-1) == 15)
  }

}
