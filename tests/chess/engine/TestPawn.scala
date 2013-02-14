package chess.engine

import org.junit._
import org.junit.Assert._

class TestPawn {

  @Test
  def testPawnAvailableMoves = {

    val p = Pawn(White, "E2")
    assertTrue(p.availableMoves.size == 4)

    val alreadyMovedPawn = Pawn(White, "E3")
    assertTrue(alreadyMovedPawn.availableMoves.size == 3)

    val bp = Pawn(Black, "E7")
    assertTrue(bp.availableMoves.size == 4)

    val alreadyMovedBlackPawn = Pawn(Black, "E6")
    assertTrue(alreadyMovedBlackPawn.availableMoves.size == 3)
  }

  @Test
  def testPawnMove = {
    val p = Pawn(White, "E2")
    val movedP = p.move("E4")
    assertTrue(movedP.position == Position('E', 4)) // compiler won't be able to infer the need for the implicit conversion on the RHS
  }

  @Test(expected = classOf[Exception])
  def testInvalidPawnMove = {

    val p = Pawn(White, "E2")
    val movedP = p.move("D4")
  }

  @Test
  def testMoveOffBoard = {
    val w = Pawn(White, "E8")
    assert(w.availableMoves.isEmpty)

    val b = Pawn(Black, "E1")
    assert(b.availableMoves.isEmpty)
  }

}