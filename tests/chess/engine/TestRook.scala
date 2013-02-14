package chess.engine

import org.junit._
import org.junit.Assert._

class TestRook {

  @Test
  def testAvailableMoves = {
    val w = Rook(White, "A1")
    assertTrue(w.availableMoves.size == 14)
    
    val b = Rook(Black, "A8")
    assertTrue(b.availableMoves.size == 14)  
  }

  @Test
  def testMove = {
    val w = Rook(White, "C2")
    val movedW = w.move("C5")
    assertTrue(movedW.position == Position('C', 5)) // compiler won't be able to infer the need for the implicit conversion on the RHS
  }

  @Test(expected = classOf[Exception])
  def testInvalidMove = {
    val w = Rook(White, "A3")
    val movedW = w.move("C6")
  }


}