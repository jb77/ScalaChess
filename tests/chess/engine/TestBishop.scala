package chess.engine

import org.junit._
import org.junit.Assert._

class TestBishop {

  @Test
  def testAvailableMoves = {
    val w = Bishop(White, "C1")
    assertTrue(w.availableMoves.size == 7)
    
    val b = Bishop(Black, "C8")
    assertTrue(b.availableMoves.size == 7)  
  }

  @Test
  def testMove = {
    val w = Bishop(White, "A3")
    val movedW = w.move("C5")
    assertTrue(movedW.position == Position('C', 5)) // compiler won't be able to infer the need for the implicit conversion on the RHS
  }

  @Test(expected = classOf[Exception])
  def testInvalidMove = {
    val w = Bishop(White, "A3")
    val movedW = w.move("C6")
  }


}