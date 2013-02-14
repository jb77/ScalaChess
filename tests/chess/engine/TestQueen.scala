package chess.engine

import org.junit._
import org.junit.Assert._

class TestQueen {

  @Test
  def testAvailableMoves = {
    val w = Queen(White, "D1")
    assertTrue(w.availableMoves.size == 14+7)
    
    val b = Queen(Black, "D8")
    assertTrue(b.availableMoves.size == 14+7)  
  }

  @Test
  def testMove = {
    val w = Queen(White, "D1")
    val movedW = w.move("B3")
    assertTrue(movedW.position == Position('B', 3)) // compiler won't be able to infer the need for the implicit conversion on the RHS
  }

  @Test(expected = classOf[Exception])
  def testInvalidMove = {
    val w = Queen(White, "E1")
    val movedW = w.move("H2")
  }


}