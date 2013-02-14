package chess.engine

import org.junit._
import org.junit.Assert._

class TestKnight {

  @Test
  def testAvailableMoves = {
    val w = Knight(White, "B1")
    assertTrue(w.availableMoves.size == 3)
    
    val b = Knight(Black, "B8")
    assertTrue(b.availableMoves.size == 3)  
  }

  @Test
  def testMove = {
    val w = Knight(White, "B1")
    val movedW = w.move("A3")
    assertTrue(movedW.position == Position('A', 3)) // compiler won't be able to infer the need for the implicit conversion on the RHS
  }

  @Test(expected = classOf[Exception])
  def testInvalidMove = {
    val w = Knight(White, "B1")
    val movedW = w.move("A6")
  }


}