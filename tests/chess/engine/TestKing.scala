package chess.engine

import org.junit._
import org.junit.Assert._

class TestKing {

  @Test
  def testAvailableMoves = {
    val w = King(White, "E1",false)
    assertTrue(w.availableMoves.size == 7)
    
    val b = King(Black, "E8")
    assertTrue(b.availableMoves.size == 5)  
  }

  @Test
  def testMove = {
    val w = King(White, "E1")
    val movedW = w.move("E2")
    assertTrue(movedW.position == Position('E', 2)) // compiler won't be able to infer the need for the implicit conversion on the RHS
  }

  @Test(expected = classOf[Exception])
  def testInvalidMove = {
    val w = King(White, "E1")
    val movedW = w.move("E3")
  }


}