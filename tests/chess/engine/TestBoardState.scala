package chess.engine

import org.junit.Test
import org.junit.Assert._

class TestBoardState {

  @Test
  def testPlayGame = {
    val bs = BoardState.startingBoard

    val bsAfterMoves = bs
      .move("G1", "F3")
      .move("G8", "F6")
      .move("C2", "C4")
      .move("G7", "G6")
      .move("B1", "C3")
      .move("F8", "G7")
      .move("D2", "D4")
      .move("A7", "A6")
      .move("C1", "E3")
      .move("B7", "B6")
      .move("D1", "D2")

  }

  @Test
  def testInCheck = {
    val pieces = Set[Piece](
      King(White, "A8"),
      Queen(Black, "A1"))
    val checkGame = new BoardState(pieces, None)
    assertTrue(checkGame.inCheck(White))
    assertFalse(checkGame.inCheck(Black))
  }
  
  @Test
  def testCheckMate = {
    val pieces = Set[Piece](
      King(White, "A8"),
      Pawn(White, "E5"),
      Queen(Black, "A1"),
      Rook(Black, "B1")) 
    val checkGame = new BoardState(pieces, None)
    assertTrue(checkGame.isCheckmated(White))
    assertFalse(checkGame.isCheckmated(Black))
  }  

  @Test
  def testCantMoveIntoCheck = {
    val pieces = Set[Piece](
      King(White, "A8"),
      Queen(Black, "B6"))
    val checkGame = new BoardState(pieces, None)
    assertTrue(checkGame.legalMoves(King(White, "A8")).isEmpty)
  }

  @Test
  def testCantCaptureKing = {
    val pieces = Set[Piece](
      King(White, "A8"),
      Queen(Black, "A1"))
    val board = new BoardState(pieces, None)
    val lmv=board.legalMoves(Queen(Black, "A1"))
    assertFalse( lmv.contains("A8") )
  }

}