package chess.engine

import org.junit._
import org.junit.Assert._

class TestGame {

  @Test
  def testInit: Unit = {
    val g = Game()
    Assert.assertTrue(g.nextToMove == White)
  }

  @Test
  def testMove: Unit = {
    val g = Game()
    val g2 = g.move("E2", "E4")
    assertTrue(g2.nextToMove == Black)

    val g3 = g2.move("E7", "E5")
    Assert.assertTrue(g3.nextToMove == White)
    val movedWhitePawn = g3.currentBoard.pieces.find(piece => (piece.position.rank == 4 && piece.position.file == 'E'))
    val movedBlackPawn = g3.currentBoard.pieces.find(piece => (piece.position.rank == 5 && piece.position.file == 'E'))
    assertTrue(movedWhitePawn.isDefined)
    assertTrue(movedBlackPawn.isDefined)
  }

  @Test(expected = classOf[Exception])
  def testMoveInWrongOrder = {
    val g = Game()
    val g2 = g.move("E7", "E5")
  }

  @Test
  def testSaveAndLoad = {
    val g = Game()
    val g2 = g.move("E2", "E4")

    g2.save("testSave.dat");

    val loadedGame = Game.load("testSave.dat");
    assertTrue(loadedGame.nextToMove == Black)
    assertTrue(loadedGame.history.size == 2) // starting board + board after first move
    assertTrue(loadedGame.currentBoard.pieces.find(piece => (piece.position.rank == 4 && piece.position.file == 'E')).isDefined)
  }

  @Test
  def testCastling = {
    val g = Game()
    val g2 = g
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

    val g3 = g2.castle(Black, Kingside)
    val g4 = g3.castle(White, Queenside)

    assertTrue(g3.currentBoard.pieceAt("G8").get.isInstanceOf[King])
    assertTrue(g3.currentBoard.pieceAt("F8").get.isInstanceOf[Rook])

    assertTrue(g4.currentBoard.pieceAt("C1").get.isInstanceOf[King])
    assertTrue(g4.currentBoard.pieceAt("D1").get.isInstanceOf[Rook])

  }

  @Test
  def testAvailableMoves = {
    val g = Game()
    val moves = g.availableMoves
    assertTrue(moves.size == 20)
  }

  @Test
  def testSuggestions = {
    val pieces = Set[Piece](
      King(White, "A8"),
      Pawn(White, "B6"),
      Rook(Black, "B5"))
    val eBs = new BoardState(pieces, None)
    val eg = new Game(Black, List[BoardState](eBs))
    assertEquals( (Position('B',5),Position('B',6)), eg.suggestMove())
  }
  
  @Test
  def testCheckMateSuggestions = {
    val pieces = Set[Piece](
      King(White, "A8"),
      Rook(Black, "C1"),
      Rook(Black, "B5"))
    val eBs = new BoardState(pieces, None)
    val eg = new Game(Black, List[BoardState](eBs))
    assertEquals( (Position('C',1),Position('A',1)), eg.suggestMove())
  }  
  
  

}