package chess.engine

import java.io.FileOutputStream
import scala.util.Marshal
import java.io.FileInputStream

sealed trait Player
case object White extends Player
case object Black extends Player

sealed trait Side
case object Kingside extends Side
case object Queenside extends Side

@serializable
class Game(
  val nextToMove: Player,
  val history: List[BoardState]) {

  def currentBoard: BoardState = history.last

  def move(currPos: Position, newPos: Position): Game = {

    val pieceToMove = currentBoard.pieceAt(currPos)
    pieceToMove match {
      case None => throw new Exception("No piece at " + currPos)
      case Some(x) => if (x.player != nextToMove) throw new Exception("Piece at " + currPos + " is the wrong colour");
    }

    val newBoard = currentBoard.move(currPos, newPos)
    val newNextMove = nextToMove match {
      case White => Black
      case Black => White
    }

    new Game(newNextMove, history.:+(newBoard))
  }

  def castle(player: Player, side: Side): Game = {
    if (player != nextToMove) throw new IllegalStateException("Not " + player + "'s turn to move");

    // TODO check neither piece has moved
    // TODO check no pieces in the way 
    val wK = new Position('E', 1)
    val bK = new Position('E', 8)

    val wQsR = new Position('A', 1)
    val wKsR = new Position('H', 1)

    val bQsR = new Position('A', 8)
    val bKsR = new Position('H', 8)

    val newBoard = (player, side) match {
      case (White, Queenside) => currentBoard.move(wK, Piece.whiteQueensideCastlePos).move(wQsR, "D1")
      case (White, Kingside) => currentBoard.move(wK, Piece.whiteKingsideCastlePos).move(wKsR, "F1")
      case (Black, Queenside) => currentBoard.move(bK, Piece.blackQueensideCastlePos).move(bQsR, "D8")
      case (Black, Kingside) => currentBoard.move(bK, Piece.blackKingsideCastlePos).move(bKsR, "F8")
    }

    val newNextMove = nextToMove match {
      case White => Black
      case Black => White
    }

    new Game(newNextMove, history.:+(newBoard))
  }

  def availableMoves: Set[(Piece, Position)] = {
    val piecesForPlayerToMove = currentBoard.pieces.filter(p => p.player == nextToMove)
    val x = for (piece <- piecesForPlayerToMove) yield currentBoard.legalMoves(piece).map(move => (piece, move))
    x.flatten
  }

  @throws(classOf[Exception]) // probably not best way to do this - Option result type better?
  def suggestMove(timeLimit: Long = 5000, levelLimit: Int = 4): (Position, Position) = {
    Analyser.suggestMove(this, timeLimit, levelLimit)
  }
       
  def print = {
    history.foreach(bs => {
      println("move " + history.indexOf(bs) + ": " + bs.moveToReachThisState)
      println("score " + bs.evaluate)
      println(bs.toString)
    })
  }

  def printMoves = {
    history.foreach(bs => {
      println("move " + history.indexOf(bs) + ": " + bs.moveToReachThisState)
      println("score " + bs.evaluate)
    })
  }

  def save(name: String) = {
    val out = new FileOutputStream(name)
    out.write(Marshal.dump(this))
    out.close
  }

  override def toString(): String = {
    history.toString + "\nto play: " + nextToMove
  }

}

object Game {
  def apply() = {
    new Game(White, List(BoardState.startingBoard))
  }

  def load(name: String): Game = {
    val in = new FileInputStream(name)
    val bytes = Stream.continually(in.read).takeWhile(-1 !=).map(_.toByte).toArray
    Marshal.load[Game](bytes)
  }

}

