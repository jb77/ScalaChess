package chess.engine

sealed trait Piece {
  val position: Position
  val player: Player
  val value: Int
  val moved: Boolean

  def move(newPos: Position): Piece

  def availableMoves: List[Position] = { throw new UnsupportedOperationException("TBC") }

  protected def availableMovesAcc(
    p: Position,
    mover: Position => Option[Position],
    acc: List[Position]): List[Position] =
    {
      val nextCandidatePos = mover(p)
      nextCandidatePos match {
        case Some(newPos) => availableMovesAcc(newPos, mover, p :: acc)
        case None => p :: acc
      }
    }

  val up: Position => Option[Position] = p => if (p.rank == 8) None else Some(Position(p.file, p.rank + 1))
  val down: Position => Option[Position] = p => if (p.rank == 1) None else Some(Position(p.file, p.rank - 1))
  val left: Position => Option[Position] = p => if (p.file == 'A') None else Some(Position(((p.file.toInt) - 1).toChar, p.rank))
  val right: Position => Option[Position] = p => if (p.file == 'H') None else Some(Position(((p.file.toInt) + 1).toChar, p.rank))

  // Can these be composed out of up, down, left and right? flatMap?
  val upLeft: Position => Option[Position] = p => if ((p.rank == 8) || (p.file == 'A')) None else Some(Position(((p.file.toInt) - 1).toChar, p.rank + 1))
  val upRight: Position => Option[Position] = p => if ((p.rank == 8) || (p.file == 'H')) None else Some(Position(((p.file.toInt) + 1).toChar, p.rank + 1))
  val downLeft: Position => Option[Position] = p => if ((p.rank == 1) || (p.file == 'A')) None else Some(Position(((p.file.toInt) - 1).toChar, p.rank - 1))
  val downRight: Position => Option[Position] = p => if ((p.rank == 1) || (p.file == 'H')) None else Some(Position(((p.file.toInt) + 1).toChar, p.rank - 1))
}

object Piece {
  val whiteKingsideCastlePos = new Position('G', 1)
  val whiteQueensideCastlePos = new Position('C', 1)
  val blackKingsideCastlePos = new Position('G', 8)
  val blackQueensideCastlePos = new Position('C', 8)
}

case class Position(val file: Char, val rank: Int) {
  // rank == row
  // file == column
  assert(rank >= 1 && rank <= 8)
  assert(file >= 'A' && file <= 'H')

  def x: Int = file.toInt
  def y: Int = rank
}

object Position {
  implicit def createPosFromNotation(fr: (Char, Int)): Position = Position(fr._1, fr._2)
  implicit def createPosFromNotation(fr: String): Position = Position(fr.charAt(0), fr.tail.toInt)
}

case class King(val player: Player, val position: Position, val moved: Boolean = true) extends Piece {

  val value: Int = 0

  def move(newPos: Position): King = {
    if (availableMoves.contains(newPos)) King(player, newPos)
    else throw new Exception("not a valid move")
  }

  override def availableMoves: List[Position] = {

    val ulMove = upLeft(position)
    val dlMove = downLeft(position)
    val uRMove = upRight(position)
    val drMove = downRight(position)

    val upMove = up(position)
    val downMove = down(position)
    val leftMove = left(position)
    val rightMove = right(position)

    val diagonalMoves = (ulMove :: dlMove :: uRMove :: drMove :: Nil).flatten
    val straightMoves = (upMove :: downMove :: leftMove :: rightMove :: Nil).flatten

    (moved, player) match {
      case (false, White) => (diagonalMoves ++ (Piece.whiteKingsideCastlePos :: Piece.whiteQueensideCastlePos :: straightMoves)) - position
      case (false, Black) => (diagonalMoves ++ (Piece.blackKingsideCastlePos :: Piece.blackQueensideCastlePos :: straightMoves)) - position
      case (true, _) => (diagonalMoves ++ straightMoves) - position
    }

  }

}

case class Queen(val player: Player, val position: Position, val moved: Boolean = true) extends Piece {

  val value: Int = 9

  def move(newPos: Position): Queen = {
    if (availableMoves.contains(newPos)) Queen(player, newPos)
    else throw new Exception("not a valid move")
  }

  override def availableMoves: List[Position] = {

    val ulMoves = availableMovesAcc(position, upLeft, List[Position]())
    val dlMoves = availableMovesAcc(position, downLeft, List[Position]())
    val uRMoves = availableMovesAcc(position, upRight, List[Position]())
    val drMoves = availableMovesAcc(position, downRight, List[Position]())

    val upMoves = availableMovesAcc(position, up, List[Position]())
    val downMoves = availableMovesAcc(position, down, List[Position]())
    val leftMoves = availableMovesAcc(position, left, List[Position]())
    val rightMoves = availableMovesAcc(position, right, List[Position]())

    val diagonalMoves = (ulMoves ++ dlMoves ++ uRMoves ++ drMoves) - position
    val straightMoves = (upMoves ++ downMoves ++ leftMoves ++ rightMoves) - position
    diagonalMoves ++ straightMoves
  }
}

case class Bishop(val player: Player, val position: Position, val moved: Boolean = true) extends Piece {

  val value: Int = 3

  def move(newPos: Position): Bishop = {
    if (availableMoves.contains(newPos)) Bishop(player, newPos)
    else throw new Exception("not a valid move")
  }

  override def availableMoves: List[Position] = {

    val ulMoves = availableMovesAcc(position, upLeft, List[Position]())
    val dlMoves = availableMovesAcc(position, downLeft, List[Position]())
    val uRMoves = availableMovesAcc(position, upRight, List[Position]())
    val drMoves = availableMovesAcc(position, downRight, List[Position]())

    (ulMoves ++ dlMoves ++ uRMoves ++ drMoves) - position
  }
}

case class Knight(val player: Player, val position: Position, val moved: Boolean = true) extends Piece {

  val value: Int = 3

  def move(newPos: Position): Knight = {
    if (availableMoves.contains(newPos)) Knight(player, newPos)
    else throw new Exception("not a valid move")
  }

  override def availableMoves: List[Position] = {

    val luu = left(position).flatMap(up(_).flatMap(up(_)))
    val ruu = right(position).flatMap(up(_).flatMap(up(_)))
    val ldd = left(position).flatMap(down(_).flatMap(down(_)))
    val rdd = right(position).flatMap(down(_).flatMap(down(_)))

    val llu = left(position).flatMap(left(_).flatMap(up(_)))
    val rru = right(position).flatMap(right(_).flatMap(up(_)))
    val lld = left(position).flatMap(left(_).flatMap(down(_)))
    val rrd = right(position).flatMap(right(_).flatMap(down(_)))

    (luu :: ruu :: ldd :: rdd :: llu :: rru :: lld :: rrd :: Nil).flatten
  }
}

case class Rook(val player: Player, val position: Position, val moved: Boolean = true) extends Piece {

  val value: Int = 5

  def move(newPos: Position): Rook = {
    if (availableMoves.contains(newPos)) Rook(player, newPos)
    else throw new Exception("not a valid move")
  }

  override def availableMoves: List[Position] = {

    val upMoves = availableMovesAcc(position, up, List[Position]())
    val downMoves = availableMovesAcc(position, down, List[Position]())
    val leftMoves = availableMovesAcc(position, left, List[Position]())
    val rightMoves = availableMovesAcc(position, right, List[Position]())

    (upMoves ++ downMoves ++ leftMoves ++ rightMoves) - position
  }

}

case class Pawn(val player: Player, val position: Position, val moved: Boolean = true) extends Piece {

  val value: Int = 1

  def move(newPos: Position): Pawn = {
    if (availableMoves.contains(newPos)) Pawn(player, newPos)
    else throw new Exception("not a valid move")
  }

  override def availableMoves: List[Position] = {
    moves ++ captureMoves
  }

  def moves = {
    player match {
      case White if (position.rank == 2) => List[Position](Position(position.file, position.rank + 1), Position(position.file, position.rank + 2))
      case White if (position.rank == 8) => List[Position]()
      case White => List[Position](Position(position.file, position.rank + 1))
      case Black if (position.rank == 7) => List[Position](Position(position.file, position.rank - 1), Position(position.file, position.rank - 2))
      case Black if (position.rank == 1) => List[Position]()
      case Black => List[Position](Position(position.file, position.rank - 1))
    }
  }

  def captureMoves = {
    player match {
      case White => (up(position).flatMap(left(_)) :: up(position).flatMap(right(_)) :: Nil).flatten
      case Black => (down(position).flatMap(left(_)) :: down(position).flatMap(right(_)) :: Nil).flatten
    }
  }

  def canCapture(target: Piece): Boolean = {
    val otherPlayer = this.player != target.player

    val dx = this.position.x - target.position.x
    val dy = this.position.y - target.position.y

    val positionOkForCapture = player match {
      case White => (dx == 1 || dx == -1) && dy == -1
      case Black => (dx == 1 || dx == -1) && dy == 1
    }

    otherPlayer && positionOkForCapture
    
    // TODO extra rules for en-passant?
  }
}



