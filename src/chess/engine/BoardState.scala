package chess.engine

@serializable
class BoardState(val pieces: Set[Piece], val moveToReachThisState: Option[(Position, Position)]) {

  def move(currPos: Position, newPos: Position, checkLegality: Boolean = true): BoardState = {
    val pieceToMove = pieceAt(currPos)

    val piece = pieceToMove match {
      case None => throw new Exception("No piece at: " + currPos)
      case Some(p) => p
    }

    if (checkLegality) {
      if (!legalMoves(piece).contains(newPos)) throw new IllegalStateException("Illegal Move")
    }

    val movedPiece = piece.move(newPos)
    val newPieces = pieceAt(newPos) match {
      case None => pieces - piece + movedPiece
      case Some(occupyingPiece) if (occupyingPiece.player == piece.player) => throw new IllegalStateException("Piece from the same colour in the destination position")
      case Some(occupyingPiece) => pieces - piece + movedPiece - occupyingPiece
    }

    new BoardState(newPieces, Some(currPos, newPos))

    // TODO pawn promotion
  }

  def legalMoves(piece: Piece, checkChecks: Boolean = true): Set[Position] = {
    val baseSet = piece.availableMoves.toSet
    val pieceColour = piece.player
    val piecesAtDestinations = baseSet.map(pieceAt(_)).flatten

    val piecesAtDestsOfSameColour = piecesAtDestinations.filter(_.player == pieceColour)
    val problemPositions = checkChecks match {
      case false => piecesAtDestsOfSameColour.map(_.position)
      case true => piecesAtDestsOfSameColour.map(_.position) ++
        piecesAtDestinations.filter(p => p.player != pieceColour && p.isInstanceOf[King]).map(_.position)
    }

    val destinationsAvailable = baseSet.filterNot(p => problemPositions.contains(p))
    val availableAndReachable = piece match {
      case Knight(_, _, _) => destinationsAvailable
      case Rook(_, _, false) => destinationsAvailable.filterNot(
        anythingBlocking(_, piece, pieces.filterNot(p => p.isInstanceOf[King]) - piece)) // TODO improve this special case to prevent King blocking castling move - currently weak 
      case _ => destinationsAvailable.filterNot(anythingBlocking(_, piece, pieces - piece))
    }

    val invalidPawnCapturesRemoved = piece match {
      case p: Pawn => availableAndReachable.filter(
        pos =>
          (pieceAt(pos).map(p.canCapture(_)).getOrElse(false))
            || (p.moves.contains(pos)))
      case _ => availableAndReachable
    }

    if (checkChecks) {
      val movesThatLeaveYouInCheckRemoved = invalidPawnCapturesRemoved.filterNot(
        pos => move(piece.position, pos, false).inCheck(piece.player))

      movesThatLeaveYouInCheckRemoved
    } else invalidPawnCapturesRemoved
  }

  def anythingBlocking(target: Position, pieceToMove: Piece, otherPieces: Set[Piece]): Boolean = {

    def collinear(a: Position, b: Position, c: Position): Boolean = {
      // Return true iff a, b, and c all lie on the same line.
      (b.x - a.x) * (c.y - a.y) == (c.x - a.x) * (b.y - a.y)
    }

    def within(p: Int, q: Int, r: Int): Boolean = {
      // Return true iff q is between p and r (inclusive).
      ((p <= q) && (q <= r)) || ((r <= q) && (q <= p))
    }

    def inTheWay(candidatePos: Position, sourcePos: Position, targetPos: Position) = {
      (collinear(sourcePos, targetPos, candidatePos)
        && (
          if (sourcePos.x != targetPos.x) within(sourcePos.x, candidatePos.x, targetPos.x)
          else within(sourcePos.y, candidatePos.y, targetPos.y)))
    }

    otherPieces.exists(p => inTheWay(p.position, pieceToMove.position, target) && p.position != target)
  }

  def pieceAt(pos: Position): Option[Piece] = pieces.find(p => p.position == pos)

  override def toString(): String = {
    // TODO rewrite in idiomatic Scala
    var str: String = ""

    str = str + " "
    for (file <- 'A'.to('H')) str = str + " " + file + " "
    str = str + "\n"

    for (rank <- 8.to(1, -1)) {
      str = str + rank
      for (file <- 'A'.to('H')) {
        val op: Option[Piece] = pieceAt(Position(file,rank))
        if (!op.isEmpty) {
          val p = op.get
          val s = p match {
            case King(White, _, _) => " K "
            case Queen(White, _, _) => " Q "
            case Bishop(White, _, _) => " B "
            case Knight(White, _, _) => " N "
            case Rook(White, _, _) => " R "
            case Pawn(White, _, _) => " P "
            case King(Black, _, _) => " k "
            case Queen(Black, _, _) => " q "
            case Bishop(Black, _, _) => " b "
            case Knight(Black, _, _) => " n "
            case Rook(Black, _, _) => " r "
            case Pawn(Black, _, _) => " p "
            case _ => " _ "
          }

          str = str + s
        } else
          str = str + " _ "
      }
      str = str + rank + "\n"
    }

    str = str + " "
    for (file <- 'A'.to('H')) str = str + " " + file + " "

    str
  }

  def evaluate: Int = {
    val whitePieces = pieces.filter(_.player == White)
    val blackPieces = pieces.filter(_.player == Black)

    val blackMatedBonus = if (isCheckmated(Black)) 1000 else 0
    val whiteMatedBonus = if (isCheckmated(White)) 1000 else 0

    val whiteTotal: Int = whitePieces.toList.map(_.value).sum + blackMatedBonus
    val blackTotal: Int = blackPieces.toList.map(_.value).sum + whiteMatedBonus

    (whiteTotal - blackTotal)
  }

  def inCheck(player: Player): Boolean = {
    val playerPieces = pieces.filter(_.player == player)
    val opposingPieces = pieces.filter(_.player != player)
    val kingPos = playerPieces.find(_.isInstanceOf[King]).map(_.position)
    val opposingMoves = opposingPieces.flatMap(legalMoves(_, false))

    kingPos match {
      case None => false
      case Some(p) => opposingMoves.contains(p)
    }
  }

  def isCheckmated(player: Player): Boolean = {
    inCheck(player) && pieces.filter(_.player == player).flatMap(legalMoves(_)).isEmpty
  }

  def isStalemated(player: Player): Boolean = {
    (!inCheck(player)) && pieces.filter(_.player == player).flatMap(legalMoves(_)).isEmpty
  }

}

object BoardState {

  def startingBoard: BoardState = {
    val pieces = Set[Piece](
      King(White, "E1", false),
      Queen(White, "D1", false),
      Bishop(White, "C1", false),
      Bishop(White, "F1", false),
      Knight(White, "B1", false),
      Knight(White, "G1", false),
      Rook(White, "A1", false),
      Rook(White, "H1", false),

      King(Black, "E8", false),
      Queen(Black, "D8", false),
      Bishop(Black, "C8", false),
      Bishop(Black, "F8", false),
      Knight(Black, "B8", false),
      Knight(Black, "G8", false),
      Rook(Black, "A8", false),
      Rook(Black, "H8", false),

      Pawn(White, "A2", false),
      Pawn(White, "B2", false),
      Pawn(White, "C2", false),
      Pawn(White, "D2", false),
      Pawn(White, "E2", false),
      Pawn(White, "F2", false),
      Pawn(White, "G2", false),
      Pawn(White, "H2", false),

      Pawn(Black, "A7", false),
      Pawn(Black, "B7", false),
      Pawn(Black, "C7", false),
      Pawn(Black, "D7", false),
      Pawn(Black, "E7", false),
      Pawn(Black, "F7", false),
      Pawn(Black, "G7", false),
      Pawn(Black, "H7", false))

    new BoardState(pieces, None)
  }

}
