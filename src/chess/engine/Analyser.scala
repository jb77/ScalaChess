package chess.engine

object Analyser {

  /*
     * Should also consider some of the following? :
     * Squares controlled.
     * Control of the centre
     * Pins
     * Forks
     * Hanging pieces
     * Number of pieces developed
     * Castled (or King safety more generally)
     * Don't move the same piece twice early on
     * Bishop pair is more valuable than knight pair, or bishop knight?
     * Passed Pawns & pawn promotion
     * 
     * Probably should factor out the evaluator(s) to a new class
     */

  @throws(classOf[Exception]) // probably not best way to do this - Option result type better?
  def suggestMove(game: Game, timeLimit: Long = 5000, levelLimit: Int = 4): (Position, Position) = {
    val startTime = System.currentTimeMillis()

    def search(g: Game, acc: List[Game], level: Int): List[Game] = {
      // TODO would be better as breadth first rather than depth first search?        
      val elapsedTime = System.currentTimeMillis() - startTime
      if (g.nextToMove != game.nextToMove) {
        try {
          val bestMoveForOpponent = g.suggestMove(timeLimit - elapsedTime, levelLimit - level)
          return g.move(bestMoveForOpponent._1, bestMoveForOpponent._2) :: acc
        } catch {
          case _:Throwable => return acc
        }
      }

      val m = g.availableMoves
      val nextGames = m.map(pp => g.move(pp._1.position, pp._2))

      // TODO add mate bounding?      
      val continue = (level <= levelLimit) && (elapsedTime < timeLimit)
      if (continue && !m.isEmpty) {
        val nextLevel = (for (gPrime <- nextGames) yield search(gPrime, gPrime :: g :: acc, level + 1)).flatten.toList // TODO use suggestMove here?
        nextLevel.filterNot(_==g)
      } else g :: acc
    }

    val gamesFound = search(game, List[Game](), 0)
    extractBestMove(game, gamesFound)
  }

  def extractBestMove(game: Game, games: List[Game]): (Position, Position) = {
    val sorted = games.sortBy(_.currentBoard.evaluate)

    if (sorted.isEmpty) throw new Exception("No moves found")

    val bestGameScore = game.nextToMove match {
      case White => sorted.last.currentBoard.evaluate
      case Black => sorted.head.currentBoard.evaluate
    }

    val possibleGames = sorted.filter(g => g.currentBoard.evaluate == bestGameScore)

    // Assume best game is the one that gets the best score with the least moves
    val gameAndMovesPair =
      for (
        game <- possibleGames;
        val extraBoardStatesFromCurrentGame = game.history filterNot (game.history contains);
        val movesToReachBestScore = extraBoardStatesFromCurrentGame.indexWhere(bs => bs.evaluate == bestGameScore)
      ) yield (game, movesToReachBestScore)

    val bestGame = gameAndMovesPair.minBy(_._2)._1
    val extraBoardStatesFromCurrentGame = bestGame.history filterNot (game.history contains)
    if (extraBoardStatesFromCurrentGame.isEmpty) throw new Exception("No moves found")
    extraBoardStatesFromCurrentGame.head.moveToReachThisState.get
  }

}