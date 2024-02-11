# You may find this exercise a bit more challenging than other homework tasks,
# to balance the relatively easy 01 and 02 R-files in this homework. On the
# other hand, this is your chance to distinguish yourself!
#
# This is a continuation of the Gravity Tic Tac Toe exercise from a previous
# homework.
#
# To use the functions from the previous exercise, we have copied a solution of
# that exercise into `not_an_exercise_tictactoe.R`.
#
# If you are doing interactive experiments, you can "source" the file:
# > source("R/not_an_exercise_tictactoe.R")
# The evaluate_submission tests will make use of the content of
# not_an_exercise_tictactoe.R automatically.

# Write a function that plays Gravity Tic Tac Toe perfectly.
#
# This function should have arguments `position` and `player`,
# that behave just as in ex03HumanPlayer or ex04RandomPlayer from the Tic Tac
# Toe exercise, and return a move coordinate just as they do.
#
# Your function should make the optimal move in every situation: If it is
# to win, even against a perfrect opponent, your function should win.
# Otherwise, if it is possible to force a draw, your function should force a
# draw.
#
# Your function should find out which move is the best to make by:
#  - trying out all possible moves (i.e. all three columns) and
#  - creating a new position matrix representing the hypothetical position
#    where that move was played
#  - computing what the best move of the opposing player would be at this
#    point, and what the outcome of that would be, for example by recursively
#    calling ex01TBot itself directly or indirectly for each hypothetical
#    position.
# As a hint, a relatively elegant solution contains a call to ex05Tournament
# with ex01TBot for both players (using `suppressMessages` to keep output in
# check). If you want to read more about the system here (I don't think you
# have to to solve the exercise, but it gives some context):
# <https://en.wikipedia.org/wiki/Minimax#Minimax_algorithm_with_alternate_moves>.
#
# When you finish this, you should be able to play Gravity Tic Tac Toe against
# an unbeatable opponent using
# > ex05Tournament(ex03HumanPlayer, ex01TBot)
# (using ex05Tournament from not_an_exercise_tictactoe.R).
#
# ex01TBot should be able to make perfect moves even for 'unreachable'
# positions, such as two "O" and no "X" present, so don't rely on
# precomputation for this task.


ex01TBot <- function(position, player) {
  assertMatrix(position, mode = "character", nrows = 3, ncols = 3)
  assertChoice(player, choices = c("X", "O"))
  # Hilfsfunktion zum Wechseln des Spielers
  switchPlayer <- function(player) {
    ifelse(player == "X", "O", "X")
  }
  # Hilfsfunktion zur Bewertung des Spielstands
  score <- function(status, player) {
    if (status == player) {
      return(1)
    } else if (status == "") {
      return(0)
    } else {
      return(-1)
    }
  }
  # Minimax-Funktion
  minimax <- function(position, player) {
    status <- ex01Winner(position)
    if (!is.na(status)) {
      return(list(score = score(status, player), move = NULL))
    }
    bestScore <- -Inf
    bestMove <- NULL
    for (move in c("A", "B", "C")) {
      if (is.na(position[1, match(move, LETTERS[1:3])])) {
        coord <- ex02MoveStringToCoordinate(move, position)
        newPosition <- position
        newPosition[coord[[1]], coord[[2]]] <- player
        result <- minimax(newPosition, switchPlayer(player))
        if (-result$score > bestScore) {
          bestScore <- -result$score
          bestMove <- coord
        }
      }
    }
    if (is.null(bestMove)) {
      return(list(score = 0, move = NULL))  # Keine weiteren Züge möglich
    }
    return(list(score = bestScore, move = bestMove))
  }
  result <- minimax(position, player)
  return(result$move)
}