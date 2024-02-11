
# This exercise is concerned with control structures: conditional execution and loops.
# (we will skip functions for now).

# Consider the following `data.frame` that records "opposites" of concepts:
#
#        concept           opposite
#      1   black              white
#      2    slow               fast
#      3     big              small
#      4     man              woman
#      5    many                few
#      6     man              woman
#      7    fast               slow
#
# indicating that "many" is the opposite of  "few" etc.
#
# Write a function that takes a database of this form, as well as a concept, as input and returns
# the opposite of the given concept. Note, however, that the opposite of an opposite is the concept
# itself, so with the database above, your function should report "white" as the opposite of "black",
# but also "black" as the opposite of "white".
#
# The function should now take two inputs, `database` and `concept` and return a `character(1)` string.
#
# Example inputs using the the knowledge database from above:
# > database = data.frame(concept = c("black", "slow", "big", "man", "many", "man", "fast"),
#     opposite = c("white", "fast", "small", "woman", "few", "woman", "slow"), stringsAsFactors = FALSE)
# The function should give the following outputs, as examples:
# > ex01Opposite(database, "black")
# --> "white"
# > ex01Opposite(database, "slow")
# --> "fast"
# > ex01Opposite(database, "small")
# --> "big"
# > ex01Opposite(database, "man")
# --> "woman"
# > ex01Opposite(database, "woman")
# --> "man"
#
# You may assume that the `concept` input can always be found in at least one of the
# two columns of `database`. Some of the concepts and their opposites may occur more than once.
# However, there are never any contradictions: if a concept occurs more than once, its opposite
# is the same, and if an entry of the `opposite` column is also in the `concept` column, then the
# corresponding `concept` entry is in the `opposite` column (see the "fast"/"slow" example above).
ex01Opposite <- function(database, concept) {
  conceptIndices <- which(database$concept == concept)
  if (length(conceptIndices) > 0) {
    return(database$opposite[conceptIndices[[1]]])
  }
  oppositeIndices <- which(database$opposite == concept)
  return(database$concept[oppositeIndices[[1]]])
}


# A "cellular automaton" is a discrete model of state evolution. We consider a state
# of a vector with "cells" of values 0 or 1, for example the vector c(0, 1, 0, 1, 0, 0, 0).
# The state now changes according to a special rule: Every cell changes to state 1
# whenever its immediate left neighbour has state 1. A cell that has state 1 remains at state 1.
# Each cell that has no left neighbour or a left neighbour that is 0, and that is also 0 itself, remains 0 otherwise.
# The evolution of the vector above would be
# > c(0, 1, 0, 1, 0, 0, 0)
# > c(0, 1, 1, 1, 1, 0, 0)
# > c(0, 1, 1, 1, 1, 1, 0)
# > c(0, 1, 1, 1, 1, 1, 1)
#
# Write a function that takes as input a vector of 0s and 1s and "evolves" the state until no
# more changes occur. The return value should be a
# list of states, with the first entry being the input vector.
# The upper example would look like this:
# > initial.state = c(0, 1, 0, 1, 0, 0, 0)
# --> returns
# list(c(0, 1, 0, 1, 0, 0, 0),
#      c(0, 1, 1, 1, 1, 0, 0),
#      c(0, 1, 1, 1, 1, 1, 0),
#      c(0, 1, 1, 1, 1, 1, 1))
#
# Other examples:
#
# > initial.state = c(0, 1, 0, 0, 0, 1)
# --> returns
# list(c(0, 1, 0, 0, 0, 1),
#      c(0, 1, 1, 0, 0, 1),
#      c(0, 1, 1, 1, 0, 1),
#      c(0, 1, 1, 1, 1, 1))
#
# > initial.state = c(0, 0, 0, 0, 0)
# --> returns the value as of
# list(c(0, 0, 0, 0, 0))
# (i.e. a single entry, since this is already a state that will not change.)
#
evolveState <- function(state) {
  newState <- state
  for (i in 2:length(state)) {
    if (state[i - 1] == 1) {
      newState[i] <- 1
    }
  }
  return(newState)
}

ex02CellularAutomaton <- function(initial.state) {
  statesList <- list(initial.state)
  while (TRUE) {
    newState <- evolveState(tail(statesList, 1)[[1]])
    if (all(newState == tail(statesList, 1)[[1]])) {
      break
    }
    statesList <- c(statesList, list(newState))
  }
  return(statesList)
}