# In this exercise, you will be trying to make functions faster.
#
# Although performance is a decidedly tertiary goal of almost all code you will
# write, after correctness and understandability, it still helps to get a
# feeling for what operations are slow in R, and which ones are faster, and
# by how much.
#
# In the following exercises, you are given a reference-solution for each
# exercise. Your task is to write a function that behaves the same, but runs
# in less than 1/2 the time for the specified input (median speedup,
# as per `microbenchmark()`).
#
# Many of the functions can easily be sped up much more than by a factor of 2,
# and you are invited to see what methods give the largest gain.
# The speed measurement process with microbenchmark is a bit stochastic,
# however, and the test environment on the server probably also has a different
# CPU than your laptop, so different things may speed up by a different amount.
# The effect of this is not very large, but try to make the functions a bit
# faster than 2x speedup to have a safety buffer for the evaluation.
#
# As in the submissions before, you should only use R, not call any external
# programs or use any libraries except checkmate.
#
# Note: Because the reference functions were written to be deliberately slow,
# they should not serve as inspiration for your own code in the future!
#
# Hint: You probably want to copy the upper part of each function (the asserts)
# since they are usually not the slow part that can be made faster.


# You have taken a few noisy measurements, but you are worried about outlier
# values. You therefore want to extract a few of the largest, and of the
# smallest measurement values you have taken, so you can analyse them further.
#
# Write a function that keeps the `n.keep` largest, and the `n.keep`
# smallest values from a vector `x`, returning a vector of length
# `2 * n.keep` (or the original vector `x` if `n.keep` is larger than half the
# length of `x`).
# Input:
# - `x`: `numeric` vector of values from which to extract outliers. You can
#   assume that `x` does not have duplicate values.
# - `n.keep`: non-negative integer `numeric(1)` indicating the number of
#   outliers to keep.
# Returns: A `numeric` where the top `n.keep` and the bottom `n.keep` values
# of x are kept, but where their order is preserved.
#
# Your function should have a median runtime 1/2 as much as
# `ex01KeepOutliersReference` when called with a vector `x` of length
# 1000 and n.keep between 25 and 35.
# However, the correctness of your function is also checked for other input.
ex01KeepOutliersReference <- function(x, n.keep) {
  assertCount(n.keep, tol = 1e-100)
  assertNumeric(x, any.missing = FALSE)
  x.discard <- x
  # create a vector of everything that we want to discard
  for (i in seq_len(n.keep)) {
    x.discard <- x.discard[-which.max(x.discard)]
    x.discard <- x.discard[-which.min(x.discard)]
  }
  # get the complement of elements that are discarded. This works because
  # there are no duplicates in x
  setdiff(x, x.discard)
}

ex01KeepOutliers <- function(x, n.keep) {
  assertNumeric(x, any.missing = FALSE)
  assertCount(n.keep, null.ok = FALSE, tol = 1e-100)
  if (n.keep == 0) {
    return(numeric(0))
  }
  if (n.keep >= length(x) / 2) {
    return(x)
  }
  topIndices <- order(x, decreasing = TRUE)[1:n.keep]
  bottomIndices <- order(x)[1:n.keep]
  outlierIndices <- sort(c(topIndices, bottomIndices))
  x[outlierIndices]
}


# You have to make a decision between two alternative choices, choice A and
# choice B. To help in your decision, you have asked a large panel of experts
# in different fields how much they think choice A and choice B are preferrable.
# The experts evaluated the choices based on different things they consider
# relevant, such as profitability, environmental sustainability,
# effect on public relations, etc., and each expert has given choice A and
# choice B a score between 0 and 1. You will probably consider the experts'
# judgement in more detail, because some experts may be more reliable than
# others or consider more important issues than others. However, there is a
# specific shortcut you can take: If *no* expert has given choice A a *lower*
# score than choice B, but *at least one* expert has given choice A a *higher*
# score than choice B, then choice A "dominates" choice B, and you can disregard
# choice B right away.
# This is the concept of "Pareto Dominance" or "Pareto Efficiency":
# <https://en.wikipedia.org/wiki/Pareto_efficiency>.
#
# Write a function that calculates whether choice A dominates choice B that
# is faster than the following reference implementation.
# Inputs:
# - `scores.a`: `numeric` with values between 0 and 1 (inclusive), indicating
#   the scores given by each expert to choice A.
# - `scores.b`: `numeric` with values between 0 and 1 (inclusive), indicating
#   the scores given by each expert to choice B.
# `scores.a` and `scores.b` should have the same length.
# The `i`th component of `scores.a` and `scores.b` are the scores given by
# expert `i`, so they can be compared directly.
#
# Your function should have a median runtime 1/2 as much as
# `ex02DominatesReference` on input for `scores.a` and `scores.b` generated
# as follows:
# scores.a <- runif(200, 0.478, 1)
# scores.b <- runif(200, 0, 0.522)
# (I.e. evaluations from 400 experts, choice A dominates choice B in
# approximately 50% of cases)
# However, the correctness of your function is also checked for other input.
ex02DominatesReference <- function(scores.a, scores.b) {
  assertNumeric(scores.a, any.missing = FALSE, lower = 0, upper = 1)
  assertNumeric(scores.b, any.missing = FALSE, lower = 0, upper = 1, len = length(scores.a))

  a.can.dominate <- FALSE
  a.can.not.dominate <- FALSE
  for (i in seq_along(scores.a)) {
    if (scores.a[[i]] < scores.b[[i]]) a.can.not.dominate <- TRUE
    if (scores.a[[i]] > scores.b[[i]]) a.can.dominate <- TRUE
  }
  if (a.can.not.dominate) return(FALSE)
  if (a.can.dominate) return(TRUE)
  FALSE
}

ex02Dominates <- function(scores.a, scores.b) {
  assertNumeric(scores.a, any.missing = FALSE, lower = 0, upper = 1)
  assertNumeric(scores.b, any.missing = FALSE, lower = 0, upper = 1, len = length(scores.a))
  !any(scores.a < scores.b) && any(scores.a > scores.b)
}
