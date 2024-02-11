
# Ecologies
#
# The objective here is to again rewrite the following functions to be faster than
# the provided reference versions.
#
# Consider a very simple ecological model that predicts how many individuals
# of a given species are alive in time `t`. The system starts out with
# `x(1)` individuals at time 1. In every discrete time step, the population
# changes because of two effects:
# - reproduction with reproduction rate `qr`: After every time step `t`,
#   `(qr - 1) * x(t)` individuals are added.
# - starvation with carrying capacity `G`: The *proportion* of individuals that
#   die and are removed after time step `t` is `x(t) / G` (and therefore depends
#   on the quantity of individuals `x(t)`, i.e. the more individuals are alive,
#   the greater the fraction of individuals that die). The *absolute quantity*
#   of individuals that are removed is therefore `x(t) * x(t) / G`.
# With the two effects, the quantity of alive individuals at time step `t + 1`
# is therefore
#   x(t + 1) = x(t) + (qr - 1) * x(t)   - x(t) * x(t) / G
#            = qr * x(t)                - x(t) * x(t) / G
#            = (1 / G) * x(t) * qr * G  - (1 / G) * x(t) * x(t)
#            = (1 / G) * x(t) * (qr * G - x(t))
# (we are not doing rounding here and are working with continuous quantities.)
#
#
# Write a function that calculates the sequence of quantities `x(t)` for `t`
# between 1 and `t.max` (inclusive).
# Inputs:
# - `x1`: `numeric(1)` indicating `x(1)`, at least 0
# - `qr`: `numeric(1)` the reproduction rate, at least 1.
# - `g`: `numeric(1)` the carrying capacity, at least 0.
# - `t.max`: positive integer `numeric(1)` indicating the length of the result.
# Returns: a `numeric` vector giving the `x(t)` for `t` in 1 .. `t.max`.
#
# Your function should have a median runtime 1/2 as much as
# `ex01SimpleEcologyReference` for values of `t.max` of 1000, `qr` between 1
# and 4, `g` between 0 and 100, and x1 between 0 and `qr * g`.
#
# (If you are interested: This is the "logistic map" and plays a role in chaos
# theory: <https://en.wikipedia.org/wiki/Logistic_map>. To get from our formula
# to the one in Wikipedia, set `qr` to `r` and `G` to `1 / r`.)
ex01SimpleEcologyReference <- function(x1, qr, g, t.max) {
  assertNumber(x1, lower = 0)
  assertNumber(qr, lower = 1)
  assertNumber(g, lower = 0)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- x1
  xt <- x1
  for (t in seq_len(t.max - 1)) {
    xt.next <- 1 / g * xt * (qr * g - xt)
    result <- append(result, xt.next)
    xt <- xt.next
  }
  result
}

ex01SimpleEcology <- function(x1, qr, g, t.max) {
  assertNumber(x1, lower = 0)
  assertNumber(qr, lower = 1)
  assertNumber(g, lower = 0)
  assertInt(t.max, lower = 1, tol = 1e-100)
  if (t.max == 1) {
    return(c(x1))
  }
  result <- numeric(t.max)
  result[[1]] <- x1
  for (t in 2:t.max) {
    result[t] <- 1 / g * result[t - 1] * (qr * g - result[t - 1])
  }
  result
}

# A more complex ecological system, made up of two species, is described by the
# "Lotka-Volterra" equations. It is made up of the "predator" and the "prey"
# species, where the quantity of prey grows by a constant factor and gets
# reduced proportionally to how many predators are present, and the quantity of
# predators grows proportionally to how many prey are present and gets reduced
# by a constant factor. The actual Lotka-Volterra equations are differential
# equations, but here we will use a discretization of the model:
#   - prey(t + 1)     = prey(t) * qr.prey         - prey(t) * predator(t) * qi.prey
#   - predator(t + 1) = predator(t) * qr.predator + predator(t) * prey(t) * qi.predator
#   - `prey` and `predator` never become negative. When any of the equations
#     above has a negative result, then the values are set to 0.
# Where `qr.prey` >= 1 and 0 <= `qr.predator` <= 1: With no predators present,
# the prey would grow exponentially, and with no prey present, the predators
# would die out.
#
# Write a function that calculates the sequence of quantities `prey(t)` and
# `predator(t)` for `t` between 1 and `t.max` (inclusive).
# Inputs:
# - `x1`: `numeric(2)` indicating `prey(1)` in component 1 and `predator(1)`
#   in component 2, both at least 0.
# - `qr`: `numeric(2)` indicating the natural growth rates in the absence of
#   the other species: `qr[1]` is `qr.prey` (at least 1) and `qr[2]` is
#   `qr.predator` (between 0 and 1, inclusive).
# - `qi`: `numeric(2)` indicating the effect of growth rates of the species on
#   each other. `qi[1]` is `qi.prey` and `qi[2]` is `qi.predator`, both at least
#   0.
# - `t.max`: positive integer `numeric(1)` indicating the number of entries in
#   the result.
# Returns: a `matrix` with two columns and `max.t` rows, where columns are named
# `prey` and `predator` containing the quantities of both at each time between
# 1 and `max.t`.
#
# Your function should have a median runtime 1/2 as much as
# `ex02ComplexEcologyReference` for values of `t.max` of 100, `qr[1]` between
# 1 and 1.2, `qr[2]` between 0.8 and 1, and `qi` between 0 and 0.2. Note that `t.max`
# is less than in ex03!
ex02ComplexEcologyReference <- function(x1, qr, qi, t.max) {
  assertNumeric(x1, len = 2, lower = 0, any.missing = FALSE)
  assertNumeric(qr, len = 2, any.missing = FALSE)
  if (qr[[1]] < 1 || qr[[2]] < 0 || qr[[2]] > 1) stop("qr[1] must be at least 1 and qr[2] must be between 0 and 1.")
  assertNumeric(qi, len = 2, lower = 0, upper = 1, any.missing = FALSE)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("prey", "predator")))
  xt <- x1
  for (t in seq_len(t.max)) {
    result <- rbind(result, xt, deparse.level = 0)
    xt <- xt * qr + c(-1, 1) * xt[[1]] * xt[[2]] * qi
    xt <- pmax(xt, 0)  # set values that are < 0 to 0
  }
  result
}

ex02ComplexEcology <- function(x1, qr, qi, t.max) {
  assertNumeric(x1, len = 2, lower = 0, any.missing = FALSE)
  assertNumeric(qr, len = 2, any.missing = FALSE)
  if (qr[[1]] < 1 || qr[[2]] < 0 || qr[[2]] > 1) stop("qr[1] must be at least 1 and qr[2] must be between 0 and 1.")
  assertNumeric(qi, len = 2, lower = 0, upper = 1, any.missing = FALSE)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- matrix(0, nrow = t.max, ncol = 2, dimnames = list(NULL, c("prey", "predator")))
  result[1, ] <- x1
  if (t.max > 1) {
    qrPrey <- qr[[1]]
    qrPredator <- qr[[2]]
    qrDiff <- c(-1, 1) * qi
    for (t in 2:t.max) {
      prevPrey <- result[t - 1, 1]
      prevPredator <- result[t - 1, 2]
      preyPredatorInteraction <- prevPrey * prevPredator
      newXPrey <- prevPrey * qrPrey - preyPredatorInteraction * qi[[1]]
      newXPredator <- prevPredator * qrPredator + preyPredatorInteraction * qi[[2]]
      result[t, 1] <- newXPrey
      result[t, 2] <- newXPredator
      if (newXPrey <= 0) result[t, 1] <- 0
      if (newXPredator <= 0) result[t, 2] <- 0
    }
  }
  result
}
