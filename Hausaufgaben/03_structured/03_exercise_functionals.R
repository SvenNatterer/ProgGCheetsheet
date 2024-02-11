
# Write a function that finds the root (i.e. the input for which its value is 0) of a function.
#
# The input function is always continuous and monotonically increasing, so fun(x + y) is
# greater or equal to f(x) when y is positive.
#
# Your function should have three arguments:
# * `fun`, a function
# * `lower`, a finite numeric scalar (single element vector)
# * `upper`, a finite numeric scalar greater or equal to `lower`
#
# `fun` can be assumed to be zero somewhere between `lower` and `upper`.
# You only need to find the root with a tolerance of 0.001, so
# > ex01FindRoot(function(x) x, lower = -1, upper = 1)
# has the result `0`, but `-0.001`, `0.001`, and `0.0002422213` are all accepted.
#
# Your result should always be between `lower` and `upper`, even if `fun` is also 0 outside this region.
#
# A simple way to solve this is to call `fun` with values `lower`, `lower + 0.001`, `lower + 0.002`, ...
# and return the input value for which `fun` is non-negative for the first time.
#
# Advanced students may try to use a more efficient way to solve this, such as e.g. the
# "Bisection method": <https://en.wikipedia.org/wiki/Bisection_method>.
#
# Your function should *not* use the `uniroot()` method or similar library methods.
#
# Example behaviour:
# input: ex01FindRoot(cos, -pi, 0)
# output: -1.570796 (or someting between -1.571796 and -1.569796)
# input: ex01FindRoot(function(x) round(x), -1, 1)  # should work even for functions that are not strictly monotonic
# output: something between -0.501 and 0.501
# input: ex01FindRoot(function(x) round(x), -1, 0)
# output: anything between -0.501 and 0
# input: ex01FindRoot(cos, 1, -1)
# --> ERROR because the upper bound is below the lower bound
# input: ex01FindRoot(function(x, y) x^3, -1, 1)  # y-parameter is ok here since it is not used
# output: anything between -0.001 and 0.001
ex01FindRoot <- function(fun, lower, upper) {
  assertFunction(fun, null.ok = FALSE)
  assertNumeric(lower, len = 1, finite = TRUE)
  assertNumeric(upper, len = 1, finite = TRUE, lower = lower)
  tolerance <- 0.001
  while ((upper - lower) / 2.0 > tolerance) {
    midpoint <- (lower + upper) / 2.0
    if (fun(midpoint) == 0.0) {
      return(midpoint)
    } else if (fun(midpoint) * fun(lower) < 0.0) {
      upper <- midpoint
    } else {
      lower <- midpoint
    }
  }
  return((lower + upper) / 2.0)
}

# Write a function that returns a function that checks a vector for conditions
#
# Your function should take one input:
# * `threshold`: a scalar numeric
# Your function should *return a function*. The returned function should have
# one argument `vect`: a numeric vector with no missing values.
# The returned function should return either TRUE or FALSE.
#
# The returned function should check whether its input contains
# at least one element greater than `threshold`.
#
# Examples
# fun <- ex02VectorCondition(threshold = 10)
# fun(c(1, 2, 3, 4)) --> FALSE
# fun(9:11) --> TRUE
# fun(numeric(0)) --> FALSE
#
# fun <- ex02VectorCondition(threshold = -1)
# fun(0) --> TRUE
# fun(c(-200, -100)) --> FALSE
# fun(c("10", "20")) --> ERROR (argument is not a numeric)
#
# ex02VectorCondition(threshold = "10") --> ERROR (not a numeric)
# ex02VectorCondition(threshold = c(1, 2, 3)) --> ERROR (not a scalar)
ex02VectorCondition <- function(threshold) {
  assertNumeric(threshold, len = 1, any.missing = FALSE)
  function(vect) {
    assertNumeric(vect, any.missing = FALSE)
    any(vect > threshold)
  }
}


# Write a function that selects out elements of a list based on a condition
#
# Your function should have two arguments
# * `vectors`: a list of `numeric` vectors
# * `threshold`: a scalar numeric
# Your function should return a character vector containing all
# elements from `vectors` that contain at least one number greater
# than `threshold`.
#
# Examples:
# ex03VectorThreshold(
#   list(numeric(0), 1:3, 8:11, c(-100, 100)),
#   threshold = 10)
# --> list(8:11, c(-100, 100))
# ex03VectorThreshold(list(), 10) --> list()
# ex03VectorThreshold(list(numeric(0), 0, -1), 10) --> list()
# ex03VectorThreshold(10, 10) --> ERROR (`vectors` is not a list of numerics)
# ex03VectorThreshold(list(10), "10") --> ERROR (threshold is not a numeric)
#
# You may want to use `Filter`, and the solution of ex02VectorCondition may be useful here.
ex03VectorThreshold <- function(vectors, threshold) {
  assertList(vectors)
  lapply(vectors, function(vec) assertNumeric(vec, any.missing = FALSE))
  assertNumeric(threshold, len = 1, any.missing = FALSE)
  conditionFunction <- ex02VectorCondition(threshold)
  Filter(conditionFunction, vectors)
}
