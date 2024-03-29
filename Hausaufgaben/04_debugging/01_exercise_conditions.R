
# Write a function that measures how long another function was running. The
# function should return the runtime of the given function, in seconds, rounded
# to the nearest integer second.
#
# The function being timed may sometimes throw an error, in which case the
# return value should depend on the `impute.inf`-argument: If it is `TRUE`, a
# value of `Inf` should be returned when an error was thrown, instead of the
# actual runtime. This would indicate that the function never actually finished.
#
# This function gets three arguments:
# - `fn` (function) the function to measure. It takes no arguments and should
#   be called as `fn()`.
# - `impute.inf` (`logical(1)`) whether to return `Inf`, instead of the runtime,
#   when `fn` throws an error.
#
# Functions that may be useful here are the `system.time()` function, or the
# `proc.time()` function. Make sure to use the `elapsed` part of the times that
# they report.
#
# Example functions to try out:
sleep1 <- function() Sys.sleep(1)
sleep2 <- function() Sys.sleep(2)
sleepErr <- function() {
  Sys.sleep(1)
  stop("error :-(")
}
# Example calls:
# > ex01TimeFun(sleep1, impute.inf = FALSE)  # 1
# > ex01TimeFun(sleep1, impute.inf = TRUE)  # 1
# > ex01TimeFun(sleep2, impute.inf = FALSE)  # 2
# > ex01TimeFun(sleepErr, impute.inf = FALSE)  # 1
# > ex01TimeFun(sleepErr, impute.inf = TRUE)  # Inf
ex01TimeFun <- function(fn, impute.inf) {
  assertFunction(fn)
  assertLogical(impute.inf, len = 1)
  startTime <- proc.time()
  tryCatch({
      time <- system.time(fn())[["elapsed"]]
      time <- round(time)
      return(time)
    }, error = function(e) {
      errorTime <- proc.time() - startTime
      if (!impute.inf) {
        as.numeric(round(errorTime[["elapsed"]]))
      } else {
        Inf
      }
    })
}

# Write a function that behaves like a simple version of `lapply()`: It applies
# a given `FUN` to a list or vector `X`. However, if `FUN` throws an error,
# instead of aborting, the function should impute a value `impute`.
#
# This function gets three arguments:
# - `X`: Any kind of object that `lapply()` can iterate over. It is not
#   necessary to use a `checkmate` assert on this argument.
# - `FUN`: A function taking one argument.
# - `impute`: The value to use in cases where `FUN` throws an error.  This can
#   be any type, it is not necessary to use a `checkmate` assert here.
#
# > ex02SafeLapply(c(2, 16, 64), log2, impute = -1)  # list(1, 4, 6)
# > ex02SafeLapply(list(2, 16, "x"), log2, impute = -1)  # list(1, 4, -1)
# > ex02SafeLapply(list(2, 16, "x"), log2, impute = NULL)  # list(1, 4, NULL)
#
# Be aware that the return value of `FUN` could be the result of a `try()` call,
# so you should likely use `tryCatch()` instead of `try()` here:
# > ex02SafeLapply(list(1, NULL, try(stop("."), silent = TRUE)), identity, impute = -1)
# ## returns `list(1, NULL, try(stop("."), silent = TRUE))` -- -1 is not imputed
# ## here.

ex02SafeLapply <- function(X, FUN, impute) {
  assertFunction(FUN)
  lapply(X,function(x) tryCatch(FUN(x), error = function(e) impute))
}


