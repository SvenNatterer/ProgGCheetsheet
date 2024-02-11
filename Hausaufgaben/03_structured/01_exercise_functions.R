
# Write a function that converts distances between units.
#
# Your function should convert distances between the units "km",
# "miles", and "nautical miles", where one "miles" is exactly 1.609344 "km",
# and one "nautical miles" is exactly 1.852 "km".
#
# Your function should take three arguments:
# * `distance`, a finite numeric vector of arbitrary length
# * `from`, a string (single element character vector) which should be one of
#   "km", "miles", or "nautical miles"
# * `to`, a sring (single element character vector) which should be one of
#   "km", "miles", or "nautical miles"
# Output should always be rounded to three decimal places; you will probably
# want to use the `round()` function.
# The `from` argument should be *optional*; if it is not given, conversion should
# be from "km".
# The `to` argument should be *optional*; if it is not given, conversion should
# be to "km".
# You may want to edit the `function(...)` line to get default arguments.
#
#
# Example behaviour:
# input: ex01DistConversion(0)
# output: 0
# input: ex01DistConversion(numeric(0))
# output: numeric(0)
# input: ex01DistConversion(100, "miles", "nautical miles")
# output: 86.898
# input: ex01DistConversion(c(1, 10000000), from = "miles")
# output: c(1.609, 16093440)
# input: ex01DistConversion(c(1, 2, 3, 4), "miles", "miles")
# output: c(1, 2, 3, 4)
ex01DistConversion <- function(distance, from = "km", to = "km") {
  assertNumeric(distance, any.missing = FALSE)
  assert_character(from, len = 1)
  assert_character(to, len = 1)
  km.to.miles <- 1.609344
  miles.to.km <- 1 / km.to.miles
  km.to.nautical <- 1.852
  nautical.to.km <- 1 / km.to.nautical
  if (from == "miles") {
    distance <- distance * km.to.miles
  } else if (from == "nautical miles") {
    distance <- distance * km.to.nautical
  }
  if (to == "miles") {
    distance <- distance * miles.to.km
  } else if (to == "nautical miles") {
    distance <- distance * nautical.to.km
  }
  return(round(distance, 3))
}


# Write a function that returns the n'th Fibonacci number
#
# The Fibonacci-Numbers are defined as
# F(0) = 0,  F(1) = 1,  F(n) = F(n - 1) + F(n - 2)
#
# Your function should take one parameter:
# * `n` a non-negative scalar integer value
# and should return a scalar value: the n'th Fibonacci number.
#
# Example behaviour:
# ex02Fibonacci(-1) -> ERROR
# ex02Fibonacci(0.5) -> ERROR
# ex02Fibonacci(0) -> 0
# ex02Fibonacci(1) -> 1
# ex02Fibonacci(2) -> 1
# ex02Fibonacci(3) -> 2
# ex02Fibonacci(4) -> 3
# ex02Fibonacci(5) -> 5
# ex02Fibonacci(6) -> 8
# ...
# ex02Fibonacci(11) -> 89
# ... and so on
#
# Think how you can use the definition above and recursion to solve this
# problem in very few lines.
ex02Fibonacci <- function(n) {
  assertNumeric(n, lower = 0, len = 1)
    if (n == 0) return(0)
    if (n == 1) return(1)
    return(ex02Fibonacci(n - 1) + ex02Fibonacci(n - 2))
}
