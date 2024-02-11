# Solve these exercises by filling in the function bodies, and try to read up on
# things that you didn't know or surprise you.

# write a function that multiplies x by y
# x and y are both single numbers.
ex01Multiply <- function(x, y) {
  z <- x * y
  return(z)
}

# write a function that multiplies x by y
# x and y are numeric vectors. Does this function need to
# differ from the one above?
ex02MultiplyVectors <- function(x, y) {
  x * y
}

# write a function that checks if a "logical", a "numeric", or a "character" vector or something else.
# the function should return "lg" if the input is logical, "nm" if it is numeric, "ch" for characters,
# and otherwise "x".
ex03VectorType <- function(x) {
  if (is.logical(x)) return("lg")
  if (is.numeric(x)) return("nm")
  if (is.character(x)) return("ch")
  "x"
}

# write a function that takes a vector of integers and returns all numbers that are odd.
# an empty vector should be returned when no odd numbers are found.
ex04Odd <- function(x) {
  x[x %% 2 == 1]
}
