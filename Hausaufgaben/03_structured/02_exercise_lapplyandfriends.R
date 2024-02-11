# Write a function that collects a bunch of strings into one string.
#
# By default, the function should take multiple arguments that are then collected
# together into one string, quoted with quotation marks and separated by commas.
#
# ex01Collate("a", "b", "c") --> '"a", "b", "c"'
# ex01Collate("x") --> '"x"'
# ex01Collate(" ", "") --> '" ", ""'
# ex01Collate('"') --> '"""'
# ex01Collate() --> ""
#
# (Note that the separation is with comma and a space, so ', ', not ',')
#
# The arguments should take a scalar string each.
# ex01Collate(character(0)) --> ERROR
# ex01Collate(c("a", "b", "c")) --> ERROR
# ex01Collate(3) --> ERROR
# ex01Collate(NULL) --> ERROR
#
ex01Collate <- function(...) {
  args <- list(...)
  if (length(args) == 0) return("")
  for (arg in args) {
  assertCharacter(arg, len = 1)
  }
  newString <- paste0('"', paste(args, collapse = '", "'), '"')
  return(newString)
}





# Write a function that calls the ex01Collate-function multiple times.
#
# ex02CollateLists(list(c("a", "b"), c("c", "d", "e"))) --> c('"a", "b"', '"c", "d", "e"')
#
# This function should take three arguments:
# * `inputs`: a list of character vectors that are given to the `ex01Collate` function.
#   The ex01Collate function is called with each list element in turn, and the individual
# The return value should be a character vector of the same length as the `input` list
# Examples:
# ex02CollateLists(list()) --> character(0)
# ex02CollateLists(list(c(" ", ""), '"', character(0), c("a", "b", "c"))) -->
#   c('" ", ""', '"""', "", '"a", "b", "c"')
# ex02CollateLists(list("a", c("b", "c"))) --> c('"a"', '"b", "c"')
# ex02CollateLists(list("a", c(x = "b", y = "c"))) --> c('"a"', '"b", "c"')  # names of vector elements are ignored
# ex02CollateLists(list(c("a", "b"), c(1, 2, 3))) --> ERROR (only character vectors should be given in the list)
# ex02CollateLists(list(list("a", "b", "c"))) --> ERROR (only char vectors in the list)
#
# Of course you can also solve this problem without using `ex01Collate` (we are just testing that
# this function behaves *as if* ex01Collate is being called) but that would probably be more work.
#
# You may find `do.call` to be useful here, as well as `lapply` or `vapply`.
ex02CollateLists <- function(inputs) {
  assertList(inputs, types = "character", null.ok = TRUE)
  if (length(inputs) == 0) {
    return(character(0))
  }
  results <- lapply(inputs, function(x) {
    if (length(x) == 0) {
      return("")
    } else {
      return(do.call(ex01Collate, as.list(x)))
    }
  })
  return(unlist(results))
}


# Write a function that takes in a `matrix` and returns a vector indicating, for each row of the input,
# the column of the first nonzero element.
# e.g.
#
# 0 1 0
# 1 2 3
# 0 0 1
# --> c(2, 1, 3)
# (the first row's first nonzero element is in the 2nd column, the second row has the first column nonzero,
# and in the last column the last column is the only nonzero one)
#
# Your function should have one input:
# * `mat`: a matrix with numeric
#
# Examples:
# ex03RowFirsts(diag(4)) --> c(1, 2, 3, 4)
# ex03RowFirsts(matrix(c(0, 1, 0, 1, 2, 0, 0, 3, 1), nrow = 3)) --> c(2, 1, 3)
#
# You can assume that every row has at least one nonzero element.
#
# You may find the `apply` function useful here.
ex03RowFirsts <- function(mat) {
  assertMatrix(mat, mode = "numeric", any.missing = FALSE)
  assertFlag(all(apply(mat, 1, function(row) any(row != 0))))
  return(apply(mat, 1, function(row) which(row != 0)[[1]]))
}



# write a function that returns the diagonal of a data.frame.
#
# Your function should have one argument
# * `df` a data.frame with arbitrary rows and columns
# the return value should be an unnamed `list` of the diagonal elements of `df`.
# The diagonal elements are those elements, where the row index and column index
# coincide; this is defined even for non-square `data.frame` inputs; the length
# of the output is `min(nrow(df), ncol(df))`.
#
# Example:
# ex04TableDiag(data.frame(a = c(1, 2), b = c("x", "y"), stringsAsFactors = FALSE))
# --> list(1, "y")
# ex04TableDiag(matrix(c(1, 2, 2, 1), nrow = 2)) --> ERROR (not a dataframe)
# ex04TableDiag(iris)
# --> list(5.1, 3.0, 1.3, 0.2, factor("setosa", levels = c("setosa", "versicolor", "virginica")))
# ex04TableDiag(iris[FALSE, ]) --> list()
# # (zero-row data.frame has lengt-0 diagonal)
ex04TableDiag <- function(df) {
  assertDataFrame(df)
  if (nrow(df) == 0 || ncol(df) == 0) {
    return(list())
  }
  len <- min(nrow(df), ncol(df))
  diagonal <- vector("list", len)
  for (i in 1:len) {
    diagonal[[i]] <- df[i, i]
  }
  return(diagonal)
}
