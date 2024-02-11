
# This exercise is concerned with basic operations on matrices and data frames.
# We are using only methods contained in the standard R installation; don't use
# tidy here.

# Imagine a square matrix as a chess board, where the top left field is white, the
# second row left field as well as the first row, second column field are black, etc.
#
# Given a square matrix as input, return the sum of the values of all elements that
# would be on black fields:
#
# 1 2
# 3 4  --> 2 and 3 are on 'black' fields --> result 5
#
# 1 2 3
# 4 5 6 --> 2 + 4 + 6 + 8 --> 20
# 7 8 9
#
# 1 0 0 0
# 0 1 0 0
# 0 0 1 0 --> 0
# 0 0 0 1
#
# Make sure this works with the trivial matrix with only one element!
# The `row()` and `column()` function are useful here.
ex01ChessboardSum <- function(mat) {
  sum(mat[(row(mat) + col(mat)) %% 2 == 1])
}

# Given a matrix as input, return the coordinates (row and column) of the minimum element,
# as a two element numeric vector. If multiple elements are tied for minimum, you
# can return the coordinates of any of them.
#
# E.g.
#
# 1 2 3
# 4 5 6 --> c(1, 1)
# 7 8 9
#
#  0  0
#  0  0 --> c(3, 1)
# -3  0
#
# 1 0 --> both c(2, 1) and c(1, 2) would be correct
# 0 1
ex02MatrixWhichMin <- function(mat) {
  linearIndex <- which.min(mat)
  matrixIndices <- arrayInd(linearIndex, dim(mat))
  return(matrixIndices[1, ])
}

# Your function is given a `data.frame` with columns that can each be of any of numeric, logical
# or factor type. You are supposed to return a `data.frame` containing only columns selected
# by type. For this you are given the "type" argument: a character vector containing a single
# argument containing one of c("numeric", "logical", "character").
#
# E.g. * data = iris (the "iris" dataset as contained in R)
#        type = "numeric"
#        --> return
#                     Sepal.Length Sepal.Width Petal.Length Petal.Width
#                   1          5.1         3.5          1.4         0.2
#                   2          4.9         3.0          1.4         0.2
#                   3          4.7         3.2          1.3         0.2
#                   .........
#      * data = iris
#        type = "logical"
#        --> return a data frame with 0 columns and 150 rows
#      * data = data.frame(a = c(1, 2, 3), b = c("alpha", "beta", "gamma"))
#        type = "character"
#        --> return
#                        b
#                  1 alpha
#                  2  beta
#                  3 gamma
#    ... etc.
# The order of all included columns should not be changed with respect to their order in the input.
# Be aware that there are could be other columns that are not one of numeric, logical, or character,
# in particular "factor" columns; they should always be discarded.
ex03SelectDF <- function(data, type) {
  selectedData <- data.frame(matrix(ncol = 0, nrow = nrow(data)))
  for (colName in names(data)) {
    if (type == "numeric" && is.numeric(data[[colName]]) && !is.factor(data[[colName]])) {
      selectedData[[colName]] <- data[[colName]]
    } else if (type == "logical" && is.logical(data[[colName]])) {
      selectedData[[colName]] <- data[[colName]]
    } else if (type == "character" && is.character(data[[colName]])) {
      selectedData[[colName]] <- data[[colName]]
    }
  }
  return(selectedData)
}

# You are given a `data.frame` with some data about study participants. Its columns
# are numeric, some of which have missing values (NA). Your task is to *impute* the missing values,
# i.e. set them to putative numeric values inferred from the other participants. In particular,
# you are to set the missing values in each variable to the *mean value* of all the *non-missing values*
# in that variable.
#
# E.g. for the input
# > data.frame(
#     height = c(178, 185, NA, 157, NA, 174), weight = c(95, 90, 99, 70, 77, NA),
#     age = c(23, NA, NA, NA, 21, 22))
# the returned value should be
# > data.frame(
#     height = c(178, 185, 173.5, 157, 173.5, 174), weight = c(95, 90, 99, 70, 77, 86.2),
#     age = c(23, 22, 22, 22, 21, 22))
#
# The return value should be a data.frame with the same columns as the input and with the missing values
# imputed. You can assume that there is at least one value present in each variable.
# Your input may have different column names.
ex04Imputation <- function(data) {
  imputedData <- data
  for (i in seq_along(data)) {
    column <- data[[i]]
    if (is.numeric(column)) {
      missingIndices <- is.na(column)
      columnMean <- mean(column, na.rm = TRUE)
      column[missingIndices] <- columnMean
      imputedData[[i]] <- column
    }
  }
  return(imputedData)
}