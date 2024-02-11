library("checkmate")
library("data.table")
library("R6")
library("numbers")

sumEvensMinusOdds <- function(x) {
  assertIntegerish(x)
  even <- x[x %% 2 == 0]
  odd <- x[x %% 2 == 1]
  return(sum(even) - sum(odd))
}

extractListEntries <- function(l, extract) {
  assertList(l, type = "list")
  assertCharacter(extract)
  lapply(l, function(x) unname(x[[extract]]))
}

sortByNumberMissings <- function(mat) {
  assertMatrix(mat)
  mat[order(rowSums(is.na(mat))),, drop = FALSE]
}

removeAllNonconforming <- function(x, fs) {
  assertList(fs, type = "function")
  if (is.list(x)) {
    for (i in 1:length(fs)) {
        x<- x[lapply(x, function(el) fs[[i]](el)) == TRUE]
    }
  }else {
    for (i in 1:length(fs)) {
      x <- x[fs[[i]](x) == TRUE]
    }
  }
return(x)
}

longestRepeatedWord <- function(s) {
  assertCharacter(s)
  matches <- gregexpr("[a-zA-Z]+", s, perl = TRUE) #RegEx kann natürlich angepasst werden
  found <- regmatches(s, matches)
  tabled <- table(found)
  found <- tabled[tabled != 1]
  if (length(found) == 0) return("No repeated words.") 
  found <- names(found)
  length <- as.integer()
  for (i in 1:length(found)) {
     length[i] <- nchar(found[i])
  }
  max <- which.max(length)
  found[max]

}

randomSearch <- function(f, n) {
  assertFunction(f)
  assertIntegerish(n)
  samplex <- runif(n, min = 0, max = 1)
  sampley <- runif(n, min = 0, max = 1)
  samplez <- runif(n, min = 0, max = 1)
  funmin <- f(samplex, sampley, samplez)
  min <- which.min(funmin)
  return(list(x = samplex[min], y = sampley[min], z = samplez[min], f = funmin[min] ))
}


tbl <- fread(text = "
id.sensor, value,            datetime
        a,     1, 2024-01-01T12:00:01
        b,    10, 2024-01-01T12:00:02
        c,   100, 2024-01-01T12:00:03
        c,   300, 2024-01-01T12:00:04
        a,     0, 2024-01-01T12:00:05
        a,    -1, 2024-01-01T12:00:06
        a,   -20, 2024-01-01T12:00:07
        a,     0, 2024-01-01T12:00:08
")


removeOutliers <- function(data) {
  assertDataTable(data)
  outliersrem <- data[, count := .N, by = id.sensor][count > 4, .(value, datetime, lower = quantile(value, 0.25) - 1.5*IQR(value), upper = quantile(value, 0.75) + 1.5*IQR(value)), by = id.sensor ]  
  # outliersrem <- data[, .(value, datetime, lower = quantile(value, 0.25) - 1.5*IQR(value), upper = quantile(value, 0.75) + 1.5*IQR(value)), by = id.sensor]
  outliers <- outliersrem[value < lower | value > upper]
  data <- data[!outliers, on = c("id.sensor", "value", "datetime")]
  data[, .(id.sensor, value, datetime)]
}

isMatrixPath <- function(mat, path) {
  assertMatrix(mat)
  assertCharacter(path)
  path <- strsplit(path, "")
  if (length(path[[1]]) == 0) return(TRUE)
  for (i in 1:(length(path[[1]]) - 1)){
    startpos <- which(apply(mat, c(1,2), function(x) x == path[[1]][i]), arr.ind = TRUE)
    nextpos <- which(apply(mat, c(1,2), function(x) x == path[[1]][i + 1]), arr.ind = TRUE)   
    diff <- nextpos - startpos
    if (diff[1] > 1 | diff[2] > 1) return(FALSE)
    if(diff[2] == 1 & diff[1] == 1) return(FALSE)
    if(diff[2] == 0 & diff[1] == 0) return(FALSE)
  }
  return(TRUE)
}

Memoiser <- R6Class('Memoiser',
                    public = list(
                      initialize = function(func){
                        private$.func <- func
                      },
                      call = function(x){
                        # browser()
                        name <- format(x, scientific = FALSE)
                        if (name %in% names(private$.value)) return(private$.value[[name]])
                        private$.value[[name]] <- list(private$.func(x))
                        # private$.value[name] <- list(private$.func(x))
                        private$.value[[name]]
                        print(private$.value)

                      }
                    ),
                    
                    private = list(
                      .func = NULL,
                      .value = list()
                    )
                    )


Fraction <- function(numerator, denominator = 1){
  assertIntegerish(numerator)
  assertIntegerish(denominator)
  both <- GCD(numerator,denominator)
  numerator <- numerator / both
  denominator <- denominator / both
  if (denominator == 0) return("Division by Zero")
  if (denominator < 0){
    numerator <- -numerator  
    denominator <- -denominator
  } 
  fraction <- list(numerator = numerator, denominator = denominator)
  class(fraction) <- "Fraction"
  return(fraction)
}

print.Fraction <- function(x, ...) {
  cat(sprintf("A Fraction: %d // %d", x$numerator, x$denominator))
  invisible(x)
}

plus <- function(e1, e2) {
  assertClass(e2, "Fraction")
  UseMethod("plus")
}

times <- function(e1, e2) {
  assertClass(e2, "Fraction")
  UseMethod("times")
}

plus.Fraction <- function(e1, e2) {
  if (e1$denominator == e2$denominator) {
    e3n <- e1$numerator + e2$numerator
    e3d <- e1$denominator
    Fraction(e3n, e3d)
  }else {
    e3d <- e1$denominator * e2$denominator
    num1 <- e1$numerator * e2$denominator
    num2 <- e2$numerator * e1$denominator
    e3n <- num1 + num2
    Fraction(e3n, e3d)
  }
}

times.Fraction <- function(e1, e2) {
  e3n <- e1$numerator * e2$numerator
  e3d <- e1$denominator * e2$denominator
  Fraction(e3n, e3d)
}

plus.numeric <- function(e1, e2) {
  assertInt(e1, tol = 0)
  e1 <- Fraction(e1)
  plus(e1, e2)
}

times.numeric <- function(e1, e2) {
  assertInt(e1, tol = 0)
  e1 <- Fraction(e1)
  times(e1, e2)
}

numberToWords <- function(n) {
  assertIntegerish(n, lower = 0, upper = 1e9)
  number <- as.character(n)
  one <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  teens <- c("eleven", "twelve", "therteen", "fourteen", "fifhteen", "sixteen", "seventeen", "eighteen", "nineteen")
  tens <- c("ten","tewenty","thirty","fourty","fifty","sixty","seventy","eighty", "ninety")
  
  tiplet <- function(x) {
    
  }
}



# Exercise 1: Multi-Index

#Write a function that takes a positive integer `n` as argument, as well as various other elements, and returns an unnamed list of `n`th elements of all these other arguments.
#For each argument that has fewer than `n` elements, `NULL` should be returned instead.
#Use checkmate to assert that `n` is a positive integer-valued numeric.

multiIndex <- function (n, ...) {
  assertCount(n, positive = TRUE)
  arg <- list(...)
  if (length(arg) == 0) return (list())
  result <- list()
  for (i in 1:length(arg)) {
    if (length(arg[[i]]) < n) result[i] <- list(NULL)
    else result[i] <- list(arg[[i]][[n]])
    
  }
  result
}



# Exercise 2: Function Sum

#Write a function that takes two functions, `f()` and `g()` as input and generates a single function as output.

#When the returned function is called with arguments `(...)`, it should return the result of `f(...) + g(...)`.

sumFunction <- function(f, g) {
  assertFunction(f)
  assertFunction(g)
  function(...) f(...) + g(...)
}

#Write a function that takes a `data.table` `data` with columns `name`, `x` and `y`, where `x` and `y` are coordinates.
#It should return a `data.table` with the same number of rows, with columns `name` and `neighbour`.
#`name` should be the same as in the `data`.
#`neighbour` should be the entry of `data` that has the shortest euclidean distance to `name` (but is not `name` itself).

nearestNeighbour <- function(data) {
  assertDataTable(data)
  data[, neighbour := name, by = name]
}

# library("data.table")
# data1 <- data.table(
#   name = c("A", "B", "C", "D"),
#   x = c(1, 2, 4, 5),
#   y = c(1, 3, 5, 7)
# )
# 
# data2 <- data.table(
#   name = c("A", "B", "C", "D", "E"),
#   x = c(1, 2, 2, 5, 5),
#   y = c(1, 2, 3, 5, 6)
# )
# 
# data3 <- data.table(
#   name = c("A", "B", "C", "D"),
#   x = c(1, 2.5, 3, 3),
#   y = c(1, 2, 3, 3.2)
# )
# library("ggplot2")
# ggplot(rbind(
#   copy(data1)[, d:='data1'],
#   copy(data2)[, d:='data2'],
#   copy(data3)[, d:='data3']),
#   aes(x = x, y = y, label = name)) +
#   geom_label() + facet_grid(cols = vars(d)) + coord_fixed() + theme_bw()


nearestNeighbour(data1)
#>    name neighbour
#> 1:    A         B
#> 2:    B         A
#> 3:    C         D
#> 4:    D         C
nearestNeighbour(data2)
#>    name neighbour
#> 1:    A         B
#> 2:    B         C
#> 3:    C         B
#> 4:    D         E
#> 5:    E         D
nearestNeighbour(data3)
#>    name neighbour
#> 1:    A         B
#> 2:    B         C
#> 3:    C         D
#> 4:    D         C