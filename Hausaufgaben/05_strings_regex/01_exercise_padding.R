# Write a function that pads non-negative integer numbers to the same length by
# preprending 0s when necessary.
#
# Input:
# - `numbers`: A `numeric` containing positive integer numbers.
#
# Return: A `character` with the same length as `numbers`, containing strings
# that each have as many digits as the longest number in `numbers`.
#
# > ex01PadNumbers(c(1, 2, 3))  # c("1", "2", "3")
# > ex01PadNumbers(c(1, 2, 30))  # c("01", "02", "30")
# > ex01PadNumbers(c(100, 2, 30))  # c("100", "002", "030")
# > ex01PadNumbers(numeric(0))  # character(0)
# > ex01PadNumbers(c(0, 0, 0))  # c("0", "0", "0")
# > ex01PadNumbers(NA)  # NA
# > ex01PadNumbers(c(1, NA, 100))  # c("001", NA, "100")
#
# `numbers` may contain missing values, which should result in `NA` values. Make
# sure that your function does not return a string containing "NA" here.
#
# Your function should assert that numbers are non-negative values.
#
# `spintf()` could help you here.
ex01PadNumbers <- function(numbers) {
  assert_integerish(numbers, lower = 0, any.missing = TRUE, null.ok = TRUE)
  if (length(numbers) == 0 || all(is.na(numbers))) {
    return(as.character(numbers))
  }
  maxLenght <- max(nchar(as.character(na.omit(numbers))))
  padNumber <- function(x, maxLenght) {
    if (is.na(x)) return(NA)
    sprintf(paste0("%0", maxLenght, "d"), x)
  }
  paddedNumbers <- vector("character", length(numbers))
  for (i in seq_along(numbers)) {
    paddedNumbers[i] <- padNumber(numbers[i], maxLenght)
  }
  return(paddedNumbers)
}

# Write a function that pads numbers in file-names.
#
# Your function is given a vector of file names that may each contain at most
# one number. The numbers that occur in all files should be padded to the same
# length, as done in ex01PadNumbers.
#
# Padding numbers like this can be useful when sorting files alphabetically, to
# make sure that the same files that differ by a number are sorted correctly.
#
# Input:
# - `filenames`: A `character` containing file-names.
#
# Return: A `character` with the same length as `filenames`, containing
# filenames with padded numbers, if any.
#
# > ex02PadFiles("file.pdf")  # "file.pdf"
# > ex02PadFiles(c("podcast_ep1.mp3", "podcast_ep3.mp3", "podcast_ep22.mp3"))
#   # c("podcast_ep01.mp3", "podcast_ep03.mp3", "podcast_ep22.mp3")
# > ex02PadFiles(c("file1.pdf", "file10.pdf"))  # c("file01.pdf", "file10.pdf")
# > ex02PadFiles(c("file.pdf", "file10.pdf"))  # c("file.pdf", "file10.pdf")
# > ex02PadFiles(c("100-music.pdf", "file-10.pdf", "help.txt"))
#   # c("100-music.pdf", "file-010.pdf", "help.txt")
# > ex02PadFiles(character(0))  # character(0)
#
# Your function should not accept missing values.
#

ex02PadFiles <- function(filenames) {
  extractNumbers <- function(str) {
    matches <- regmatches(str, gregexpr("\\d+", str))
    if (length(matches[[1]]) > 0) {
      return(as.numeric(matches[[1]]))
    } else {
      return(numeric(0))
    }
  }
  assert_character(filenames, any.missing = FALSE, len = length(filenames))
  if (length(filenames) == 0) {
    return(character(0))
  }
  # Extrahieren und Formatieren der Zahlen in jedem Dateinamen
  max.length <- 0
  numbers.list <- lapply(filenames, extractNumbers)
  for (numbers in numbers.list) {
    if (length(numbers) > 0) {
      max.length <- max(max.length, max(nchar(as.character(numbers))))
    }
  }
  padded.filenames <- filenames
  for (i in seq_along(filenames)) {
    if (length(numbers.list[[i]]) > 0) {
      for (number in numbers.list[[i]]) {
        padded.number <- sprintf(paste0("%0", max.length, "d"), number)
        padded.filenames[i] <- sub(as.character(number), padded.number, padded.filenames[i])
      }
    }
  }
  return(padded.filenames)
}