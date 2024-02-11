# Palindromes are words or sentences that, when read backwards, give the same sentence,
# ignoring punctuation, spacing, or capitalization. Examples are
# > "Was it a car or a cat I saw?"
# > "Go hang a salami, I'm a lasagna hog"
# Write a function that returns TRUE if a given string is a palindrome, FALSE otherwise.
# Input arguments:
# - `input`: A `character(1)`.
# The input value is a character vector with one element (written `character(1)` and what we
# call a "string"). You can rely on there only being latin letters, punctuation marks and
# spaces being present, and that the string contains at least one letter.
#
# The `strsplit()` and `toupper()` or `tolower()` functions may help you here.
ex01Palindrome <- function(input) {
  assertCharacter(input, len = 1)
  input <- gsub(" ","",input)
  input <- gsub(",","",input)
  input <- gsub("'","",input)
  input <- tolower(input)
  cleanInput <- gsub("[^a-zA-Z]", "", input)
  cleanInput <- toupper(cleanInput)
  cleanInput == paste(rev(strsplit(cleanInput, "")[[1]]), collapse = "")
  return(cleanInput)
}


# We call the positive integer N a 'palindromic integer' if reversing the
# (decimal) digits of that number does not change its value. Examples of
# palindromic integers are: 1, 121, 45854. Examples of integers that are not
# palindromic are 10, 12, 46814.
# Write a function that, given a number, computes the smallest palindromic
# number greater than `n`.
# Input arguments:
# - `n`: non-negative integer `numeric(1)` scalar.
#
#> ex03NextPalindrome(100) --> 101
#> ex03NextPalindrome(4243) --> 4334
# Note that the return value is always greater than the input:
#> ex03NextPalindrome(1) --> 2
#> ex03NextPalindrome(0) --> 1
#> ex03NextPalindrome(11) --> 22
ex02NextPalindrome <- function(n) {
  assert_number(n, lower = 0)
  assert_integerish(n, tol = 0)
  n <- max(0, as.integer(n))
  isPalindrome <- function(x) {
    sx <- as.character(x)
    return(sx == paste(rev(strsplit(sx, "")[[1]]), collapse = ""))
  }
  repeat {
    n <- n + 1
    if (isPalindrome(n)) {
      break
    }
  }
  return(n)
}
