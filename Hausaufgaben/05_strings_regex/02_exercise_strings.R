# Describe a parent-child relationship.
#
# This function gets three arguments:
# - `parent` (character vector length 1, i.e. string)
# - `male` (length 1 `logical` vector, indicating whether `parent` is male)
# - `child` (character vector length 1, i.e. string)
# Return a single string describing this relationship in human words.
# E.g. parent = "Eric", male = TRUE, child = "Bob" --> `Eric is the father of "Bob".`
#      parent = "Herbert", male = TRUE, child = "Klaus Dieter" --> `Herbert is the father of "Klaus Dieter".`
#      parent = "Hildegard", male = FALSE, child = "Adelheid" --> `Hildegard is the mother of "Adelheid".`
#      parent = "Y", male = FALSE, child  = "A"     --> `Y is the mother of "A".`
# Watch out for punctuation (quotation around children but not parent, period at the end).
ex01Children <- function(parent, male, child) {
  # Assert the input types and lengths
  assert_character(parent, len = 1, any.missing = FALSE)
  assert_logical(male, len = 1)
  assert_character(child, len = 1, any.missing = FALSE)
  # Determine the role based on the gender
  role <- if (male) "father" else "mother"
  # Construct the description
  description <- sprintf("%s is the %s of \"%s\".", parent, role, child)
  return(description)
}


# Now reverse the above:
# Given a string `sentence`, extract the `parent`, `male`, and `child` arguments
# from above in that order as a named list.
# Input:
# - `sentence`: A `character(1)` string containing a sentence to analyse.
# E.g. 'Eric is the father of "Bob".' --> list(parent = "Eric", male = TRUE, child = "Bob")
#      'Herbert is the father of "Klaus Dieter".' --> list(parent = "Herbert", male = TRUE, child = "Klaus Dieter")
# (You can rely on the sentence structure, but do assert that `sentence` is a non-missing string.)
# You should be able to handle the case that the child's name contains quotation marks:
# 'Gudrun is the mother of "Rosamunde ("Rosi")".'
#   --> list(parent = "Gudrun", male = FALSE, child = 'Rosamunde ("Rosi")')
ex02ChildrenInverse <- function(sentence) {
  # Assert that sentence is a non-missing string
  assert_character(sentence, len = 1, any.missing = FALSE)
  # Regex pattern to extract the parent name and child name
  pattern <- "^(.*) is the (father|mother) of \"(.*)\"\\.$"
  # Use regmatches to extract the matched groups
  matches <- regmatches(sentence, regexec(pattern, sentence))
  # Extract the parent, role, and child from matches
  if (length(matches[[1]]) == 4) {
    parent <- matches[[1]][[2]]
    role <- matches[[1]][[3]]
    child <- matches[[1]][[4]]
    # Determine gender based on the role
    male <- role == "father"
    # Return as a named list
    return(list(parent = parent, male = male, child = child))
  } else {
    stop("The sentence does not match the expected format.")
  }
}



# Write a function that extracts all words from a text that are capitalized but
# not at the beginning of a sentence. Words can be returned in any order, but
# each word should only be returned at most once.
#
# Your function should take one argument:
#  - `text`: `character(1)` scalar string, the text from which to extract
#    capitalized words.
# Your function should return a `character` vector: The capitalized words found
# in the string.
#
# Example results:
#
#> ex03ProperNoun("Proper Nouns are usually Capitalized.")
# --> "Nouns"  "Capitalized"
#> ex03ProperNoun("proper nouns are usually Capitalized. This is, Proper for proper nouns.")
# --> "Capitalized" "Proper"
#> ex03ProperNoun("The IBM5100 Portable Computer was one of the first portable computers.")
# --> "IBM5100" "Portable" "Computer"
#> ex03ProperNoun("IBM5100 is the name of one of the first portable computers.")
# --> character(0)
# You can assume that the text only contains uppercase and lowercase letters
# from the latin alphabet, as well as digits, points (".") and commas (",").
# Words are separated from each other by exactly one space, and are possibly
# followed by a point (which starts a new sentence, so capitalization of the
# following word is ignored) or a comma (which does *not* start a new sentence).
ex03ProperNoun <- function(text) {
  assertString(text, na.ok = FALSE)
  capitalized.words <- gregexpr("(?<![.!?]\\s|^)\\b\\p{Lu}\\w*\\b", text, perl = TRUE)
  result <- unique(unlist(regmatches(text, capitalized.words)))
  return(result)
}