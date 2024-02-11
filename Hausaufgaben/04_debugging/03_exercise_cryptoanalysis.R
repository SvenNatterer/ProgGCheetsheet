# The "Caeser" Cipher is a method of encrypting text that has sometimes been used,
# supposedly by the namesake.
# It uses a `plaintext` and a `key`, which is a single letter, and generates encrypted text.
# Each letter in the plaintext is converted to a number corresponding to its position in the alphabet.
# Likewise, the key is converted to a number.
# The value of the key is added to the letter values of the plaintext and converted back
# to letters (modulo the number of letters). Decryption can be done by subtracting,
# instead of adding, the key.
#
# We are working with the alphabet of the space character " " (value 0) and the 26 capital
# letters of the latin alphabet (contained in the R variable `LETTERS`, numbered 1..26.)
#
# Example:
# plaintext     = I LOVE MY PARENTS KYLIE MINOGUE AND KERMIT THE FROG
# key           = L
# plaintext converted to numbers:
#               = c( 9,  0, 12, 15, 22,  5,  0, 13, 25,  0, 16,  1, 18,  5, 14, 20, 19,  0, .....
# value of the key: 12
# sum of these two
#               = c(21, 12, 24, 27, 34, 17, 12, 25, 37, 12, 28, 13, 30, 17, 26, 32, 31, 12, .....
# some of the values are larger than 26, so we have to wrap them back around (modulo 27 -- note
# how we have 27 letters: 26 in the alphabet plus the space!):
# sum of plaintext and key with modulo:
#               = c(21, 12, 24,  0,  7, 17, 12, 25, 10, 12,  1, 13,  3, 17, 26,  5,  4, 12, .....
# converted back to letters:
#               = ULX GQLYJLAMCQZEDLWJXUQLYUZ SFQLMZPLWQCYUELETQLRC S
#
# A few more examples:
# plaintext: COME LETS EAT GRANDPA
# key:       A
# result:    DPNFAMFUTAFBUAHSBOEQB
# plaintext: I LIKE COOKING MY FRIENDS AND MY FAMILY
# key:       " " (space)
# result:    I LIKE COOKING MY FRIENDS AND MY FAMILY
#            (no encryption because " " corresponds to 0, so values are not changed)
# Implement a function that performs Caesar encryption or decryption, taking one
# `plaintext` and one `key` parameter. Both are Strings.
# Input:
# - `plaintext`: `character(1)`
# - `key`: `character(1)`
# - `decrypt`: `logical(1)` with *default value* `FALSE`.
# You should check that `key` is exactly a single uppercase letter from `LETTERS` (or single space)
# and throw an error if not. Likewise, if `plaintext` contains anything other than uppercase
# letters or spaces, an error should be thrown (note the empty string `""` should be permitted).
# Also your function should take a logical `decrypt` argument. If `decrypt` is TRUE,
# then decryption should be performed instead of encryption (subtraction instead of addition).
# Default should be FALSE.
# You may find the `match()` function and the modulo operator %% useful.
#
# Be aware that this cipher is very insecure and you should not use it to actually hide information,
# as you will find in the following exercises.
# You can read more about the cipher at Wikipedia: <https://en.wikipedia.org/wiki/Caesar_cipher>
ex01CaesarCipher <- function(plaintext, key, decrypt = FALSE) {
  assertCharacter(plaintext, len = 1)
  assertCharacter(key, len = 1)
  keyVal <- ifelse(key == " ", 0, match(key, LETTERS))
  plaintextValues <- if (nchar(plaintext) > 0) {
    match(strsplit(plaintext, "")[[1]], c(" ", LETTERS)) - 1
  }
  return(plaintextValues)
}




# The following concerns itself with breaking the Caesar Cipher.
# This means we are writing a function that, when given a long enough
# encrypted text, can infer the encryption key and decrypt the message
# automatically.

# To efficiently break encryption, we need an indicator that tells us whether
# the decrypted text is, in fact, a plain text that would be interesting.
#
# Write a function that estimates the log-likelihood that a given text is, in
# fact, a non-encrypted plain text, using the distribution of letters.
# Input:
# - text: A `character(1)` string made up of upper case letters and space
# Return: a scalar `numeric` giving the log likelihood of a given text. It can
# be calculated as the sum of the log likelihoods of individual letters in
# the text. The likelihoods of individual letters for the english language is
# given in the following table (based on an average word length of 4.79,
# as well as on the table in <https://en.wikipedia.org/wiki/Letter_frequency>):
letterfrequencies <- 1 / 100 * c(
  A = 6.756, B = 1.234, C = 2.302, D = 3.518, E = 10.508, F = 1.843, G = 1.667,
  H = 5.041, I = 5.763, J = 0.127, K = 0.639, L = 3.330, M = 1.990, N = 5.583,
  O = 6.210, P = 1.596, Q = 0.079, R = 4.953, S = 5.234, T = 7.492, U = 2.282,
  V = 0.809, W = 1.952, X = 0.124, Y = 1.633, Z = 0.061, ` ` = 17.272)
# Example results of this are:
# ex02TextLikelihood("COME LETS EAT GRANDPA")
# #> -59.58358
# ex02TextLikelihood("DQPFBOFVVAGDUBJSCQERD")
# #> -86.87443
# Note that the log likelihood is larger (i.e. less negative) for the true
# english language text, and smaller for the encrypted text.
ex02TextLikelihood <- function(text) {
  assertCharacter(text, len = 1, any.missing = FALSE)
  assertSubset(strsplit(text, "")[[1]], c(LETTERS, " "))
  length <- nchar(text)
  text <- strsplit(text, "")
  sum(vapply(text, function(x) log(letterfrequencies[x]), double(length)))
}




# Write a function that estimates the most likely key for a given ciphertext.
# This is the key that generates a text that is most likely according to
# ex02TextLikelihood. The possible keys are the 26 letters as well as the space
# (`" "` -- this one does not change the text). You can make use of your
# `ex01CaesarCipher()` function with `decrypt = TRUE` to find out the
# decrypted text for a key.
#
# Input:
# - `ciphertext`: A `character(1)` string made up of upper case letters and space
# Return: a list with two entries:
# - `key`: `character(1)` giving an upper case letter or space.
# - `log.likelihood`: `numeric(1)` giving the log likelihood of the text when
#   decrypting with this key.
# The result for the ciphertext
#   "NDJPBJHIPLDG PILUCINPVDJGPXDJGPHXYVIHPIXYHPBDCIX"
# should, for example, be
# #> list(key = "P", log.likelihood = -142.08608554750788)

ex03BreakCaesar <- function(ciphertext) {

}






# Someone tries to hide his encrypted text by combining it with other text that
# is made up of actual random letters, for example:
example.hidden <- c(
  "VBXLMQEYX XCKDCCLWZKWUUYBIFEDUQOMFNBLCWIARSCMMGK",
  "SXEUTM SOGYPLPPBRRJQWYKRCSMITBULZJVSWZTKEVFB JLB",
  "JTILBWAPELZEPD SNFTWEFMPSICAPJNMCASVZTKYGPDEGQQW",
  "ZDMEETHKBUZEPI  ZCKTDVHYH NLIOGENKDMVDUEOHXXTZNL",
  "NDJPBJHIPLDG PILUCINPVDJGPXDJGPHXYVIHPIXYHPBDCIX",
  "SOOKGBYLFOSXVGTOZNJMQMRHFKRRK UQPNLOUHFZDXILX ES",
  "NQKLUWOVNHAB F LSOWGOT E JSWR ZCWOCQIJLRRVYJHJTT",
  "EI UENSNER KELJFNVHTWNICT FWYJINYYXYEEGPJBSMJBWO",
  "MBDVGJDKZILLIOLINAAAMGJI  HGDOVPLQQCYSG SVJOWQQM",
  "WVRHDDWBAQRJCJNYJLRPVENPGOKBM CXLQHJPJBAXBCNLDLJ"
)
# The 5th element of this example vector is the ciphertext from the ex03 example
# above, the others are noise, i.e. letters sampled uniformly at random.
#
# Write a function that takes one input:
# - `hidden`: a `character` vector with no missing values and at least one element.
# Return: a `character(1)`: the decrypted text of the one element that is not random noise.
#
# The function can identify the true element by calling `ex03BreakCaesar` and
# deciding based on `log.likelihood` -- the highest (least negative) value
# is most likely a true sentence. The identified ciphertext can then be used to
# decrypt the identified element.
#
# (You may assume that if `hidden` is a `character`, then all its elements have the
# same length (otherwise the `log.likelihood` would need to be treated differently.)
#
# With input c("TSGWUQSHQURORLFBUUFLY", "DPNFAMFUTAFBUAHSBOEQB", "LQMOYL OPFJIFHBLQPYPM")
# the result should be "COME LETS EAT GRANDPA".
ex04BreakHidden <- function(hidden) {

}