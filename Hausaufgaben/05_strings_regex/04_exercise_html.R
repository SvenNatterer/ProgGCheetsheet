# Write a function that finds all URLs in a text. Input is a long string `input`
# containing text, some parts of which are URLs.
#
# A URL is the sequence 'http://' or 'https://' (where the http/https-part
# indicates the PROTOCOL) if it is not immediately preceded by another letter
# ("abchttp://" does not start a url, but "abc http://" may), followed by the
# DOMAIN NAME (containing two or more "labels" separated by a dot, where each
# label consists of letters, numbers or minus-signs but does not start with a
# minus-sign; labels contain at least one character), followed by "/", followed
# by a PATH. We limit ourselves to PATHs that contain only latin letters,
# numbers, minus-signs and "/"s (slashes). All these rules are case-insensitive,
# so a URL may start with HTTP or hTTp.
#
# In short: <PROTOCOL>://<DOMAIN NAME>/<PATH>
#
# Given an input String, return a `data.frame` with the character columns
# "protocol", "domainname" and "path", containing the information about all the
# URLs found *in the order in which they were found*. (The same URL may occur
# multiple times and should then be listed in the data frame multiple
# times). Make sure you don't include things that are not URLs, i.e. which don't
# satisfy all the rules.
#
# Input:
# - `input`: A `character(1)` containing the input string.
#
# Example input:
# > "https://www.google.com/ is probably the most popular url, sometimes used ashttp://www.google.com/ if one forgets
#    to use the more secure https:// protocol. Another URL that is popular is http://youtube.com which is owned
#    by the same company. On some computers, it is possible to find a website at http://localhost/ if the
#    computer runs a webserver. A URL used in this course was HTTPS://GITHUB.COM/PROGR-2324/01_GIT."
# Output:
# > data.frame(
#     protocol = c("https", "HTTPS"),
#     domainname = c("www.google.com", "GITHUB.COM"),
#     path = c("", "PROGR-2324/01"))
# Example input:
# > "this text does not contain a url."
# Output:
# > data.frame(protocol = character(0), domainname = character(0),
#     path = character(0))
#
# Notes: many of the occurrences of http.... in the example do not count because
# they are either preceded directly by a letter, have no "/" following the
# domain name, or have a domain name with less than two labels. The path of the
# last URL is cut off early because we don't consider underscores as parts of
# the path.
#
# You should probably look into `regexpr`, `gregexpr`, `regexec`, `gregexec` and
# similar to solve this problem.
ex01UrlFinder <- function(input) {
  # Assert that the input is a single string and not NA
  assertCharacter(input, len = 1, any.missing = FALSE, null.ok = FALSE)
  # Updated regex for the URL
  regex <- "\\b(https?://)([a-zA-Z0-9][-a-zA-Z0-9]*(\\.[a-zA-Z0-9][-a-zA-Z0-9]*)+/)([-a-zA-Z0-9/]*)"
  # Find all matches of the regex
  matches <- gregexpr(regex, input, perl = TRUE, ignore.case = TRUE)
  # Extract the matches
  urls <- regmatches(input, matches)[[1]]
  # Initialize components
  protocols <- character(length(urls))
  domainnames <- character(length(urls))
  paths <- character(length(urls))
  # Process each URL
  for (i in seq_along(urls)) {
    parts <- strsplit(urls[i], "://|/")[[1]]
    protocols[i] <- parts[[1]]
    domainnames[i] <- parts[[2]]
    # Extract the entire URL after the protocol and domain
    urlRemainder <- substr(urls[i], nchar(parts[[1]]) + nchar(parts[[2]]) + 4, nchar(urls[[i]]))
    # Set the path, including trailing slash if it exists
    if (urlRemainder != "") {
      paths[i] <- substr(urlRemainder, 2, nchar(urlRemainder))
    } else {
      paths[i] <- ""
    }
  }
  # Create a data frame
  data.frame(protocol = protocols, domainname = domainnames, path = paths, stringsAsFactors = FALSE)
}





# Most websites are rendered from HTML, which is a markup language where
# ordinary text is augmented through "tags" such as '<p>', '<strong>', '<br>'
# etc., which informs the browser about how the given text should be displayed.
# Many tags are used in pairs, with 'opening' tags such as '<p>', and
# corresponding 'closing' tags ('</p>').
#
# One way to display a *table* in HTML is to use the '<table>'/'</table>' tags
# (around the whole table), in combination with the tags '<tr>'/'</tr>' (which
# enclose individual *t*able *r*ows) and '<td>'/'</td>' (which enclose
# individual cells inside a row; the abbrevation is for *t*able *d*ata). The
# header of a table can be separated from the rest by using '<thead>'/'</thead>'
# and '<tbody>'/'</tbody>' tags. Header cells then use '<th>'/'</th>' instead of
# '<td>'/'</td>'. A very simple representation of the table
# | x | y |
# +---+---+
# | 1 | a |
# | 2 | / |
# Could, for example, be
#
example.table <- " The following is a table. this text preceding the table is not part of the table.
<table>
  <thead>
    <tr>
      <th> x </th>
      <th> y </th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td> 1 </td>
      <td> a </td>
    </tr>
    <tr>
      <td> 2 </td>
      <td> / </td>
    </tr>
  </tbody>
</table>
"
#
# Where each line ends is non-essential here, and spaces around tags are
# ignored. The following is equivalent to the above:
#
# <table><thead><tr><th>x</th><th>y</th></tr></thead><tbody><tr><td>1
# </td><td>a</td></tr><tr><td>2</td><td>/</td></tr></tbody></table>
#
# (Although not necessary for this problem, if you are interested you can see
# https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table for a demo;
# you can e.g. go to https://www.ard-text.de/mobil/253, right-click and view
# source code to see how this used in the wild.)
#
# Write a function that, given the input 'html', a string containing an html
# table, extracts that table and returns a `data.frame` representing the table.
# The resulting `data.frame` should contain `character` typed columns that
# contain the data enclosed in '<tbody>'/'</tbody>', with column names according
# to the content in '<thead>'/'</thead>'. Text coming before and after the table
# should be ignored.
#
# Your function only needs to work with tables with cells that do not contain
# spaces, newlines, or the special characters "<" and ">" (i.e., the only "<"
# and ">" in the `html` string are the ones belonging to HTML tags). However,
# the content of the cells *may* contain "/". You can assume that the string is
# well-formed: There is exactly one table, with one '<thead>'/'</thead>' pair,
# containing one 'tr' pair, containing a number of 'th' pairs (at least
# one). '</thead>' is followed by a 'tbody'-pair with one or more 'tr' pairs,
# each containing as many 'td' pairs as there were 'th' pairs in 'thead'. Your
# function only needs to work with lowercase HTML-tags (i.e. no '<TABLE>',
# although that would be valid HTML elsewhere).
#
# The result for the `example.table` above should be
example.table.df <- data.frame(x = c("1", "2"), y = c("a", "/"))
#
# If you want to generate example cases for yourself, you can use knitr::kable:
#> example.input <-  knitr::kable(example.table.df, format = "html", align = NULL, row.names = FALSE)
# However, make sure you don't write this in the exercise R file, since the
# style checks won't accept the "::".
# The following input should also work:
#> example.input.stripped <- gsub(" |\n", "", example.input)
# Make sure that `ex02ParseHtmlTable(example.input)`,
# `ex02ParseHtmlTable(example.input.stripped)`, and
# `ex02ParseHtmlTable(example.table)` all give the same result, which should be
# `example.table.df`.
ex02ParseHtmlTable <- function(html) {
  assertString(html)
  th.positions <- regmatches(html, gregexpr("<th>(.*?)</th>", html, perl = TRUE))[[1]]
  td.positions <- regmatches(html, gregexpr("<td>(.*?)</td>", html, perl = TRUE))[[1]]
  column.names <- vapply(th.positions, function(x) trimws(gsub("<.*?>", "", x)), character(1))
  data <- vapply(td.positions, function(x) trimws(gsub("<.*?>", "", x)), character(1))
  num.columns <- length(column.names)
  num.rows <- length(data) / num.columns
  result.matrix <- matrix(data, ncol = num.columns, byrow = TRUE)
  dim(result.matrix) <- c(num.rows, num.columns)
  result.df <- as.data.frame(result.matrix, stringsAsFactors = FALSE)
  names(result.df) <- column.names
  return(result.df)
}