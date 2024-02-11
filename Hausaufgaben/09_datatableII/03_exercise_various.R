
# Write a function that `cbind`s two data.tables, avoiding duplicate column
# names in a certain way.
# Input:
# - table.a: `data.table`
# - table.b: `data.table`
# Output: Return a `data.table` that contains all the columns of `table.a`,
# followed by the columns of `table.b`, in order. However, should a column
# name occur in `table.b` that is already in `table.a`, then the column
# should be suffixed by `_copy`. If that *also* leads to a name collision,
# it should be suffixed instead by `_copy.1` (or `_copy.2` etc.)
# You can rely on `table.a` and `table.b` for themselves having unique column
# names, but some of their column names may *already* end on `_copy` or
# `_copy.#`.
#
# If `table.a`. and `table.b` have a different number of rows, an error
# should be thrown.
# Example input:
table.a.example <- data.table(x = 1, y = 2, y_copy = 3, y_copy.1 = 4)
table.b.example <- data.table(z = 100, x = 200, y = 300, y_copy.1 = 400, y_copy.2 = 500)
# This should give the output
table.ab.example <- data.table(x = 1, y = 2, y_copy = 3, y_copy.1 = 4,
  z = 100, x_copy = 200, y_copy.3 = 300, y_copy.1_copy = 400, y_copy.2 = 500)
# (note that `y` gets renamed to `y_copy.3` because `_copy`, `_copy.1` and
# `_copy.2` are already taken; `y_copy.1` of `table.b` turns into
# `y_copy.1_copy`, because `y_copy.1` is already present in `table.a`.)
ex01CbindNameClash <- function(table.a, table.b) {
  assertDataTable(table.a)
  assertDataTable(table.b)
  if (nrow(table.a) != nrow(table.b)) {
    stop("Die Tabellen haben eine unterschiedliche Anzahl von Zeilen")
  }
  commonNames <- intersect(names(table.a), names(table.b))
  for (name in commonNames) {
    newName <- name
    suffix <- 0
    while (newName %in% names(table.a) || newName %in% names(table.b)) {
      suffix <- suffix + 1
      newName <- paste0(name, "_copy", ifelse(suffix > 1, paste0(".", suffix - 1), ""))
    }
    setnames(table.b, old = name, new = newName)
  }
  result <- cbind(table.a, table.b)
  return(result)
}

# Write a function that removes duplicate entries according to some columns.
# Input:
# - `table`: a `data.table` with the columns `year`, `month`, `day`, and
#   arbitrarily many more columns additionally to that.
# Output: The input `data.table` where duplicate entries according to the
# `year`, `month`, `day` columns are removed. I.e. for each such indicated date,
# the resulting table should only contain the *last* line with that date.
# The lines that are not removed should remain in order.
dup.table <- rbindlist(list(
    list(year = NULL, month = NULL, day = NULL, reference = NULL, id = NULL),
    list(2009,        4,            13,         "ae8f43b4b8",     6054),
    list(2009,        4,            14,         "e0e57942dd",     3453),
    list(2009,        4,            13,         "d63a61d9fc",     1470)
))
# Here the first line should be skipped because the third line has an identical
# date:
deduped.table <- rbindlist(list(
    list(year = NULL, month = NULL, day = NULL, reference = NULL, id = NULL),
    list(2009,        4,            14,         "e0e57942dd",     3453),
    list(2009,        4,            13,         "d63a61d9fc",     1470)
))

ex02DedupTable <- function(table) {
  assertDataTable(table)
  setDT(table)
  dedupedTable <- table[!duplicated(table, by = c("year", "month", "day"), fromLast = TRUE), ]
  return(dedupedTable)
}


# You are looking for a new flat. For this, you query a database for monthly
# rent prices of available flats, which returns data in the following format:
flat.prices <- rbindlist(list(
    list(address = NULL, prices = NULL),
    list("134 Charles St", list(list(c(2340, 2193), NULL, 4023, NULL, NULL, c(10234, 9203)))),
    list("12 East 69th St", list(list(2493, NULL, NULL, NULL))),
    list("2 Park Pl", list(list(NULL, NULL, 1924, 3921))),
    list("172 Madison Ave", list(list(NULL, NULL))),
    list("25 Columbus Circle", list(list(10234)))
))
# This lists the adresses where the agency manages flats, and for each address
# the column `prices` lists the flat prices. This column contains a list for
# each address, where the first entry lists all prices of all available flats
# on the first floor, the second entry lists all prices of available flats on
# the second floor etc. In this example, the `"134 Charles St"` building has
# two flats available on the ground floor (one for 2340, one for 2193), one flat
# for 4023 on the second floor, and two flats (10234 and 9203) on the fifth
# floor.
#
# You plan to look at all available addresses individually, but within each
# address you only consider the *cheapest flat that is not on the ground floor*.
# You therefore need to write a function that lists the address, the price of
# the cheapest apartment that is not on the ground floor, and the floor of that
# aparment. If there are no apartments available that are not on the ground
# floor, the address should be absent. The result for the input above should
# therefore be
flat.choices <- rbindlist(list(
    list(address = NULL, price = NULL, floor = NULL),
    list("134 Charles St", 4023, 2),
    list("2 Park Pl", 1924, 2)
))
# Input:
# - `prices`: a `data.table` with columns `address`, `prices`, as described
#   above
# Output:
# A `data.table with columns `address`, `price`, `floor`, as shown above,
# with arbitrary order of rows.
ex03FlatPrices <- function(prices) {
  assertDataTable(prices)
  results <- list()
  for (i in seq_len(nrow(prices))) {
    address <- prices$address[i]
    priceList <- prices$prices[[i]]
    if (length(priceList) > 1) {
      cheapestPrice <- NULL
      cheapestFloor <- NULL
      for (floor in 2:length(priceList)) {
        if (!is.null(priceList[[floor]])) {
          minPrice <- min(priceList[[floor]], na.rm = TRUE)
          if (is.null(cheapestPrice) || minPrice < cheapestPrice) {
            cheapestPrice <- minPrice
            cheapestFloor <- floor - 1
          }
        }
      }
      if (!is.null(cheapestPrice)) {
        results[[length(results) + 1]] <- list(address, cheapestPrice, cheapestFloor)
      }
    }
  }
  if (length(results) == 0) {
    return(data.table(address = character(), price = numeric(), floor = integer()))
  } else {
    resultDt <- rbindlist(results)
    setnames(resultDt, c("address", "price", "floor"))
    return(resultDt)
  }
}


# You are organising a small hackathon for R developers working on a specific
# project. Everyone should come to a place over the weekend to code. You want
# to order food and drinks for everyone, as well as reserve a hotel for those
# who don't live nearby or sleep over at friends' places. For this you have
# set up a Google Forms Survey, where those interested in coming should answer
# questions, for example about their dietary needs and whether they need a
# hotel. However, some participants may forget to input some information, or
# may change their mind about certain things later. In that case, they just
# submit a new response in the survey, updating the old information. The data
# you get could, for example, have the following format:
participants.response <- rbindlist(list(
    list(name = NULL,        coming = NULL, hotel = NULL, dietary.reqs = NULL, submission.date = NULL),
    list("Donald Knuth",     TRUE,          TRUE,         "dessert first",     3),
    list("Ross Ihaka",       NA,            TRUE,         NA,                  6),
    list("Ross Ihaka",       TRUE,          FALSE,        NA,                  4),
    list("Vladimir Vapnik",  TRUE,          FALSE,        "",                  5),
    list("Donald Knuth",     FALSE,         NA,           NA,                  5),
    list("Robert Gentleman", TRUE,          TRUE,         NA,                  3),
    list("Ross Ihaka",       NA,            NA,           "vegetarian",        5)
))
# Here the `submission.date` column indicates the number of days since the form
# was set up. In this example, Donald Knuth at first signed up to come but then
# later had to cancel. Ross Ihaka at first did not provide information
# about his dietary requirements, which he then set to `"vegetarian"`. He
# furthermore updated that he did want to stay at a hotel after all.
# Robert Gentleman and Vladimir Vapnik both indicated they would be coming and
# provided information, although Robert Gentleman's dietary requirements stay
# unknown.
# (*This data is made up. Please do not use the dietary requirement data if you
# are actually hosting someone from this list. Please email me if you happen
# to know the dietary requirements of someone on this list)
#
# Write a function that takes data of this form and constructs the final
# response for each participant, by taking the last value, according to the
# submission date that is not missing.
# Input: `response`, a `data.table` with the columns `name`, `submission.date`,
# and multiple other columns of any type. The order of the columns, as well as
# the other columns present could be arbitrary.
# Return: The result `data.table` should contain all columns except the
# `submission.date` column, and the columns should be in the order they were
# given. There should be one row for each participant (according to `name`)
# with the most recently given response for each other column.
# The response for the data above could, for example, be:
participants.response.final <- rbindlist(list(
    list(name = NULL,        coming = NULL, hotel = NULL, dietary.reqs = NULL),
    list("Donald Knuth",     FALSE,         TRUE,         "dessert first"),
    list("Ross Ihaka",       TRUE,          TRUE,         "vegetarian"),
    list("Vladimir Vapnik",  TRUE,          FALSE,        ""),
    list("Robert Gentleman", TRUE,          TRUE,         NA)
))
ex04LastResponse <- function(response) {
  assertDataTable(response)
  setDT(response)
  setorder(response, name, -submission.date)
  getMostRecentResponse <- function(x) {
    nonMissingValues <- na.omit(x)
    if (length(nonMissingValues) > 0) {
      return(nonMissingValues[[1]])
    } else {
      return(NA)
    }
  }
  colsToProcess <- setdiff(names(response), c("name", "submission.date"))
  resultsList <- vector("list", length(unique(response$name)))
  for (i in seq_along(resultsList)) {
    participantData <- response[name == unique(response$name)[i]]
    resultsList[[i]] <- lapply(participantData[, ..colsToProcess], getMostRecentResponse)
    resultsList[[i]]$name <- unique(response$name)[i]
  }
  resultDt <- rbindlist(resultsList)
  setcolorder(resultDt, c("name", colsToProcess))
  return(resultDt)
}


# You are given data about the organizational hierarchy of a small company. It
# tracks, for every employee, who their supervisor / immediate superior is.
# An example of this data would be
org.table <- rbindlist(list(
    list(name = NULL,               superior = NULL),
    list("Gottfried Ofers",         ""),
    list("Kunigunde von Lauerberg", "Gottfried Ofers"),
    list("Agata Weyen",             "Kaspar Vinken"),
    list("Kaspar Vinken",           "Gottfried Ofers")
))
# (The CEO has his superior listed as `""`)
# The company needs to track which employee is supervising other employees for
# accounting purposes.
# Write a function that adds a `logical` column `has.inferior` which is `TRUE`
# for every employee that has inferiors.
# Input: `org`, a `data.table` as in the format above
# Output: The `data.table` with an additional column `has.inferior`. The example
# above should return
org.table.augmented <- rbindlist(list(
    list(name = NULL,               superior = NULL,   has.inferior = NULL),
    list("Gottfried Ofers",         "",                TRUE),
    list("Kunigunde von Lauerberg", "Gottfried Ofers", FALSE),
    list("Kaspar Vinken",           "Gottfried Ofers", TRUE),
    list("Agata Weyen",             "Kaspar Vinken",   FALSE)
))
ex05OrgHasInf <- function(org) {
  assertDataTable(org)
  setDT(org)
  superiors <- unique(org$superior[org$superior != ""])
  org[, has.inferior := name %in% superiors]
  return(org)
}
