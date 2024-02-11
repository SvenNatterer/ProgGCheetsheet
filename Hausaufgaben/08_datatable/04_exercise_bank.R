
# This exercise concerns itself with bank account transactions.
#
# You are asked to assist a small bank that keeps its record of financial
# transactions (a "ledger") in a tabular format. The table contains one row for
# each transfer of money from a certain account to another account. The table
# has columns for the date of the transfer (`yr`, `mth`, and `day`), the account
# from which a transfer is made (`src`), and where the money is transferred to
# (`dst`). Finally, a column for the transferred amount (`amt`) exists.
#
# Customers may also deposit money to their bank-account, in which case the bank
# records a transaction from the fictional DEPOSIT account. Customers may also
# withdraw money, in which case a transaction to the fictional WITHDRAW account
# is recorded.
#
# The table is not necessarily stored in chronological order.
#
# An example ledger could look like the following:
bank.ledger <- rbindlist(list(
    list(yr = NULL, mth = NULL, day = NULL, src = NULL, dst = NULL, amt = NULL),
    list(2008,      12,         7,          "DEPOSIT",  "Helmut",   19),
    list(2007,      8,          22,         "DEPOSIT",  "Agathe",   21),
    list(2011,      2,          1,          "Helmut",   "Annabel",  4),
    list(2011,      2,          1,          "Helmut",   "Agathe",   5),
    list(2011,      2,          4,          "Agathe",   "Annabel",  3),
    list(2008,      12,         8,          "Helmut",   "WITHDRAW", 10),
    list(2012,      1,          30,         "Agathe",   "Annabel",  23),
    list(2011,      4,          4,          "DEPOSIT",  "Erwin",    10),
    list(2011,      3,          4,          "DEPOSIT",  "Helmut",   1)
))

# Write a function that brings the table in chronological order.
# Input:
# - `ledger`: the bank's ledger in the format described above.
# Output should be a `data.table` with the rows of `ledger` in chronological
# order, starting with the oldest entry. The output with the example above
# should look like the following:
bank.ledger.sorted <- rbindlist(list(
    list(yr = NULL, mth = NULL, day = NULL, src = NULL, dst = NULL, amt = NULL),
    list(2007,      8,          22,         "DEPOSIT",  "Agathe",   21),
    list(2008,      12,         7,          "DEPOSIT",  "Helmut",   19),
    list(2008,      12,         8,          "Helmut",   "WITHDRAW", 10),
    list(2011,      2,          1,          "Helmut",   "Annabel",  4),
    list(2011,      2,          1,          "Helmut",   "Agathe",   5),
    list(2011,      2,          4,          "Agathe",   "Annabel",  3),
    list(2011,      3,          4,          "DEPOSIT",  "Helmut",   1),
    list(2011,      4,          4,          "DEPOSIT",  "Erwin",    10),
    list(2012,      1,          30,         "Agathe",   "Annabel",  23)
))
#
# You could use the `ISOdate()` function, but you may learn more if you try this
# exercise without it.
ex01SortLedger <- function(ledger) {
  assertDataTable(ledger)
  return(ledger[order(yr, mth, day)])
}

# Write a function that returns all transactions performed before a given date.
# Input:
# - `ledger`: the bank's ledger in the format described above.
# - `year`: integer `numeric(1)`
# - `month`: integer `numeric(1)` between 1 and 12 (inclusive)
# - `day`: integer `numeric(1)` between 1 and 31 (inclusive)
#   (it is not necessary to check whether `day` is valid within `month`. E.g.
#    `month = 2, day = 30` can be ignored, but `month = 2, day = 100` should
#    give an error.)
# Output should be a `data.table` with all rows of `ledger` that occur *before*
# the given date, in any order.
# An example result for the call
# `ex02TrimLedger(bank.ledger, 2008, 12, 8)` would be
bank.ledger.trimmed <- rbindlist(list(
    list(yr = NULL, mth = NULL, day = NULL, src = NULL, dst = NULL, amt = NULL),
    list(2008,      12,         7,          "DEPOSIT",  "Helmut",   19),
    list(2007,      8,          22,         "DEPOSIT",  "Agathe",   21)
))
# (note the entry from 2008-12-08 is *excluded*).
# The result should be an empty `data.table` with appropriate columns and types
# when no entry occurs before the given date.
# An easy solution for this exercise is to use `ISOdate` both for the
# function arguments specifying the time, as well as the date columns in the
# table, and select columns based on this.
#
# (A problem may be that the function argument `day` and the `data.table` column
# `day` have the same name. The easiest workaround here is to use `..day`, or
# to rename the local `day` variable or use it outside the `[ ]` statement. If you
# are interested, you may look at these other solutions:
# https://stackoverflow.com/a/21664128, https://stackoverflow.com/a/58358092 )
ex02TrimLedger <- function(ledger, year, month, day) {
  assertDataTable(ledger)
  assertCharacter(colnames(ledger), len = 6)
  assertSubset(c("yr", "mth", "day", "src", "dst", "amt"), colnames(ledger))
  assertIntegerish(year, len = 1, lower = 1)
  assertIntegerish(month, len = 1, lower = 1, upper = 12)
  assertIntegerish(day, len = 1, lower = 1, upper = 31)
  comparisonDate <- as.Date(paste(year, month, day, sep = "-"))
  ledger[as.Date(paste(yr, mth, day, sep = "-")) < comparisonDate]
}

# You are asked to write a simple account balance reporting interface that
# returns the balance of given accounts at the end of given dates. The function
# will be called with the bank's ledger, as well as a `query` table that details
# which accounts and dates are requested. It could, for example, look like the
# following:
bank.query <- rbindlist(list(
    list(yr = NULL, mth = NULL, day = NULL, account = NULL),
    list(2008,      12,         6,          "Helmut"),
    list(2008,      12,         7,          "Helmut"),
    list(2019,      2,          3,          "Agathe"),
    list(2011,      2,          3,          "Agathe"),
    list(2008,      12,         6,          "Balthasar")
))
# Note that entries are not necessarily in chronological order. Accounts that
# have no transactions at the given time, or that are not mentioned at all in
# the ledger, have a balance of 0.
#
# Input:
# - `ledger`: the bank's ledger in the format described above.
# - `query`: a query in the format shown above.
# Return: return the `query` table with an added column with rows in their
# original order with an added column `balance`. The result for the input
# given above would be
bank.balance <- rbindlist(list(
    list(yr = NULL, mth = NULL, day = NULL, account = NULL, balance = NULL),
    list(2008,      12,         6,          "Helmut",       0),
    list(2008,      12,         7,          "Helmut",       19),
    list(2019,      2,          3,          "Agathe",       0),
    list(2011,      2,          3,          "Agathe",       26),
    list(2008,      12,         6,          "Balthasar",    0)
))
# You could make use of `ex02TrimLedger()` here and do some extra steps to
# calculate the result for each query row in a loop. You are, however,
# encouraged to use merging and aggregation here: Look up "non-equi joins"
# or "inequality joins".
# (You would probably need to use `ISOdate()` for this.)
# A third (and a bit more challenging) way is to create a table of account
# balances after each transaction and use rolling joins:
# https://www.gormanalysis.com/blog/r-data-table-rolling-joins/
ex03Balance <- function(ledger, query) {
  assertDataTable(ledger)
  assertDataTable(query)
  query[, date := as.Date(paste(yr, mth, day, sep = "-"))]
  results <- lapply(seq_len(nrow(query)), function(idx) {
    row <- query[idx]
    nextDay <- row$date + 1
    year <- as.integer(format(nextDay, "%Y"))
    month <- as.integer(format(nextDay, "%m"))
    day <- as.integer(format(nextDay, "%d"))
    trimmedLedger <- ex02TrimLedger(ledger, year, month, day)
    balance <- sum(trimmedLedger[dst == row$account, amt]) - sum(trimmedLedger[src == row$account, amt])
    list(yr = row$yr, mth = row$mth, day = row$day, account = row$account, balance = balance)
  })
  resultDt <- rbindlist(results)
  setnames(resultDt, c("yr", "mth", "day", "account", "balance"))
  resultDt
}

# The bank wants to analyse the usage that their system sees over time. Write a
# function that calculates the number of *unique users* of their system, as
# indicated by the transactions, for each month.
# The user who originates a transaction is identified by the `src` column,
# *unless* it is `DEPOSIT`, in which case the `dst` column identifies the user.
#
# Input:
# - `ledger`: the bank's ledger in the format described above.
# - `year`: integer `numeric(1)`. The year for which to generate the analytics.
# Output should be a `data.table` with 12 rows for each month, with the columns
# `month` (counting from 1 to 12), and `users`, indicating the number of unique
# users in that month. An example return for the call
# `ex04MonthlyUsers(bank.ledger, 2011)` could look like this:
bank.users <- rbindlist(list(
    list(month = NULL, users = NULL),
    list(1,            0),
    list(2,            2),
    list(3,            1),
    list(4,            1),
    list(5,            0),
    list(6,            0),
    list(7,            0),
    list(8,            0),
    list(9,            0),
    list(10,           0),
    list(11,           0),
    list(12,           0)
))
# The two unique users in February are `"Helmut"` and `"Agathe"` (both counted
# once even though `"Helmut"` originates two transactions), and the users in
# March and April are `"Helmut"` and `"Erwin"`, who both make a deposit.
# Note that `"Helmut"` is counted as a unique user every month that he makes a
# transaction, but only counted at most once per month.
ex04MonthlyUsers <- function(ledger, year) {
  assertDataTable(ledger)
  assertIntegerish(year, len = 1, any.missing = FALSE)
  ledger <- ledger[yr == year]
  ledger[, user := ifelse(src == "DEPOSIT", dst, src)]
  uniqueUsersPerMonth <- ledger[, .(users = uniqueN(user)), by = mth]
  allMonths <- data.table(month = 1:12)
  result <- merge(allMonths, uniqueUsersPerMonth, by.x = "month", by.y = "mth", all.x = TRUE)
  result[is.na(users), users := 0]
  setnames(result, c("month", "users"))
  return(result)
}

# Furthering the analysis, the bank wants to measure the daily system
# utilization, i.e. the number of transactions per day. However, to smooth out
# the numbers, a *ten day rolling average* should be applied, i.e. for each
# day, the average number of transactions per day for the last 10 days should
# be reported.
#
# Input:
# - `ledger`: the bank's ledger in the format described above
# - `year`: integer `numeric(1)`. The year for which to generate analytics.
# - `month`: integer `numeric(1)` between 1 and 12 inclusive. The month for
#   which to generate analytics.
# Output should be a `data.table` with columns `yr`, `mth`, `day`,
# `transactions`. `yr` and `mth` should be the `year` and `month` argument
# given to the function, `day` should count up the days of the month, and
# `transactions` should be the 10-day rolling average of the number of
# transactions up to (and including) the day listed. (Note that this average
# should consider the transactions done in the previous month, where applicable,
# i.e. the 1st entry is the average of the 1st of that month and 9 days from
# the preceding month etc.)
# An example result for the call `ex05Transactions(bank.ledger, 2011, 2)`
# would be:
bank.daily.users <- rbindlist(list(
    list(yr = NULL, mth = NULL, day = NULL, transactions = NULL),
    list(2011,      2,          1,          0.2),
    list(2011,      2,          2,          0.2),
    list(2011,      2,          3,          0.2),
    list(2011,      2,          4,          0.3),
    list(2011,      2,          5,          0.3),
    list(2011,      2,          6,          0.3),
    list(2011,      2,          7,          0.3),
    list(2011,      2,          8,          0.3),
    list(2011,      2,          9,          0.3),
    list(2011,      2,          10,         0.3),
    list(2011,      2,          11,         0.1),
    list(2011,      2,          12,         0.1),
    list(2011,      2,          13,         0.1),
    list(2011,      2,          14,         0),
    list(2011,      2,          15,         0),
    list(2011,      2,          16,         0),
    list(2011,      2,          17,         0),
    list(2011,      2,          18,         0),
    list(2011,      2,          19,         0),
    list(2011,      2,          20,         0),
    list(2011,      2,          21,         0),
    list(2011,      2,          22,         0),
    list(2011,      2,          23,         0),
    list(2011,      2,          24,         0),
    list(2011,      2,          25,         0),
    list(2011,      2,          26,         0),
    list(2011,      2,          27,         0),
    list(2011,      2,          28,         0)
))
# It is probably easiest to create a table of daily transactions in the
# interesting window (including 9 days from "last month") and then use
# `frollmean()`.
# You should probably use commands to the effect of
# `date <- as.Date(ISOdate())` on the data to handle dates.
# You can directly subtract days from the date: `date - 9`, and you can
# get the last day of the month by using
# `as.Date(ISOdate(year, month + 1, 1)) - 1`. (Of course there are also
# other ways of handling this, in particular `data.table`'s own `IDate`
# class). Possibly also helpful: `?seq.Date`, and the `data.table::year()`,
# `data.table::month()` and `data.table::mday()` functions that extract parts
# of dates.
ex05Transactions <- function(ledger, year, month) {
  assertDataTable(ledger)
  assertIntegerish(year, len = 1, any.missing = FALSE)
  assertIntegerish(month, lower = 1, upper = 12, len = 1, any.missing = FALSE)
  ledger[, date := as.Date(ISOdate(yr, mth, day))]
  startDate <- as.Date(ISOdate(year, month, 1)) - 9
  if (month == 12) {
    endDate <- as.Date(ISOdate(year, month, 31))
  } else {
    endDate <- as.Date(ISOdate(year, month + 1, 1)) - 1
  }
  allDates <- data.table(date = seq(from = startDate, to = endDate, by = "day"))
  dailyTransactions <- ledger[date >= startDate & date <= endDate, .(transactions = .N), by = .(date)]
  allDates <- merge(allDates, dailyTransactions, by = "date", all = TRUE)
  allDates[is.na(transactions), transactions := 0]
  allDates[, rollingAvg := frollmean(transactions, 10, align = "right")]
  startDate <- as.Date(ISOdate(year, month, 1))
  allDates <- allDates[date >= startDate & date <= endDate]
  allDates[, `:=`(yr = year(date), mth = month(date), day = mday(date))]
  result <- allDates[, .(yr, mth, day, transactions = rollingAvg)]
  return(result)
}