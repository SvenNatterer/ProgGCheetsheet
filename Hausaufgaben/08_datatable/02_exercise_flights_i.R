
# This exercise concerns itself with flight data. Every exercise of `data.table`
# has flight data somewhere, so here we go.
# Note that the data.table vignette at
# https://rdatatable.gitlab.io/data.table/articles/datatable-intro.html
# uses very similar data (although we have a reduced version).
#
# The data is in the `flights.zip` file and can be loaded in the following way:
flights.data <- as.data.table(read.csv(unz("tables/flights.zip", "flights.csv"), stringsAsFactors = FALSE))
# (note that data.table offers a more convenient way of loading it, but it does
# not always work reliably on windows if 'unzip' is not in your $PATH:
# > flights.data <- fread("unzip -cq tables/flights.zip")
# )
#
# The data has the following columns:
# - year: year of the flight.
# - month: month of the flight (1-12)
# - day: day of the month of the flight (1-31)
# - dep_delay: delay of departure, in minutes
# - arr_delay: delay of arrival, in minutes
# - carrier: IATA airline designator of operating carrier
# - origin: IATA airport code of departure airport
# - dest: IATA airport code for destination airport
# - hour: planned hour of day of departure
#
# A few comments on the particular dataset:
# - The example data is from 2014, but multiple years in a dataset are possible
# - The example dataset contains only the six most frequent carriers, but the
#   tests could contain more or fewer.
# - The example dataset contains only the busiest airports, but the tests could
#   contain more or fewer.


# Write a function that calculates the median and maximal arrival delay of the
# 3 most frequent carriers.
#
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# Output:
# `data.table` with columns `carrier`, `delay.median`, `delay.max`, indicating
# the minimum and maximum arrival delay experienced by flights of the three
# most represented carriers in the dataset. The median delay should be of the
# flights that were delayed at all, i.e. flights that were not delayed should
# not be counted. (If there were no delays in any flights of a carrier, this
# value should be zero). Only the lines for the three carriers with the most
# flights should be given (in any order). You can rely on there being at least
# three different carriers in the dataset.
# The result with the example dataset could be (up to row order):
flights.delays <- rbindlist(list(
    list(carrier = NULL, delay.median = NULL, delay.max = NULL),
    list("AA",           19,                  1524),
    list("DL",           15,                  1107),
    list("UA",           18,                  668)
))
# (There may be a datatype error when you use `median()` inside a `[ ]`
# aggregation; in that case, use `as.numeric(median())`.)
ex01DelayStats <- function(flights) {
  assert(is.data.table(flights))
  topCarriers <- flights[, .N, by = carrier][order(-N)][1:3, carrier]
  result <- flights[carrier %in% topCarriers, .(
    delay.median = as.numeric(median(arr_delay[arr_delay > 0], na.rm = TRUE)),
    delay.max = max(arr_delay, na.rm = TRUE)
  ), by = carrier]
  return(result)
}

# Write a function that returns the median delay of flights for each month and
# for each route.
#
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# - `year`: the year for which to aggregate. (Remember that `flights` could
#   contain data from multiple years.)
# Output:
# `data.table` with columns `month`, `delay`, `origin`, `dest`.
# `delay` should indicate the median arrival delay of flights from `origin` to
# `dest` in that month. The median delay should be of the flights that were
# delayed at all, i.e. flights that were not delayed should not be counted. (If
# there were no delays in any flight of given route, the `delay` should be 0.)
# All months of the given `year` should be considered, and the routes from
# and to all airports that are in either `origin` or `dest` of the `flights`
# argument. I.e. if there is only a flight from `"DEN"` to `"ATL"`, but no
# flight from `"ATL"` to `"DEN"`, then the `"ATL"` to `"DEN"` route should
# be *included* in the returned table and listed with a delay of 0.
# This is even true if the airports are listed in different years than
# the `year` given as argument. I.e. `ex02MonthlyDelays(flights.data, 2000)`
# should give a table with many entries that all have `delay` 0.
# A possible return value of this function when called with `flights.data` and
# 2014 is saved in `"ex02MonthlyDelaysResult.csv"`:
flights.monthlydelays <- fread("tables/ex02MonthlyDelaysResult.csv")
# It is a good idea to use the `CJ()` function to generate the table of all
# possible combinations of months, and origin and destination airports. In that
# case you have to take care to remove flights with same origin and destination
# airport, however.
ex02MonthlyDelays <- function(flights, year) {
  assertDataTable(flights)
  assertInt(year)
  jahr <- year
  flightsYear <- flights[year == jahr, ]
  allCombinations <- CJ(month = 1:12, origin = unique(flights$origin), dest = unique(flights$dest))
  allCombinations <- allCombinations[origin != dest]
  medianDelays <- flightsYear[, .(delay = as.double(median(arr_delay[arr_delay > 0], na.rm = TRUE))),
                                by = .(month, origin, dest)]
  result <- merge(allCombinations, medianDelays, by = c("month", "origin", "dest"), all.x = TRUE)
  result <- result[, .(month, delay, origin, dest)]
  result[is.na(delay), delay := 0]
  result <- as.data.table(result)
  result
}



# Write a function that counts the number of flights on each route that departed
# each month until a flight on that route was delayed more than 60 minutes.
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# - `year`: the year for which to aggregate. (Remember that `flights` could
#   contain data from multiple years.)
# Output:
# `data.table` with columns `month`, `flights.to.delay`, `origin`, `dest`.
# Similarly to `ex02MonthlyDelays`, this function should aggregate across flight
# routes and months in a given given year.
# Rows should be chronologically ordered.
# For each route and month, the number
# of flights that departed from the start of that month until one flight's
# arrival was delayed more than 60 minutes should be given. The delayed flight
# should not be counted. So if 10 flights departed for a route in a given month
# and the 5th flight was delayed > 60 minutes, the return value would be 4. If
# no flight was delayed more than 60 minutes, the return value should be  the
# total number of flights on that route and month, i.e. 10 in this example. If
# no flight was on the given route during the month, the value is 0. Note that,
# as in `ex02MonthlyDelays`, the year could be different from the years in the
# `flights` dataset, which should result in a table full of `0`s.
#
# A possible return value of this function when called with `flights.data` and
# 2014 is saved in `"ex03FlightsToDelayResult.csv"`:
flights.flightstodelay <- fread("tables/ex03FlightsToDelayResult.csv")
ex03FlightsToDelay <- function(flights, year) {
  assertDataTable(flights)
  assertInt(year)
  jahr <- year
  flightsYear <- flights[year == jahr, ][order(month, day, hour)]
  allCombinations <- CJ(month = 1:12, origin = unique(flights$origin), dest = unique(flights$dest))
  allCombinations <- allCombinations[origin != dest]
  countFlightsTillDelay <- function(indexes) {
    subsetFlights <- flightsYear[indexes, ]
    delayIndices <- which(subsetFlights$arr_delay > 60)
    if (length(delayIndices) == 0) {
      return(nrow(subsetFlights))
    } else {
      return(delayIndices[[1]] - 1)
    }
  }
  flightsTillDelay <- tapply(seq_len(nrow(flightsYear)),
                             list(flightsYear$month, flightsYear$origin, flightsYear$dest),
                             FUN = countFlightsTillDelay)
  result <- as.data.table(flightsTillDelay, keep.rownames = TRUE)
  setnames(result, c("month", "origin", "dest", "flights.to.delay"))
  result[, month := as.integer(month)]
  result <- merge(allCombinations, result, by = c("month", "origin", "dest"), all.x = TRUE)
  result[is.na(flights.to.delay), flights.to.delay := 0]
  result <- result[, .(month, flights.to.delay, origin, dest)]
  return(result)
}

# Write a function that, for each month in a given year, and for each airline,
# calculates (1) the airline's mean flight delay, and (2) the flight delay of
# that airline's competition, i.e. every other airline's mean flight delay.
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# - `year`: the year for which to aggregate. (Remember that `flights` could
#   contain data from multiple years.)
# Output:
# `data.table` with columns `month`, `carrier`, `mean.delay`,
# `mean.delay.competition`.
# The table should contain a row for each month and each each carrier, even
# if the given carrier (or its competition) do not have any flights on that
# month (in which case the respective `mean.delay` / `mean.delay.competition`
# should be 0).
# Rows should be chronologically ordered.
# A possible return value of this function when called with `flights.data`
# and 2014 is saved in `ex04CarrierDelayResult.csv`:
flights.carrierdelay <- fread("tables/ex04CarrierDelayResult.csv")
# As in other tasks, the `CJ()` function can be handy here to create a
# table of for all carriers within each month.
ex04CarrierDelay <- function(flights, year) {
  assertDataTable(flights)
  assertInt(year)
  jahr <- year
  flightsYear <- flights[year == jahr, ][order(month, carrier)]
  meanDelays <- flightsYear[, .(mean.delay = mean(arr_delay, na.rm = TRUE)), by = .(month, carrier)]
  allCombinations <- CJ(month = 1:12, carrier = unique(flights$carrier))
  result <- merge(allCombinations, meanDelays, by = c("month", "carrier"), all = TRUE)
  result[is.na(mean.delay), mean.delay := 0]
  result[, mean.delay.competition := {
    competitionDelays <- flightsYear[carrier != .BY$carrier & month == .BY$month, arr_delay]
    mean(competitionDelays, na.rm = TRUE)
  }, by = .(month, carrier)]
  result[is.na(mean.delay.competition), mean.delay.competition := 0]
  return(result)
}