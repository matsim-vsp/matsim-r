test_that("Reading Trips Table works", {
  expect_gt(nrow(readTripsTable("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/berlin/berlin-v5.5-1pct/output-berlin-v5.5-1pct/berlin-v5.5.3-1pct.output_trips.csv.gz")),0)
})
