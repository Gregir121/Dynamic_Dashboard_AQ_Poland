library(testthat)
#source("../../api_functions.R")

test_that("API Response as expected", {
  df <- fetch_stations()
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
})

test_that("Function fetch_sensors returns expected data", {
  data <- fetch_sensors(11)
  expect_s3_class(data, "data.frame")
  expect_true(nrow(data) > 0)
  expect_named(data, c("Identyfikator stanowiska", "Wskaźnik", "Identyfikator parametru", "Nazwa parametru"))
})

test_that("API returns non-existing station comment", {
  expect_error(fetch_sensors(99999))
})
