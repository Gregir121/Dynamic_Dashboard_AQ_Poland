test_that("API Response as expected", {
  df <- fetch_stations()
  expect_true(is.data.frame(as.data.frame(df))) 
  expect_true(nrow(as.data.frame(df)) > 0)
})

test_that("Function fetch_sensors returns expected data", {
  data <- fetch_sensors(11) 
  expect_true(is.data.frame(as.data.frame(data)))
  expect_true("Wskaźnik" %in% names(data))
})

test_that("API handle non-existing station", {
  res <- fetch_sensors(99999)
  expect_true(is.null(res) || length(res) == 0 || nrow(res) == 0)
})
