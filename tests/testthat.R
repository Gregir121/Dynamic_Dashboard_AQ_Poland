library(testthat)

if (file.exists("R/api_functions.R")) {
  source("R/api_functions.R")
} else if (file.exists("api_functions.R")) {
  source("api_functions.R")
}

test_dir("tests/testthat")