library(testthat)
library(tidycountries)

test_that("get_country_info returns correct data for a valid country", {
  result <- get_country_info("Nigeria")

  expect_true(nrow(result) > 0)
  expect_equal(result$common_name, "Nigeria")
  expect_equal(result$cca2, "NG")
  expect_equal(result$region, "Africa")
  expect_equal(result$continents, "Africa")
})


test_that("get_country_info returns all countries when 'all' is passed", {
  result <- get_country_info('all')

  expect_true(nrow(result) > 200 ) # assuming data has 200+ countries
  expect_true("Nigeria" %in% result$common_name)
})
