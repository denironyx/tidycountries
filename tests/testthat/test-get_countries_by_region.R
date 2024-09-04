test_that("get_countries_by_region returns correct data for a valid region",{
  result <- get_countries_by_region('Africa')

  expect_true(nrow(result) > 50) # we know that countries in Africa is morethan 50
  expect_equal(result$region[1], 'Africa')
})
#> future considering testing that handles invalid region
#>
#>
# test_that("get_countries_by_region handles invalid region gracefully", {
#   expect_warning(result <- get_countries_by_region("InvalidRegion"))
#   expect_true(is.null(result))
# })

