testthat::test_that("runs correctly", {
  cities <- greenSD::check_available_urban(test = TRUE)
  boundary <- greenSD::check_urban_boundary(test = TRUE)
  testthat::expect_type(cities, "NULL")
  testthat::expect_type(boundary, "NULL")
})
