testthat::test_that("runs correctly", {
  city <- greenSD::get_gsds_data()
  sample <- greenSD::sample_values()
  testthat::expect_type(city, "NULL")
  testthat::expect_type(sample, "NULL")
})
