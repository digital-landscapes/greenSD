testthat::test_that("runs correctly", {
  city <- greenSD::get_gsdc_data()
  sample <- greenSD::sample_values()
  pwgf <- greenSD::pop_weg()
  testthat::expect_type(city, "NULL")
  testthat::expect_type(sample, "NULL")
  testthat::expect_type(pwgf, "NULL")
})
