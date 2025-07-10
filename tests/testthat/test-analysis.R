testthat::test_that("runs correctly", {
  pwgf <- greenSD::exposure()
  testthat::expect_type(pwgf, "NULL")
})
