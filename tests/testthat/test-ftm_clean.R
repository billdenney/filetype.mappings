test_that("ftm_clean really does nothing by itself.", {
  expect_error(ftm_clean(data.frame()))
})
