context("example")

test_that("time series example is generated correctly", {
  ts <- sample_collection(n = 10)

  expect_true(is_collection(ts))
  expect_length(list.files(ts$storage, recursive = T, pattern = '.*rds$'), 20)
})
