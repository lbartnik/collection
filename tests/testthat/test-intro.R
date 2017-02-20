context("intro samples")

test_that("create a collection", {
  handle <- empty_collection()
  expect_true(dir.exists(handle$storage))
  expect_length(handle, 0)

  handle <- sample_collection()
  expect_true(dir.exists(handle$storage))
  expect_length(handle, 10)
})


test_that("load data", {
  handle <- empty_collection('sample')

  id <- store(iris, handle)
  files <- list.files(file.path(tempdir(), 'sample'), recursive = TRUE)

  expect_true(paste0(id, '.rds') %in% basename(files))
  expect_true(paste0(id, '_tags.rds') %in% basename(files))
})


test_that("process data", {
  handle <- sample_collection('sample')

  values <- capply(handle, mean)
  expect_equal(length(handle), length(values))
  expect_equal(values, seq(from = 5.5, by = 1, length.out = 10))
})


