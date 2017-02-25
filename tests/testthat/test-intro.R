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

  expect_length(id, 1)
  expect_true(paste0(id, '.rds') %in% basename(files))
  expect_true(paste0(id, '_tags.rds') %in% basename(files))

  tags <- readRDS(file.path(handle$storage, grep("tags.rds", files, value = 1)))
  expect_named(tags, 'group')
  expect_equal(tags$group, id)
})


test_that("process data", {
  handle <- filled_collection('sample')

  expect_warning(values <- capply(handle, mean))
  expect_equal(length(handle), length(values))
  expect_equal(sort(unlist(values)),
               seq(from = 5.5, by = 1, length.out = 10))
})


test_that("filter", {
  handle <- filled_collection('sample')

  g1 <- filter(handle, group == 'data1')
  expect_s3_class(g1, 'clist')
  expect_length(g1, 5)
})
