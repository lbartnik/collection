context("capply")

test_that("identity", {
  col <- sample_collection()
  on.exit(os_remove(attr(col, 'storage')))

  res <- capply(col, function(object,tags)object, execute = TRUE)
  expect_length(res, 10)
  expect_true(all(vapply(res, class, character(1)) == 'numeric'))
  expect_true(all(vapply(res, length, numeric(1)) == 10))
})
