
empty_collection <- function (name = 'sample')
{
  # first make sure it doesn't exist
  path <- file.path(tempdir(), name)
  unlink(path, recursive = TRUE, force = TRUE)

  collection(name, create = TRUE, storage = filesystem(tempdir()))
}


filled_collection <- function (name = 'sample')
{
  handle <- empty_collection(name)
  store_xy <- function(x, y) store(handle, seq(from = x, length.out = 10), paste0('data', y))
  lapply(seq(5),   function(x) store_xy(x, 1))
  lapply(seq(5)+5, function(x) store_xy(x, 2))
  handle
}

