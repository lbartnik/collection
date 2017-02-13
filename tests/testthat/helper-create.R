
empty_collection <- function (name = 'sample')
{
  # first make sure it doesn't exist
  path <- file.path(tempdir(), name)
  unlink(path, recursive = TRUE, force = TRUE)

  collection(name, create = TRUE, storage = filesystem(tempdir()))
}


sample_collection <- function (name = 'sample')
{
  handle <- empty_collection(name)
  lapply(seq(10), function(x) store(handle, seq(from = x, length.out = 10), 'data'))
  handle
}

