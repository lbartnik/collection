sample_path <- function (topdir = 'sample')
{
  file.path(tempdir(), topdir)
}

empty_collection <- function (path = sample_path())
{
  stopifnot(require(storage))

  # first make sure it doesn't exist
  unlink(path, recursive = TRUE, force = TRUE)
  collection(character(0), storage = storage::filesystem(path, create = TRUE))
}


sample_collection <- function (path = sample_path())
{
  col <- empty_collection(path)

  store_xy <- function(x, y) add(col, seq(from = x, length.out = 10))
  lapply(seq(5),   function(x) store_xy(x, 1))
  lapply(seq(5)+5, function(x) store_xy(x, 2))

  all_objects(col)
}

