#' @export
#' @importFrom storage filesystem
default_storage <- function (storage)
{
  if (!missing(storage))
    stop("not implmemented yet")

  filesystem(tempdir())
}


#' @export
all_objects <- function (x = default_storage()) {
  if (is_object_store(x)) {
    return(as_collection(x))
  }
  if (is_collection(x)) {
    return(as_collection(collection_storage(x)))
  }
  stop('cannot extract a collection from ', class(x)[[1]], call. = FALSE)
}
