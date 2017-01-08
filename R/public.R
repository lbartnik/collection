# Public API for the collection package.
#
# Top-level functions and methods implemented specifically in other
# files.


#' @rdname store
#' @export
store <- function (...) UseMethod("store")



#' The default storage method.
#' 
#' @param obj Object to be store.
#' @param collection Collection to store \code{obj} in.
#' 
#' @rdname store
#' @export
store.default <- function (obj, collection, ...) {
  store(collection, obj, ...)
}



#' @description \code{restore} reads an object back from a storage
#' source (e.g. a \code{\link{collection}}).
#' 
#' @rdname store
#' @export
#' 
restore <- function (...)
{
  UseMethod("restore")
}


