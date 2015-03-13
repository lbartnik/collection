
is_collection <- function (x) inherits(x, 'collection')


#' Convert a vector of attributes into a collection.
#' 
#' Utility function. Whenever you need to convert one or more
#' identifiers into a collection with attributes copied from
#' \code{col}, use this function.
#' 
#' @param ids Object identifiers.
#' @param col Collection to copy attributes from.
from_collection <- function (ids, col) {
  stopifnot(is_collection(col))
  stopifnot(is.character(ids))
  attributes(ids) <- attributes(col)
  ids
}


# --- collection: constructors -----------------------------------------

#' Read or create a new collection.
#' 
#' @param x A directory path for file-system-based collections.
#' @param comment Used only if \code{.create} is set to \code{TRUE}.
#' @param .create Create a new collection if \code{x} does not exist.
#' 
#' @return A collection object.
#' 
#' @export
collection <- function (x, comment, ..., .create = F) UseMethod('collection')


#' @rdname collection
#' @export
collection.default <- function (x, ...) {
  stop('cannot read/create collection from ', class(x)[1], call. = FALSE)
}


#' @rdname collection
#' @param x Path to a directory.
#' @export
collection.character <- function (x, comment, .create) {
  fs_collection(x, comment, .create)
}


#' Convert object to a collection.
#'
#' @export
as_collection <- function (x, comment, ...) UseMethod('as_collection')


#' @rdname as_collection
#' @export
as_collection.default <- function (x, comment, ...) {
  stop(class(x)[1], ' cannot be converted to a collection',
       call. = FALSE)
}


#' @rdname as_collection
#' @export
as_collection.list <- function (x, comment, ...) {
  path <- file.path(tempdir(), hash32(Sys.time()))
  col  <- fs_collection(path, paste0('temporary collection, created on ', Sys.time()),
                        .create = TRUE, .ro = FALSE)
  lapply(x, function(o) add_object(col, o))
  fs_collection(path)
}


# --- adding objects ---------------------------------------------------

#' @export
#' @importFrom lazyeval lazy_dots
add_object <- function (col, obj, ...) {
  dots <- lazy_dots(...)
  res  <- add_object_(col, obj, dots = dots)
  
  # notify user only in the interactive version of "add"
  if (identical(parent.frame(), globalenv()))
    message("collection has changed, refresh it")
  
  res
}


#' @export
add_object_ <- function (col, obj, dots, tags) {
  # basic checks
  stopifnot(is_collection(col))
  if (!missing(tags)) stopifnot(is.list(tags))
  
  # check and combine .dots & .tags
  tags <- all_tags(obj, dots, tags)
  
  add_object_internal(col, obj, tags)
}


#' Collections should implement this method.
add_object_internal <- function (col, obj, tags) UseMethod('add_object_internal')
