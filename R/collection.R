#' Create a collection.
#'
#' @param ... All values are concatenated and \code{unlist}ed into
#'        a \code{character} vector of identifiers that must be present
#'        in the \code{storage}.
#' @param storage A \code{\link[storage]{storage}} object where actual
#'        objects are stored.
#'
#' @rdname collecion
#' @export
#'
#' @importFrom storage is_object_store os_exists
#'
collection <- function (..., storage)
{
  stopifnot(is_object_store(storage))

  ids <- unlist(list(...), recursive = TRUE)
  stopifnot(is.character(ids), all(os_exists(storage, ids)))

  class(ids) <- 'collection'
  attr(ids, 'storage') <- storage
  ids
}


#' @rdname collecion
#' @export
is_collection <- function (x) inherits(x, 'collection')


collection_storage <- function (x)
{
  stopifnot(is_collection(x))
  attr(x, 'storage')
}


#' @rdname collecion
#' @export
as_collection <- function (x, ...) UseMethod("as_collection")

#' @rdname collecion
#' @export
as_collection.default <- function (x, storage) {
  stop('cannot cast ', class(x)[[1]], ' to collection', call. = FALSE)
}

#' @description In the \code{as_collection} for the \code{object_store}
#'              the \code{storage} parameter is ignored.
#'
#' @rdname collecion
#' @export
as_collection.object_store <- function (x, storage) {
  collection(os_list(x), storage = x)
}

#' @rdname collecion
#' @export
as_collection.character <- function (x, storage) {
  collection(x, storage = storage)
}



#' @rdname collecion
#' @export
add <- function (col, object, ...) {
  tags <- list(...)
  if (!all(nchar(names(tags)) > 0)) {
    stop("all tags must be named", call. = FALSE)
  }

  add_(col, object, tags)
}


#' @rdname collecion
#' @export
add_ <- function (col, object, tags, auto_tags = FALSE) {
  stopifnot(is_collection(col))

  if (isTRUE(auto_tags)) {
    stop('auto_tags not implemented yet', call. = FALSE)
    # TODO extract automatic tags and make sure they don't overlap
    #      with user-provided ones
  }

  id <- os_write(collection_storage(col), object, tags)
  as_collection(c(col, id), storage = collection_storage(col))
}


#' Taps into \code{dplyr}'s filtering method.
#'
#' @param .data A \code{collection} object.
#' @param ... Filtering expression.
#' @param .dots A list of \code{lazyeval} expressions.
#'
#' @return A \code{clist} whose elements match the specified filter.
#'
#' @export
#'
filter_.collection <- function (.data, ..., .dots)
{
  stopifnot(is_collection(.data))
  clist <- as_clist(.data)

  # it only makes sense to implement search (filter) logic once and
  # clist will need it much more often than collection
  dots <- lazyeval::all_dots(.dots, ...)
  filter_(clist, .dots = dots)
}

