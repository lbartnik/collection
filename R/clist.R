#' @export
clist <- function () {}


#' Create a \emph{collection list}.
#'
#' A \code{clist} is a container of references to objects stored in a
#' \code{collection}. When used with the \emph{verb} functions
#' (\code{filter}, \code{arrange}, \code{apply})
#'
#' @param x Object to be coerced to a \code{clist}.
#' @param ... Other arguments, depending on type of \code{x}.
#'
#' @return \code{as_clist()} return an object of class \emph{clist}.
#'
#' @export
#' @rdname clist
#'
as_clist <- function (x, ...)
{
  dots <- list(...)

  # from a collection
  if (is_collection(x)) {
    clist <- list_ids(x$storage)
    class(clist) <- 'clist'
    attr(clist, 'collection') <- x
    return(clist)
  }

  # from a vector of identifiers; requires a collection in ...
  if (is.character(x)) {
    if ('col' %nin% names(dots)) {
      stop('to coerce a character vector, a collection is required', call. = FALSE)
    }
    class(x) <- 'clist'
    attr(x, 'collection') <- dots$col
    return(x)
  }

  stop('cannot create a clist from ', class(col)[[1]], call. = FALSE)
}


#' @description \code{is_clist()} tests whether an object is a \emph{clist}.
#'
#' @param x Object to be tested.
#'
#' @export
#' @rdname clist
#'
is_clist <- function (x) inherits(x, 'clist') && has_attr(x, 'collection')


extract_collection <- function (clist)
{
  stopifnot(is_clist(clist))
  attr(clist, 'collection')
}


#' @description \code{store.clist()} stores the whole \emph{clist} in
#' collection \code{col}.
#'
#' @rdname clist
#' @export
#'
store.clist <- function (clist, col)
{

}


#' @rdname clist
#' @export
#'
restore.clist <- function (clist, .simplify = TRUE)
{
  # no `id` here because if subset is needed one can use filter or
  # the bracket operator

  restore(extract_collection(clist), as.character(clist))
}



# --- extra verbs ---


#' Taps into \code{dplyr}'s filtering method.
#'
#' @param .data A \code{clist} object.
#' @param ... Filtering expression.
#' @param .dots A list of \code{lazyeval} expressions.
#'
#' @return A \code{clist} whose elements match the specified filter.
#'
#' @export
#'
filter_.clist <- function (.data, ..., .dots)
{
  stopifnot(is_clist(.data))
  col  <- extract_collection(.data)

  dots <- lazyeval::all_dots(.dots, ...)
  ids  <- search_ids(col, dots)

  as_clist(ids, col = col)
}


