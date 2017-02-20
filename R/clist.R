#' @export
clist <- function () {}


#' Create a \emph{collection list}.
#'
#' A \code{clist} is a container of references to objects stored in a
#' \code{collection}. When used with the \emph{verb} functions
#' (\code{filter}, \code{arrange}, \code{apply})
#'
#' @param col A \code{collection} or another \code{clist} (?)
#' @return \code{as_clist()} return an object of class \emph{clist}.
#'
#' @export
#' @rdname clist
#'
as_clist <- function (col)
{
  # from a collection
  if (is_collection(col)) {
    clist <- list_ids(col$storage)
    class(clist) <- 'clist'
    attr(clist, 'collection') <- col
    return(clist)
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
is_clist <- function (x) inherits(x, 'clist')


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


#' @export
do.clist <- function (clist, fun, ..., lazy = FALSE)
{
  do(extract_collection(clist), as.character(clist), fun, ..., lazy = lazy)
}





