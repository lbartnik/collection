#' @export
clist <- function () {}

#' Create a \emph{collection list}.
#'
#' A \code{clist} is a container of references to objects stored in a
#' \code{collection}. When used with the \emph{verb} functions
#' (\code{filter}, \code{arrange}, \code{apply})
#'
#' @param col A \code{collection} or another \code{clist} (?)
#' @return A \code{clist}.
#' @export
#'
as_clist <- function (col)
{
  # from a collection
  if (is_collection(col)) {
    id <- list_ids(col$storage)
    attr(id, 'colletion') <- col
    class(id) <- 'clist'
    return(id)
  }

  stop('cannot create a clist from ', class(col)[[1]], call. = FALSE)
}


clist_collection <- function (clist)
{
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

  restore(clist_collection(clist), as.character(clist))
}


#' @export
do.clist <- function (clist, fun, ..., lazy = FALSE)
{
  do(clist_collection(clist), as.character(clist), fun, ..., lazy = lazy)
}





