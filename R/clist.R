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
clist <- function (col)
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




#' @rdname store
#' @export
#' 
restore.clist <- function (col, .simplify = TRUE)
{
  clist <- col
  col   <- clist_collection(col)
  restore(col, as.character(clist))
}

