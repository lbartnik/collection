
#' Filter objects in a collection or clist.
#' 
#' @param col Collection or a \code{clist}.
#' @return A \code{clist}.
#' 
#' @rdname filter
#' @name filter
NULL


#' @rdname filter
filter.collection <- function (col, ...)
{
  # TODO take all object from a collection, turn them into a clist
  #      and run the filter
  return(filter(clist(col), ...))
}

#' @rdname filter
filter.clist <- function (col, ...)
{
  # TODO run filter
}



#' Apply a function to objects in a collection or clist.
#' 
#' @param col Collection or a \code{clist}.
#' @param fun A \code{function} or a \code{deferred} fuction object.
#' 
#' @return A \code{list} of values returned from calls.
#' 
#' @rdname filter
#' @name filter
NULL


apply.collection <- function (col, fun, ...)
{
  
}


apply.clist <- function (col, fun, ...)
{
  
}
