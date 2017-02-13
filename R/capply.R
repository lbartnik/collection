
#' @export
capply <- function (x, fun, ...) UseMethod('capply')


#' @export
capply.default <- function (x, fun, ...) {
  stop('cannot run capply() on object of class ', class(x)[[1]],
       call. = FALSE)
}


capply.collection <- function (col, fun, ...) {
  capply(as_clist(col), fun, ...)
}


capply.clist <- function (cl, fun, ...) {
  # TODO this should go into here(); this method simply prepares
  #      the computational task; possibly could be based on defer
  lapply(cl, function (id) {
    pair <- restore(clist_collection(cl), id)
    result <- try(fun(pair$object, pair$tags, ...))
  })
}



