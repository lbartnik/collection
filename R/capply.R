#' Apply a funtion to each object in a colletion.
#'
#' Both \code{capply} and \code{capply_} both apply a function
#' \code{fun} on each object in collection \code{col}. \code{capply} is
#' designed for interactive use while \code{capply_} gives more control
#' over the exact way the call is executed.
#'
#' \code{capply} by default executes the task immediately
#' \code{execute = TRUE}) and returns execution results as a regular
#' \code{list}.
#'
#' \code{capply_} by default returns an \emph{execution task} object
#' (\code{execute = FALSE}) which then can be passed to an explicit
#' \emph{execution} method. Combined with the \code{\link{store}} method
#' it enables a scenario where results are never aggregated in memory
#' as a \code{list}.
#'
#' \code{capply} will additionally produce messages that instruct
#' the interactive user how to access the result of the call.
#'
#' @param col A \code{\link{collection}} object.
#' @param fun A \code{function} object.
#' @param ... Extra arguments for \code{fun}.
#' @param execute Whether to execute the call immediately (\code{TRUE})
#'        or return an \emph{execution task} object (\code{FALSE}).
#'
#' @rdname capply
#' @export
capply <- function (col, fun, ..., execute = TRUE) {
  stopifnot(is_collection(col), is.function(fun), is.logical(execute))
  args <- list(...)

  if (isTRUE(execute)) {
    return(locally(col, fun, args))
  }

  stop("execute must be set to TRUE", call. = FALSE)
}


#' @rdname capply
#' @export
capply_ <- function (col, fun, ..., execute = FALSE) {
  stop()
}



locally <- function (col, fun, args)
{
  stopifnot(is_collection(col), is.function(fun), is.list(args))

  lapply(as.character(col), function (id) {
    obj_tags <- os_read(collection_storage(col), id)
    args$object <- obj_tags$object
    args$tags   <- obj_tags$tags
    do.call(fun, args)
  })
}

