# Applying a function to a set of objects is the central operation for
# the whole package. All other operations (filtering, grouping, printing,
# etc.) are implemented on top of various apply-like functions. Therefore,
# to introduce a new backend, it is enough to implement apply methods
# for this new backend.


#' @export
#' @importFrom lazyeval lazy
clply <- function (col, fun, .parallel = NA) {
  env <- parent.frame()
  fun <- substitute(fun)
  fun <- lazy_(fun, env)
  clply_(col, fun, .parallel)
}

#' @export
clply_ <- function (col, lazy_fun, .parallel = NA, ...) {
  stopifnot(is_collection(col))
  stopifnot(is_lazy(lazy_fun))
  # TODO make sure function accepts two arguments: obj & tags
  UseMethod('clply_')
}


#' @export
#' @importFrom lazyeval lazy
tlply <- function (col, fun, .parallel = NA) {
  env <- parent.frame()
  fun <- substitute(fun)
  fun <- lazy_(fun, env)
  clply_(col, fun, .parallel)
}

#' @export
tlply_ <- function (col, lazy_fun, .parallel = NA, ...) {
  stopifnot(is_collection(col))
  stopifnot(is_lazy(lazy_fun))
  # TODO make sure function accepts argument tags
  UseMethod('tlply_')
}


# --- library code -----------------------------------------------------

#' Process one object.
process_object <- function (id, fun, preprocess, postprocess) {
  tryCatch({
      data <- preprocess(id)
      res  <- do.call(fun, data)
      res  <- postprocess(id, res)
    },
    error = function(e)e
  ) # tryCatch
}


#' Apply function to all objects in collection.
#' 
#' @param .parallel Object used to recognize the desired parallel
#'        backend.
#' @param col Collection, also filtered collection.
#' @param lazy_fun Function to be applied to all objects (a
#'        \code{\link[lazyeval]{lazy}}) object).
#' @param preprocess Function to be called on each id to obtain the data.
#' @param postprocess Function to be called on each result (e.g. to store
#'        it in another collection).
#' @return A \code{list} of results: either \code{postprocess}ed objects
#'         or exceptions from \code{tryCatch} if an error occured
#'         for any of the objects.
#'
#' @export
xxply <- function (.parallel, col, lazy_fun, preprocess, postprocess)
  UseMethod('xxply')


#' @rdname xxply
#' 
#' @description  The default \code{xxply} implementation runs \code{fun}
#' via \code{\link{lapply}}. If \code{.parallel} is not \code{NA} a
#' warning message is issued.
#' 
#' @importFrom lazyeval lazy_eval
xxply.default <- function (.parallel, col, lazy_fun, preprocess, postprocess) {
  if (!is.na(.parallel)) {
    warning('backend not recognized, running via lapply', call. = FALSE)
  }
  
  fun <- lazy_eval(lazy_fun)
  lapply(col, process_object,
        fun = fun, preprocess = preprocess, postprocess = postprocess)
}


#' @rdname xxply
#' 
#' @description If \code{.parallel} is a \code{numeric} it is assumed to
#' be the number of cores and \code{fun} is run via
#' \code{\link[parallel]{mclapply}}, where \code{.parallel} is passed as
#' \code{mc.cores}.
#' 
#' @importFrom parallel mclapply
#' @importFrom lazyeval lazy_eval
xxply.numeric <- function (.parallel, col, lazy_fun, preprocess, postprocess) {
  if (.parallel <= 0) {
    stop('the number of cores must be positive', call. = FALSE)
  }
  
  fun <- lazy_eval(lazy_fun)
  mclapply(col, process_object,
           fun = fun, preprocess = preprocess, postprocess = postprocess,
           mc.cores = .parallel)
}


#' @rdname xxply
#' 
#' @description \code{xxply_defer}: another backend based on defer
#' (planned).
xxply_defer <- function (.parallel, col, lazy_fun, preprocess, postprocess) {
  # TODO implement on top of defer
}


