# Public API for the collection package.
#
# Top-level functions and methods implemented specifically in other
# files.


#' Store data in a collection.
#'
#' If the first argument is a collection, the \emph{store} operation is
#' performed in a single step - the whole object is stored under a single
#' identifier (key). If the first argument is the object, the \emph{store}
#' operation variant is chosen based on the object's class. E.g. a plain
#' \code{list} will be broken down into \code{n} \emph{store} operations,
#' one for each element of the list.
#'
#' @param x Object (contents) or collection (destination).
#' @param y Collection or object.
#' @param group Optional group name, especially useful when storing
#'              multiple, related objets.
#'
#' @rdname store
#' @export
#'
store <- function (x, y, group, ...) UseMethod("store")



#' @description The default \code{store} variant writes all data under
#' a single key (identifier).
#'
#' @param obj Object to be store.
#' @param collection Collection to store \code{obj} in.
#'
#' @rdname store
#' @export
#'
store.default <- function (obj, collection, group, ...) {
  store(collection, obj, group, ...)
}


#' @description The \code{list} variant of the \code{store} method stores
#' each element of the list under a separate key (identifier).
#'
#' @return \code{store.list} returns a \emph{clist}, which is a handle to
#' a set of objects.
#'
#' @rdname store
#' @export
#'
store.list <- function (obj, collection, group, ...)
{
  # if it's more than just a list (an object), store as a single entity
  if (!identical(class(obj), 'list')) {
    store(collection, obj, group, ...)
  }
  else {
    lapply(obj, function (x) {
      store(collection, x, group, ...)
    })
  }
}

