
#' Store objects in a collection and read them back.
#'
#' @param col A \code{\link{collection}}.
#' @param obj Any object.
#' @param group Store under this group name.
#' @param ... Named tags (\code{store}) or additional parameters (\code{restore}).
#'
#' @return Object identifier.
#'
#' @rdname collecion
#' @name store.collection
#' @export
#'
store.collection <- function (col, object, group = NULL, ...)
{
  stopifnot(is_collection(col))
  if (!is.null(group)) stopifnot(is_nonempty_character(group))

  id <- id_of(object)
  if (find_id(col$storage, id)) {
    warning("object already present in collection, not storing", call. = FALSE)
  }


  tags <- list(...)
  tags$group <- if (!is.null(group)) group else id
  # TODO auto_tags

  write_object(col$storage, object, id)
  write_tags(col$storage, tags, id)

  id
}


restore.collection <- function (col, id, ...)
{
  stopifnot(is_collection(col))
  stopifnot(missing(id) || find_id(col$storage, id))

  tags <- list(...)
  if (!missing(id) && (length(tags) > 0)) {
    stop("use either `id` or tags to restore from a collection",
         call. = FALSE)
  }

  if (!missing(id)) {
    return (list(
      object = read_object(col$storage, id),
      tags   = read_tags(col$storage, id)
    ))
  }
  else {
    # TODO search for ids
    stop("not implemented yet")
  }
}




#' Search for objects whose tags match filter(s).
#'
#' @param col A \code{collection} object.
#' @param dots List of lazy-eval expressions.
#'
#' @return A vector of object identifiers.
#'
search_ids <- function (col, dots)
{
  stopifnot(is_collection(col))
  stopifnot(is.list(dots), inherits(dots, "lazy_dots"))

  ids <- list_ids(col$storage)
  ans <- lapply(ids, function (id) {
    tags <- read_tags(col$storage, id)
    ans <- vapply(dots, function (lazy) {
      isTRUE(lazyeval::lazy_eval(lazy, data = tags))
    }, logical(1))
    all(ans)
  })

  ids[unlist(ans)]
}


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
#'              multiple (related) objets.
#' @param ... Optional tags.
#'
#' @return Each \code{store()} method returns a \emph{clist}, which is
#' a handle to a set of objects in a \emph{collection}.
#'
#' @rdname store
#' @export
#'
store <- function (x, y, group = NULL, ...) UseMethod("store")



#' @description The default \code{store()} variant writes all data under
#' a single key (identifier).
#'
#' @param obj Object to be store.
#' @param collection Collection to store \code{obj} in.
#'
#' @rdname store
#' @export
#'
store.default <- function (obj, collection, group = NULL, ...) {
  store(collection, obj, group, ...)
}



#' @description The \code{list} variant of the \code{store()} method
#' stores each element of the list under a separate key (identifier).
#' This method does not take any additional, single-object-level tags
#' via \code{...}
#'
#' @rdname store
#' @export
#'
store.list <- function (obj, collection, group = NULL)
{
  # if it's more than just a list (an object), store as a single entity
  # but wa
  if (!identical(class(obj), 'list')) {
    warning("because `obj` has a class other than `list` it is stored as a single object",
            call. = FALSE)
    store(collection, obj, group)
  }
  else {
    lapply(obj, function (x) {
      store(collection, x, group)
    })
  }
}



#' Restore an object.
#'
#' @param from Restore from collection, clist...
#' @param id Restore object by its identifier.
#' @param ... Restore object(s) by their tags.
#'
#' @export
restore <- function (from, id, ...)
{
  UseMethod("restore")

  if (length(list(...)) > 0) {
    stop("search by tags is not implemented yet")
  }
}


#' @export
restore.default <- function (from, id, ...)
{
  stop("`from` of class ", paste(class(from), collapse = '::'),
       " is not supported", call. = FALSE)
}



