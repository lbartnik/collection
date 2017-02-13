#' @rdname collecion
#' @export
#'
collection <- function (name, create = FALSE, storage = filesystem(getwd()))
{
  # returns an object of the base class that simply points to the storage
  # (all objects)
  #
  # to point to a subset of objects a new class needs to be introduced:
  # a "clist" - that is, a "collection list"; it will be a vector of IDs
  # plus storage handle as its attribute

  # TODO add one more level of directories: objects, meta, graph of creation
  storage <- workspace(storage, name, create)
  stopifnot(is_storage(storage))

  structure(list(storage = storage), class = 'collection')
}


is_collection <- function (x) inherits(x, 'collection')


#' @export
length.collection <- function (x) length(list_ids(x$storage))


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
store.collection <- function (col, object, group, ...)
{
  stopifnot(is_collection(col))
  if (!missing(group)) stopifnot(is_nonempty_character(group))

  id <- id_of(object)
  if (find_id(col$storage, id)) {
    warning("object already present in collection, not storing", call. = FALSE)
  }


  tags <- list(...)
  tags$group <- if (!missing(group)) group else id
  # TODO auto_tags

  write_object(col$storage, object, id)
  write_tags(col$storage, tags, id)

  id
}

