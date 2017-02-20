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



# ---- private collection API ----


search_ids <- function (col, tags)
{
  stopifnot(is_collection(col))

  lapply(list_ids(col$storage), function (id) {
    tags <- read_tags(col$storage, id)
    # TODO verify whether object matches the filter
    stop("not implemented yet")
  })
}

