#' @export
collection <- function (name, dir = getwd())
{
  # returns an object of the base class that simply points to the storage
  # (all objects)
  #
  # to point to a subset of objects a new class needs to be introduced:
  # a "clist" - that is, a "collection list"; it will be a vector of IDs
  # plus storage handle as its attribute
  
  # TODO add one more level of directories: objects, meta, graph of creation
  storage <- filesystem_storage(file.path(dir, into_file_name(name)))
  structure(list(storage = storage), class = 'collection')
}


is_collection <- function (x) inherits(x, 'collection')


#' Store objects in a collection and read them back.
#' 
#' @param col A \code{\link{collection}}.
#' @param object Any object.
#' @param ... Named tags (\code{store}) or additional parameters (\code{restore}).
#' @param id Object identifier in the collection.
#' @param .auto_tags Generate and store standard tags.
#' 
#' @return Object identifier.
#' 
#' @rdname store
#' @name store
#' @export
#' 
store <- function (col, object, ..., id = id_of(object), .auto_tags = TRUE)
{
  stopifnot(is_collection(col))
  
  # TODO auto_tags
  
  write_object(col$storage, object, id)
  write_tags(col$storage, list(...), id)
  
  id
}


#' @description \code{restore} reads an object back from a
#' \code{\link{collection}}.
#' 
#' @param .simplify If only one object can be restored do not wrap it in
#'        a \code{list}.
#'
#' @rdname store
#' @export
#' 
restore <- function (col, ..., .simplify = TRUE)
{
  UseMethod("restore")
}


#' @rdname store
#' @export
#' 
restore.collection <- function (col, id, .simplify = TRUE)
{
  stopifnot(is_collection(col))
  stopifnot(is.character(id))

  res <- lapply(id, function (id) read_object(col$storage, id))

  if (length(res) == 1 && isTRUE(.simplify))
    return(res[[1]])
  res
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
