#' @rdname collecion
#' @export
#' 
collection <- function (name, storage_factory = filesystem(getwd()), create = FALSE)
{
  stopifnot(is_storage(storage))

  # returns an object of the base class that simply points to the storage
  # (all objects)
  #
  # to point to a subset of objects a new class needs to be introduced:
  # a "clist" - that is, a "collection list"; it will be a vector of IDs
  # plus storage handle as its attribute
  
  # TODO add one more level of directories: objects, meta, graph of creation
  storage <- storage_factory(dir, name, create)
  structure(list(storage = storage), class = 'collection')
}


is_collection <- function (x) inherits(x, 'collection')



# --- storing & restoring ----------------------------------------------



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
#' @rdname collecion
#' @name store.collection
#' @export
#' 
store.collection <- function (col, object, ..., id = id_of(object), .auto_tags = TRUE)
{
  stopifnot(is_collection(col))
  
  # TODO auto_tags
  
  write_object(col$storage, object, id)
  write_tags(col$storage, list(...), id)
  
  id
}



#' @rdname collection
#' @export
#' 
restore.collection <- function (col, id, .simplify = TRUE)
{
  stopifnot(is_collection(col))
  stopifnot(is.character(id))

  # TODO handle the missing(id) case

  res <- lapply(id, function (id) read_object(col$storage, id))

  if (length(res) == 1 && isTRUE(.simplify))
    return(res[[1]])
  res
}



#' @export
do.collection <- function (col, fun, ids, ..., lazy = FALSE)
{
  # TODO handle lazy

  if (missing(ids)) {
    ids <- list_ids(col$storage)
  }

  run_fun(col$storage, ids, function (obj, tags) fun(obj, ...))
}




run_fun <- function (storage, id, fun)
{
  stopifnot(is_storage(storage))
  stopifnot(is.character(id))
  stopifnot(is.function(fun))

  lapply(ids, function (id) {
    obj  <- read_object(storage, id)
    tags <- read_tags(storage, id)
    fun(obj, tags)
  })
}

