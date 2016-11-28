#' @export
collection <- function (name)
{
  # returns an object of the base class that simply points to the storage
  # (all objects)
  #
  # to point to a subset of objects a new class needs to be introduced:
  # a "clist" - that is, a "collection list"; it will be a vector of IDs
  # plus storage handle as its attribute
}


#' @return Object identifier.
#' @export
store <- function (col, object, ..., id = id_of(object), .auto_tags = TRUE)
{
  
}


#' @export
restore <- function (col, id, tags)
{
  # 1. restore() needs to be a method
  # 2. classes are: collection + id, clist
  #    for clist() see collection()
}

