
#' @importFrom digest sha1
hash <- function (obj)
{
  sha1(obj)
}

id_of <- function (obj) {
  # TODO can be cached as attribute of obj
  hash(obj)
}


as_file_name <- function (string)
{
  chartr("\\/ ", "___", string)
}
