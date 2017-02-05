
#' @importFrom digest sha1
hash <- function (obj)
{
  sha1(obj)
}


as_file_name <- function (string)
{
  chartr("\\/", "__", string)
}
