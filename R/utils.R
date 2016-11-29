
#' @importFrom digest sha1
id_of <- function (obj)
{
  sha1(obj)
}


into_file_name <- function (string)
{
  chartr("\\/", "__", string)
}
