#' @importFrom digest digest
hash32 <- function (x) digest(x, 'xxhash32')

#' @importFrom digest digest
md5 <- function (x) digest(x, 'md5')


is_lazy <- function (x) inherits(x, 'lazy')