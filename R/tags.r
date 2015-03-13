# --- check & eval -----------------------------------------------------

all_tags <- function (obj, dots, tags) {
  # lazy tags
  new_tags <- if (!missing(dots)) eval_tags(dots, obj) else list()
  if (!check_tags(dots)) {
    stop('dots have to evaluate to a named list of atomic values',
         call. = FALSE)
  }

  # add default tags
  new_tags$.class   <- class(obj)
  new_tags$.created <- Sys.time()
  
  # pre-computed tags, copy only these that do not yet exist
  if (!missing(tags)) {
    if (!check_tags(tags)) {
      stop('tags have to be a named list of atomic values',
           call. = FALSE)
    }
    cpy <- setdiff(names(tags), names(new_tags))
    new_tags <- c(new_tags, tags[cpy])
  }
  
  #
  new_tags
}


# tags must be a named list of atomic values
check_tags <- function (tags) {
  if (!length(tags)) return(TRUE)
  if (!is.list(tags)) return(FALSE)
  if (!all(nchar(names(tags)))) return(FALSE)
  if (!all(vapply(tags, is.atomic, logical(1)))) return(FALSE)
  T
}


#' @importFrom lazyeval lazy_eval
eval_tags <- function (dots, obj) {
  data <- list(. = obj)
  if (is.list(obj))
    data <- c(data, obj)
  lazy_eval(dots, data)
}

