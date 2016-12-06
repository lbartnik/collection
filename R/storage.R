# 1. store single objects and their tags
# 2. find objects by id
# 3. find objects by tags
# 4. find a single object, find multiple objects


filesystem_storage <- function (path)
{
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) {
    stop('could not create directory ', path, call. = FALSE)
  }
  
  structure(path, class = 'storage')
}


is_storage <- function (x) inherits(x, 'storage')


# --- public interface -------------------------------------------------

write_object <- function (storage, object, id)
{
  stopifnot(is_storage(storage), is_nonempty_character(id))

  path <- full_path(storage, id, '.rds', .create = TRUE)
  saveRDS(object, path)
}


write_tags <- function (storage, tags, id)
{
  stopifnot(is_storage(storage), is_nonempty_character(id))
  
  path <- full_path(storage, id, '_tags.rds', .create = TRUE)
  saveRDS(tags, path)
}


read_object <- function (storage, id)
{
  stopifnot(is_storage(storage), is_nonempty_character(id))
  readRDS(full_path(storage, id, '.rds'))
}


read_tags <- function (storage, id)
{
  stopifnot(is_storage(storage), is_nonempty_character(id))
  readRDS(full_path(storage, id, '_tags.rds'))
}


# --- private interface ------------------------------------------------

#' @importFrom tools file_path_sans_ext
list_ids <- function (storage)
{
  stopifnot(is_storage(storage))
  
  # [^s] means ignore _tags.rds
  files <- basename(list.files(storage, pattern = '*[^s].rds', recursive = TRUE))
  unique(file_path_sans_ext(files))
}


full_path <- function (storage, id, ext, .create = FALSE)
{
  # parent path
  path <- file.path(storage, substr(id, 1, 2), substr(id, 3, 4))
  
  # make sure parent directory exists
  if (isTRUE(.create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(dirname(path))) {
      stop("cannot create directory for id ", id, call. = FALSE)
    }
  }
  
  # return the fill path
  file.path(path, paste0(id, ext))
}



