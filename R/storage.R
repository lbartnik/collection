# Storage.
#
# Basic operations:
#   1. store single objects and their tags
#   2. find objects by id
#   3. find objects by tags
#   4. find a single object, find multiple objects
#


is_storage <- function (x) inherits(x, 'storage')

write_object <- function (storage, object, id = hash(object)) UseMethod("write_object")

write_tags <- function (storage, tags, id) UseMethod("write_tags")

read_object <- function (storage, id) UseMethod("read_object")

read_tags <- function (storage, id) UseMethod("read_tags")

list_ids <- function (storage) UseMethod("list_ids")



# --- filesystem storage -----------------------------------------------


filesystem <- function (path = getwd(), create = FALSE)
{
  assert_dir(path, create)
  function (name, create = FALSE) new_filesystem(path, name, create)
}


new_filesystem <- function (path, name, create = FALSE)
{
  assert_dir(file.path(path, as_file_name(name)), create)
  structure(path, class = c('storage', 'filesystem'))
}


is_filesystem <- function (x) is_storage(x) && inherits(x, 'filesystem')



write_object.filesystem <- function (storage, object, id)
{
  stopifnot(is_filesystem(storage), is_nonempty_character(id))

  path <- full_path(storage, id, '.rds', .create = TRUE)
  saveRDS(object, path)
}


write_tags.filesystem <- function (storage, tags, id)
{
  stopifnot(is_filesystem(storage), is_nonempty_character(id))
  
  path <- full_path(storage, id, '_tags.rds', .create = TRUE)
  saveRDS(tags, path)
}


read_object.filesystem <- function (storage, id)
{
  stopifnot(is_filesystem(storage), is_nonempty_character(id))
  readRDS(full_path(storage, id, '.rds'))
}


read_tags.filesystem <- function (storage, id)
{
  stopifnot(is_filesystem(storage), is_nonempty_character(id))
  readRDS(full_path(storage, id, '_tags.rds'))
}



#' @importFrom tools file_path_sans_ext
list_ids.filesystem <- function (storage)
{
  stopifnot(is_filesystem(storage))
  
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


assert_dir <- function (path, create)
{
  if (!dir.exists(path) && isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (!dir.exists(path)) {
    stop('could not create directory ', path, call. = FALSE)
  }
}
