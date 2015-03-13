# File-system-based collection is stored in a local directory in the
# file system. The main directory contains two sub-entries:
#
#  - "comment.rds" which holds the optional comment given upon collection's
#    creation
#  - "data" - a directory where all object and tag files are stored
#
#

# --- constructor ------------------------------------------------------

fs_collection <- function (path, comment, .create = FALSE, .ro = TRUE) {
  if (!file.exists(path)) {
    if (!.create) stop('directory does not exist and .create is FALSE',
                       call. = FALSE)
    return(create_fs_collection(path, comment))
  }
  
  # read from an existing collection
  col <- structure(list_ids(path),
                   class     = c('fs_collection', 'collection'),
                   path      = path,
                   read_only = .ro)
  #verify_files(col)
  
  col
}


# creates a new collection
create_fs_collection <- function (path, comment) {
  stopifnot(!file.exists(path))         # dir does not exist
  stopifnot(file.exists(dirname(path))) # up-dir must exist
  
  dir.create(path)
  dir.create(file.path(path, 'data'))
  
  # if cared to comment
  if (!missing(comment))
    saveRDS(as.character(comment), file.path(path, 'comment.rds'))
  
  structure(character(), path = path, class = c('fs_collection', 'collection'))
}


# --- utilities --------------------------------------------------------

make_path <- function (col) {
  stopifnot(all(nchar(col) >= 4))
  paths <- file.path(attr(col, 'path'), 'data', substr(col, 1, 2), substr(col, 3, 4), col)
  normalizePath(paths, mustWork = FALSE)
}


#' @importFrom dplyr %>%
list_ids <- function (path) {
  file.path(path, 'data') %>%
    list.files('^[a-z0-9]+.rds$', recursive = T, full.names = F) %>%
    basename %>%
    {gsub('.rds$', '', .)}
}


read_both <- function (path) {
  paths <- paste0(path, c('.rds', '_tags.rds'))
  if (any(!file.exists(paths))) {
    stop('could not read objects ', path, '*', call. = FALSE)
  }
  
  res <- lapply(paths, readRDS)
  `names<-`(res, c('obj', 'tags'))
}


read_tags <- function (path) {
  tpath <- paste0(path, '_tags.rds')
  if (!file.exists(tpath)) {
    stop('could not read tags ', path, '*', call. = FALSE)
  }
  list(tags = readRDS(tpath))
}


# --- apply ------------------------------------------------------------

#' @export
clply_.fs_collection <- function (col, lazy_fun, .parallel = NA) {
  res <- xxply(.parallel, make_path(col), lazy_fun, read_both, function(id, res)res)
  `names<-`(res, as.character(col))
}


#' @export
tlply_.fs_collection <- function (col, lazy_fun, .parallel = NA) {
  res <- xxply(.parallel, make_path(col), lazy_fun, read_tags, function(id, res)res)
  `names<-`(res, as.character(col))
}


# --- adding objects ---------------------------------------------------

#' Objects in collection can repeat if with different tags.
add_object_internal.fs_collection <- function (col, obj, tags) {
  
  # hash of the object's contents
  id   <- md5(list(obj, tags))
  ncol <- from_collection(id, col)
  path <- make_path(ncol)
  
  # destination file
  if (file.exists(paste0(path, '.rds')))
    stop('object already', call. = F)
  
  dir.create(dirname(path), recursive = T, showWarnings = F)

  # save data to filesystem
  saveRDS(obj, paste0(path, '.rds'))
  saveRDS(tags, paste0(path, '_tags.rds'))
  
  ncol
}

