#' Produce sample data or collection.
#'
#' @description \code{sample_time_series()} generates a list of
#' \code{data.frame}s, each containing a sample time series. There are
#' four columns in each \code{data.frame}: columns \emph{a}, \emph{b}
#' and \emph{c} describe three independent variables and column \emph{x}
#' describes the dependent time series.
#'
#' @param n Number of objects to generate.
#' @param len Number of samples in each series.
#' @param seed Seed for the random number generator.
#'
#' @return A list of time series data frames.
#' @export
#' @rdname examples
#'
sample_time_series <- function (n = 10, len = 84, seed = 1)
{
  set.seed(seed)
  lapply(seq(n), function (i) {
    data <- data.frame(
      a = cumsum(rnorm(len)),
      b = cumsum(rnorm(len)),
      c = cumsum(rnorm(len))
    )
    data$x <- with(data, .1*a + .2*b + 0.3*c) + rnorm(len)
  })
}


#' @description \code{sample_collection()} internally calls
#' \code{sample_time_series()} and stores its output in a file-based
#' collection.
#'
#' @param path Where to create the collection.
#' @return \code{sample_collection()} returns a \code{collection} object.
#'
#' @export
#' @rdname examples
#'
sample_collection <- function (path = tempdir(), n = 10, len = 84, seed = 1)
{
  data <- sample_time_series(n, len, seed)
  sample <- collection('sample', TRUE, filesystem(path))
  store(data, sample)
  sample
}

