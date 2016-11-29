#' Produce a sample collection.
#' 
#' @param n Number of objects to generate.
#' @param len Number of samples in each series.
#' @param dir Parent directory for the new collection.
#' 
#' @return A \code{\link{collection}} object.
#' @export
#' 
sample_time_series <- function (n = 10, len = 84, dir = tempdir())
{
  c <- collection('time_series', dir = dir)

  lapply(seq(n), function (i) {
    data <- data.frame(
      a = cumsum(rnorm(len)),
      b = cumsum(rnorm(len)),
      c = cumsum(rnorm(len))
    )
    data$x <- with(data, .1*a + .2*b + 0.3*c) + cumsum(rnorm(len))
    store(c, data, no = i)
  })
  
  return(c)
}


