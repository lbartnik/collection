#' Produce a sample collection.
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
  
  return(c)
}
