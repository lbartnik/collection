% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clist.R, R/collection.R
\name{restore.clist}
\alias{restore}
\alias{restore.clist}
\alias{restore.collection}
\alias{store}
\title{Store objects in a collection and read them back.}
\usage{
\method{restore}{clist}(col, .simplify = TRUE)

store(col, object, ..., id = id_of(object), .auto_tags = TRUE)

restore(col, ..., .simplify = TRUE)

\method{restore}{collection}(col, ..., .simplify = TRUE)
}
\arguments{
\item{col}{A \code{\link{collection}}.}

\item{.simplify}{If only one object can be restored do not wrap it in
a \code{list}.}

\item{object}{Any object.}

\item{...}{Named tags (\code{store}) or additional parameters (\code{restore}).}

\item{id}{Object identifier in the collection.}

\item{.auto_tags}{Generate and store standard tags.}
}
\value{
Object identifier.
}
\description{
Store objects in a collection and read them back.

\code{restore} reads an object back from a
\code{\link{collection}}.
}

