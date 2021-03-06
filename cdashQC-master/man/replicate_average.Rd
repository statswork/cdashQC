% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_stats.R
\name{replicate_average}
\alias{replicate_average}
\title{get the averanges of the replicates for \code{vs} or \code{eg} data.}
\usage{
replicate_average(data_clean, digits = NULL, na_rm = TRUE)
}
\arguments{
\item{data_clean}{an object returned from \code{replicate_clean}}

\item{digits}{should the averages be rounded? Default NO.}

\item{na_rm}{should missing values be excluede? Default \code{TRUE}.}
}
\value{
the averages
}
\description{
Get the averages of the replicates
}
\examples{
eg2 <- replicate_data(eg) # find the triplicates
}
\seealso{
\code{\link{replicate_clean}}
}

