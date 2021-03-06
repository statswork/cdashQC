% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/useful.R
\name{get_summary_stats}
\alias{get_summary_stats}
\title{get summary statistics}
\usage{
get_summary_stats(data, group = "EX_TRT_C", var = "race", na.rm = TRUE)
}
\arguments{
\item{data}{the data}

\item{group}{which column should be the group}

\item{var}{which columns are used to summarize}

\item{na.rm}{should missing value be removed? \code{TRUE} by default.}
}
\value{
a data frame
}
\examples{
SEQ = rep(c("A", "B", "C"), 3); subtype = sample(c("ONE", "TWO", "THREE"), 9, replace = TRUE)
data <- data.frame(SEQ, subtype, BMI = rnorm(9, 25, 4), HEIGHT = rnorm(9, 175,3)) 
get_summary_stats(data, group = "SEQ", var = "subtype")
get_summary_stats(data, group = "SEQ", var = "BMI")
}

