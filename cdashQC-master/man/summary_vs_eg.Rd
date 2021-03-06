% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_stats.R
\name{summary_vs_eg}
\alias{summary_vs_eg}
\title{summary statistics}
\usage{
summary_vs_eg(data_clean, inter_digit = NULL, final_digits = 3,
  na_rm = TRUE, ischangefrombase = FALSE)
}
\arguments{
\item{data_clean}{the clean data set returned by \code{\link{replicate_clean}}.}

\item{inter_digit}{if rounding happens for the intermediate averages, what digits should be kept.}

\item{na_rm}{should missing values be excluede? Default set to be \code{TRUE}.}

\item{ischangefrombase}{Is this summary for change from baseline? Default set to be \code{FALSE}}

\item{final_digit}{what is the digit for final summary?}
}
\value{
a data frame with summary statistics by test, time point and treatment.
}
\description{
Summary statistics for vital signs or eg
}

