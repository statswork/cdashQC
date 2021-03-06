% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_included.R
\name{create_included}
\alias{create_included}
\title{an integrated data set.}
\usage{
create_included(ex, cr, ds)
}
\arguments{
\item{ex}{the dataset ex read from sas}

\item{cr}{the dataset cr read from sas}

\item{ds}{the dataset ds read from sas}
}
\value{
the included data set which contains the following information
\item{CLIENTID}{the patient ID}
\item{SCRID}{the corresponding screening ID}
\item{SEQ}{the treatment sequence, from \code{ex} data}
\item{PERIOD}{CRF study period, from \code{ex} data}
\item{EX_ROUTE}{Route}
\item{EX_FORM}{Formulation}
\item{EX_STDAT}{Date of first dosing}
\item{EX_STTIM}{Time of first dosing}
\item{EX_DOSE}{Dose Amount}
\item{EX_LASTDOSEDAT}{Date of last dosing (Created in this function)}
\item{EX_LASTDOSETIM}{Time of last dosing}
\item{CHK_IN_DAT}{date of check-in for each period}
\item{CHK_IN_TIM}{time of check in for each period}
\item{RETURN_DAT}{date of return for each period}
\item{RETURN_TIM}{time of return for each period}
\item{DS_TRM_D}{Primary reason for discontinuation}
\item{DS_SPEC}{specify}
\item{DS_STDAT}{Date of completion or discontinuation per subject}
\item{DS_LSDAT}{Date of last contact per subject}
}
\description{
create_included.
}

