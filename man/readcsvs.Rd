% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readcsvs.R
\name{readcsvs}
\alias{readcsvs}
\title{Load Fast Track csv files}
\usage{
readcsvs(path, fileinformation = NA, progressbar = TRUE, write = FALSE)
}
\arguments{
\item{path}{The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.}

\item{fileinformation}{--.}

\item{progressbar}{if TRUE, a progress bar prints out in the console.}

\item{write}{--.}
}
\value{
A dataframe or list of dataframes, as determined by the \code{asone} parameter.
}
\description{
This function quickly reads in all the files contained in the "/csvs" folder within a Fast Track directory. Since Fast Track exports the acoustic measurements into separate files per token, this function is a quick way to read those all in at once.
}
\details{
Note that missing F0 data is stored as \code{0} in the .csv files. These are converted to \code{NA} when read in using this function.
}
\examples{
\dontrun{
csvs <- readcsvs()
}
}
