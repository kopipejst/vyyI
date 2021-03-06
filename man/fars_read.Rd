% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Reading and parsing csv}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{path to a data file}
}
\value{
data.frame
}
\description{
It will check if file exist, try to parse as csv, and it will return
data as data.frame
In case that file doesn't exist it will return error. If file can't be
parsed as csv error will be thrown.
}
\examples{
\dontrun{
fars_read('data/accident_2013.csv.bz2')
}
}
