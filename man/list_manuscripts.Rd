% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_manuscripts.R
\name{list_manuscripts}
\alias{list_manuscripts}
\title{List manuscripts in environment}
\usage{
list_manuscripts(envir = parent.frame(1))
}
\arguments{
\item{envir}{The environment to which the manuscript should be assigned.}
}
\value{
A character vector, or \code{NULL} if no manuscripts
exist in the environment.
}
\description{
Returns a character vector with the names of manuscripts
loaded in a given environment.
}
\examples{
library(revise)

temp_file <- tempfile(fileext = ".Rmd")
write("`r 1 + 1", file = temp_file)

read_manuscript(temp_file)
list_manuscripts()
}
