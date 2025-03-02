% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_manuscript.R
\name{read_manuscript}
\alias{read_manuscript}
\title{Read 'Rmarkdown' Manuscript}
\usage{
read_manuscript(
  path,
  PDF = FALSE,
  to_envir = getOption("revise_use_envir", TRUE),
  envir = parent.frame(1)
)
}
\arguments{
\item{path}{Character, path to the 'Rmarkdown' file.}

\item{PDF}{Logical or character. Default \code{FALSE} does nothing. If set to \code{TRUE}, attempts to determine PDF file path from \code{YAML} front matter. If character, should be the path to the PDF rendered from the 'Rmarkdown' file. This PDF will be loaded for page matching.}

\item{to_envir}{Logical, indicating whether or not the manuscript should
be invisibly assigned to an environment variable names \code{.revise_manuscript}.
Defaults to \code{TRUE}. Can be disabled by setting \code{options(revise_use_envir = FALSE)}.}

\item{envir}{The environment to which the manuscript should be assigned.}
}
\value{
Invisibly returns a list of class \code{manuscript}, containing
the following elements:
\itemize{
\item $sections: Extracted revisions
\item $PDF: Optionally, the PDF document
\item $refs: References to figures and tables
\item $rmd: The raw rmarkdown
\item $mtime: When the rmarkdown file was created
}
}
\description{
Reads an 'Rmarkdown' manuscript and, if possible, an associated PDF.
}
\examples{
tempfile <- tempfile("tmp", fileext = ".txt")
writeLines("[Maecenas mollis consectetur purus.]{#test}", con = tempfile)
read_manuscript(tempfile)
}
\seealso{
\code{\link[rmarkdown]{yaml_front_matter}}
\code{\link[tools]{fileutils}}
}
