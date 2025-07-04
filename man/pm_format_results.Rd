% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pubmed.R
\name{pm_format_results}
\alias{pm_format_results}
\title{Format PubMed API results}
\usage{
pm_format_results(response, include_raw = FALSE)
}
\arguments{
\item{response}{The response object from an API call.}

\item{include_raw}{Logical indicating whether to include raw XML in the
result.}
}
\value{
A data frame with standardized article data.
}
\description{
Internal function to process and normalize data from PubMed API responses.
}
\keyword{internal}
