% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pubmed.R
\name{process_batch_response}
\alias{process_batch_response}
\title{Process a PubMed batch response into a data frame}
\usage{
process_batch_response(response, include_raw = FALSE)
}
\arguments{
\item{response}{The response object from an API call.}

\item{include_raw}{Logical indicating whether to include raw XML in the
result.}
}
\value{
A data frame of processed article data.
}
\description{
Internal function to process a response from the PubMed API.
}
\keyword{internal}
