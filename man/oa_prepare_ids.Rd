% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openalex.R
\name{oa_prepare_ids}
\alias{oa_prepare_ids}
\title{Prepare IDs for openalexR}
\usage{
oa_prepare_ids(ids)
}
\arguments{
\item{ids}{character vector}
}
\value{
A list with one element named after the ID type provided (\code{doi},
\code{pmid}, or \code{openalex}).
}
\description{
Given a character vector of IDs (DOI, PMID, or full OpenAlex IDs), return a
list with only the provided ID type.
}
