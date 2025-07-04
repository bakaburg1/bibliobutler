% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openalex.R
\name{oa_fetch_by_ids}
\alias{oa_fetch_by_ids}
\title{Fetch OpenAlex works by IDs using parallel HTTP requests}
\usage{
oa_fetch_by_ids(ids, select_param, filters, per_page, max_results)
}
\arguments{
\item{ids}{Character vector of prepared IDs (output from oa_prepare_ids)}

\item{select_param}{Comma-separated string of fields to select from the API}

\item{filters}{List of additional filters to apply to the query}

\item{per_page}{Number of results per page (max 200)}

\item{max_results}{Maximum number of results to return}
}
\value{
Data frame of raw results from the OpenAlex API
}
\description{
This function handles ID-based queries to the OpenAlex API with intelligent
batching and parallel processing. It automatically groups IDs by type (DOI,
PMID, PMC ID, OpenAlex ID) and respects the API's 50-ID limit per filter type
by creating multiple batches when necessary.
}
\keyword{internal}
