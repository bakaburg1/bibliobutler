% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crossreference.R
\name{get_crossref_articles}
\alias{get_crossref_articles}
\title{Retrieve Articles from Crossref API}
\usage{
get_crossref_articles(
  ids = NULL,
  query = NULL,
  year_filter = NULL,
  filters = NULL,
  fields = NULL,
  per_page = 1000,
  max_results = Inf
)
}
\arguments{
\item{ids}{A character vector of DOIs.}

\item{query}{A character string to search in article metadata. Only one of
\code{ids} or \code{query} should be non-NULL.}

\item{year_filter}{A string indicating a publication year or range. Examples:
"2020", "2015-2020", "2010-", "-2018".}

\item{filters}{A named list of additional filters to pass. For example,
\code{filters = list(type = "journal-article")} will add
\code{filter=type:journal-article}.}

\item{fields}{A character vector of fields to return. If NULL, a set of
default fields is returned.}

\item{per_page}{Number of results per page (default is 100).}

\item{max_results}{Maximum number of results to return (default is Inf).}
}
\value{
A data frame of article metadata from Crossref with standardized
columns: \code{.record_name}, \code{.paperId}, \code{.title}, \code{.abstract}, \code{.authors},
\code{.year}, \code{.journal}, \code{.is_open_access}, \code{.url}, \code{.pubtype}, \code{.api}, \code{.ids},
\code{.references}, \code{.citations}, and \code{.related}.
}
\description{
This function queries the Crossref REST API (https://api.crossref.org/works)
to fetch article metadata. You can either supply a vector of DOIs (via \code{ids})
or perform a free‐text search (via \code{query}). A year filter (e.g., "2016",
"2010-2015", "2018-", or "-2019") and additional filters can also be
provided.
}
\examples{
\dontrun{
# Get specific articles by DOI
get_crossref_articles(ids = c("10.1037/0003-066X.59.1.29"))

# Search for articles about "machine learning" published in 2020
get_crossref_articles(query = "machine learning", year_filter = "2020")

# Filtering by type and a year range
get_crossref_articles(
  query = "deep learning",
  year_filter = "2018-2020",
  filters = list(type = "journal-article")
)
}

}
