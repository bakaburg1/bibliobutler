% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pubmed.R
\name{get_pubmed_articles}
\alias{get_pubmed_articles}
\title{Retrieve Articles from PubMed via NCBI E-utilities API}
\usage{
get_pubmed_articles(
  ids = NULL,
  title = NULL,
  query = NULL,
  include_raw = FALSE,
  max_results = Inf,
  per_page = 1000,
  concurrent = TRUE
)
}
\arguments{
\item{ids}{Character vector of PMIDs or DOIs. Default is NULL.}

\item{title}{Character string to search for in article titles. Default is
NULL.}

\item{query}{Character string for a custom PubMed search query using PubMed's
search syntax. Default is NULL.}

\item{include_raw}{Logical, whether to include raw XML data in the results.
Default is FALSE.}

\item{max_results}{Integer, maximum number of results to return. Default is
Inf (all available results).}

\item{per_page}{Integer, number of results to retrieve in each batch request.
Default is 1000. Large values provide better API efficiency but may
increase memory usage.}

\item{concurrent}{Logical, whether to send HTTP requests concurrently.
Default is TRUE. Set to FALSE for sequential HTTP requests.}
}
\value{
A data frame containing article metadata from PubMed with
standardized columns:
\itemize{
\item .api: Source API identifier (always "pubmed")
\item pmid: PubMed ID
\item doi: Digital Object Identifier
\item title: Article title
\item abstract: Article abstract
\item authors: Author names (in format "LastName FirstName, LastName
FirstName, ...")
\item year: Publication year
\item journal: Journal name (preferably the ISO abbreviation)
\item pubtype: Publication type(s)
}
}
\description{
This function fetches article data from the PubMed database using the NCBI
E-utilities API. Articles can be retrieved by PMIDs, DOIs, titles, or custom
search queries. The function handles the conversion of different ID types and
returns a standardized data format.
}
\details{
This function requires at least one of the three main parameters
(ids, title, or query) to be provided. It uses the NCBI E-utilities API
(ESearch and EFetch) to retrieve article data. For ID-based searches, it
uses EFetch directly. For title or query searches, it uses ESearch followed
by EFetch. The function automatically handles API rate limits and
authentication, and will fetch all available results (up to max_results)
using pagination.

PubMed responses can be large, with each batch of 1000 articles being
approximately 25MB in size. The function uses concurrent HTTP requests to
speed up retrieval of multiple batches. This behavior can be disabled by
setting concurrent=FALSE.

Note that while PubMed theoretically supports batch sizes up to 10000,
large batch sizes can cause API timeouts or slow responses, so a default
of 1000 is used. For very large datasets, consider using a smaller per_page
value if you experience issues.
}
\examples{
\dontrun{
# Get article by PMID
article <- get_pubmed_articles(ids = "33400058")

# Search by title
results <- get_pubmed_articles(title = "Machine learning in bioinformatics")

# Use a complex query
query_results <- get_pubmed_articles(
  query = "cancer AND immunotherapy AND (2020[PDAT]:2023[PDAT])"
)

# Get all articles about COVID-19 vaccines (potentially thousands)
all_results <- get_pubmed_articles(
  query = "covid-19 vaccine effectiveness"
)

# Limit to 500 results for a broad query
limited_results <- get_pubmed_articles(
  query = "cancer treatment",
  max_results = 500
)

# Use a different batch size and disable parallelization
results <- get_pubmed_articles(
  query = "machine learning",
  per_page = 500,
  concurrent = FALSE
)
}

}
