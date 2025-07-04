% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pubmed.R
\name{get_pubmed_linked}
\alias{get_pubmed_linked}
\title{Retrieve Linked Articles from PubMed}
\usage{
get_pubmed_linked(ids, links = c("citations", "references", "related"))
}
\arguments{
\item{ids}{Character vector of article identifiers (PMIDs, DOIs, etc.).}

\item{links}{Character vector specifying the types of links to fetch:
"citations", "references", or "related". Default is all three types.}
}
\value{
A list of data frames, one for each requested link type, each with
columns:
\itemize{
\item linked_id: The linked PMID
\item source_id: List column containing the source PMIDs that link to
the linked_id
}
}
\description{
This function fetches citations, references, or related articles for a given
set of PubMed IDs using the NCBI E-utilities API. It automatically converts
other ID types (DOIs, PMCIDs) to PMIDs and handles the API requests to
retrieve all linked articles.
}
\details{
This function uses the NCBI ELink API to retrieve connected
articles. Input IDs are automatically converted to PMIDs using the
convert_article_id function. For citations, it fetches articles that cite
the input article(s). For references, it fetches articles that are cited by
the input article(s). For related articles, it fetches articles that are
conceptually related according to PubMed's algorithm.
}
\examples{
\dontrun{
# Get all link types for a PMID
links <- get_pubmed_linked("33400058")

# Get only citations for a DOI
citations <- get_pubmed_linked(
 "10.1038/s41586-020-2012-7", links = "citations")

# Get references for multiple articles
refs <- get_pubmed_linked(c("33400058", "34320281"), links = "references")
}

}
