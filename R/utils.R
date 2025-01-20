#' Check input for article retrieval functions
#'
#' This function validates the input for article retrieval functions, ensuring
#' that either a DOI or a title is provided, but not both.
#'
#' @param doi A character string representing the Digital Object Identifier
#' (DOI) of an article.
#' @param title A character string representing the title of an article.
#' @param fun A character string representing the name of the calling function,
#' used for error messages.
#'
#' @return This function does not return a value. It stops execution with an
#' error message if the input is invalid.
#'
#' @examples
#' check_article_get_input(doi = "10.1000/xyz123", fun = "get_article")
#' check_article_get_input(title = "Example Article Title", fun = "get_article")
#'
check_article_get_input <- function(doi = NULL, title = NULL, fun) {

  if (!is.null(doi) && !is.null(title)) {
    stop(fun, ": Use either doi or title as arguments, not both.", call. = FALSE)
  }

  if (is.null(doi) && is.null(title)) {
    stop(fun, ": Either doi or title must used.", call. = FALSE)
  }
}

#' Check and filter fields for article retrieval
#'
#' This function validates the requested fields against a set of allowed fields
#' and filters the select_field list to include only the requested fields.
#'
#' @param fields A character vector of field names to be checked and filtered.
#' @param select_field A named list of allowed fields and their corresponding values or functions.
#'
#' @return A filtered version of select_field containing only the requested fields.
#'
#' @examples
#' allowed_fields <- list(title = "Title", author = "Author", year = "Year")
#' check_article_get_fields(c("title", "year"), allowed_fields)
check_article_get_fields <- function(fields, select_field) {
  if (!all(fields %in% names(select_field))) {
    stop('"',
         paste(setdiff(fields, names(select_field)), collapse = ", "),
         '" is not among the allowed fields: "',
         paste(names(select_field), collapse = '", "'),
         '".')
  }

  select_field <- select_field[fields]
}

#' Get a column or nested element from a data frame
#'
#' This function retrieves a column or nested element from a data frame using
#' multiple column names. It uses purrr::pluck() to access nested elements and
#' returns NA if the element does not exist.
#'
#' @param ... One or more column names or indices to access nested elements.
#'   For example, col_get("authors", "name") would retrieve the "name" field
#'   from the "authors" column.
#' @return The specified element from the data frame. If the element does not
#'   exist, returns NA.
#'
col_get <- function(...) {

  data <- dplyr::pick(everything())

  purrr::pluck(data, ...) %||% NA
}

#' Get the type of article ID
#'
#' This function determines the type of article ID based on the provided string.
#' It checks for PMIDs, PMCIDs, DOIs, OpenAlex IDs, and Semantic Scholar IDs.
#'
#' @param ids A character vector of article IDs.
#' @return A character vector indicating the type of article ID, or NA if the
#'   type cannot be determined.
#'
#' @examples
#' get_article_id_type(c("12345678", "PMC1234567890",
#' "10.1000/xyz123", "W1234567890", "99e1a52b664ec2b24605ce48eed7f4ff26c1bde9"))
#'
get_article_id_type <- function(ids) {
  # Regular expressions for identifying PMIDs, PMCIDs, and DOIs
  pmid_regex <- "^\\d+$" # In theory can be 1-8 digits but could grow?
  pmcid_regex <- "^(?i)PMC\\d+$"
  doi_regex <- r"[\b(10\.\d{4,}(?:\.\d+)*\/\S+(?:(?!["&\'<>])\S)*)]"
  openalex_regex <- "^W\\d+$"
  semanticscholar_regex <- "^[0-9a-f]{40}$"

  dplyr::case_when(
    stringr::str_detect(ids, pmid_regex) ~ "pmid",
    stringr::str_detect(ids, pmcid_regex) ~ "pmcid",
    stringr::str_detect(ids, doi_regex) ~ "doi",
    stringr::str_detect(ids, openalex_regex) ~ "openalex",
    stringr::str_detect(ids, semanticscholar_regex) ~ "semanticscholar",
    .default = NA_character_
  )
}

#' Generate a unique name for an article
#'
#' This function takes an article data frame and generates a unique name for
#' each article based on the year, first author, and first word of the title.
#'
#' @param article_data A data frame containing article data, including the
#'   `.authors`, `.title`, and `.year` columns.
#'
#' @return A character vector of unique record names.
#'
generate_record_name <- function(article_data) {

  # Check that the article data contains the required columns
  if (!all(c(".authors", ".title", ".year") %in% names(article_data))) {
    msg_warn("No record name generated: missing required columns")
    return(rep(NA, nrow(article_data)))
  }

  # Extract the first author
  if (!is.null(unlist(article_data[[".authors"]]))) {
    # Check if authors were parsed already
    if (is.character(article_data$.authors[[1]])) {
      authors <- article_data$.authors |> purrr::map(parse_authors)
    } else {
      authors <- article_data$.authors
    }

    first_author <- authors |> purrr::map_chr(
      ~ .x$last_name[1] %||% NA_character_)
  } else {
    first_author <- rep(NA, nrow(article_data))
  }

  # Extract the first word of the title
  first_word <- article_data$.title |>
    tolower() |>
    # Remove common prefixes
    stringr::str_remove("^author response: ") |>
    stringr::str_remove("^review paper: ") |>
    stringr::str_remove("^decision letter for ") |>
    stringr::str_remove("^review for ") |>
    remove_stopwords() |>
    stringr::str_trim() |>
    stringr::str_extract("[a-z]{3,}")

  # Generate a unique name for the article
  record_names <- sprintf(
        "%s_%s_%s",
    article_data$.year,
    first_author,
    first_word
  )

  # Sanitize the text and remove special characters
  record_names <- coalesce(
    iconv(record_names, to = "ASCII//TRANSLIT"), # Convert to ASCII
    record_names
  ) |>
    stringr::str_remove_all("\\W")

  record_names[record_names == "NA_NA_NA"] <- NA_character_

  # Add a sequential number to the record name if there are duplicates
  dups <- record_names[
    duplicated(record_names) & !is.na(record_names)
  ] |> unique()

  for (dup in dups) {
    idx <- which(record_names == dup)
    record_names[idx] <- sprintf("%s_%d", dup, seq_along(idx))
  }

  record_names

}


#' Remove common English stopwords from a character vector
#'
#' This function takes a character vector and removes common English stopwords
#' from it. The stopwords are defined in a hard-coded list within the function.
#' Stopword list and code adapted from the `tm` package
#' https://cran.r-project.org/package=tm
#'
#' @param x A character vector to remove stopwords from.
#' @return The input character vector with stopwords removed.
#'
remove_stopwords <- function(x) {
  words <- c(
    "i", "me", "my", "myself", "we", "our", "ours", "ourselves",
    "you", "your", "yours", "yourself", "yourselves", "he", "him", "his",
    "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they",
    "them", "their", "theirs", "themselves", "what", "which", "who", "whom",
    "this", "that", "these", "those", "am", "is", "are", "was", "were", "be",
    "been", "being", "have", "has", "had", "having", "do", "does", "did",
    "doing", "would", "should", "could", "ought", "i'm", "you're", "he's",
    "she's", "it's", "we're", "they're", "i've", "you've", "we've", "they've",
    "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", "i'll", "you'll",
    "he'll", "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't",
    "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't",
    "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot", "couldn't",
    "mustn't", "let's", "that's", "who's", "what's", "here's", "there's",
    "when's", "where's", "why's", "how's", "a", "an", "the", "and", "but", "if",
    "or", "because", "as", "until", "while", "of", "at", "by", "for", "with",
    "about", "against", "between", "into", "through", "during", "before",
    "after", "above", "below", "to", "from", "up", "down", "in", "out", "on",
    "off", "over", "under", "again", "further", "then", "once", "here", "there",
    "when", "where", "why", "how", "all", "any", "both", "each", "few", "more",
    "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same",
    "so", "than", "too", "very"
  )

  # Create regex pattern of sorted stopwords separated by |
  pattern <- paste(sort(words, decreasing = TRUE), collapse = "|")

  # Build regex to match word boundaries with Unicode properties
  regex <- sprintf("(*UCP)\\b(%s)\\b", pattern)

  # Remove stopwords using regex pattern
  gsub(regex, "", x, perl = TRUE)
}

#' Parse author names
#'
#' This function takes a vector of author names and parses them into a
#' standardized format: full family name followed by initials.
#'
#' @param authors A vector of author names.
#'
#' @return A data frame with parsed author names.
#'
parse_authors <- function(
    authors,
    to_string = FALSE
) {

  authors <- authors |>
    stringr::str_split("\\s*,\\s*") |>
    unlist() |>
    purrr::discard(~ .x %in% "") |>
    # Fix all capital case names
    stringr::str_replace_all("(?<=[A-Z])([A-Z])", \(x) tolower(x)) |>
    purrr::map_chr(\(nm) {
      ending_initials <- stringr::str_extract_all(nm, "(\\b[A-Z]\\.? ?)+$") |>
        unlist()

      if (!rlang::is_empty(ending_initials)) {
        nm <- stringr::str_remove(nm, ending_initials)
        nm <- paste(ending_initials, nm) |> stringr::str_trim()
      }

      nm
    }) |>
    humaniformat::parse_names() |>
    mutate(
      across(
        c("first_name", "middle_name"),
        ~ stringr::str_remove_all(.x, "[^A-Z ]")
      )
    ) |>
    dplyr::mutate(
      last_name = stringr::str_remove_all(.data$last_name, "\\."),
      own_names = paste(
        .data$first_name,
        if_else(!is.na(.data$middle_name), .data$middle_name, "")
      ) |>
        stringr::str_trim(),
      .keep = "none"
    )

  if (to_string) {
    authors <- paste(authors$last_name, authors$own_names, collapse = ", ")
  }

  authors
}
