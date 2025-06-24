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
    stop(
      fun,
      ": Use either doi or title as arguments, not both.",
      call. = FALSE
    )
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
    stop(
      '"',
      paste(setdiff(fields, names(select_field)), collapse = ", "),
      '" is not among the allowed fields: "',
      paste(names(select_field), collapse = '", "'),
      '".'
    )
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
  # Extract the first author
  if (!is.null(unlist(article_data[[".authors"]]))) {
    # Check if authors were parsed already
    if (is.character(article_data$.authors[[1]])) {
      authors <- article_data$.authors |> purrr::map(parse_authors)
    } else {
      authors <- article_data$.authors
    }

    last_name <- authors |>
      purrr::map_chr(~ .x$last_name[1] %||% "")

    given_name <- authors |>
      purrr::map_chr(~ .x$first_name[1] %||% "")

    # If the last name is a single character (likely an initial), fall back to
    # the given name which is more informative (e.g. "Kalaiselvi").
    first_author <- ifelse(nchar(last_name) <= 1, given_name, last_name)

    # Replace empty strings with NA for consistency
    first_author[first_author == ""] <- NA_character_
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

  # If first_author is still missing or just an initial, try to recover a
  # longer token from the raw `.authors` string (before parsing). This handles
  # cases like "Kalaiselvi K" where the family name is stored as an initial in
  # the Crossref metadata.
  need_fallback <- is.na(first_author) | nchar(first_author) <= 1
  if (any(need_fallback)) {
    fallback_names <- article_data$.authors[need_fallback] |>
      purrr::map_chr(function(x) {
        if (is.null(x) || is.na(x)) return(NA_character_)
        # Take the text before the first comma, then the first word with >1
        # characters.
        first_part <- strsplit(x, ",")[[1]][1]
        token <- stringr::str_extract(first_part, "[A-Za-z]{2,}")
        tolower(token)
      })
    first_author[need_fallback] <- fallback_names
  }

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
  ] |>
    unique()

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
    "i",
    "me",
    "my",
    "myself",
    "we",
    "our",
    "ours",
    "ourselves",
    "you",
    "your",
    "yours",
    "yourself",
    "yourselves",
    "he",
    "him",
    "his",
    "himself",
    "she",
    "her",
    "hers",
    "herself",
    "it",
    "its",
    "itself",
    "they",
    "them",
    "their",
    "theirs",
    "themselves",
    "what",
    "which",
    "who",
    "whom",
    "this",
    "that",
    "these",
    "those",
    "am",
    "is",
    "are",
    "was",
    "were",
    "be",
    "been",
    "being",
    "have",
    "has",
    "had",
    "having",
    "do",
    "does",
    "did",
    "doing",
    "would",
    "should",
    "could",
    "ought",
    "i'm",
    "you're",
    "he's",
    "she's",
    "it's",
    "we're",
    "they're",
    "i've",
    "you've",
    "we've",
    "they've",
    "i'd",
    "you'd",
    "he'd",
    "she'd",
    "we'd",
    "they'd",
    "i'll",
    "you'll",
    "he'll",
    "she'll",
    "we'll",
    "they'll",
    "isn't",
    "aren't",
    "wasn't",
    "weren't",
    "hasn't",
    "haven't",
    "hadn't",
    "doesn't",
    "don't",
    "didn't",
    "won't",
    "wouldn't",
    "shan't",
    "shouldn't",
    "can't",
    "cannot",
    "couldn't",
    "mustn't",
    "let's",
    "that's",
    "who's",
    "what's",
    "here's",
    "there's",
    "when's",
    "where's",
    "why's",
    "how's",
    "a",
    "an",
    "the",
    "and",
    "but",
    "if",
    "or",
    "because",
    "as",
    "until",
    "while",
    "of",
    "at",
    "by",
    "for",
    "with",
    "about",
    "against",
    "between",
    "into",
    "through",
    "during",
    "before",
    "after",
    "above",
    "below",
    "to",
    "from",
    "up",
    "down",
    "in",
    "out",
    "on",
    "off",
    "over",
    "under",
    "again",
    "further",
    "then",
    "once",
    "here",
    "there",
    "when",
    "where",
    "why",
    "how",
    "all",
    "any",
    "both",
    "each",
    "few",
    "more",
    "most",
    "other",
    "some",
    "such",
    "no",
    "nor",
    "not",
    "only",
    "own",
    "same",
    "so",
    "than",
    "too",
    "very"
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

      # Move trailing initials to the front **only** when there are two or
      # more initials (e.g. "Doe J K" → "J K Doe").  If there is just a single
      # initial, keep the original order so that "Kalaiselvi K" is not turned
      # into "K Kalaiselvi".
      if (length(ending_initials) > 1) {
        nm <- stringr::str_remove(nm, ending_initials)
        nm <- paste(ending_initials, nm) |> stringr::str_trim()
      }

      nm
    }) |>
    humaniformat::parse_names() |>
    # Keep the parsed names verbatim; we no longer strip lowercase letters so
    # that given names like "Kalaiselvi" are preserved.
    identity() |>
    dplyr::mutate(
      last_name = stringr::str_remove_all(.data$last_name, "\\."),
      clean_last = stringr::str_remove_all(.data$last_name, "\\."),
      swap_needed = nchar(clean_last) == 1 & nchar(.data$first_name) > 1,
      last_name  = if_else(swap_needed, .data$first_name, .data$last_name),
      first_name = if_else(swap_needed, substr(clean_last, 1, 1), .data$first_name),
      own_names = paste(
        .data$first_name,
        if_else(!is.na(.data$middle_name), .data$middle_name, "")
      ) |>
        stringr::str_remove_all("\\s+"),
      .keep = "none"
    )

  if (to_string) {
    authors <- paste(authors$last_name, authors$own_names, collapse = ", ")
  }

  authors
}

#' Remove URL prefixes from a vector of IDs
#'
#' This function takes a vector of IDs and removes the URL prefixes from them.
#'
#' @param ids A vector of IDs.
#'
#' @return The input vector with URL prefixes removed.
#'
remove_url_from_id <- function(ids) {
  urls <- c(
    "doi.org",
    "openalex.org",
    "pubmed.ncbi.nlm.nih.gov",
    "ncbi.nlm.nih.gov/pmc/articles"
  ) |>
    paste(collapse = "|")

  stringr::str_remove(
    ids,
    sprintf("https://(www.)?(%s)/", urls)
  )
}

#' safe_mirai_map
#'
#' A drop-in replacement for mirai::mirai_map() that degrades to sequential
#' evaluation with purrr::map() when no daemons have been set.
#'
#' @param .x A list, vector, data frame or matrix to iterate over
#' @param .f A function to apply to each element / row
#' @param ... Named objects referenced (but not defined) inside .f
#' @param .args Further constant arguments to .f, passed as a list
#' @param .promise As in mirai::mirai_map(); ignored in fallback
#'
#' @return A list of **mirai** objects when daemons are available; otherwise a
#'   plain list of results (same length as .x)
#'
#' @examples safe_mirai_map(1:3, ~.x * 2) # sequential if no daemons()
#'   mirai::daemons(2); # start two workers safe_mirai_map(1:3, ~.x * 2)[] #
#'   parallel, then collect
safe_mirai_map <- function(.x,
                           .f,
                           ...,
                           .args    = list(),
                           .promise = NULL) {
  # Logging setup -------------------------------------------------
  # Use a filesystem-safe timestamp (no spaces or colons) so that the
  # `file()` call succeeds on all operating systems.
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%OS3")

  # Ensure that the logs directory exists.
  dir.create("logs", showWarnings = FALSE)

  log_file <- file.path("logs", paste0("mirai_log_", timestamp, ".txt"))

  log_message <- function(...) {
    write(
      sprintf("[%s] %s", Sys.time(), paste0(...)),
      file = log_file,
      append = TRUE
    )
  }

  log_message("safe_mirai_map called.")

  # Check if any daemons are available (named or default)
  current_daemons <- mirai::daemons()
  daemons_available <- current_daemons$connections > 0

  if (daemons_available) { # parallel branch
    log_message("Executing in parallel with ", current_daemons$connections, " workers.")
    
    # Capture the unevaluated expression for logging
    f_expr <- rlang::enexpr(.f)
    log_message("Function to apply: ", rlang::expr_text(f_expr))

    # Add logging wrapper to the function
    .f_logged <- function(...) {
      # This part executes on the worker
      worker_log_file <- file.path(
        "logs",
        paste0("worker_", Sys.getpid(), "_", timestamp, ".txt")
      )
      worker_log <- function(...) {
        write(
          sprintf("[%s] %s", Sys.time(), paste0(...)),
          file = worker_log_file,
          append = TRUE
        )
      }

      worker_log("Worker started for an item.")
      
      # Try to execute the original function
      tryCatch({
        result <- .f(...)
        worker_log("Function executed successfully.")
        return(result)
      }, error = function(e) {
        worker_log("!!! ERROR in worker: ", conditionMessage(e))
        worker_log("Backtrace: ", paste(capture.output(rlang::trace_back()), collapse = "\n"))
        # Re-throw the error so mirai can see it
        stop(e)
      })
    }

    mirai::mirai_map(.x, .f_logged,
      ...,
      .args = .args,
      .promise = .promise
    )
  } else { # sequential branch
    log_message("Executing sequentially.")
    # When no daemons are present, just use purrr::map sequentially.
    # The ... arguments are forwarded to .f.
    purrr::map(.x, .f, ...)
  }
}

#' Enable parallel processing for bibliobutler operations
#'
#' This convenience function enables parallel processing for bibliobutler
#' functions by setting up background workers.
#'
#' @param workers Number of parallel workers to use.
#'   Defaults to `parallel::detectCores() - 1`.
#'
#' @return Invisibly returns the number of workers created.
#'
#' @details
#' This function sets up parallel workers that bibliobutler functions will
#' automatically use to speed up operations. If workers are already running,
#' this will stop them and create new ones.
#'
#' To disable parallel processing, use `disable_parallel()`.
#'
#' @examples
#' \dontrun{
#' # Enable parallel processing with default number of workers
#' enable_parallel()
#'
#' # Use specific number of workers
#' enable_parallel(workers = 4)
#'
#' # Disable parallel processing
#' disable_parallel()
#' }
#'
#' @export
enable_parallel <- function(workers = parallel::detectCores() - 1) {
  
  if (workers < 1) {
    stop("workers must be at least 1")
  }
  
  # Check for existing daemons
  current_daemons <- mirai::daemons()
  if (current_daemons$daemons > 0) {
    # Stop existing daemons if any
    mirai::daemons(0)
    msg_info("Stopped existing {current_daemons$daemons} workers")
  }
  
  # Start new daemons
  mirai::daemons(workers)
  
  msg_success("Enabled parallel processing with {workers} workers")
  
  invisible(workers)
}

#' Disable parallel processing for bibliobutler operations
#'
#' This function stops all background workers and disables parallel processing.
#' After calling this function, bibliobutler operations will run sequentially.
#'
#' @return Invisibly returns `TRUE` if workers were stopped, `FALSE` if none were running.
#'
#' @examples
#' \dontrun{
#' # Enable then disable parallel processing
#' enable_parallel()
#' # ... do some work ...
#' disable_parallel()
#' }
#'
#' @export
disable_parallel <- function() {
  
  current_daemons <- mirai::daemons()
  if (current_daemons$daemons > 0) {
    mirai::daemons(0)
    msg_success("Disabled parallel processing ({current_daemons$daemons} workers stopped)")
    invisible(TRUE)
  } else {
    msg_info("No parallel workers were running")
    invisible(FALSE)
  }
}
