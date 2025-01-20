#' BiblioButler: AI-Powered Bibliographic Reference Management
#'
#' BiblioButler is an R package for performing tasks related to bibliographic
#' references collection, selection, and analysis with the support of AI and
#' machine learning functionalities.
#'
#' @section Main functions:
#' \itemize{
#'   \item \code{\link{collect_references}}: Collect references from a specified source
#'   \item [Add more main functions as they are implemented]
#' }
#'
#' @name BiblioButler-package
#'
#' @import dplyr
#'
#' @keywords internal
"_PACKAGE"
NULL

.onLoad <- function(libname, pkgname) {
  options(
    bibliobutler.terminate_retry_on = c(400, 403, 415, 404, 500, 502, 503, 504),
    future.rng.onMisuse = "ignore"
  )
}
