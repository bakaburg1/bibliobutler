#' Display a process/status message in silver
#' @param ... One or more strings to be concatenated into a message
#' @param appendLF Logical. Should a line feed be appended? Default TRUE
#' @keywords internal
msg_status <- function(..., appendLF = TRUE) {
  message(crayon::silver("→", paste0(...)), appendLF = appendLF)
}

#' Display a warning message in yellow
#' @param ... One or more strings to be concatenated into a message
#' @param appendLF Logical. Should a line feed be appended? Default TRUE
#' @keywords internal
msg_warn <- function(..., appendLF = TRUE) {
  message(crayon::yellow("!", paste0(...)), appendLF = appendLF)
}

#' Display an error message in red
#' @param ... One or more strings to be concatenated into a message
#' @param appendLF Logical. Should a line feed be appended? Default TRUE
#' @keywords internal
msg_error <- function(..., appendLF = TRUE) {
  message(crayon::red("✖", paste0(...)), appendLF = appendLF)
}

#' Display a success message in green
#' @param ... One or more strings to be concatenated into a message
#' @param appendLF Logical. Should a line feed be appended? Default TRUE
#' @keywords internal
msg_success <- function(..., appendLF = TRUE) {
  message(crayon::green("✔", paste0(...)), appendLF = appendLF)
}

#' Display an info message in blue
#' @param ... One or more strings to be concatenated into a message
#' @param appendLF Logical. Should a line feed be appended? Default TRUE
#' @keywords internal
msg_info <- function(..., appendLF = TRUE) {
  message(crayon::blue("ℹ", paste0(...)), appendLF = appendLF)
}