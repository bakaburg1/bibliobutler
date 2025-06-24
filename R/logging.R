#' Display a process/status message in silver
#' @param ... One or more strings to be concatenated into a message
#' @param appendLF Logical. Should a line feed be appended? Default TRUE
#'
#' @keywords internal
msg_status <- function(..., appendLF = TRUE) {
  message(
    crayon::white("→", stringr::str_glue(.envir = parent.frame(), ...)),
    appendLF = appendLF
  )
}

#' Display a warning message in yellow
#' @param ... One or more strings to be concatenated into a message
#' @param appendLF Logical. Should a line feed be appended? Default TRUE
#'
#' @keywords internal
msg_warn <- function(..., appendLF = TRUE) {
  message(
    crayon::yellow("!", stringr::str_glue(.envir = parent.frame(), ...)),
    appendLF = appendLF
  )
}

#' Display an error message in red
#' @param ... One or more strings to be concatenated into a message
#' @param appendLF Logical. Should a line feed be appended? Default TRUE
#' @param stop Logical. Should the function stop? Default TRUE
#'
#' @keywords internal
msg_error <- function(..., appendLF = TRUE, stop = TRUE) {
  # The message to be displayed
  error_message <- stringr::str_glue(.envir = parent.frame(), ...)

  # Display the message
  message(
    crayon::red("✖", error_message),
    appendLF = appendLF
  )

  # Stop execution with the message
  if (stop) {
    stop(error_message, call. = FALSE)
  }
}

#' Display a success message in green
#' @param ... One or more strings to be concatenated into a message
#' @param appendLF Logical. Should a line feed be appended? Default TRUE
#'
#' @keywords internal
msg_success <- function(..., appendLF = TRUE) {
  message(
    crayon::green("✔", stringr::str_glue(.envir = parent.frame(), ...)),
    appendLF = appendLF
  )
}

#' Display an info message in blue
#' @param ... One or more strings to be concatenated into a message
#' @param appendLF Logical. Should a line feed be appended? Default TRUE
#'
#' @keywords internal
msg_info <- function(..., appendLF = TRUE) {
  message(
    crayon::blue("ℹ", stringr::str_glue(.envir = parent.frame(), ...)),
    appendLF = appendLF
  )
}
