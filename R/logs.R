#' Format a Log Message
#'
#' Create a formatted log message that includes the current timestamp,
#' a log level indicator, and a user-provided message.
#'
#' @param msg Character string containing the log message.
#' @param level Character string specifying the log level (e.g., `"INFO"`,
#'   `"WARNING"`, `"ERROR"`). The value is left-aligned to width 7.
#'
#' @return A formatted character string representing the log message.
#'
#' @examples
#' logmsg("Starting analysis")
#' logmsg("File not found", "ERROR")
#'
#' @family logging_tools
#' @export
logmsg <- function (msg, level = "INFO") {

  time  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  level <- sprintf("%-7s", level)  # left-align level width 7
  return(sprintf("[%s] [%s] %s", time, level, msg))

}


#' Print a Section Header
#'
#' Print a formatted section header to the console. The title is centered
#' within a line of repeated characters.
#'
#' @param title Character string representing the section title.
#' @param width Integer specifying the total width of the section header.
#' @param char Character used to draw the horizontal lines.
#'
#' @return Invisibly returns `NULL`. The formatted section header is printed
#'   to the console.
#'
#' @examples
#' section("Data Preparation")
#'
#' @family logging_tools
#' @export
section <- function (title, width = 70, char = "=") {

  # Create top/bottom line
  line <- paste(rep(char, width), collapse = "")

  # Calculate padding for centering
  padding_total <- width - nchar(title)
  left_pad  <- floor(padding_total / 2)
  right_pad <- padding_total - left_pad

  centered_title <- paste0(
    paste(rep(" ", left_pad), collapse = ""),
    title,
    paste(rep(" ", right_pad), collapse = "")
  )

  cat("\n", line, "\n", centered_title, "\n", line, "\n\n", sep = "")

}


#' Print a Subsection Header
#'
#' Print a formatted subsection header to the console with a title centered
#' between repeated characters.
#'
#' @param title Character string representing the subsection title.
#' @param width Integer specifying the total width of the line.
#' @param char Character used to draw the horizontal separators.
#'
#' @return Invisibly returns `NULL`. The formatted subsection header is printed
#'   to the console.
#'
#' @examples
#' subsection("Model Fitting")
#'
#' @family logging_tools
#' @export
subsection <- function (title, width = 70, char = "-") {

  title_line <- paste0(" ", title, " ")
  side_width <- floor((width - nchar(title_line)) / 2)

  left  <- paste(rep(char, side_width), collapse = "")
  right <- paste(rep(char, width - nchar(left) - nchar(title_line)), collapse = "")

  cat("\n", left, title_line, right, "\n", sep = "")

}