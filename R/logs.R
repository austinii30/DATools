logmsg <- function(msg, level = "INFO") {
  time  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  level <- sprintf("%-7s", level)  # left-align level width 7
  cat(sprintf("[%s] [%s] %s\n", time, level, msg))
}


section <- function(title, width = 70, char = "=") {

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


subsection <- function(title, width = 70, char = "-") {
  title_line <- paste0(" ", title, " ")
  side_width <- floor((width - nchar(title_line)) / 2)

  left  <- paste(rep(char, side_width), collapse = "")
  right <- paste(rep(char, width - nchar(left) - nchar(title_line)), collapse = "")

  cat("\n", left, title_line, right, "\n", sep = "")
}
