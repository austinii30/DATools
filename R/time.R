#' Convert Time Objects to Numeric
#'
#' Convert a time difference object to a numeric value in the specified
#' time units.
#'
#' @param t A time difference object (e.g., `difftime`) or other object
#'   supported by `as.numeric()`.
#' @param units Character string specifying the time unit used for conversion.
#'   Common values include `"secs"`, `"mins"`, `"hours"`, and `"days"`.
#'
#' @return Numeric value representing the time difference in the specified
#'   units.
#'
#' @examples
#' t <- Sys.time() - (Sys.time() - 120)
#' time_convert(as.difftime(120, units = "secs"))
#'
#' @family time_tools
#' @export
time_convert <- function (t, units="secs") {

    return(as.numeric(t, units=units))

}


#' Format Time for Directory Names
#'
#' Convert a time object into a string formatted for use in directory
#' or file names.
#'
#' @param t A time object (typically `POSIXct`).
#'
#' @return A character string formatted as `"YYYY-MM-DD_HH-MM-SS"`.
#'
#' @examples
#' time_dir(Sys.time())
#'
#' @family time_tools
#' @export
time_dir <- function (t) {

    return(format(t, "%Y-%m-%d_%H-%M-%S"))

}


#' Format Time for Logging
#'
#' Convert a time object into a human-readable timestamp commonly used
#' in log messages.
#'
#' @param t A time object (typically `POSIXct`).
#'
#' @return A character string formatted as `"YYYY-MM-DD HH:MM:SS"`.
#'
#' @examples
#' time_log(Sys.time())
#'
#' @family time_tools
#' @export
time_log <- function (t) {

    return(format(t, "%Y-%m-%d %H:%M:%S"))

}