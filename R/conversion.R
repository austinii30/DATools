#' Convert Integer to Binary Representation
#'
#' Convert a non-negative integer into its binary representation as a
#' character string.
#'
#' @param x Non-negative integer.
#'
#' @return A character string representing the binary form of `x`.
#'
#' @examples
#' as.binary(5)
#' as.binary(10)
#'
#' @export
# turn an integer into a binary digit
as.binary <- function (x) {

  if(x == 0) {
    return("0")
  }

  res <- ""

  while(x > 0) {
    res <- paste0(res, x %% 2)
    x <- x %/% 2
  }

  return(res)

}