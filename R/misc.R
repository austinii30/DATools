#' Generate Boolean Combinations
#'
#' Generate all possible combinations of `TRUE` and `FALSE` for a specified
#' number of positions. Each row of the returned matrix represents one
#' combination.
#'
#' @param n Integer. Number of positions (columns).
#'
#' @return A logical matrix where each row represents one combination of
#'   `TRUE` and `FALSE`. The matrix has `2^n` rows and `n` columns.
#'
#' @examples
#' generate_combinations(1)
#' generate_combinations(2)
#'
#' @export
generate_combinations <- function (n) {

  if (n == 1)
    return(matrix(c(TRUE, FALSE), ncol = 1))

  # Recursive step: generate combinations for n-1 positions
  temp <- generate_combinations(n - 1)

  # Combine TRUE and FALSE for the current position
  combinations <- rbind(
    cbind(TRUE, temp),
    cbind(FALSE, temp)
  )

  return(combinations)

}