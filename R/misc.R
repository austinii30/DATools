#'
#'
#' @export
generate_combinations <- function(n) {
  "
  Generate all combinations of TRUE/FALSE for 'n' positions.

  [Args]
    n(int): number of positions (columns)

  [Return]
    (Matrix): each row represents a combination of TRUE/FALSE
  "
  if (n == 1)
    return(matrix(c(TRUE, FALSE), ncol = 1))

  # Recursive step: generate combinations for n-1 positions
  temp <- generate_combinations_GPT(n - 1)

  # Combine TRUE and FALSE for the current position
  combinations <- rbind(
    cbind(TRUE, temp),
    cbind(FALSE, temp)
  )

  return(combinations)
}

