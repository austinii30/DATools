

validate_input <- function (valid_inputs, inputs) {
  if ("all" %in% inputs) {
      return(valid_inputs)
  } else {
      invalid <- setdiff(inputs, valid_inputs)
      if (length(invalid) > 0) {
          stop("Invalid step(s): ", paste(invalid, collapse = ", "))
      }
      return(inputs)
  }
}
