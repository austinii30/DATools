#' Parse Values Following a CLI Flag
#'
#' Extract the values associated with a specific command-line flag from a
#' vector of command-line arguments. Values are collected until another
#' flag (matching `flag_prefix`) is encountered.
#'
#' @param args Character vector of command-line arguments (typically from
#'   `commandArgs(trailingOnly = TRUE)`).
#' @param flag Character string specifying the flag to parse (e.g. `"--step"`).
#' @param flag_preflix Regular expression identifying the beginning of a flag.
#'   Defaults to `"^--"`.
#'
#' @return A character vector containing the values associated with the
#'   specified flag. If the flag is not present, an empty character vector
#'   is returned.
#'
#' @examples
#' args <- c("--step", "clean", "train", "--model", "rf")
#' parse_cliarg(args, "--step")
#'
#' @family cli_tools
#' @export
parse_cliarg = function (args, flag, flag_prefix="^--") {
    
    idx <- which(args == flag)
    
    if (length(idx) == 0) { 
        return(character(0)) 
    }
    
    values <- args[(idx + 1):length(args)]
    
    next_flag <- grep(flag_prefix, values)
    
    if (length(next_flag) > 0) {
        values <- values[1:(next_flag[1] - 1)]
    }
    
    return(values)

}


#' Validate Inputs
#'
#' Validate a set of user-specified inputs against a list of allowed values.
#' If the keyword specified by `all` appears in `inputs`, all valid values
#' are returned.
#'
#' @param valid_inputs Character vector of allowed inputs.
#' @param inputs Character vector of user-specified inputs.
#' @param all Character string representing a keyword that expands to all
#'   valid inputs. Default is `"all"`.
#'
#' @return A character vector containing validated inputs.
#'
#' @examples
#' valid <- c("clean", "train", "predict")
#'
#' validate_input(valid, c("clean", "train"))
#' validate_input(valid, "all")
#'
#' @family cli_tools
#' @export
validate_input <- function (valid_inputs, inputs, all="all") {

  if (all %in% inputs) {
      return(valid_inputs)
  } else {
      invalid <- setdiff(inputs, valid_inputs)
      if (length(invalid) > 0) {
          stop("Invalid step(s): ", paste(invalid, collapse = ", "))
      }
      return(inputs)
  }

}


#' Parse Multiple CLI Flags
#'
#' Parse and validate values for multiple command-line flags. For each flag,
#' the corresponding values are extracted using `parse_cliarg()` and validated
#' using `validate_input()`.
#'
#' @param args Character vector of command-line arguments.
#' @param flags Character vector of flags to parse.
#' @param valid_input_list Named list. Each element contains the valid
#'   inputs for the corresponding flag.
#'
#' @return A named list containing parsed and validated values for each flag.
#'
#' @examples
#' args <- c("--step", "clean", "train", "--model", "rf")
#'
#' flags <- c("--step", "--model")
#'
#' valid <- list(
#'   "--step" = c("clean", "train", "predict"),
#'   "--model" = c("rf", "svm")
#' )
#'
#' get_all_cliargs(args, flags, valid)
#'
#' @family cli_tools
#' @export
get_all_cliargs = function (args, flags, valid_input_list) {

    res <- vector("list", length(flags))
    names(res) <- flags
    for (i in seq_along(flags)) { 
        f <- flags[i]
        parsed_args <- parse_cliarg(args, f) 
        res[[f]] <- validate_input(
           valid_input_list[[f]], parsed_args
        )
    }

    return(res)

}