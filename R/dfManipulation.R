#' Filter Columns by Data Type
#'
#' Return a data frame containing only columns whose class matches
#' one of the specified data types.
#'
#' @param df A data frame.
#' @param validColumns Character vector specifying allowed column classes.
#'   Defaults to `c("numeric", "integer")`.
#'
#' @return A data frame containing only the columns whose class is in
#'   `validColumns`.
#'
#' @examples
#' df <- data.frame(
#'   a = 1:5,
#'   b = letters[1:5],
#'   c = rnorm(5)
#' )
#'
#' siftcoltype(df)
#'
#' @family dataframe_tools
#' @export
siftcoltype <- function (df, validColumns=c("numeric", "integer")) {

    validCols <- c()
    for (i in 1:ncol(df))
        if (class(df[, i]) %in% validColumns)
            validCols <- c(validCols, i)

    return(df[, validCols])
    
}


#' Rename a Response Column
#'
#' Change the name of a specified column in a data frame.
#'
#' @param df A data frame.
#' @param orgname Character string specifying the original column name.
#' @param altname Character string specifying the new column name.
#'
#' @return A data frame with the renamed column.
#'
#' @examples
#' df <- data.frame(x = 1:3, y = 4:6)
#' cyname(df, "y", "response")
#'
#' @family dataframe_tools
#' @export
cyname <- function (df, orgname, altname="y") {

  if (! orgname %in% colnames(df))
    stop("Invalid response column specified.")
  if (length(orgname) != 1)
    stop("Can only set up 1 response column")
  
  colIdx <- which(colnames(df) == orgname)
  colnames(df)[colIdx] <- altname
   
  return(df)

}


#' Set Row Names from a Column
#'
#' Use the values of a specified column as the row names of a data frame.
#' Optionally remove that column afterward.
#'
#' @param df A data frame.
#' @param column Character string specifying the column to use as row names.
#' @param delete Logical indicating whether the column should be removed
#'   after assigning row names. Default is `TRUE`.
#'
#' @return A data frame with updated row names.
#'
#' @examples
#' df <- data.frame(id = c("a","b","c"), value = 1:3)
#' setrownames(df, "id")
#'
#' @family dataframe_tools
#' @export
setrownames <- function (df, column, delete=TRUE) {
  
  if (! column %in% colnames(df))
    stop("Invalid column for row names was specified.")
  if (length(column) != 1)
    stop("Can only set up 1 column for row names")
  
  colIdx <- which(colnames(df) == column)
  rownames(df) <- df[, colIdx]
  
  if (delete)
    return(df[, -colIdx])
    
  return(df)

}


#' Convert Variables to Factors
#'
#' Convert selected columns of a data frame into factors.
#' By default, columns of type `character` and `logical` are converted.
#' Additional column types or specific columns can also be specified.
#'
#' @param df A data frame.
#' @param otherTypes Character vector specifying additional data types
#'   that should be converted to factors.
#' @param specificCols Character vector specifying column names that
#'   should be converted to factors regardless of their type.
#'
#' @return A data frame with specified variables converted to factors.
#'
#' @examples
#' df <- data.frame(
#'   a = letters[1:3],
#'   b = c(TRUE, FALSE, TRUE),
#'   c = 1:3
#' )
#'
#' factorizevars(df)
#'
#' @family dataframe_tools
#' @export
factorizevars <- function (df, otherTypes=c(), specificCols=c()) {
  
  if (!is.data.frame(df)) 
    stop("Input data must be a data-frame.") 
  
  typeToConvert <- c("character", "logical", otherTypes) 
  
  for (col in 1:ncol(df))
    if (class(df[, col]) %in% typeToConvert
        | colnames(df)[col] %in% specificCols) 
      df[, col] <- as.factor(df[, col])
      
  return(df)

}


#' Count Missing Values
#'
#' Count the number of missing (`NA`) values in a vector.
#'
#' @param vec A vector.
#'
#' @return Integer count of `NA` values.
#'
#' @examples
#' na_count(c(1, NA, 3, NA))
#'
#' @family dataframe_tools
#' @export
na_count <- function (vec) { 

    return(sum(is.na(vec))) 

}


#' Count Missing Values for Each Column
#'
#' Compute the number of missing (`NA`) values in each column
#' of a data frame.
#'
#' @param df A data frame.
#'
#' @return A named numeric vector where each element represents
#'   the number of `NA` values in the corresponding column.
#'
#' @examples
#' df <- data.frame(
#'   a = c(1, NA, 3),
#'   b = c(NA, NA, 2)
#' )
#'
#' na_eachcol(df)
#'
#' @family dataframe_tools
#' @export
na_eachcol <- function (df) {

    if (!is.data.frame(df)) { 
        stop("'df' must be a data.frame!") 
    }

    NAs <- sapply(1:ncol(df), FUN = na_count )
    names(NAs) <- colnames(df)
    return (NAs)
    
}