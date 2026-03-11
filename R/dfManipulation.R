##################################################
# Filename: dfManipulation.R
# Purpose : Manipulate data-frames
# Author  : Potumas Liu
# Date    : 2025/01/22
##################################################


siftcoltype <- function(df, validColumns=c("numeric", "integer")) {
    "Return a data-frame with specified datatypes 'validColumns'" 

    validCols <- c()
    for (i in 1:ncol(df))
        if (class(df[, i]) %in% validColumns)
            validCols <- c(validCols, i)

    return(df[, validCols])
}


cyname <- function(df, orgname, altname="y") {
  " Change the name of the response column. 
    [Args]
      df      (df): the data-frame to change
      orgname (char): the original name of the response
      altname (char): the altered name
    [Return]
      df (df): the data-frame after changing the name of the response
  "
  if (! orgname %in% colnames(df))
    stop("Invalid response column specified.")
  if (length(orgname) != 1)
    stop("Can only set up 1 response column")
  
  colIdx <- which(colnames(df) == orgname)
  colnames(df)[colIdx] <- altname
   
  return(df)
}


setrownames <- function(df, column, delete=T) {
  " Recognize a row as the row name for each obs. (and delete the row) "
  
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


factorizevars <- function(df, otherTypes=c(), specificCols=c()) {
  " Convert 'character' and 'logical' variables into 'factor'.
    [Args]
      df           (df): the input data.frame
      otherTypes   (char): additional data types that should be factorized
      specificCols (char): specific columns to factorize
    [Return]
      df (df): the data.frame that has been converted
  "
  
  if (class(df) != "data.frame") 
    stop("Input data must be a data-frame.") 
  
  typeToConvert <- c("character", "logical", otherTypes) 
  
  for (col in 1:ncol(df))
    if (class(df[, col]) %in% typeToConvert
        | colnames(df)[col] %in% specificCols) 
      df[, col] <- as.factor(df[, col])
      
  return(df)
}

na.count <- function(vec) { 
    return(sum(is.na(vec))) 
}

eachcolna <- function(df) {
    "Calculate the amount of missings for each column"
    if (class(df) != "data.frame") { 
        stop("'df' must be a data.frame!") 
    }

    NAs <- sapply(1:ncol(df), FUN = na.count )
    names(NAs) <- colnames(df)
    return (NAs)
}
