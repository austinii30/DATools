##################################################
# Filename: basics.R
# Purpose : Basic in/out functions
# Author  : Potumas Liu
# Date    : 2025/01/22
##################################################

time.convert <- function (t, units="secs") {
    return(as.numeric(t, units=units))
}


time.dir <- function (time) {
    return(format(time, "%Y-%m-%d_%H-%M-%S"))
}


time.log <- function (t) {
    return(format(t, "%Y-%m-%d %H:%M:%S"))
}


# turn an integer into a binary digit
as.binary <- function(x) {
  if(x == 0) return("0")
  res <- ""
  while(x > 0) {
    res <- paste0(res, x %% 2)
    x <- x %/% 2
  }
  return(res)
}


generateColors <- function(number) {
    "
    Generate colors from the HSV space.
    [Args]
        number (int): the amount of colors to generate.
    [Return]
        (as.character): a vector of characters of hex-valued colors
    "
    sep <- floor(360/number)
    h <- seq(0, sep*(number-1), sep) / 360
    s <- 0.5; v <- 1

    colors <- data.frame(colorspace::HSV(h, s, v)@coords)
    # HSV(): generate the hsv object
    # hsv(): convert hsv to hex
    return(hsv(colors$H, colors$S, colors$V))
}
