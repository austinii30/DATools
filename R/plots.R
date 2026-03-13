#' Draw an Empty Plot
#'
#' Create an empty plot that can be used as a placeholder when arranging
#' multiple figures in a grid layout. Optionally, a text label can be
#' displayed in the center of the plot.
#'
#' @param text Character string to display in the center of the plot.
#'   If an empty string (default), no text is drawn.
#'
#' @return A recorded plot object produced by `recordPlot()`.
#'
#' @examples
#' emptyPlot()
#' emptyPlot("No Data")
#'
#' @family plot_tools
#' @export
emptyPlot <- function (text="") {

    plot(1, 1, type = "n", 
         xlim = c(0, 2), ylim = c(0, 2),
         xlab = "", ylab = "", axes = FALSE)
    if (text != "")
        text(1, 1, text, cex = 3, col="black")

    return(recordPlot())

}


#' Generate Distinct Colors
#'
#' Generate a set of visually distinct colors using evenly spaced values
#' in the HSV color space.
#'
#' @param number Integer specifying the number of colors to generate.
#'
#' @return A character vector containing hexadecimal color codes.
#'
#' @examples
#' generateColors(5)
#'
#' @family plot_tools
#' @export
generateColors <- function (number) {

    sep <- floor(360/number)
    h <- seq(0, sep*(number-1), sep) / 360
    s <- 0.5; v <- 1

    # HSV(): generate the hsv object
    colors <- data.frame(colorspace::HSV(h, s, v)@coords)
    
    # hsv(): convert hsv to hex
    return(hsv(colors$H, colors$S, colors$V))

}