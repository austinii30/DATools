##################################################
# Filename: graphs.R
# Purpose: Draw graphs in EDA
##################################################


emptyPlot <- function(text="") {
    "Draw an empty figure (for figures arranged in grids)."
    plot(1, 1, type = "n", 
         xlim = c(0, 2), ylim = c(0, 2),
         xlab = "", ylab = "", axes = FALSE)
    if (text != "")
        text(1, 1, text, cex = 3, col="black")

    return(recordPlot())
}
