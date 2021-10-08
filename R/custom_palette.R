#' Custom color palette: 25 distinct colors
#'
#' @return a vector of 25 color names
#' @export
#'
#' @examples
#' custom_palette_c25()
custom_palette_c25 <- function(){
  c("black",
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
  )
}




##==============================================================================
##                Function to control color transparency                      ==
##==============================================================================
#' Function to control color transparency on a plot
#'
#' @param someColor some color name, such as "green", "red", etc.
#' @param percent the desired % of transparency, between 0 (no transparency) and 100 (full transparency) .
#'
#' @return a Hex Code including the desired transparency level, such as #RRGGBB7F
#' @export
#'
#' @import RColorBrewer
#' @import grDevices
#'
#' @examples
#' make_transparent("green", percent=50)
make_transparent <- function(someColor, percent=100)
{
  newColor <- col2rgb(someColor)
  apply(newColor, 2,
        function(curcoldata){
          rgb(red = curcoldata[1], green = curcoldata[2], blue = curcoldata[3],
              alpha = (100 - percent) * 255 / 100, maxColorValue = 255)
        }
  )
} # End function
################################################################################
