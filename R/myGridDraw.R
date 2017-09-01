###################################################
# myGridDraw.R
# 
# Author: Helena Edlund
# Created on: 2017-03-23
# Modified on:
# Purpose: Function for printing list of grids to pdf
# Dependencies: grid
###################################################

myGridDraw <- function(x){
# forces a new page for grids to avoid printing all on the same page
  if(is.grob(x)){
    grid.newpage()
    grid.draw(x)
  } else {
    grid.draw(x)
  }
}
