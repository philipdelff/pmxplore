###################################################
# lmEq.R
# 
# Author: copied from:
# https://stackoverflow.com/questions/7549694/adding-regression-line-equation-and-r2-on-graph
# Created on: 2017-08-09
# Modified on:
# Purpose: Function to add regression line and R2 to plot
# Dependencies: 
###################################################
lmEq <- function(lmFit){
  
  l <- list(a = format(coef(lmFit)[1], digits = 2),
            b = format(abs(coef(lmFit)[2]), digits = 2),
            r2 = format(summary(lmFit)$r.squared, digits = 3));
  
  if (coef(lmFit)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(R)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}