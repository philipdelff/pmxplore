###################################################
# ggScatterWithSmooth.R
# 
# Author: Helena Edlund
# Created on: 2017-08-04
# Modified on:
# Purpose: 
# Dependencies: ggplot2, rlang
###################################################

ggScatterWithSmooth <- function(df, x, y, method="loess", ...) {
  
  x <- enexpr(x)
  y <- enexpr(y)
  
  p <- rlang::quo(
    ggplot(df, aes(x=!!x, y=!!y)) + 
    geom_point() + 
    geom_smooth(method=!!method)
  )
  return(eval_tidy(p))
}
