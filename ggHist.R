###################################################
# ggHist.R
# 
# Author: Helena Edlund
# Created on: 2017-08-04
# Modified on:
# Purpose: 
# Dependencies: ggplot2, rlang
###################################################
ggHist <- function(df, x, ...){
  
  x <- enexpr(x)
  
  p <- rlang::quo(
    ggplot(data=df, aes(x=!!x)) + 
      geom_histogram()
  )
  
  return(eval_tidy(p))
}
