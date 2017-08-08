###################################################
# ggQQPlot.R
# 
# Author: Helena Edlund
# Created on: 2017-08-04
# Modified on: 
# Purpose: qq plots
# Dependencies: ggplot2, rlang
###################################################

ggQQPlot <- function(df, sample=CWRES, stat="qq", ...){
  sample <- rlang::enexpr(sample)
  
  p <- rlang::quo(ggplot(df, aes(sample=!!sample)) + 
      geom_abline(slope=1, intercept=0) +  # line of identity
      geom_point(stat=!!stat))

  return(eval_tidy(p))
}