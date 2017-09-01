###################################################
# ggForest.R
# 
# Author: Helena Edlund
# Created on: 2017-08-18
# Modified on: 
# Purpose: qq plots
# Dependencies: ggplot2, rlang
###################################################

ggForest <- function(df, x, y, xCIu, xCIl,
                      impactArea = c(0.8, 1.25), ...){
  
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  xCIu <- rlang::enexpr(xCIu)
  xCIl <- rlang::enexpr(xCIl)
  facet <- rlang::enexpr(facet)
  
  p <- rlang::quo(
    ggplot(df, aes(x=!!x, y=!!y)) + 
    geom_rect(xmin = !!impactArea[1], xmax= !!impactArea[2],
              ymin = -Inf, ymax = Inf, 
              fill="lightgray") + 
      geom_point(size=3, shape=18) +
      geom_errorbarh(aes(xmin = !!xCIl, xmax=!!xCIu), height = 0.2) +
      geom_vline(xintercept=1, linetype = "longdash")
    )
  return(eval_tidy(p))
}
