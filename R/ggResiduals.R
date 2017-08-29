###################################################
# ggResiduals.R
# 
# Author: Helena Edlund
# Created on: 2017-08-02
# Modified on: 
# Purpose: residuals vs predictions
# Dependencies: ggplot2, rlang, dplyr
###################################################

ggResiduals <- function(df, y=CWRES, x=PRED, method="loess", ...){
  y <- enexpr(y)
  x <- enexpr(x)

  # Settings for axes
  ymax <- rlang::quo(
    df %>% dplyr::summarize(ymax=ceiling(max(abs(!!y), na.rm = T))))
  ymax <- as.numeric(eval_tidy(ymax))
  
  p <- rlang::quo(
    ggplot(data=df, aes(x=!!x, y=!!y)) +
      geom_hline(aes(yintercept=0), colour="darkgray") + # line at y=0
      geom_hline(yintercept=c(-2, 2), 
                 linetype="dashed", colour="darkgray") + # line at 2 and -2
      geom_point() +
      geom_smooth(method=!!method) + 
      coord_cartesian(ylim=c(-!!ymax, !!ymax))
    )
  return(eval_tidy(p))
}