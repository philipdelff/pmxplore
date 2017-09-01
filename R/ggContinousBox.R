###################################################
# ggContinuousBox.R
# 
# Author: Helena Edlund
# Created on: 2017-08-04
# Modified on:
# Purpose: 
# Dependencies: ggplot2, rlang
###################################################
ggContinuousBox <- function(df, x, y, 
                            fill=NULL, dodge=0, ...){
  x <- enexpr(x)
  y <- enexpr(y)
  fill <- enexpr(fill)
  
  xCol <- df[[expr_text(x)]]
  xBreaks <- sort(unique(xCol))
  
  fillCol <- df[[expr_text(fill)]]
  
  if(!is.numeric(xCol)){
    stop(paste(expr_text(x),"is not numeric"))
  }
  
  getN <- function(x){
    return(c(y = median(x, na.rm=T), label = length(x)))
  }
  
  if(is.null(fillCol)){
    p <- rlang::quo(
      ggplot(data=df, aes(x = !!x, y=!!y)) + 
        geom_boxplot(aes(group= !!x)) +
        stat_summary(aes(y=!!y, x=!!x), fun.data = getN, 
                     geom = "label", inherit.aes=F,
                     position = position_dodge(!!dodge)) +
        scale_x_continuous(breaks = !!xBreaks) +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank())
    )
  } else {
    p <- rlang::quo(
      ggplot(data=df, aes(x=!!x, y=!!y)) + 
        geom_boxplot(aes(group=interaction(!!x, !!fill), fill=!!fill)) +
        stat_summary(aes(y=!!y, x=!!x, group=interaction(!!x, !!fill)), 
                     fun.data = getN, 
                     geom = "label", inherit.aes=F,
                     position = position_dodge(!!dodge)) +
        scale_x_continuous(breaks = !!xBreaks) +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank())
    )
   }
  return(eval_tidy(p))
}
