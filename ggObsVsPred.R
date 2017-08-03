###################################################
# ggObsVsPred.R
# 
# Author: Helena Edlund
# Created on: 2017-03-23
# Modified on: 2017-08-02
# Purpose: GOF plot, observed vs prediction
# Dependencies: ggplot2, rlang
###################################################

ggObsVsPred <- function(df, y, x, 
                        grp = NULL, 
                        method="loess", 
                        labs=list(y="Observed", x="Predicted")){
    x <- enexpr(x)
    y <- enexpr(y)
    grp <- enexpr(grp)
  
    # Maxvalue for axes
    maxXY <- rlang::quo(df %>% dplyr::summarize(maxVal=max(c(!!x,!!y), na.rm=T))) %>% 
      eval_tidy()
    maxVal <- maxXY[1,]
    
    p <- 
      rlang::quo(ggplot(data=df, aes(x=!!x, y=!!y)) + 
                   geom_abline(slope=1, intercept=0) +  # line of identity
                   geom_point(aes(shape=factor(BLQ))) +
                   scale_shape_manual(values=c(1, 4)) + guides(shape="none") +
                   geom_smooth(method=method) + 
                   coord_cartesian(xlim=c(0, maxVal), ylim=c(0, maxVal)) +
                   labs(y=labs[["y"]], x=labs[["x"]]))
    return(eval_tidy(p))
}