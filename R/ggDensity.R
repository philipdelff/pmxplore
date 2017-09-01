###################################################
# ggDensity.R
# 
# Author: Helena Edlund
# Created on: 2017-08-18
# Modified on: 
# Purpose: qq plots
# Dependencies: ggplot2, rlang
###################################################

ggDensity <- function(df, x, nBin=6, wEst=F, dist=NULL, var=NULL, ...){
  
  x <- rlang::enexpr(x)
  nBin <- rlang::enexpr(nBin)
  dist <- rlang::enexpr(dist)
  var <- rlang::enexpr(var)
  
  if(wEst){
    p <- rlang::quo(
      ggplot(df) + 
        # add distribution with estimated pars
        stat_function(fun=!!dist, args = list(sd=sqrt(!!var)), 
                      linetype="dashed", col="#898989") +
        geom_freqpoly(aes(!!x, ..density..), bins=!!nBin)
    )
  } else {
    p <- rlang::quo(
      ggplot(df) + geom_freqpoly(aes(!!x, ..density..), bins=!!nBin)
    )
  }

  return(eval_tidy(p))
}