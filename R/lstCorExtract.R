###################################################
# lstCorExtract.R
# 
# Author: Helena Edlund
# Created on: 2017-03-23
# Modified on: 2017-08-02
# Purpose: extract correlation pars from read.lst
# Dependencies: xpose
###################################################

lstCorExtract <- function(listobject, omegas, correlations, occ=F) {
  
  if(occ==F){
    par1 <- paste0("BSV_", correlations[1])
    par2 <- paste0("BSV_", correlations[2])
  } else {
    par1 <- paste0("IOV_", correlations[1])
    par2 <- paste0("IOV_", correlations[2])
  }
  
  position1 <- which(omegas %in% par1)
  position2 <- which(omegas %in% par2)
  
  pos1 <- min(position1, position2)
  pos2 <- max(position1, position2)
  
  var1 <- listobject$omega[[pos1]][[pos1]]
  var2 <- listobject$omega[[pos2]][[pos2]]

  covariance   <- listobject$omega[[pos2]][pos1]
  secovariance <- listobject$seomega[[pos2]][pos1]
  rse <- (secovariance/covariance)*100
    
  correlation  <- covariance / (sqrt(var1) * sqrt(var2))

  return(data.frame(par = paste("Cor",correlations[1],correlations[2], sep="_"),
                    estimate = signif(correlation, digits=3),
                    CV = NA,
                    SD = NA,
                    SE = signif(secovariance, digits=3),
                    RSE = signif(abs(rse), digits=3),
                    Shrink = NA))
}

