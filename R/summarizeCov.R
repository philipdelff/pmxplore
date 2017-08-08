###################################################
# summarizeCov.R
# 
# Author: Helena Edlund
# Created on: 2017-08-03
# Modified on: 
# Purpose: wrapper for summarizing list of covariates
# Dependencies: 
###################################################

summarizeCov <- function(df, covVector, type = "Cont", ...){
  
  if(type == "Cont"){
    summaryFun <- summarizeCont
  }
  if(type == "Cat"){
    summaryFun <- summarizeCat
  }

  for(i in unique(covVector)) {
    tmp <- summaryFun(df, i, ...)
    if(i == covVector[1]){
      out <- tmp
    }else{
      out <- rbind(out, tmp)
    }
  }
  return(out)
}
