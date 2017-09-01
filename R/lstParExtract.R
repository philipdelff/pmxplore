###################################################
# lstParExtract.R
# 
# Author: Helena Edlund
# Created on: 2017-03-23
# Modified on: 2017-08-15
# Purpose: Extract thetas and omegas using read.lst
# Dependencies: xpose lst object, dplyr
###################################################

lstParExtract <- function(listobject, 
                          thetaPar, omegaPar, sigmaPar, 
                          signif=3) {
  
  nThetaPar <- length(thetaPar)
  nOmegaPar <- length(omegaPar)
  nSigmaPar <- length(sigmaPar)
  
  # thetas
  tPars <- data.frame(par=thetaPar, stringsAsFactors=F)
  tPars$estimate <- listobject$thetas
  tPars$SE <- listobject$sethetas
  tPars$RSE <- (listobject$sethetas /tPars$estimate)*100

  # omegas
  oPars <- data.frame(par=omegaPar, stringsAsFactors=F)
  
  for(i in 1:nOmegaPar){
    diag <- listobject$omega[[i]][[i]]
    if(i==1){
      estimate <- diag
    }else{
      estimate <- c(estimate, diag)
    }
  }
  oPars$estimate <- estimate
  oPars$CV <- sqrt(oPars$estimate)*100
  for(i in 1:nOmegaPar){
    diag <- listobject$seomega[[i]][[i]]
    if(i==1){
      estimate <- diag
    }else{
      estimate <- c(estimate, diag)
    }
  }
  oPars$SE <- estimate
  oPars$RSE <- (oPars$SE/oPars$estimate)*100
  
  # shrinkage
  if(nOmegaPar <= 10){
    oPars$Shrink <- as.numeric(
      str_split(listobject$term[str_detect(listobject$term, "ETAshrink")], "  ")[[1]][-1])
    # oPars$ebvShrink <- as.numeric(
    #   str_split(listobject$term[str_detect(listobject$term, "EBVshrink")], "  ")[[1]][-1])
  } else {
    oPars$Shrink <- rep(NA, nOmegaPar)
    oPars$Shrink[1:10] <- as.numeric(
      str_split(listobject$term[str_detect(listobject$term, "ETAshrink")], "  ")[[1]][-1])
    pos <- which(str_detect(listobject$term, "ETAshrink") == "TRUE") + 1
    oPars$Shrink[11:nOmegaPar] <- as.numeric(
      str_split(listobject$term[pos], "  ")[[1]][str_split(listobject$term[pos], "  ")[[1]] != ""])
    }
  
  # sigmas
  sPars <- data.frame(par = sigmaPar, stringsAsFactors=F)
  for(i in 1:nSigmaPar){
    diag <- listobject$sigma[[i]][[i]]
    if(i==1){
      estimate <- diag
    }else{
      estimate <- c(estimate, diag)
    }
  }
  sPars$estimate <- estimate
  sPars$SD <- sqrt(sPars$estimate)
  for(i in 1:nSigmaPar){
    diag <- listobject$sesigma[[i]][[i]]
    if(i==1){
      estimate <- diag
    }else{
      estimate <- c(estimate, diag)
    }
  }
  sPars$SE <- estimate
  sPars$RSE <- (sPars$SE / sPars$estimate)*100
  
  # shrinkage
  sPars$Shrink <- as.numeric(
    str_split(listobject$term[str_detect(listobject$term, "EPSshrink")], "  ")[[1]][-1])
  
  
  # Bind all parameters together
  Pars <- dplyr::bind_rows(tPars, oPars, sPars)

  ## Round and format
  Pars$RSE      <- abs(Pars$RSE)
  Pars[,c("estimate", "SE", "RSE", "CV", "Shrink", "SD")] <- 
    signif(Pars[,c("estimate", "SE", "RSE", "CV", "Shrink", "SD")], 
           digits=signif)

  return(Pars[,c("par", "estimate", "CV", "SD", "SE", "RSE", "Shrink")])
}
