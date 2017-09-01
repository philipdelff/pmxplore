###################################################
# summarizeRun.R
# 
# Author: Helena Edlund
# Created on: 2017-03-23
# Modified on:
# Purpose: Generate functions used troughout project
# Dependencies: s00_main.R (for libraries)
#               s01_projectVariables.R
###################################################

# ------------------------------------------------------------------
# Functions for extracting parameter estimates and precision and merge to data.frame
# ------------------------------------------------------------------
summarizeRun <- function(modNo, refModNo, listobject, record, descr){
  summary <- 
    data.frame(Run = as.character(modNo),
               Minimization = listobject$term[1],
               OFV = as.numeric(as.character(listobject$ofv)),
               Reference = as.numeric(refModNo),
               dOFV = ifelse(is.na(refModNo), NA, 
                             as.numeric(as.character(listobject$ofv)) - 
                               record$OFV[record$Run==refModNo]),
               Description = descr)
  return(summary)
}