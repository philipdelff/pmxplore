###################################################
# indDataSplit.R
# 
# Author: Helena Edlund
# Created on: 2017-08-04
# Modified on:
# Purpose: Generate list of dataset for individual graphs
# Dependencies: zoo
###################################################

indDataSplit <- function(df, nPerPage=12, id="ID"){
  # Generates a list of dataset subsets to get nSplit plots per page

  # Define number of splits
  nId <- length(unique(df[,id]))
  nSplit <- ceiling(nId/nPerPage)
  
  # Add splitting columns to dataset
  df$Splits[!duplicated(df[,id])] <-
    rep(1:nSplit, each=nPerPage, length.out=nId)
  df$Splits <- na.locf(df$Splits)
  
  # generate a list with nSplit datasets
  dataSplits <- split(df, df$Splits)
  
  return(dataSplits)
}
