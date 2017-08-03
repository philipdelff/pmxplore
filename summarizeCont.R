###################################################
# summarizeCont.R
# 
# Author: Helena Edlund
# Created on: 2018-08-03
# Modified on: 
# Purpose: Summarizing continuous covariates
# Dependencies: dplyr (>0.7), rlang
###################################################

summarizeCont <- function(df, colName, p=c(0.05, 0.95)){
  symColName <- rlang::sym(colName)
  # colName <- rlang::enexpr(colName)
  
  tab <- rlang::quo(df %>% dplyr::summarise(
    Characteristic = colName,
    N        = sum(!is.na(!!symColName) & !!symColName!=-99),
    NMissing = sum(is.na(!!symColName) | !!symColName==-99),
    Mean     = mean(!!symColName, na.rm = T), 
    SD       = sd(!!symColName, na.rm = T), 
    Min      = min(!!symColName, na.rm = T),
    Median   = quantile(!!symColName,probs =0.50, na.rm = T, names=F), 
    Max      = max(!!symColName, na.rm = T)))
  
  tab <- eval_tidy(tab) 
  
  for(i in 1:length(p)){
    nameP <- paste0("p",p[i])
    colP <- rlang::quo(df %>% summarize(
      !!nameP := quantile(!!symColName, probs=p[i], na.rm = T, names=F)))
    tab <- cbind(tab, eval_tidy(colP))
  }
  return(tab)
}
