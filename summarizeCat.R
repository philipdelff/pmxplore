###################################################
# summarizeCat.R
# 
# Author: Helena Edlund
# Created on: 2017-08-03
# Modified on: 
# Purpose: Summarizing categorical covariates
# Dependencies: dplyr, rlang
###################################################

summarizeCat <- function(df, colName){
  
  symColName <- rlang::sym(colName)
  
  tab <- rlang::quo(df %>% 
    group_by(Category = !!symColName) %>% 
    summarise(N = n()) %>%
    mutate(Percent = 100*(N / sum(N)), 
           Characteristic = colName, 
           Category = as.character(Category), 
           Category = ifelse(is.na(Category), "Missing", Category)) %>% 
    select(Characteristic, Category, N, Percent))

  tab <- eval_tidy(tab)
  
  return(as.data.frame(tab))
}
