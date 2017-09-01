###################################################
# aztable.R
# 
# Author: Helena Edlund
# Created on: 2017-08-02
# Modified on: 2017-08-03
# Purpose: Wrapper for az specifications of pixiedust tables
# Dependencies: github(nutterb/pixiedust)
###################################################

aztable <- function(dataframe, caption=NULL, label=NULL, long = F){
  table <- 
    dust(dataframe,
         longtable=long,
         hhline=T, 
         caption = caption, 
         label = label) %>% # label without the "tab:" (added automatically)
    sprinkle(part="head", bold=T, 
             border=c("top", "bottom"), 
             border_thickness=3, border_units="pt") %>% 
    sprinkle(row=nrow(dataframe), border=c("bottom"), 
             border_thickness=3, border_units="pt") %>% 
    sprinkle_print_method("latex")
  return(table)
  # To fix: 
  # Probably good idea to set a default for significant digits of numeric cols
  # Border thickness does for some reason not work:  
  # Trying to update latex packages on cluster to see it that solves it
}
