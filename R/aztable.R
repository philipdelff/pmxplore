###################################################
# aztable.R
# 
# Author: Helena Edlund
# Created on: 2017-08-02
# Modified on: 2017-08-03
# Purpose: Wrapper for az specifications of pixiedust tables
# Dependencies: github(nutterb/pixiedust)
###################################################

#' @title az specific pixidust table
#' @description  Wrapper for az specifications of pixiedust tables
#' @param df data frame
#' @param caption string with caption for table, Default: NULL
#' @param label string with label for cross-reference. "tab:" is automatically added, Default: NULL
#' @param print_method "latex" or "html",  Default: "latex" 
#' @param long use longtable environment (for latex-use only), Default: F
#' @return pixiedust object
#' @rdname aztable
#' @export 

aztable <- function(df, caption=NULL, label=NULL,
                    print_method="latex", long=F){
  # To fix: 
  # Probably good idea to set a default for significant digits of numeric cols
  # Border thickness does for some reason not work:  
  # Trying to update latex packages on cluster to see it that solves it
  
  if(!long & print_method!="latex") {
    long <- F
    warning("Longtable environment only supported for LaTeX. Argument ignored")
  }
  
  tab <- 
    dust(df,
         longtable=long,
         hhline=T, 
         caption = caption, 
         label = label) %>% 
    sprinkle(part="head", bold=T, 
             border=c("top", "bottom"), 
             border_thickness=3, border_units="pt") %>% 
    sprinkle(row=nrow(df), border=c("bottom"), 
             border_thickness=3, border_units="pt") %>% 
    sprinkle_print_method(print_method)
  
  return(tab)
}
