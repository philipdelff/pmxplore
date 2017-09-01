###################################################
# allyBar.R
# 
# Author: Helena Edlund
# Created on: 2017-08-09
# Modified on:
# Purpose: 
# Dependencies: ggplot2, ggally
###################################################
allyBar <- function(data, mapping, numbers=F, ...){
  
  p <- ggplot(data, mapping) + 
    geom_bar() +
    theme(axis.text.x = element_text(angle=40, hjust=1),
          panel.grid = element_blank()) 
  
  if(numbers){
    p <- ggplot(data, mapping) + 
      geom_bar() +
      theme(axis.text.x = element_text(angle=40, hjust=1),
            panel.grid = element_blank()) +
      geom_label(aes(label=..count.., y=(..count..)),
                 stat= "count", fontface="bold")
    # numbers does not work well in the scenario of many covariates
  }
  return(p)
}