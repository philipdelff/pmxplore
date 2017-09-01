###################################################
# allyBox.R
# 
# Author: Helena Edlund
# Created on: 2017-08-09
# Modified on:
# Purpose: 
# Dependencies: ggplot2, ggally
###################################################
allyBox <- function(data, mapping, ...){
  
  xCol <- data[[deparse(mapping$x)]]
  yCol <- data[[deparse(mapping$y)]]
  
  p <- ggplot(data, mapping) + 
      geom_boxplot() +
      theme(axis.text.x = element_text(angle=40, hjust=1),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
  
  # consider adding correlation
  return(p)
}