###################################################
# allyScatterLmCor.R
# 
# Author: Helena Edlund
# Created on: 2017-08-09
# Modified on:
# Purpose: linear correlation with coefficient
# Dependencies: ggplot2, ggally
###################################################

allyScatterLmCor <- 
  function(data, mapping, 
           corThreshold=0.4,
           corMethod = "pearson", ...) {
    
  xCol <- data[[deparse(mapping$x)]]
  yCol <- data[[deparse(mapping$y)]]
  
  # Calculate correlation only if there are values different from zero
  # (important for ETA corr plots)
  if(all(xCol==0) | all(yCol==0)){
    cor <- 0
  }else{
    cor <- cor(xCol, yCol, 
               method=corMethod,
               use="pairwise.complete.obs")
  }
  
  # Set to red colour if more or less than threshold
  if(cor <= -corThreshold | cor >= corThreshold) {
    pointColour <- "red" 
  }else{
    pointColour <- "black"
  }
  
  # Plot
  if(!all(xCol==0) & !all(yCol==0)){
    p <- 
      ggplot(data, mapping) + 
      geom_point(colour=pointColour) + 
      geom_smooth(method="lm")
  }else{
    p <- ggplot(data, mapping) + 
      geom_point()
  }
  return(p)
}