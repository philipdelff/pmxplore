###################################################
# allyCor.R
# 
# Author: Helena Edlund
# Created on: 2017-08-09
# Modified on:
# Purpose: linear correlation with coefficient
# Dependencies: ggplot2, ggally
###################################################

allyCor <- function(data, mapping, 
                    corThreshold=0.4,
                    corMethod = "pearson", ...) {
  
  xCol <- data[[deparse(mapping$x)]]
  yCol <- data[[deparse(mapping$y)]]
  
  ## If numeric columns 
  if(is.numeric(xCol) & is.numeric(yCol)){
    
    # Calculate correlation only if there are values different from zero
    # (important for ETA corr plots)
    if(all(xCol==0) | all(yCol==0)){
      cor <- 0
      corDf <- data.frame(
        posX = (max(xCol, na.rm = TRUE)-min(xCol, na.rm = TRUE))/2,
        posY = (max(yCol, na.rm = TRUE)-min(yCol, na.rm = TRUE))/2,
        lab = "Corr:\nNA")
    }else{
      cor <- cor(xCol, yCol, 
                 method=corMethod,
                 use="pairwise.complete.obs")
      corDf <- data.frame(
        posX = (max(xCol, na.rm = TRUE)-min(xCol, na.rm = TRUE))/2,
        posY = (max(yCol, na.rm = TRUE)-min(yCol, na.rm = TRUE))/2,
        lab = paste0("Corr:\n", round(cor, digits = 3)))
    }
  }
  
  ## If categorical columns 
  if(is.factor(xCol) & is.factor(yCol)){
    # Calculate correlation if more than 1 level
    if(length(levels(xCol)) > 1 & length(levels(yCol))>1){
      
      contingencyTab <- table(yCol, xCol)
      cor <- vcd::assocstats(contingencyTab)$cramer
      
      # Correct position if vector includes NA
      if(any(is.na(xCol))){
        addLevelx <- 1 
      }else{
        addLevelx <- 0
      }
      if(any(is.na(yCol))){
        addLevely <- 1 
      }else{
        addLevely <- 0
      }
      corDf <- data.frame(
        posX = (length(levels(xCol))+addLevelx)/2,
        posY = (length(levels(yCol))+addLevely)/2,
        lab = paste0("Corr:\n", round(cor, digits = 3)))
    }else{
      cor <- 0
      corDf <- data.frame(
        posX = (length(levels(xCol))+addLevelx)/2,
        posY = (length(levels(yCol))+addLevely)/2,
        lab = "Corr:\n NA")
    }
  }
  
  # Set to red colour if more or less than threshold
  if(cor <= -corThreshold | cor >= corThreshold) {
    textColour <- "red" 
  }else{
    textColour <- "black"
  }
  
  # Plot
    p <- 
      ggplot(data) + 
      geom_text(data=corDf, 
                 aes(x=posX, y=posY, label=lab), 
                 col=textColour) + 
      theme(panel.grid = element_blank())

    return(p)
}