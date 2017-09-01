###################################################
# allyCount.R
# 
# Author: Helena Edlund
# Created on: 2017-08-09
# Modified on: 2017-08-14
# Purpose: 
# Dependencies: ggplot2, rlang, vcd
###################################################

allyCount <- function(data, mapping, numbers="abs", ...){
  # numbers can be "abs" or "rel"
  
  xCol <- data[[deparse(mapping$x)]]
  yCol <- data[[deparse(mapping$y)]]
  
  if(!(is.factor(xCol) & is.factor(yCol))){
    stop(paste(deparse(mapping$x), "or", deparse(mapping$x),"is not a factor"))
  }
  
  if(numbers=="abs"){
    countN <- function(x){
      return(c( y=unique(x), label=length(x)))
    }
    
    p <- ggplot(data, mapping) +
      geom_count(colour="#bdbdbd", shape=16) + 
      scale_size_area() + guides(size = "none") +
      stat_summary(fun.data=countN, geom = "text", size=3) + 
      theme(axis.text.x = element_text(angle = 35, hjust=1),
            axis.text.y = element_text(angle = 35, vjust=0))
  } 
  
  if(numbers=="rel"){ 
    
    if(length(levels(xCol)) > 1 | length(levels(yCol))>1){
      
      # handling of NAs in plot
      if(any(is.na(xCol))){
        xMax <- length(levels(xCol))+1
      } else {
        xMax <- length(levels(xCol))
      }
      
      if(any(is.na(yCol))){
        yMax <- length(levels(yCol))+1
      }else {
        yMax <- length(levels(yCol))
      }
      
      # Get the contingency table
      contingencyTab <- table(yCol, xCol, useNA="ifany")
      contingencyTab <- as.data.frame(contingencyTab)
      
      # Summarize for x
      xColSum <- contingencyTab %>% 
        group_by(xCol) %>% 
        mutate(total = sum(Freq), 
               relative = ifelse(total!=0, 
                                 round(100*(Freq / total), digits=0), 
                                 0),
               y = ifelse(is.na(yCol),
                          yMax+0.35,
                          as.numeric(yCol)+0.35)) # need to round
      
      # Summarize for y
      yColSum <- contingencyTab %>% 
        group_by(yCol) %>% 
        mutate(total = sum(Freq), 
               relative = ifelse(total!=0, 
                                 round(100*(Freq / total), digits=0), 
                                 0),
               x=ifelse(is.na(xCol),
                        xMax-0.35,
                        as.numeric(xCol)-0.35)) # need to round
    } else {
      warning("All data in the same level.")
    }
    
    p <- ggplot(data, mapping) +
      geom_point(alpha=0) + # invisible layer to set the axes properly
      
      # add "anchor points" 
      geom_point(data=xColSum, aes(y=yCol, x=xCol), colour="gray15",
                 shape=16, size=1.5) +
      
      # Add numbers relative y-axis
      geom_text(data=xColSum, aes(y=y, x=xCol, label=relative), 
                size=2.8, colour="gray15") + 
      
      # Add numbers relative x-axis
      geom_text(data=yColSum, aes(y=yCol, x=x, label=relative), 
                size=2.8, colour="gray15") +
      
      coord_cartesian(xlim=c(0.75, seq(1, xMax, by=1)), 
                      ylim=c(seq(1, yMax, by=1), yMax+0.25)) + 
      
      theme(axis.text.x = element_text(angle = 35, hjust=1),
            axis.text.y = element_text(angle = 35, vjust=0))
  } 
  
  return(p)
}
