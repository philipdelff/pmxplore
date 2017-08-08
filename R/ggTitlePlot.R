###################################################
# ggTitlePlot.R
# 
# Author: Helena Edlund
# Created on: 2017-03-23
# Modified on: 2017-08-02
# Purpose: Plot to serve as divider page for multiple plot pdfs
# Dependencies: ggplot2, rlang
###################################################

ggTitlePlot <- function(title, txtSize=18){
  
  tmp <- data.frame(x=5, y=5, label=title)
    
  p <- ggplot(data=tmp) + 
  geom_text(aes(x=x, y=y, label=label), size=txtSize) +
  coord_cartesian(xlim=c(0,10), ylim=c(0,10)) + 
  theme_bw() + 
  theme(axis.title = element_blank(),
       axis.text = element_blank(),
       axis.ticks = element_blank(),
       panel.border = element_blank(),
       panel.grid = element_blank()
     )
  return(p)
}