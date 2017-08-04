
countNSize <- function(x){
  return(c( y=unique(x), label=length(x)))
}

ggCount <- function(df, x, y, 
                        corThreshold=0.4, 
                        numbers=F, ...){
  x <- enexpr(x)
  y <- enexpr(y)
  
  xCol <- df[[expr_text(x)]]
  yCol <- df[[expr_text(y)]]
  

  if(!is.factor(xCol) | !is.factor(yCol)){
    stop(paste("Plot requires categorical input.",
               expr_text(x),"or", expr_text(y), "is not a factor"))
  }
  
  # Calculate correlation
  contingencyTab <- table(yCol, xCol)
  cor <- assocstats(contingencyTab)$cramer
  corDf <- data.frame(
    posX = (length(levels(xCol))+0.5),
    posY = (length(levels(yCol))+0.5),
    lab = round(cor, digits = 3))
  
  if(cor <= -corThreshold | cor >= corThreshold) {
    textColour <- "red" 
  }else{
    textColour <- "black"
  }
  
  # Plot
  p <- rlang::quo(
    ggplot(data=df, aes(x=!!x, y=!!y)) +
      geom_count() + scale_size_area() + guides(size = "none") +
      geom_label(data=corDf, aes(x=posX, y=posY, label=lab), 
                 fontface = "bold", col=textColour) +
      
      coord_cartesian(xlim=c(1, corDf$posX), 
                      ylim=c(1, corDf$posY)) + 
      theme(axis.text.x = element_text(angle = 35, hjust=1),
            axis.text.y = element_text(angle = 35, vjust=0))
  )
  if(numbers){
  p <- rlang::quo(
    ggplot(data=df, aes(x=!!x, y=!!y)) +
      geom_count() + scale_size_area() + guides(size = "none") +
      geom_label(data=corDf, aes(x=posX, y=posY, label=lab), 
                 fontface = "bold", col=textColour) +
      stat_summary(fun.data = countNSize, geom = "text",
                   hjust=-1, vjust=-1) + 
      coord_cartesian(xlim=c(1, corDf$posX), 
                      ylim=c(1, corDf$posY)) + 
      theme(axis.text.x = element_text(angle = 35, hjust=1),
            axis.text.y = element_text(angle = 35, vjust=0))
    # numbers does not work well in the scenario of many covariates/small plot 
    )
 } 
  return(eval_tidy(p))
}
