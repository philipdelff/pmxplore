
countNSize <- function(x){
  return(c(y = unique(x) + 0.1, label = length(x)))
}
myCountPlot <- function(data, mapping, numbers = F,...){
  p <- ggplot(data=data, mapping = mapping) +
    geom_count(fill="#a0a0a0", colour="#898989") +
    scale_size_area() + guides(size = "none") +
    theme(axis.text.x = element_text(angle = 35, hjust=1),
          axis.text.y = element_text(angle = 35, vjust=0))
  
  if(numbers == T){
    p <- p + stat_summary(fun.data = countNSize, geom = "text", hjust=-0.5)
  } 
  return(p)
  # Needs to be improved. Should add correlation coefficient + number does not work well and difficult to see. 
}
