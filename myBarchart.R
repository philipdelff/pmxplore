myBarchart <- function(data, mapping, numbers = F ,...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_bar(fill="#a0a0a0", colour="#898989") +
    theme(axis.text.x = element_text(angle=40, hjust=1),
          panel.grid = element_blank()) 
  if(numbers==T) {
    p <- p + geom_text(aes(label = ..count.., y= (..count..)),
                       stat= "count",fontface = "bold")
  }
  return(p)
}
