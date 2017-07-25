myBoxplot <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_boxplot(fill="#a0a0a0", outlier.shape=1) +
    theme(axis.text.x = element_text(angle=40, hjust=1),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  return(p)
}
